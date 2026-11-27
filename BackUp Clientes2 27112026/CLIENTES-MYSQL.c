#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef void * SQLHANDLE;
typedef short SQLRETURN;
typedef short SQLSMALLINT;
typedef unsigned short SQLUSMALLINT;
typedef int SQLINTEGER;
typedef unsigned long SQLULEN;
typedef long SQLLEN;
typedef unsigned char SQLCHAR;
typedef void * SQLPOINTER;
typedef void * HMODULE;

#define SQL_SUCCESS 0
#define SQL_SUCCESS_WITH_INFO 1
#define SQL_NO_DATA 100
#define SQL_HANDLE_ENV 1
#define SQL_HANDLE_DBC 2
#define SQL_HANDLE_STMT 3
#define SQL_ATTR_ODBC_VERSION 200
#define SQL_OV_ODBC3 3
#define SQL_DRIVER_NOPROMPT 0
#define SQL_C_CHAR 1
#define SQL_VARCHAR 12
#define SQL_PARAM_INPUT 1
#define SQL_NTS -3

__declspec(dllimport) HMODULE __stdcall LoadLibraryA(const char *);
__declspec(dllimport) void * __stdcall GetProcAddress(HMODULE, const char *);
__declspec(dllimport) int __stdcall FreeLibrary(HMODULE);

typedef SQLRETURN (__stdcall *ALLOC_HANDLE)(
    SQLSMALLINT, SQLHANDLE, SQLHANDLE *);
typedef SQLRETURN (__stdcall *SET_ENV_ATTR)(
    SQLHANDLE, SQLINTEGER, SQLPOINTER, SQLINTEGER);
typedef SQLRETURN (__stdcall *DRIVER_CONNECT)(
    SQLHANDLE, void *, SQLCHAR *, SQLSMALLINT, SQLCHAR *,
    SQLSMALLINT, SQLSMALLINT *, SQLUSMALLINT);
typedef SQLRETURN (__stdcall *DISCONNECT)(SQLHANDLE);
typedef SQLRETURN (__stdcall *FREE_HANDLE)(SQLSMALLINT, SQLHANDLE);
typedef SQLRETURN (__stdcall *EXEC_DIRECT)(
    SQLHANDLE, SQLCHAR *, SQLINTEGER);
typedef SQLRETURN (__stdcall *PREPARE)(
    SQLHANDLE, SQLCHAR *, SQLINTEGER);
typedef SQLRETURN (__stdcall *BIND_PARAMETER)(
    SQLHANDLE, SQLUSMALLINT, SQLSMALLINT, SQLSMALLINT,
    SQLSMALLINT, SQLULEN, SQLSMALLINT, SQLPOINTER,
    SQLLEN, SQLLEN *);
typedef SQLRETURN (__stdcall *EXECUTE)(SQLHANDLE);
typedef SQLRETURN (__stdcall *FETCH)(SQLHANDLE);
typedef SQLRETURN (__stdcall *GET_DATA)(
    SQLHANDLE, SQLUSMALLINT, SQLSMALLINT, SQLPOINTER,
    SQLLEN, SQLLEN *);
typedef SQLRETURN (__stdcall *ROW_COUNT)(SQLHANDLE, SQLLEN *);
typedef SQLRETURN (__stdcall *GET_DIAG_REC)(
    SQLSMALLINT, SQLHANDLE, SQLSMALLINT, SQLCHAR *,
    SQLINTEGER *, SQLCHAR *, SQLSMALLINT, SQLSMALLINT *);

static HMODULE odbc_module;
static SQLHANDLE env_handle;
static SQLHANDLE dbc_handle;
static SQLHANDLE list_handle;
static ALLOC_HANDLE api_alloc_handle;
static SET_ENV_ATTR api_set_env_attr;
static DRIVER_CONNECT api_driver_connect;
static DISCONNECT api_disconnect;
static FREE_HANDLE api_free_handle;
static EXEC_DIRECT api_exec_direct;
static PREPARE api_prepare;
static BIND_PARAMETER api_bind_parameter;
static EXECUTE api_execute;
static FETCH api_fetch;
static GET_DATA api_get_data;
static ROW_COUNT api_row_count;
static GET_DIAG_REC api_get_diag_rec;
static char last_error[256];
static char last_state[6];

static int success(SQLRETURN rc)
{
    return rc == SQL_SUCCESS || rc == SQL_SUCCESS_WITH_INFO;
}

static void set_error(const char *text)
{
    strncpy(last_error, text, sizeof(last_error) - 1);
    last_error[sizeof(last_error) - 1] = '\0';
    last_state[0] = '\0';
}

static void get_error(SQLSMALLINT handle_type, SQLHANDLE handle)
{
    SQLINTEGER native_error = 0;
    SQLSMALLINT message_length = 0;

    set_error("ODBC operation failed");
    if (api_get_diag_rec == NULL || handle == NULL)
        return;

    api_get_diag_rec(handle_type, handle, 1,
        (SQLCHAR *)last_state, &native_error,
        (SQLCHAR *)last_error, (SQLSMALLINT)sizeof(last_error),
        &message_length);
    last_error[sizeof(last_error) - 1] = '\0';
}

static void *load_symbol(const char *name)
{
    return GetProcAddress(odbc_module, name);
}

static int load_odbc(void)
{
    if (odbc_module != NULL)
        return 0;

    odbc_module = LoadLibraryA("odbc32.dll");
    if (odbc_module == NULL) {
        set_error("Windows ODBC driver manager is unavailable");
        return -1;
    }

#define LOAD_API(field, symbol) \
    field = (void *)load_symbol(symbol); \
    if (field == NULL) { \
        set_error("Windows ODBC API is incomplete"); \
        return -1; \
    }
    LOAD_API(api_alloc_handle, "SQLAllocHandle")
    LOAD_API(api_set_env_attr, "SQLSetEnvAttr")
    LOAD_API(api_driver_connect, "SQLDriverConnectA")
    LOAD_API(api_disconnect, "SQLDisconnect")
    LOAD_API(api_free_handle, "SQLFreeHandle")
    LOAD_API(api_exec_direct, "SQLExecDirectA")
    LOAD_API(api_prepare, "SQLPrepareA")
    LOAD_API(api_bind_parameter, "SQLBindParameter")
    LOAD_API(api_execute, "SQLExecute")
    LOAD_API(api_fetch, "SQLFetch")
    LOAD_API(api_get_data, "SQLGetData")
    LOAD_API(api_row_count, "SQLRowCount")
    LOAD_API(api_get_diag_rec, "SQLGetDiagRecA")
#undef LOAD_API
    return 0;
}

static void field_to_string(char *out, size_t out_size,
                            const char *in, size_t in_size)
{
    size_t length = in_size;

    while (length > 0 && (in[length - 1] == ' ' || in[length - 1] == '\0'))
        --length;
    if (length >= out_size)
        length = out_size - 1;
    memcpy(out, in, length);
    out[length] = '\0';
}

static void string_to_field(char *out, size_t out_size, const char *in)
{
    size_t length = strlen(in);

    memset(out, ' ', out_size);
    if (length > out_size)
        length = out_size;
    memcpy(out, in, length);
}

static void clear_secret(char *secret)
{
    volatile char *p = secret;
    while (*p != '\0')
        *p++ = '\0';
}

static SQLRETURN execute_direct(const char *sql)
{
    SQLHANDLE stmt = NULL;
    SQLRETURN rc = api_alloc_handle(SQL_HANDLE_STMT, dbc_handle, &stmt);

    if (!success(rc)) {
        get_error(SQL_HANDLE_DBC, dbc_handle);
        return rc;
    }
    rc = api_exec_direct(stmt, (SQLCHAR *)sql, SQL_NTS);
    if (!success(rc))
        get_error(SQL_HANDLE_STMT, stmt);
    api_free_handle(SQL_HANDLE_STMT, stmt);
    return rc;
}

static SQLHANDLE prepare_statement(const char *sql)
{
    SQLHANDLE stmt = NULL;
    SQLRETURN rc = api_alloc_handle(SQL_HANDLE_STMT, dbc_handle, &stmt);

    if (!success(rc)) {
        get_error(SQL_HANDLE_DBC, dbc_handle);
        return NULL;
    }
    rc = api_prepare(stmt, (SQLCHAR *)sql, SQL_NTS);
    if (!success(rc)) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return NULL;
    }
    return stmt;
}

static int bind_text(SQLHANDLE stmt, SQLUSMALLINT position,
                     char *value, SQLLEN capacity, SQLULEN column_size,
                     SQLLEN *length)
{
    *length = (SQLLEN)strlen(value);

    return success(api_bind_parameter(
        stmt, position, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR,
        column_size, 0, value, capacity, length));
}

static int append_password(char *out, size_t capacity, size_t *used,
                           const char *password)
{
    size_t i;
    int count = snprintf(out + *used, capacity - *used, "PASSWORD={");
    if (count < 0 || (size_t)count >= capacity - *used)
        return -1;
    *used += (size_t)count;
    for (i = 0; password[i] != '\0'; ++i) {
        if (*used + 3 >= capacity)
            return -1;
        if (password[i] == '}')
            out[(*used)++] = '}';
        out[(*used)++] = password[i];
    }
    if (*used + 3 >= capacity)
        return -1;
    out[(*used)++] = '}';
    out[(*used)++] = ';';
    out[*used] = '\0';
    return 0;
}

int mysql_connect(char *host_field, char *port_field,
                  char *database_field, char *user_field,
                  char *password_field)
{
    char host[33], port[6], database[33], user[33], password[128];
    char connect_string[512];
    char sql[256];
    size_t used = 0;
    int connect_length;
    SQLRETURN rc;

    if (load_odbc() != 0)
        return -1;
    field_to_string(host, sizeof(host), host_field, 32);
    field_to_string(port, sizeof(port), port_field, 5);
    field_to_string(database, sizeof(database), database_field, 32);
    field_to_string(user, sizeof(user), user_field, 32);
    field_to_string(password, sizeof(password), password_field, 128);
    if (host[0] == '\0' || port[0] == '\0' ||
        database[0] == '\0' || user[0] == '\0') {
        set_error("MySQL connection settings are incomplete");
        clear_secret(password);
        return -1;
    }
    if (strspn(database, "abcdefghijklmnopqrstuvwxyz"
                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") !=
        strlen(database)) {
        set_error("Invalid MySQL database name");
        clear_secret(password);
        return -1;
    }

    rc = api_alloc_handle(SQL_HANDLE_ENV, NULL, &env_handle);
    if (!success(rc)) {
        get_error(SQL_HANDLE_ENV, env_handle);
        clear_secret(password);
        return -1;
    }
    rc = api_set_env_attr(env_handle, SQL_ATTR_ODBC_VERSION,
                          (SQLPOINTER)SQL_OV_ODBC3, 0);
    if (!success(rc)) {
        get_error(SQL_HANDLE_ENV, env_handle);
        clear_secret(password);
        return -1;
    }
    rc = api_alloc_handle(SQL_HANDLE_DBC, env_handle, &dbc_handle);
    if (!success(rc)) {
        get_error(SQL_HANDLE_ENV, env_handle);
        clear_secret(password);
        return -1;
    }

    strcpy(connect_string, "DRIVER={MySQL ODBC 8.0 Unicode Driver};");
    used = strlen(connect_string);
    connect_length = snprintf(connect_string + used,
        sizeof(connect_string) - used,
        "SERVER=%s;PORT=%s;USER=%s;", host, port, user);
    if (connect_length < 0 ||
        (size_t)connect_length >= sizeof(connect_string) - used) {
        set_error("MySQL connection settings are too long");
        clear_secret(password);
        clear_secret(connect_string);
        return -1;
    }
    used += (size_t)connect_length;
    if (append_password(connect_string, sizeof(connect_string), &used,
                        password) != 0) {
        set_error("MySQL connection settings are too long");
        clear_secret(password);
        clear_secret(connect_string);
        return -1;
    }
    rc = api_driver_connect(dbc_handle, NULL,
        (SQLCHAR *)connect_string, SQL_NTS, NULL, 0, NULL,
        SQL_DRIVER_NOPROMPT);
    clear_secret(password);
    clear_secret(connect_string);
    if (!success(rc)) {
        get_error(SQL_HANDLE_DBC, dbc_handle);
        return -1;
    }

    snprintf(sql, sizeof(sql),
        "CREATE DATABASE IF NOT EXISTS `%s` CHARACTER SET utf8mb4",
        database);
    if (!success(execute_direct(sql)))
        return -1;
    snprintf(sql, sizeof(sql), "USE `%s`", database);
    if (!success(execute_direct(sql)))
        return -1;
    if (!success(execute_direct(
        "CREATE TABLE IF NOT EXISTS clientes ("
        "telefone CHAR(9) NOT NULL PRIMARY KEY,"
        "nome VARCHAR(30) NOT NULL,"
        "email VARCHAR(40) NOT NULL"
        ") CHARACTER SET utf8mb4"))) {
        return -1;
    }
    return 0;
}

int mysql_insert(char *phone_field, char *name_field, char *email_field)
{
    SQLHANDLE stmt = prepare_statement(
        "INSERT INTO clientes (telefone, nome, email) VALUES (?, ?, ?)");
    SQLRETURN rc;
    SQLLEN phone_length, name_length, email_length;
    char phone[10], name[31], email[41];

    if (stmt == NULL)
        return -1;
    field_to_string(phone, sizeof(phone), phone_field, 9);
    field_to_string(name, sizeof(name), name_field, 30);
    field_to_string(email, sizeof(email), email_field, 40);
    if (!bind_text(stmt, 1, phone, 9, 9, &phone_length) ||
        !bind_text(stmt, 2, name, 30, 30, &name_length) ||
        !bind_text(stmt, 3, email, 40, 40, &email_length)) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return -1;
    }
    rc = api_execute(stmt);
    if (!success(rc)) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return strcmp(last_state, "23000") == 0 ? 1 : -1;
    }
    api_free_handle(SQL_HANDLE_STMT, stmt);
    return 0;
}

int mysql_find(char *phone_field, char *name_field, char *email_field)
{
    SQLHANDLE stmt = prepare_statement(
        "SELECT nome, email FROM clientes WHERE telefone = ?");
    SQLRETURN rc;
    SQLLEN phone_length = 9, value_length = 0;
    char phone[10], name[31], email[41];

    if (stmt == NULL)
        return -1;
    field_to_string(phone, sizeof(phone), phone_field, 9);
    if (!success(api_bind_parameter(stmt, 1, SQL_PARAM_INPUT, SQL_C_CHAR,
            SQL_VARCHAR, 9, 0, phone, 9, &phone_length))) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return -1;
    }
    rc = api_execute(stmt);
    if (!success(rc)) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return -1;
    }
    rc = api_fetch(stmt);
    if (rc == SQL_NO_DATA) {
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return 1;
    }
    if (!success(rc)) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return -1;
    }
    memset(name, 0, sizeof(name));
    memset(email, 0, sizeof(email));
    if (!success(api_get_data(stmt, 1, SQL_C_CHAR, name,
                              sizeof(name), &value_length)) ||
        !success(api_get_data(stmt, 2, SQL_C_CHAR, email,
                              sizeof(email), &value_length))) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return -1;
    }
    string_to_field(name_field, 30, name);
    string_to_field(email_field, 40, email);
    api_free_handle(SQL_HANDLE_STMT, stmt);
    return 0;
}

int mysql_update(char *phone_field, char *name_field, char *email_field)
{
    SQLHANDLE stmt = prepare_statement(
        "UPDATE clientes SET nome = ?, email = ? WHERE telefone = ?");
    SQLRETURN rc;
    char phone[10], name[31], email[41];
    SQLLEN name_length, email_length, phone_length = 9;

    if (stmt == NULL)
        return -1;
    field_to_string(phone, sizeof(phone), phone_field, 9);
    field_to_string(name, sizeof(name), name_field, 30);
    field_to_string(email, sizeof(email), email_field, 40);
    if (!bind_text(stmt, 1, name, 30, 30, &name_length) ||
        !bind_text(stmt, 2, email, 40, 40, &email_length) ||
    !success(api_bind_parameter(stmt, 3, SQL_PARAM_INPUT, SQL_C_CHAR,
                                SQL_VARCHAR, 9, 0, phone, 9,
                                &phone_length))) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return -1;
    }
    rc = api_execute(stmt);
    if (!success(rc))
        get_error(SQL_HANDLE_STMT, stmt);
    api_free_handle(SQL_HANDLE_STMT, stmt);
    return success(rc) ? 0 : -1;
}

int mysql_delete(char *phone_field)
{
    SQLHANDLE stmt = prepare_statement(
        "DELETE FROM clientes WHERE telefone = ?");
    SQLRETURN rc;
    SQLLEN phone_length = 9, rows = 0;
    char phone[10];

    if (stmt == NULL)
        return -1;
    field_to_string(phone, sizeof(phone), phone_field, 9);
    if (!success(api_bind_parameter(stmt, 1, SQL_PARAM_INPUT, SQL_C_CHAR,
            SQL_VARCHAR, 9, 0, phone, 9, &phone_length))) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return -1;
    }
    rc = api_execute(stmt);
    if (!success(rc)) {
        get_error(SQL_HANDLE_STMT, stmt);
        api_free_handle(SQL_HANDLE_STMT, stmt);
        return -1;
    }
    api_row_count(stmt, &rows);
    api_free_handle(SQL_HANDLE_STMT, stmt);
    return rows == 0 ? 1 : 0;
}

int mysql_list_start(void)
{
    SQLRETURN rc;

    if (list_handle != NULL) {
        api_free_handle(SQL_HANDLE_STMT, list_handle);
        list_handle = NULL;
    }
    rc = api_alloc_handle(SQL_HANDLE_STMT, dbc_handle, &list_handle);
    if (!success(rc)) {
        get_error(SQL_HANDLE_DBC, dbc_handle);
        return -1;
    }
    rc = api_exec_direct(list_handle,
        (SQLCHAR *)"SELECT telefone, nome, email FROM clientes "
                   "ORDER BY telefone", SQL_NTS);
    if (!success(rc)) {
        get_error(SQL_HANDLE_STMT, list_handle);
        api_free_handle(SQL_HANDLE_STMT, list_handle);
        list_handle = NULL;
        return -1;
    }
    return 0;
}

int mysql_list_next(char *phone_field, char *name_field, char *email_field)
{
    SQLRETURN rc;
    SQLLEN value_length = 0;
    char phone[10], name[31], email[41];

    if (list_handle == NULL) {
        set_error("Client report is not open");
        return -1;
    }
    rc = api_fetch(list_handle);
    if (rc == SQL_NO_DATA)
        return 1;
    if (!success(rc)) {
        get_error(SQL_HANDLE_STMT, list_handle);
        return -1;
    }
    memset(phone, 0, sizeof(phone));
    memset(name, 0, sizeof(name));
    memset(email, 0, sizeof(email));
    if (!success(api_get_data(list_handle, 1, SQL_C_CHAR, phone,
                              sizeof(phone), &value_length)) ||
        !success(api_get_data(list_handle, 2, SQL_C_CHAR, name,
                              sizeof(name), &value_length)) ||
        !success(api_get_data(list_handle, 3, SQL_C_CHAR, email,
                              sizeof(email), &value_length))) {
        get_error(SQL_HANDLE_STMT, list_handle);
        return -1;
    }
    string_to_field(phone_field, 9, phone);
    string_to_field(name_field, 30, name);
    string_to_field(email_field, 40, email);
    return 0;
}

void mysql_list_close(void)
{
    if (list_handle != NULL && api_free_handle != NULL) {
        api_free_handle(SQL_HANDLE_STMT, list_handle);
        list_handle = NULL;
    }
}

void mysql_get_error(char *out)
{
    string_to_field(out, 80, last_error);
}

void mysql_disconnect(void)
{
    mysql_list_close();
    if (dbc_handle != NULL) {
        if (api_disconnect != NULL)
            api_disconnect(dbc_handle);
        if (api_free_handle != NULL)
            api_free_handle(SQL_HANDLE_DBC, dbc_handle);
        dbc_handle = NULL;
    }
    if (env_handle != NULL) {
        if (api_free_handle != NULL)
            api_free_handle(SQL_HANDLE_ENV, env_handle);
        env_handle = NULL;
    }
    if (odbc_module != NULL) {
        FreeLibrary(odbc_module);
        odbc_module = NULL;
    }
}
