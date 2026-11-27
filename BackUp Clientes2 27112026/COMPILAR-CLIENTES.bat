@echo off
cobc -x -fstatic-call CLIENTES.cbl CLIENTES-MYSQL.c -o CLIENTES.exe
if errorlevel 1 exit /b 1
