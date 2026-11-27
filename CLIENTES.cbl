       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CLIENTES.
      *********************************
      * OBJETIVO:  SISTEMA DE GESTAO DE CLIENTES
      * AUTHOR  :  REGINNALDO JUNIOR 
      * DATA    :  25/11/2026 
      *********************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RELATO ASSIGN TO
             'C:\CURSOLOGICA\CURSOLOGICAVSCODE\RELATO.TXT'
             ORGANIZATION IS SEQUENTIAL.

           SELECT SENHAS ASSIGN TO
             'C:\CURSOLOGICA\CURSOLOGICAVSCODE\ACESSO.DAT'
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS SENHAS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD RELATO.
       01 RELATO-REG.
          05 RELATO-DADOS  PIC X(79).

       FD SENHAS.
       01 SENHAS-REG PIC X(20).


       WORKING-STORAGE SECTION.
       77 WRK-OPCAO       PIC X(1).
       77 WRK-MODULO      PIC X(25).
       77 WRK-TECLA       PIC X(1).
       77 SENHAS-STATUS   PIC X(02).
       77 WRK-DB-STATUS   PIC S9(9) COMP-5.
       77 WRK-MSGERRO     PIC X(30).
       77 WRK-CONTALINHA  PIC 9(03).
       77 WRK-QTREGISTROS PIC 9(05) VALUE 0.
       77 WRK-MSG-RELATORIO PIC X(30).
       77 MYSQL-HOST      PIC X(32) VALUE 'localhost'.
       77 MYSQL-PORTA     PIC X(05) VALUE '3306'.
       77 MYSQL-BANCO     PIC X(32) VALUE 'clientes_db'.
       77 MYSQL-USUARIO   PIC X(32) VALUE 'root'.
       77 MYSQL-SENHA     PIC X(128).
       77 WRK-DB-ERRO     PIC X(80).
       77 WRK-SENHA-ATUAL PIC X(20).
       77 WRK-SENHA-ENTRADA PIC X(20).
       77 WRK-SENHA-NOVA PIC X(20).
       77 WRK-SENHA-CONFIRMA PIC X(20).
       77 WRK-AUTENTICADO PIC X VALUE 'N'.

       01 CLIENTES-REG.
            05 CLIENTES-CHAVE.
                10 CLIENTES-FONE PIC 9(09).
            05 CLIENTES-NOME     PIC X(30).
            05 CLIENTES-EMAIL    PIC X(40).

       01 WRK-RELATORIO-PAGINA.
            05 WRK-RELATORIO-LINHA OCCURS 5 TIMES.
               10 WRK-REL-FONE  PIC X(09).
               10 WRK-REL-NOME  PIC X(30).
               10 WRK-REL-EMAIL PIC X(40).

       SCREEN SECTION.
       01 TELA.
            05 LIMPA-TELA.
                10 BLANK SCREEN.
                10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 01 COLUMN 25 PIC X(20)
                   BACKGROUND-COLOR 3  FOREGROUND-COLOR 0
                              FROM 'SISTEMA DE CLIENTES '.
                10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                   BACKGROUND-COLOR 1 FROM WRK-MODULO.
       01 TELA-MENU.
            05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
            05 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
            05 LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
            05 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR'.
            05 LINE 11 COLUMN 15 VALUE '5 - RELATORIO EM TELA'.
            05 LINE 12 COLUMN 15 VALUE '6 - RELATORIO EM DISCO'.
            05 LINE 13 COLUMN 15 VALUE '7 - ALTERAR SENHA'.
            05 LINE 14 COLUMN 15 VALUE 'X - SAIDA'.
            05 LINE 15 COLUMN 15 VALUE 'OPCAO......: ' .
            05 LINE 15 COLUMN 28 USING WRK-OPCAO.

       01 TELA-RELATORIO.
            05 LINE 04 COLUMN 10 VALUE 'RELATORIO DE CLIENTES'.
            05 LINE 06 COLUMN 10 VALUE 'TELEFONE'.
            05 LINE 06 COLUMN 21 VALUE 'NOME'.
            05 LINE 06 COLUMN 53 VALUE 'EMAIL'.
            05 LINE 08 COLUMN 10 PIC X(09)
               FROM WRK-REL-FONE(1).
            05 COLUMN PLUS 2 PIC X(30)
               FROM WRK-REL-NOME(1).
            05 COLUMN PLUS 2 PIC X(40)
               FROM WRK-REL-EMAIL(1).
            05 LINE 09 COLUMN 10 PIC X(09)
               FROM WRK-REL-FONE(2).
            05 COLUMN PLUS 2 PIC X(30)
               FROM WRK-REL-NOME(2).
            05 COLUMN PLUS 2 PIC X(40)
               FROM WRK-REL-EMAIL(2).
            05 LINE 10 COLUMN 10 PIC X(09)
               FROM WRK-REL-FONE(3).
            05 COLUMN PLUS 2 PIC X(30)
               FROM WRK-REL-NOME(3).
            05 COLUMN PLUS 2 PIC X(40)
               FROM WRK-REL-EMAIL(3).
            05 LINE 11 COLUMN 10 PIC X(09)
               FROM WRK-REL-FONE(4).
            05 COLUMN PLUS 2 PIC X(30)
               FROM WRK-REL-NOME(4).
            05 COLUMN PLUS 2 PIC X(40)
               FROM WRK-REL-EMAIL(4).
            05 LINE 12 COLUMN 10 PIC X(09)
               FROM WRK-REL-FONE(5).
            05 COLUMN PLUS 2 PIC X(30)
               FROM WRK-REL-NOME(5).
            05 COLUMN PLUS 2 PIC X(40)
               FROM WRK-REL-EMAIL(5).
            05 LINE 14 COLUMN 10 PIC X(30)
               FROM WRK-MSG-RELATORIO.

       01 TELA-SENHA-MYSQL.
            05 LINE 18 COLUMN 10 VALUE 'SENHA DO MYSQL: '.
            05 COLUMN PLUS 1 PIC X(128) USING MYSQL-SENHA SECURE.

       01 TELA-REGISTRO.
            05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'TELEFONE '.
               10 COLUMN PLUS 2 PIC 9(09) USING CLIENTES-FONE
                   BLANK WHEN ZEROS.
            05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE 'NOME.... '.
               10 COLUMN PLUS 2 PIC X(30) USING CLIENTES-NOME.
               10 LINE 12 COLUMN 10 VALUE 'EMAIL... '.
               10 COLUMN PLUS 2 PIC X(40) USING CLIENTES-EMAIL.
       01 MOSTRA-ERRO.
             02 MSG-ERRO.
               10 LINE 16 COLUMN 01 ERASE EOL
                             BACKGROUND-COLOR 3.
               10 LINE 16 COLUMN 10 PIC X(30)
                             BACKGROUND-COLOR 3
                             FROM WRK-MSGERRO.
               10 COLUMN PLUS 2 PIC X(01)
                             BACKGROUND-COLOR 3
                             USING WRK-TECLA.


       PROCEDURE DIVISION.
       0001-PRINCIPAL SECTION.
            PERFORM 1050-INICIALIZAR-SENHA.
            PERFORM 1060-AUTENTICAR
              UNTIL WRK-AUTENTICADO = 'S'.
            PERFORM 1070-CONECTAR-MYSQL.
            PERFORM 1100-MONTATELA.
            PERFORM 2000-PROCESSAR UNTIL WRK-OPCAO = 'X'.
            PERFORM 3000-FINALIZAR.
            STOP RUN.

       1050-INICIALIZAR-SENHA.
            OPEN INPUT SENHAS
            EVALUATE SENHAS-STATUS
              WHEN '35'
                OPEN OUTPUT SENHAS
                IF SENHAS-STATUS = '00'
                    MOVE 'CLIENTES' TO SENHAS-REG
                    WRITE SENHAS-REG
                    IF SENHAS-STATUS = '00'
                        MOVE SENHAS-REG TO WRK-SENHA-ATUAL
                    ELSE
                        DISPLAY 'ERRO AO GRAVAR SENHA PADRAO'
                        CLOSE SENHAS
                        STOP RUN
                    END-IF
                    CLOSE SENHAS
                ELSE
                    DISPLAY 'ERRO AO CRIAR ARQUIVO DE SENHA'
                    STOP RUN
                END-IF
              WHEN '00'
                READ SENHAS
                IF SENHAS-STATUS = '00'
                    MOVE SENHAS-REG TO WRK-SENHA-ATUAL
                ELSE
                    DISPLAY 'ERRO AO LER ARQUIVO DE SENHA'
                    CLOSE SENHAS
                    STOP RUN
                END-IF
                CLOSE SENHAS
              WHEN OTHER
                DISPLAY 'ERRO AO ABRIR ARQUIVO DE SENHA'
                STOP RUN
            END-EVALUATE.

       1060-AUTENTICAR.
            MOVE 'ACESSO AO SISTEMA' TO WRK-MODULO.
            DISPLAY TELA.
            DISPLAY 'SENHA DE ACESSO: '.
            MOVE SPACES TO WRK-SENHA-ENTRADA.
            ACCEPT WRK-SENHA-ENTRADA.
            IF WRK-SENHA-ENTRADA = WRK-SENHA-ATUAL
                MOVE 'S' TO WRK-AUTENTICADO
                IF WRK-SENHA-ATUAL = 'CLIENTES'
                    MOVE 'RECOMENDAMOS TROCAR A SENHA' TO
                      WRK-MSGERRO
                    ACCEPT MOSTRA-ERRO
                END-IF
            ELSE
                MOVE 'SENHA INCORRETA' TO WRK-MSGERRO
                ACCEPT MOSTRA-ERRO
            END-IF.

       1070-CONECTAR-MYSQL.
            MOVE 'ACESSO MYSQL' TO WRK-MODULO
            DISPLAY TELA
            MOVE SPACES TO MYSQL-SENHA
            DISPLAY TELA-SENHA-MYSQL
            ACCEPT TELA-SENHA-MYSQL
            CALL "mysql_connect"
              USING BY REFERENCE MYSQL-HOST
                               MYSQL-PORTA
                               MYSQL-BANCO
                               MYSQL-USUARIO
                               MYSQL-SENHA
              RETURNING WRK-DB-STATUS
            MOVE SPACES TO MYSQL-SENHA
            IF WRK-DB-STATUS NOT = 0
                CALL "mysql_get_error"
                  USING BY REFERENCE WRK-DB-ERRO
                DISPLAY 'ERRO MYSQL: ' WRK-DB-ERRO
                CALL "mysql_disconnect"
                STOP RUN
            END-IF.

       1100-MONTATELA.
            MOVE 0 TO WRK-QTREGISTROS.
            DISPLAY TELA.
            ACCEPT TELA-MENU.
            MOVE FUNCTION UPPER-CASE(WRK-OPCAO) TO WRK-OPCAO.

       2000-PROCESSAR.
            MOVE SPACES TO CLIENTES-NOME CLIENTES-EMAIL WRK-MSGERRO.
            EVALUATE WRK-OPCAO
              WHEN 1
               PERFORM 5000-INCLUIR
              WHEN 2
                PERFORM 6000-CONSULTAR
              WHEN 3
                PERFORM 7000-ALTERAR
              WHEN 4
                PERFORM 8000-EXCLUIR
              WHEN 5
                PERFORM 9000-RELATORIOTELA
              WHEN 6
                PERFORM 9100-RELATORIODISCO
              WHEN '7'
                PERFORM 9200-ALTERAR-SENHA
              WHEN OTHER
                IF WRK-OPCAO NOT EQUAL 'X'
                    DISPLAY 'ENTRE COM OPCAO CORRETA'
                END-IF
            END-EVALUATE.
              PERFORM 1100-MONTATELA.



       3000-FINALIZAR.
             CALL "mysql_disconnect".

       9300-ERRO-MYSQL.
             CALL "mysql_get_error"
               USING BY REFERENCE WRK-DB-ERRO
             DISPLAY 'ERRO MYSQL: ' WRK-DB-ERRO
             MOVE 'ERRO NA OPERACAO MYSQL' TO WRK-MSGERRO
             ACCEPT MOSTRA-ERRO.

       9200-ALTERAR-SENHA.
             MOVE 'MODULO - ALTERAR SENHA' TO WRK-MODULO.
             DISPLAY TELA.
             DISPLAY 'SENHA ATUAL: '.
             MOVE SPACES TO WRK-SENHA-ENTRADA.
             ACCEPT WRK-SENHA-ENTRADA
             IF WRK-SENHA-ENTRADA = WRK-SENHA-ATUAL
                 DISPLAY 'NOVA SENHA (ATE 20): '
                 MOVE SPACES TO WRK-SENHA-NOVA
                 ACCEPT WRK-SENHA-NOVA
                 DISPLAY 'CONFIRME A NOVA SENHA: '
                 MOVE SPACES TO WRK-SENHA-CONFIRMA
                 ACCEPT WRK-SENHA-CONFIRMA
                 IF WRK-SENHA-NOVA = SPACES
                     MOVE 'SENHA NAO PODE SER VAZIA' TO WRK-MSGERRO
                 ELSE
                   IF WRK-SENHA-NOVA NOT = WRK-SENHA-CONFIRMA
                     MOVE 'SENHAS NAO CONFEREM' TO WRK-MSGERRO
                   ELSE
                     OPEN OUTPUT SENHAS
                     IF SENHAS-STATUS = '00'
                         MOVE WRK-SENHA-NOVA TO SENHAS-REG
                         WRITE SENHAS-REG
                         IF SENHAS-STATUS = '00'
                             MOVE WRK-SENHA-NOVA TO WRK-SENHA-ATUAL
                             MOVE 'SENHA ALTERADA' TO WRK-MSGERRO
                         ELSE
                             MOVE 'ERRO AO GRAVAR SENHA' TO
                               WRK-MSGERRO
                         END-IF
                         CLOSE SENHAS
                     ELSE
                         MOVE 'ERRO AO ABRIR ARQUIVO DE SENHA'
                           TO WRK-MSGERRO
                     END-IF
                   END-IF
                 END-IF
             ELSE
                 MOVE 'SENHA ATUAL INCORRETA' TO WRK-MSGERRO
             END-IF.
             ACCEPT MOSTRA-ERRO.



       5000-INCLUIR.
             MOVE 'MODULO - INCLUSAO ' TO WRK-MODULO.
             DISPLAY TELA.
             INITIALIZE CLIENTES-REG.
              ACCEPT TELA-REGISTRO.
             CALL "mysql_insert"
               USING BY REFERENCE CLIENTES-FONE
                                CLIENTES-NOME
                                CLIENTES-EMAIL
               RETURNING WRK-DB-STATUS
             EVALUATE WRK-DB-STATUS
               WHEN 0
                 MOVE 'CLIENTE INCLUIDO' TO WRK-MSGERRO
                 ACCEPT MOSTRA-ERRO
               WHEN 1
                 MOVE 'JA EXISTE' TO WRK-MSGERRO
                 ACCEPT MOSTRA-ERRO
               WHEN OTHER
                 PERFORM 9300-ERRO-MYSQL
             END-EVALUATE.

       6000-CONSULTAR.
             MOVE 'MODULO - CONSULTA ' TO WRK-MODULO.
             DISPLAY TELA.
               INITIALIZE CLIENTES-REG.
               DISPLAY TELA-REGISTRO.
               ACCEPT CHAVE.
             CALL "mysql_find"
               USING BY REFERENCE CLIENTES-FONE
                                CLIENTES-NOME
                                CLIENTES-EMAIL
               RETURNING WRK-DB-STATUS
             EVALUATE WRK-DB-STATUS
               WHEN 0
                 MOVE '--  ENCONTRADO  --' TO WRK-MSGERRO
                 DISPLAY SS-DADOS
                 ACCEPT MOSTRA-ERRO
               WHEN 1
                 MOVE 'NAO ENCONTRADO' TO WRK-MSGERRO
                 ACCEPT MOSTRA-ERRO
               WHEN OTHER
                 PERFORM 9300-ERRO-MYSQL
             END-EVALUATE.

       7000-ALTERAR.
             MOVE 'MODULO - ALTERAR ' TO WRK-MODULO.
             DISPLAY TELA.
             INITIALIZE CLIENTES-REG.
             DISPLAY TELA-REGISTRO.
              ACCEPT CHAVE.
             CALL "mysql_find"
               USING BY REFERENCE CLIENTES-FONE
                                CLIENTES-NOME
                                CLIENTES-EMAIL
               RETURNING WRK-DB-STATUS
             EVALUATE WRK-DB-STATUS
               WHEN 0
                 DISPLAY SS-DADOS
                 ACCEPT SS-DADOS
                 MOVE SPACES TO WRK-TECLA
                 MOVE 'CONFIRMAR (S/N)?' TO WRK-MSGERRO
                 ACCEPT MOSTRA-ERRO
                 MOVE FUNCTION UPPER-CASE(WRK-TECLA) TO WRK-TECLA
                 IF WRK-TECLA = 'S'
                     CALL "mysql_update"
                       USING BY REFERENCE CLIENTES-FONE
                                        CLIENTES-NOME
                                        CLIENTES-EMAIL
                       RETURNING WRK-DB-STATUS
                     IF WRK-DB-STATUS = 0
                         MOVE 'REGISTRO ALTERADO' TO WRK-MSGERRO
                         ACCEPT MOSTRA-ERRO
                     ELSE
                         PERFORM 9300-ERRO-MYSQL
                     END-IF
                 ELSE
                     MOVE 'ALTERACAO CANCELADA' TO WRK-MSGERRO
                     ACCEPT MOSTRA-ERRO
                 END-IF
               WHEN 1
                 MOVE 'REGISTRO NAO ENCONTRADO' TO WRK-MSGERRO
                 ACCEPT MOSTRA-ERRO
               WHEN OTHER
                 PERFORM 9300-ERRO-MYSQL
             END-EVALUATE.

       8000-EXCLUIR.
             MOVE 'MODULO - EXCLUSAO ' TO WRK-MODULO.
             DISPLAY TELA.
             INITIALIZE CLIENTES-REG.
             DISPLAY TELA-REGISTRO.
             ACCEPT CHAVE.
             CALL "mysql_find"
               USING BY REFERENCE CLIENTES-FONE
                                CLIENTES-NOME
                                CLIENTES-EMAIL
               RETURNING WRK-DB-STATUS
             EVALUATE WRK-DB-STATUS
               WHEN 0
                 DISPLAY SS-DADOS
                 MOVE 'CONFIRMA EXCLUSAO (S/N)?' TO WRK-MSGERRO
                 ACCEPT MOSTRA-ERRO
                 MOVE FUNCTION UPPER-CASE(WRK-TECLA) TO WRK-TECLA
                 IF WRK-TECLA = 'S'
                     CALL "mysql_delete"
                       USING BY REFERENCE CLIENTES-FONE
                       RETURNING WRK-DB-STATUS
                     IF WRK-DB-STATUS = 0
                         MOVE 'REGISTRO EXCLUIDO' TO WRK-MSGERRO
                         ACCEPT MOSTRA-ERRO
                     ELSE
                       IF WRK-DB-STATUS = 1
                         MOVE 'REGISTRO JA REMOVIDO' TO WRK-MSGERRO
                         ACCEPT MOSTRA-ERRO
                       ELSE
                         PERFORM 9300-ERRO-MYSQL
                       END-IF
                     END-IF
                 ELSE
                     MOVE 'EXCLUSAO CANCELADA' TO WRK-MSGERRO
                     ACCEPT MOSTRA-ERRO
                 END-IF
               WHEN 1
                 MOVE 'REGISTRO NAO ENCONTRADO' TO WRK-MSGERRO
                 ACCEPT MOSTRA-ERRO
               WHEN OTHER
                 PERFORM 9300-ERRO-MYSQL
             END-EVALUATE.


       9000-RELATORIOTELA.
             MOVE 'MODULO - RELATORIO ' TO WRK-MODULO.
             MOVE 0 TO WRK-QTREGISTROS WRK-CONTALINHA.
             INITIALIZE WRK-RELATORIO-PAGINA.
             CALL "mysql_list_start" RETURNING WRK-DB-STATUS
             IF WRK-DB-STATUS = 0
                 PERFORM UNTIL WRK-DB-STATUS NOT = 0
                     CALL "mysql_list_next"
                       USING BY REFERENCE CLIENTES-FONE
                                        CLIENTES-NOME
                                        CLIENTES-EMAIL
                       RETURNING WRK-DB-STATUS
                     IF WRK-DB-STATUS = 0
                         ADD 1 TO WRK-QTREGISTROS
                         ADD 1 TO WRK-CONTALINHA
                         MOVE CLIENTES-FONE TO
                           WRK-REL-FONE(WRK-CONTALINHA)
                         MOVE CLIENTES-NOME TO
                           WRK-REL-NOME(WRK-CONTALINHA)
                         MOVE CLIENTES-EMAIL TO
                           WRK-REL-EMAIL(WRK-CONTALINHA)
                         IF WRK-CONTALINHA = 5
                             PERFORM 9010-EXIBIR-PAGINA
                         END-IF
                     END-IF
                 END-PERFORM
                 IF WRK-CONTALINHA > 0
                     PERFORM 9010-EXIBIR-PAGINA
                 END-IF
             END-IF
             CALL "mysql_list_close"
             IF WRK-DB-STATUS = -1
                 PERFORM 9300-ERRO-MYSQL
             ELSE
                 IF WRK-QTREGISTROS = 0
                     MOVE 'NENHUM CLIENTE CADASTRADO' TO
                       WRK-MSG-RELATORIO
                     DISPLAY TELA
                     DISPLAY TELA-RELATORIO
                 END-IF
                 MOVE 'REGISTROS LIDOS ' TO WRK-MSGERRO
                 MOVE WRK-QTREGISTROS TO WRK-MSGERRO(17:05)
                 ACCEPT MOSTRA-ERRO
             END-IF.

       9010-EXIBIR-PAGINA.
             MOVE SPACES TO WRK-MSG-RELATORIO
             DISPLAY TELA
             DISPLAY TELA-RELATORIO
             MOVE 'PRESSIONE UMA TECLA' TO WRK-MSGERRO
             ACCEPT MOSTRA-ERRO
             INITIALIZE WRK-RELATORIO-PAGINA
             MOVE 0 TO WRK-CONTALINHA
             MOVE SPACES TO WRK-MSGERRO.



       9100-RELATORIODISCO.
             MOVE 'MODULO - RELATORIO ' TO WRK-MODULO.
             DISPLAY TELA.
             MOVE 0 TO WRK-QTREGISTROS.
             CALL "mysql_list_start" RETURNING WRK-DB-STATUS
             IF WRK-DB-STATUS = 0
                 OPEN OUTPUT RELATO
                 PERFORM UNTIL WRK-DB-STATUS NOT = 0
                     CALL "mysql_list_next"
                       USING BY REFERENCE CLIENTES-FONE
                                        CLIENTES-NOME
                                        CLIENTES-EMAIL
                       RETURNING WRK-DB-STATUS
                     IF WRK-DB-STATUS = 0
                         ADD 1 TO WRK-QTREGISTROS
                         MOVE CLIENTES-REG TO RELATO-REG
                         WRITE RELATO-REG
                     END-IF
                 END-PERFORM
                 CLOSE RELATO
                 CALL "mysql_list_close"
             END-IF
             IF WRK-DB-STATUS = -1
                 PERFORM 9300-ERRO-MYSQL
             ELSE
                 MOVE 'REGISTROS LIDOS ' TO WRK-MSGERRO
                 MOVE WRK-QTREGISTROS TO WRK-MSGERRO(17:05)
                 ACCEPT MOSTRA-ERRO
             END-IF.
