       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGLOG13.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE
               ASSIGN TO WS-FULL-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT LIST-FILE
               ASSIGN TO "C:/CURSOLOGICA/DADOS/PRGLOG13_FILES.TMP"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LIST-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD USER-FILE.
       01 USER-FILE-RECORD PIC X(4096).
       FD LIST-FILE.
       01 LIST-FILE-RECORD PIC X(255).

       WORKING-STORAGE SECTION.
       01 WS-FULL-PATH PIC X(512).
       01 WS-INPUT-NAME PIC X(255).
       01 WS-FILE-STATUS PIC XX.
       01 WS-LIST-STATUS PIC XX.
       01 WS-ANSWER PIC X.
       01 WS-CONTINUE PIC X VALUE "S".
       01 WS-CHOICE-TEXT PIC X(3).
       01 WS-CHOICE PIC 9(3) COMP-5.
       01 WS-FILE-COUNT PIC 9(3) COMP-5 VALUE ZERO.
       01 WS-INDEX PIC 9(3) COMP-5.
       01 WS-INDEX-EDIT PIC ZZ9.
       01 WS-SYSTEM-COMMAND PIC X(200).
       01 WS-SYSTEM-STATUS PIC S9(9) COMP-5.
       01 WS-FILE-TABLE.
           05 WS-FILE-ITEM PIC X(255) OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       PRINCIPAL.
           PERFORM UNTIL FUNCTION UPPER-CASE(WS-CONTINUE) NOT = "S"
               PERFORM LIST-TXT-FILES
               DISPLAY "ARQUIVOS TXT DISPONIVEIS:"
               PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-FILE-COUNT
                   MOVE WS-INDEX TO WS-INDEX-EDIT
                   DISPLAY FUNCTION TRIM(WS-INDEX-EDIT)
                       " - " FUNCTION TRIM(WS-FILE-ITEM(WS-INDEX))
               END-PERFORM

               DISPLAY "0 - DIGITAR OUTRO NOME DE ARQUIVO"
               DISPLAY "ESCOLHA O NUMERO DO ARQUIVO:"
               ACCEPT WS-CHOICE-TEXT

               IF FUNCTION TRIM(WS-CHOICE-TEXT) IS NUMERIC
                   COMPUTE WS-CHOICE =
                       FUNCTION NUMVAL(WS-CHOICE-TEXT)
                   IF WS-CHOICE = ZERO
                       DISPLAY
                           "DIGITE O NOME DO ARQUIVO COM EXTENSAO .TXT:"
                       ACCEPT WS-INPUT-NAME
                   ELSE
                       IF WS-CHOICE <= WS-FILE-COUNT
                           MOVE WS-FILE-ITEM(WS-CHOICE)
                               TO WS-INPUT-NAME
                       ELSE
                           MOVE SPACES TO WS-INPUT-NAME
                           DISPLAY "OPCAO INVALIDA."
                       END-IF
                   END-IF
               ELSE
                   MOVE SPACES TO WS-INPUT-NAME
                   DISPLAY "OPCAO INVALIDA."
               END-IF

               IF WS-INPUT-NAME NOT = SPACES
                   PERFORM CHECK-SELECTED-FILE
               END-IF

               DISPLAY "DESEJA VERIFICAR OUTRO ARQUIVO? (S/N):"
               ACCEPT WS-CONTINUE
           END-PERFORM
           STOP RUN.

       LIST-TXT-FILES.
           MOVE ZERO TO WS-FILE-COUNT
           MOVE SPACES TO WS-SYSTEM-COMMAND
           STRING
               'CMD /C DIR /B /A-D "C:\CURSOLOGICA\DADOS\*.TXT"'
                   DELIMITED BY SIZE
               ' > "C:\CURSOLOGICA\DADOS\PRGLOG13_FILES.TMP" 2>NUL'
                   DELIMITED BY SIZE
               INTO WS-SYSTEM-COMMAND
           END-STRING
           CALL "SYSTEM" USING WS-SYSTEM-COMMAND
               RETURNING WS-SYSTEM-STATUS

           OPEN INPUT LIST-FILE
           IF WS-LIST-STATUS = "00"
               PERFORM UNTIL WS-LIST-STATUS = "10"
                   READ LIST-FILE
                       AT END
                           CONTINUE
                       NOT AT END
                           IF WS-FILE-COUNT < 100
                               AND LIST-FILE-RECORD NOT = SPACES
                               ADD 1 TO WS-FILE-COUNT
                               MOVE LIST-FILE-RECORD
                                   TO WS-FILE-ITEM(WS-FILE-COUNT)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE LIST-FILE
           ELSE
               IF WS-LIST-STATUS NOT = "35"
                   DISPLAY "ERRO AO LISTAR ARQUIVOS. CODIGO: "
                       WS-LIST-STATUS
               END-IF
           END-IF.
           MOVE SPACES TO WS-SYSTEM-COMMAND
           STRING
               'CMD /C DEL /Q ' DELIMITED BY SIZE
               '"C:\CURSOLOGICA\DADOS\PRGLOG13_FILES.TMP" 2>NUL'
                   DELIMITED BY SIZE
               INTO WS-SYSTEM-COMMAND
           END-STRING
           CALL "SYSTEM" USING WS-SYSTEM-COMMAND
               RETURNING WS-SYSTEM-STATUS.

       CHECK-SELECTED-FILE.
           MOVE SPACES TO WS-FULL-PATH
           STRING
               "C:/CURSOLOGICA/DADOS/" DELIMITED BY SIZE
               FUNCTION TRIM(WS-INPUT-NAME) DELIMITED BY SIZE
               INTO WS-FULL-PATH
           END-STRING

           OPEN INPUT USER-FILE
           EVALUATE WS-FILE-STATUS
               WHEN "00"
                   DISPLAY "ARQUIVO " FUNCTION TRIM(WS-INPUT-NAME)
                       " JA EXISTE!"
                   CLOSE USER-FILE
               WHEN "35"
                   DISPLAY "ARQUIVO NAO ENCONTRADO. "
                       "DESEJA CRIA-LO? (S/N):"
                   ACCEPT WS-ANSWER
                   IF FUNCTION UPPER-CASE(WS-ANSWER) = "S"
                       OPEN OUTPUT USER-FILE
                       IF WS-FILE-STATUS = "00"
                           CLOSE USER-FILE
                           DISPLAY "ARQUIVO "
                               FUNCTION TRIM(WS-INPUT-NAME)
                               " CRIADO COM SUCESSO!"
                       ELSE
                           DISPLAY "NAO FOI POSSIVEL CRIAR O ARQUIVO."
                       END-IF
                   END-IF
               WHEN OTHER
                   DISPLAY "ERRO AO VERIFICAR O ARQUIVO. CODIGO: "
                       WS-FILE-STATUS
           END-EVALUATE.
