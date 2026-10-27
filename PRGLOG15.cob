        >>SOURCE FORMAT FREE
        IDENTIFICATION DIVISION.
           PROGRAM-ID. PRGLOG15.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
       SELECT FUNCIONARIOS
           ASSIGN TO "C:/CURSOLOGICA/dados/funcionarios.txt"
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS WS-FILE-STATUS.

        DATA DIVISION.
        FILE SECTION.
        FD FUNCIONARIOS.
        01 FUNCIONARIO-LINHA PIC X(4096).

       WORKING-STORAGE SECTION.
        01 WS-FILE-STATUS  PIC XX.
        01 WS-FIM          PIC X VALUE "N".
        01 WS-TOTAL        PIC 9(9) VALUE ZERO.
        01 WS-TAMANHO      PIC 9(4) VALUE ZERO.
        01 WS-TOTAL-EDIT   PIC Z(8)9.
        01 WS-TAMANHO-EDIT PIC Z(3)9.

        PROCEDURE DIVISION.
        PRINCIPAL.
                 OPEN INPUT FUNCIONARIOS
             IF WS-FILE-STATUS = "00"
                DISPLAY "Arquivo encontrado."
               PERFORM UNTIL WS-FIM = "S"
                       READ FUNCIONARIOS
                            AT END
                                 MOVE "S" TO WS-FIM
                            NOT AT END
                              COMPUTE WS-TAMANHO =
                                   FUNCTION LENGTH(FUNCTION TRIM(
                                        FUNCIONARIO-LINHA TRAILING))
                              IF WS-TAMANHO > ZERO
                                   MOVE WS-TAMANHO TO WS-TAMANHO-EDIT
                              DISPLAY FUNCIONARIO-LINHA(1:WS-TAMANHO)
                                   " - " FUNCTION TRIM(WS-TAMANHO-EDIT)
                               ADD 1 TO WS-TOTAL
                              END-IF
                        END-READ
               END-PERFORM
               DISPLAY "---------------------Estatistica--------------"
                MOVE WS-TOTAL TO WS-TOTAL-EDIT
                   DISPLAY "Total de registros lidos..."
                        FUNCTION TRIM(WS-TOTAL-EDIT)
                   CLOSE FUNCIONARIOS
           ELSE
               DISPLAY "Erro ao abrir o arquivo. Codigo: "
                   WS-FILE-STATUS
           END-IF
           STOP RUN.
.           