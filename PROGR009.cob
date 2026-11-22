      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR009.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77  WRK-NOTE1         PIC 9(02)V99        VALUE ZEROES.
       77  WRK-NOTE2         PIC 9(02)V99        VALUE ZEROES.
       77  WRK-AVERAGE       PIC 9(02)V99        VALUE ZEROES.
       PROCEDURE DIVISION.

           DISPLAY "ENTER NOTE 1:"
      *     DISPLAY SPACE
           ACCEPT WRK-NOTE1

           DISPLAY "ENTER NOTE 2:"
           ACCEPT WRK-NOTE2.

      *     DISPLAY "ENTER VALUE 3:"
      *     ACCEPT WRK-VALUE3.

       0150-PROCESS  SECTION.

           COMPUTE WRK-AVERAGE = (WRK-NOTE1 + WRK-NOTE2) / 2.

       0200-SHOW SECTION.

           DISPLAY "AVERAGE.: " WRK-AVERAGE.
           IF  WRK-AVERAGE GREATER 7
               DISPLAY "APROVED"
           ELSE
               DISPLAY "REJECTED"
           END-IF.


       0300-FINALIZE SECTION.


            STOP RUN.
