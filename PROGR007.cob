      ******************************************************************
      * Author: Reginaldo Junior
      * Date: 28/10/2026
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRM007.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  WRK-VALUE1      PIC 9(08)V99     VALUE ZEROES.
       77  WRK-VALUE2      PIC 9(08)V99     VALUE ZEROES.
       77  WRK-RESULT      PIC S9(09)V99     VALUE ZEROES.
       77  WRK-RESULT-ED   PIC ZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
       0100-RECEIVES SECTION.

           DISPLAY "ENTER VALUE 1:"
           DISPLAY SPACE
           ACCEPT WRK-VALUE1

           DISPLAY "ENTER VALUE 2:"
           ACCEPT WRK-VALUE2

           COMPUTE WRK-RESULT = WRK-VALUE1 + WRK-VALUE2.
           MOVE WRK-RESULT TO WRK-RESULT-ED.

       0200-SHOW SECTION.

           DISPLAY "RESULT: " WRK-RESULT-ED.

       0300-FINALIZE SECTION.

           STOP RUN.
