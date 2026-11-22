      ******************************************************************
      * Author: Reginaldo Junior
      * Date: 28/10/2026
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRM008.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       77  WRK-VALUE1       PIC 9(08)V99        VALUE ZEROES.
       77  WRK-VALUE2       PIC 9(08)V99        VALUE ZEROES.
       77  WRK-VALUE3       PIC 9(08)V99        VALUE ZEROES.
       77  WRK-RESULT       PIC S9(09)V99       VALUE ZEROES.
       77  WRK-RESULT2      PIC S9(09)V99       VALUE ZEROES.
       77  WRK-RESULT3      PIC S9(09)V99       VALUE ZEROES.
       77  WRK-RESULT-ED    PIC -ZZ.ZZZ.ZZ9,99.
       77  WRK-RESULT-ED2   PIC -ZZ.ZZZ.ZZ9,99.
       77  WRK-RESULT-ED3   PIC -ZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
       0100-RECEIVES SECTION.

           DISPLAY "ENTER VALUE 1:"
      *     DISPLAY SPACE
           ACCEPT WRK-VALUE1

           DISPLAY "ENTER VALUE 2:"
           ACCEPT WRK-VALUE2.

      *     DISPLAY "ENTER VALUE 3:"
      *     ACCEPT WRK-VALUE3.

       0150-PROCESS  SECTION.
      *    ------------------ADD
      *     ADD WRK-VALUE1 WRK-VALUE2 WRK-VALUE3
      *         TO WRK-RESULT WRK-RESULT2 WRK-RESULT3.

      *     COMPUTE WRK-RESULT = WRK-VALUE1 + WRK-VALUE2.
      *     MOVE WRK-RESULT TO WRK-RESULT-ED.
      *     MOVE WRK-RESULT2 TO WRK-RESULT-ED2.
      *     MOVE WRK-RESULT3 TO WRK-RESULT-ED3.

      *    -------------SUBTRACT
      *     SUBTRACT WRK-VALUE2 FROM WRK-VALUE1
      *     GIVING WRK-RESULT WRK-RESULT2 WRK-RESULT3.
      *
      *     MOVE WRK-RESULT TO WRK-RESULT-ED.
      *     MOVE WRK-RESULT2 TO WRK-RESULT-ED2.
      *     MOVE WRK-RESULT3 TO WRK-RESULT-ED3.


      *    -------------MULTIPLY
      *     MULTIPLY WRK-VALUE2 BY WRK-VALUE1
      *     GIVING WRK-RESULT WRK-RESULT2 WRK-RESULT3.
      *         ON SIZE ERROR
      *             DISPLAY "ERROR - MULTIPLICATION BY 0".
      *
      *     MOVE WRK-RESULT TO WRK-RESULT-ED.
      *     MOVE WRK-RESULT2 TO WRK-RESULT-ED2.
      *     MOVE WRK-RESULT3 TO WRK-RESULT-ED3.



      *    -------------DIVIDE
           DIVIDE WRK-VALUE1 BY WRK-VALUE2
           GIVING WRK-RESULT WRK-RESULT2 WRK-RESULT3
               ON SIZE ERROR
                   DISPLAY "ERROR - DIVISION BY 0".

           MOVE WRK-RESULT TO WRK-RESULT-ED.
           MOVE WRK-RESULT2 TO WRK-RESULT-ED2.
           MOVE WRK-RESULT3 TO WRK-RESULT-ED3.





       0200-SHOW SECTION.

           DISPLAY "RESULT.: " WRK-RESULT-ED.
           DISPLAY "RESULT2: " WRK-RESULT-ED2.
           DISPLAY "RESULT3: " WRK-RESULT-ED3.

       0300-FINALIZE SECTION.

           STOP RUN.
