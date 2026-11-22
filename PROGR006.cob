      ******************************************************************
      * Author: REGINALDO JUNIOR
      * Date: 29/10/2026
      * Purpose: LEVEL (GROUP) VARIABLE - EXAMPLE 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR006.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WRK-ENTER.
           05 WRK-CODE     PIC 9(04) VALUE ZEROES.
           05 WRK-NAME     PIC X(15) VALUE SPACES.
           05 WRK-SALARY   PIC 9(06) VALUE ZEROES.
       PROCEDURE DIVISION.

       0100-RECEIVE SECTION.
           DISPLAY "ENTER THE DATA LINE:"
      *     DISPLAY SPACES
      *     WITH NO ADVANCING

           ACCEPT WRK-ENTER.
       0200-SHOW   SECTION.

           DISPLAY "CODE....:" WRK-CODE.

           DISPLAY "NAME....:" WRK-NAME.
           DISPLAY "SALARY..:" WRK-SALARY.

       0300-FINALIZE  SECTION.

            STOP RUN.
