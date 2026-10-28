      ******************************************************************
      * Author: REGINALDO JUNIOR
      * Date: 28/10/2026
      * Purpose: LEVEL (STRUCTURAL) VARIABLE - FILLER
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR005.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WRK-DATA.
           05 WRK-YEAR   PIC X(04) VALUE SPACES.
           05 WRK-MONTH  PIC X(02) VALUE SPACES.
           05 WRK-DAY    PIC X(02) VALUE SPACES.

       01 WRK-DATA2.
           05 WRK-DAY    PIC X(02) VALUE SPACES.
           05 FILLER     PIC X(01) VALUE "/".
           05 WRK-MONTH  PIC X(02) VALUE SPACES.
           05 FILLER     PIC X(01) VALUE "/".
           05 WRK-YEAR   PIC X(04) VALUE SPACES.



       PROCEDURE DIVISION.
       0100-RECEIVE SECTION.
           ACCEPT WRK-DATA FROM DATE YYYYMMDD.
            MOVE CORR WRK-DATA TO WRK-DATA2.

       0200-SHOW   SECTION.
           DISPLAY "FULL DATE: " WRK-DATA.
           DISPLAY "CURRENT DATE: " WRK-DATA2.

       0300-FINALIZE  SECTION.

            STOP RUN.
