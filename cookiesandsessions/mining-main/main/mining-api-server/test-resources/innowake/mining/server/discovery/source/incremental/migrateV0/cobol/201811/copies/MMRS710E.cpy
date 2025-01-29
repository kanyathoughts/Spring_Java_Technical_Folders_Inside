****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
       MMRS-COMMON-INIT-PROCESSING SECTION.
           MOVE ALL '.'  TO MMRS-DISP-DATE-JHJJMMDD
           MOVE ALL ':'  TO MMRS-DISP-TIME-HHMMSS

      * get current date and time via cobol api ---------------------
           MOVE FUNCTION CURRENT-DATE
             TO MMRS-COMMON-COBOL-DATE-FIELDS

      * get date and time data ---------------------------
           MOVE CORRESPONDING MMRS-CURRENT-DATE-JHJJMMDD
             TO MMRS-DISP-DATE-JHJJMMDD
      *    MOVE MMRS-CURRENT-YEAR-JH  TO MMRS-DISP-YEAR-JH
      *    MOVE MMRS-CURRENT-YEAR-JJ  TO MMRS-DISP-YEAR-JJ
      *    MOVE MMRS-CURRENT-MONTH    TO MMRS-DISP-MONTH
      *    MOVE MMRS-CURRENT-DAY      TO MMRS-DISP-DAY
           MOVE CORRESPONDING MMRS-CURRENT-TIME-HHMMSSMS
             TO MMRS-DISP-TIME-HHMMSS
      *    MOVE MMRS-CURRENT-HOUR     TO MMRS-DISP-HOUR
      *    MOVE MMRS-CURRENT-MINUTE   TO MMRS-DISP-MINUTE
      *    MOVE MMRS-CURRENT-SECOND   TO MMRS-DISP-SECOND

      * move date and time to screen ---------------------
           MOVE MMRS-DISP-DATE-JHJJMMDD    TO DATE001O
           MOVE MMRS-DISP-TIME-HHMMSS      TO TIME001O
           .
