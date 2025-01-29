       IDENTIFICATION DIVISION.
       PROGRAM-ID. WNDT3026.
       DATA DIVISION.
       FILE SECTION.
       
       FD  RAW-RUN-CTRL-PRINT-FILE-ONLY.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-LABEL
           LABEL RECORDS ARE OMITTED.
           
       FD  RAW-RUN-CTRL-PRINT-FILE
           LABEL RECORDS ARE OMITTED
           LINAGE IS 61 LINES.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-A
           LABEL RECORDS ARE OMITTED
           LINAGE IS 61 LINES
           LINES AT TOP 5.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-B
           LABEL RECORDS IS STANDARD
           LINAGE IS 90 LINES
           WITH FOOTING AT 9.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-C
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           LINES AT BOTTOM 7.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-D
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           WITH FOOTING AT 9
           LINES AT BOTTOM 7.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-E
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           WITH FOOTING AT 9
           LINES AT TOP 5.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-F
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           WITH FOOTING AT 9
           LINES AT TOP 5
           LINES AT BOTTOM 7.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-G
           LABEL RECORDS ARE OMITTED
           LINAGE 63
           FOOTING 9
           TOP 5
           BOTTOM 7.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-H
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           FOOTING AT 9
           LINES TOP 5
           LINES AT BOTTOM 7.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-I
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           LINES TOP 5
           LINES BOTTOM 7.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-J
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           WITH FOOTING AT 9
           LINES AT BOTTOM 7.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-F
           LABEL RECORDS ARE OMITTED
           LINAGE 63 LINES
           LINES AT TOP 5
           LINES AT BOTTOM 7.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-F
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           WITH FOOTING 9
           LINES TOP 5.
           
       FD  RAW-RUN-CTRL-PRINT-FILE-F
           LABEL RECORDS ARE OMITTED
           LINAGE IS 63 LINES
           FOOTING AT 9
           LINES AT TOP 5
           LINES AT BOTTOM 7.

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       GOBACK.
