       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCDay2.
       AUTHOR. JULIUS PUTRA TANU SETIAJI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOT OPTIONAL input-file ASSIGN TO "input"
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD input-file
           RECORD IS VARYING IN SIZE FROM 1 TO 1000.
       01 input-line PIC X(1000).
       WORKING-STORAGE SECTION.
       01 input-file-eof PIC X VALUE 'N'.
       01 table-length PIC 9(6) VALUE 0.
       01 number-count PIC 9(2).
       01 numbers-table PIC 9(2) OCCURS 1 TO 8 TIMES DEPENDING ON
           number-count.
       01 i PIC 9(3).
       01 diff PIC S9(3).
       01 new-diff PIC S9(3).
       01 result PIC 9(4) VALUE 0.
       01 result-display PIC Z(4).
       01 bad-record PIC X VALUE 'N'.

       PROCEDURE DIVISION.
           OPEN INPUT input-file.
           PERFORM UNTIL input-file-eof = 'Y'
             READ input-file
               NOT AT END
                 MOVE 0 TO number-count
                 UNSTRING FUNCTION TRIM(input-line) DELIMITED BY " "
                 INTO numbers-table(1), numbers-table(2),
                     numbers-table(3), numbers-table(4),
                     numbers-table(5), numbers-table(6),
                     numbers-table(7), numbers-table(8)
                 TALLYING IN number-count
                 COMPUTE diff = numbers-table(2) - numbers-table(1)
                 PERFORM VARYING i FROM 2 BY 1 UNTIL i > number-count
                   MOVE "N" TO bad-record
                   COMPUTE new-diff = numbers-table(i) -
                       numbers-table(i - 1)
                   EVALUATE diff
                     WHEN IS ZERO
                       MOVE "Y" TO bad-record
                       EXIT PERFORM
                     WHEN IS POSITIVE
                       IF new-diff IS NOT POSITIVE
                         MOVE "Y" TO bad-record
                         EXIT PERFORM
                       END-IF
                     WHEN IS NEGATIVE
                       IF new-diff IS NOT NEGATIVE
                         MOVE "Y" TO bad-record
                         EXIT PERFORM
                       END-IF
                   END-EVALUATE
                   IF FUNCTION ABS(new-diff) > 3 OR FUNCTION
                     ABS(new-diff) < 1 THEN
                     MOVE "Y" TO bad-record
                     EXIT PERFORM
                   END-IF
                 END-PERFORM
                 IF bad-record = "N" THEN
                   ADD 1 TO result
                 END-IF
               AT END MOVE 'Y' TO input-file-eof
           END-PERFORM.
           CLOSE input-file.
           MOVE result TO result-display.
           DISPLAY FUNCTION TRIM(result-display).
