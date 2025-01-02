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
       FD input-file RECORD IS VARYING IN SIZE FROM 1 TO 1000.
       01 input-line PIC X(1000).
       WORKING-STORAGE SECTION.
       01 input-file-eof PIC X VALUE 'N'.
       01 number-container.
           05 number-count PIC 9(2).
           05 numbers-table PIC 9(2) OCCURS 1 TO 8 TIMES DEPENDING ON
               number-count.
       01 number-container-without-one.
           05 number-without-one-count PIC 9(2).
           05 numbers-without-one-table PIC 9(2) OCCURS 1 TO 8 TIMES
               DEPENDING ON number-count.
       01 i PIC 9(2).
       01 excluded-index PIC 9(2).
       01 result PIC 9(4) VALUE 0.
       01 result-display PIC Z(4).
       01 result-with-one-bad PIC 9(4) VALUE 0.
       01 is-valid PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
         OPEN INPUT input-file.
         PERFORM UNTIL input-file-eof = 'Y'
           READ input-file
             NOT AT END
               PERFORM PARSE-LINE
               PERFORM CHECK-LINE
             AT END MOVE 'Y' TO input-file-eof
         END-PERFORM.
         CLOSE input-file.
         MOVE result TO result-display.
         DISPLAY FUNCTION TRIM(result-display).
         COMPUTE result = result + result-with-one-bad.
         MOVE result TO result-display.
         DISPLAY FUNCTION TRIM(result-display).
         STOP RUN.

       PARSE-LINE.
         MOVE 0 TO number-count.
         UNSTRING FUNCTION TRIM(input-line) DELIMITED BY " "
             INTO numbers-table(1), numbers-table(2),
                 numbers-table(3), numbers-table(4),
                 numbers-table(5), numbers-table(6),
                 numbers-table(7), numbers-table(8)
             TALLYING IN number-count.

       CHECK-LINE.
         CALL 'is-report-valid' USING number-container, is-valid.
         IF is-valid = "Y" THEN
           ADD 1 TO result
         ELSE
           PERFORM TRY-WITH-REMOVE-ONE-BAD
         END-IF.

       TRY-WITH-REMOVE-ONE-BAD.
         COMPUTE number-without-one-count = number-count - 1.
         PERFORM VARYING excluded-index FROM 1 BY 1
             UNTIL excluded-index > number-count
           PERFORM VARYING i FROM 1 BY 1
               UNTIL i > number-count
             IF i < excluded-index THEN
               MOVE numbers-table(i) TO numbers-without-one-table(i)
             END-IF
             IF i > excluded-index THEN
               MOVE numbers-table(i) TO numbers-without-one-table(i - 1)
             END-IF
           END-PERFORM
           CALL 'is-report-valid' USING number-container-without-one,
               is-valid
           IF is-valid = "Y" THEN
             ADD 1 TO result-with-one-bad
             EXIT PERFORM
           END-IF
         END-PERFORM.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. is-report-valid.
       AUTHOR. JULIUS PUTRA TANU SETIAJI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 i PIC 9(3).
       01 diff PIC S9(3).
       01 new-diff PIC S9(3).
       LINKAGE SECTION.
       01 report-container.
           05 report-count PIC 9(2).
           05 report-table PIC 9(2) OCCURS 1 TO 8 TIMES DEPENDING ON
             report-count.
       01 is-valid PIC X.

       PROCEDURE DIVISION USING report-container, BY REFERENCE is-valid.
       COMPUTE diff = report-table(2) - report-table(1)
       PERFORM VARYING i FROM 2 BY 1 UNTIL i > report-count
           MOVE "Y" TO is-valid
           COMPUTE new-diff = report-table(i) - report-table(i - 1)
           EVALUATE diff
             WHEN IS ZERO
               MOVE "N" TO is-valid
               EXIT PERFORM
             WHEN IS POSITIVE
               IF new-diff IS NOT POSITIVE
                 MOVE "N" TO is-valid
                 EXIT PERFORM
               END-IF
             WHEN IS NEGATIVE
               IF new-diff IS NOT NEGATIVE
                 MOVE "N" TO is-valid
                 EXIT PERFORM
               END-IF
           END-EVALUATE
           IF FUNCTION ABS(new-diff) > 3 OR FUNCTION ABS(new-diff) < 1
           THEN
             MOVE "N" TO is-valid
             EXIT PERFORM
           END-IF
       END-PERFORM.
       GOBACK.
       END PROGRAM is-report-valid.
