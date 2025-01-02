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
       FD input-file RECORD IS VARYING IN SIZE FROM 1 TO 99999 DEPENDING
           ON input-line-length.
       01 input-line PIC X(99999).
       WORKING-STORAGE SECTION.
       01 input-line-length PIC 9(5).
       01 i PIC 9(5).
       01 input-file-eof PIC X.
       01 state PIC 9.
         88 looking-for-prefix VALUE 0.
         88 state-first-number VALUE 1.
         88 state-second-number VALUE 2.
       01 start-index PIC 9(5).
       01 first-number PIC 9(10).
       01 second-number PIC 9(10).
       01 result PIC 9(10).
       01 result-display PIC Z(10).
       01 enabled PIC X VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM PART-ONE.
           PERFORM PART-TWO.
           STOP RUN.

       PART-ONE.
           MOVE 0 TO result.
           MOVE 'N' TO input-file-eof.
           OPEN INPUT input-file.
           PERFORM UNTIL input-file-eof = 'Y'
             READ input-file
               NOT AT END PERFORM PARSE-LINE-PART-ONE
               AT END MOVE 'Y' TO input-file-eof
           END-PERFORM.
           CLOSE input-file.
           MOVE result TO result-display.
           DISPLAY FUNCTION TRIM(result-display).

       PART-TWO.
           MOVE 0 TO result.
           MOVE 'N' TO input-file-eof.
           OPEN INPUT input-file.
           PERFORM UNTIL input-file-eof = 'Y'
             READ input-file
               NOT AT END PERFORM PARSE-LINE-PART-TWO
               AT END MOVE 'Y' TO input-file-eof
           END-PERFORM.
           CLOSE input-file.
           MOVE result TO result-display.
           DISPLAY FUNCTION TRIM(result-display).

      *> Parse using a finite state machine
       PARSE-LINE-PART-ONE.
           SET looking-for-prefix TO TRUE.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > input-line-length
             EVALUATE TRUE
               WHEN looking-for-prefix
                 IF input-line(i:4) = "mul(" THEN
                   SET state-first-number TO TRUE
                   ADD 3 TO i
                   ADD 1 TO i GIVING start-index
                 END-IF
               WHEN state-first-number
                 PERFORM HANDLE-STATE-FIRST-NUMBER
               WHEN state-second-number
                 PERFORM HANDLE-STATE-SECOND-NUMBER
             END-EVALUATE
           END-PERFORM.

       PARSE-LINE-PART-TWO.
           SET looking-for-prefix TO TRUE.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > input-line-length
             EVALUATE TRUE
               WHEN looking-for-prefix
                 EVALUATE TRUE
                   WHEN input-line(i:4) = "mul("
                     IF enabled = 'Y' THEN
                       SET state-first-number TO TRUE
                       ADD 3 TO i
                       ADD 1 to i GIVING start-index
                     END-IF
                   WHEN input-line(i:4) = "do()"
                     MOVE 'Y' TO enabled
                     ADD 3 TO i
                   WHEN input-line(i:7) = "don't()"
                     MOVE 'N' TO enabled
                     ADD 6 TO i
                 END-EVALUATE
               WHEN state-first-number
                 IF enabled = 'Y' PERFORM HANDLE-STATE-FIRST-NUMBER
               WHEN state-second-number
                 IF enabled = 'Y' PERFORM HANDLE-STATE-SECOND-NUMBER
             END-EVALUATE
           END-PERFORM.

       HANDLE-STATE-FIRST-NUMBER.
           IF input-line(i:1) = "," THEN
             MOVE input-line(start-index:(i - start-index + 1)) TO
                 first-number
             SET state-second-number TO TRUE
             ADD 1 to i GIVING start-index
           ELSE
             IF input-line(i:1) IS NOT NUMERIC THEN
               SET looking-for-prefix TO TRUE
             END-IF
           END-IF.

       HANDLE-STATE-SECOND-NUMBER.
           IF input-line(i:1) = ")"
             MOVE input-line(start-index:(i - start-index + 1)) TO
                 second-number
             SET looking-for-prefix TO TRUE
             COMPUTE result = result + first-number * second-number
           ELSE
             IF input-line(i:1) IS NOT NUMERIC THEN
               SET looking-for-prefix TO TRUE
             END-IF
           END-IF.
