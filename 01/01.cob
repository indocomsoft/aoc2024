       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCDay1.
       AUTHOR. JULIUS PUTRA TANU SETIAJI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOT OPTIONAL input-file ASSIGN TO "input"
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD input-file.
       01 input-line.
           05 first-number PIC 9(5).
           05 FILLER PIC X(3).
           05 second-number PIC 9(5).
       WORKING-STORAGE SECTION.
       01 input-file-eof PIC X VALUE 'N'.
       01 table-length PIC 9(6) VALUE 0.
       01 first-table PIC 9(5) OCCURS 1 TO 999999 TIMES DEPENDING ON
           table-length.
       01 second-table PIC 9(5) OCCURS 1 TO 999999 TIMES DEPENDING ON
           table-length.
       01 row-number PIC 9(6).
       01 difference-sum PIC 9(38) VALUE 0.
       01 difference-sum-display PIC Z(38).
       01 cardinality PIC 9(6) VALUE 0.
       01 frequencies OCCURS 1 to 999999 TIMES
           DEPENDING ON cardinality
           ASCENDING frequency-key
           INDEXED BY frequency-index.
         05 frequency-key PIC 9(5).
         05 frequency-value PIC 9(38).
       01 similarity-score PIC 9(38) VALUE 0.
       01 similarity-score-display PIC Z(38).

       PROCEDURE DIVISION.
           OPEN INPUT input-file.
           PERFORM UNTIL input-file-eof = 'Y'
             READ input-file
               NOT AT END
                 ADD 1 TO table-length
                 MOVE first-number TO first-table(table-length)
                 MOVE second-number TO second-table(table-length)
               AT END MOVE 'Y' TO input-file-eof
           END-PERFORM.
           CLOSE input-file.
      *> PART ONE
           SORT first-table ASCENDING.
           SORT second-table ASCENDING.
           PERFORM VARYING row-number FROM 1 BY 1
             UNTIL row-number > table-length
             COMPUTE difference-sum =
               FUNCTION ABS(first-table(row-number) -
               second-table(row-number)) + difference-sum
           END-PERFORM.
           MOVE difference-sum TO difference-sum-display.
           DISPLAY FUNCTION TRIM(difference-sum-display).
      *> PART TWO
           PERFORM VARYING row-number FROM 1 BY 1
             UNTIL row-number > table-length
             SET frequency-index TO 1
             SEARCH frequencies
               AT END
                 ADD 1 TO cardinality
                 MOVE second-table(row-number)
                 TO frequency-key(cardinality)
                 MOVE 1 TO frequency-value(cardinality)
               WHEN
                 frequency-key(frequency-index) =
                   second-table(row-number)
                 ADD 1 TO frequency-value(frequency-index)
             END-SEARCH
           END-PERFORM.

           SORT frequencies.

           PERFORM VARYING row-number FROM 1 BY 1
             UNTIL row-number > table-length
             SET frequency-index TO 1
             SEARCH ALL frequencies
               WHEN frequency-key(frequency-index) =
                 first-table(row-number)
                 COMPUTE similarity-score =
                   similarity-score +
                   first-table(row-number) *
                   frequency-value(frequency-index)
           END-PERFORM.
           MOVE similarity-score TO similarity-score-display.
           DISPLAY FUNCTION TRIM(similarity-score-display).
