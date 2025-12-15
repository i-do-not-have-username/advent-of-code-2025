       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day5part2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                  PIC X(2000).

       WORKING-STORAGE SECTION.
       78  MAX-RANGES              VALUE 50000.

       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                 VALUE "Y".

       01  WS-LINE                 PIC X(2000).
       01  WS-LEN                  PIC 9(9) COMP-5.

       01  WS-IN-RANGES            PIC X VALUE "Y".
           88  IN-RANGES           VALUE "Y".
           88  DONE-RANGES         VALUE "N".

       01  WS-RCNT                 PIC 9(9) COMP-5 VALUE 0.
       01  WS-MCNT                 PIC 9(9) COMP-5 VALUE 0.

       01  WS-START-STR            PIC X(64).
       01  WS-END-STR              PIC X(64).

       01  WS-A                    PIC S9(18) COMP-3.
       01  WS-B                    PIC S9(18) COMP-3.

       01  WS-I                    PIC 9(9)  COMP-5.
       01  WS-J                    PIC S9(9) COMP-5.

       01  WS-KEY-START            PIC S9(18) COMP-3.
       01  WS-KEY-END              PIC S9(18) COMP-3.

       01  WS-TOTAL                PIC S9(18) COMP-3 VALUE 0.
       01  WS-LENR                 PIC S9(18) COMP-3.
       01  WS-OUT                  PIC Z(18)9.

       01  RANGE-TABLE.
           05 RANGE-ENTRY OCCURS 50000.
              10 R-START           PIC S9(18) COMP-3.
              10 R-END             PIC S9(18) COMP-3.

       01  MERGED-TABLE.
           05 MERGED-ENTRY OCCURS 50000.
              10 M-START           PIC S9(18) COMP-3.
              10 M-END             PIC S9(18) COMP-3.

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-FILENAME FROM COMMAND-LINE
           IF WS-FILENAME = SPACES
               MOVE "input.txt" TO WS-FILENAME
           END-IF

           OPEN INPUT IN-FILE

           PERFORM UNTIL EOF
               READ IN-FILE INTO WS-LINE
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM

           CLOSE IN-FILE

           *> If file ended before blank line, still finalize
           IF IN-RANGES
               PERFORM FINALIZE-RANGES
           END-IF

           MOVE WS-TOTAL TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       PROCESS-LINE.
           *> Strip CR for Windows CRLF, then trim
           INSPECT WS-LINE REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-LINE) TO WS-LINE
           MOVE FUNCTION STORED-CHAR-LENGTH(WS-LINE) TO WS-LEN

           IF IN-RANGES
               IF WS-LEN = 0
                   SET DONE-RANGES TO TRUE
                   PERFORM FINALIZE-RANGES
                   *> Part 2 ignores the rest of the file
                   SET EOF TO TRUE
               ELSE
                   PERFORM ADD-RANGE
               END-IF
           END-IF
           .

       ADD-RANGE.
           MOVE SPACES TO WS-START-STR
           MOVE SPACES TO WS-END-STR

           UNSTRING WS-LINE
               DELIMITED BY "-"
               INTO WS-START-STR WS-END-STR
           END-UNSTRING

           MOVE FUNCTION TRIM(WS-START-STR) TO WS-START-STR
           MOVE FUNCTION TRIM(WS-END-STR)   TO WS-END-STR

           IF WS-START-STR = SPACES OR WS-END-STR = SPACES
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-A = FUNCTION NUMVAL(WS-START-STR)
           COMPUTE WS-B = FUNCTION NUMVAL(WS-END-STR)

           IF WS-A > WS-B
               MOVE WS-A TO WS-KEY-START
               MOVE WS-B TO WS-A
               MOVE WS-KEY-START TO WS-B
           END-IF

           IF WS-RCNT >= MAX-RANGES
               DISPLAY "ERROR: Too many ranges (increase MAX-RANGES)."
               STOP RUN
           END-IF

           ADD 1 TO WS-RCNT
           MOVE WS-A TO R-START(WS-RCNT)
           MOVE WS-B TO R-END(WS-RCNT)
           .

       FINALIZE-RANGES.
           IF WS-RCNT = 0
               MOVE 0 TO WS-TOTAL
               EXIT PARAGRAPH
           END-IF

           PERFORM SORT-RANGES
           PERFORM MERGE-RANGES
           PERFORM SUM-MERGED
           .

       SORT-RANGES.
           *> Insertion sort by start ascending (OK for typical AoC input sizes)
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > WS-RCNT
               MOVE R-START(WS-I) TO WS-KEY-START
               MOVE R-END(WS-I)   TO WS-KEY-END
               COMPUTE WS-J = WS-I - 1

               PERFORM UNTIL WS-J < 1 OR R-START(WS-J) <= WS-KEY-START
                   MOVE R-START(WS-J) TO R-START(WS-J + 1)
                   MOVE R-END(WS-J)   TO R-END(WS-J + 1)
                   SUBTRACT 1 FROM WS-J
               END-PERFORM

               MOVE WS-KEY-START TO R-START(WS-J + 1)
               MOVE WS-KEY-END   TO R-END(WS-J + 1)
           END-PERFORM
           .

       MERGE-RANGES.
           MOVE 0 TO WS-MCNT

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-RCNT
               IF WS-MCNT = 0
                   MOVE 1 TO WS-MCNT
                   MOVE R-START(WS-I) TO M-START(1)
                   MOVE R-END(WS-I)   TO M-END(1)
               ELSE
                   *> merge if overlaps OR touches (inclusive ranges)
                   IF R-START(WS-I) <= (M-END(WS-MCNT) + 1)
                       IF R-END(WS-I) > M-END(WS-MCNT)
                           MOVE R-END(WS-I) TO M-END(WS-MCNT)
                       END-IF
                   ELSE
                       ADD 1 TO WS-MCNT
                       MOVE R-START(WS-I) TO M-START(WS-MCNT)
                       MOVE R-END(WS-I)   TO M-END(WS-MCNT)
                   END-IF
               END-IF
           END-PERFORM
           .

       SUM-MERGED.
           MOVE 0 TO WS-TOTAL
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MCNT
               COMPUTE WS-LENR = (M-END(WS-I) - M-START(WS-I)) + 1
               ADD WS-LENR TO WS-TOTAL
           END-PERFORM
           .
