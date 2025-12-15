       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day4part2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                  PIC X(20000).

       WORKING-STORAGE SECTION.
       *> If your grid is larger, raise these three numbers consistently.
       78  MAX-ROWS                VALUE 2000.
       78  MAX-COLS                VALUE 2000.
       78  MAX-CELLS               VALUE 4000000.  *> MAX-ROWS * MAX-COLS

       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                 VALUE "Y".

       01  WS-LINE                 PIC X(20000).
       01  WS-LEN                  PIC 9(9) COMP-5.

       01  WS-ROWS                 PIC 9(9) COMP-5 VALUE 0.
       01  WS-COLS                 PIC 9(9) COMP-5 VALUE 0.

       01  WS-R                    PIC 9(9) COMP-5.
       01  WS-C                    PIC 9(9) COMP-5.
       01  WS-NR                   PIC 9(9) COMP-5.
       01  WS-NC                   PIC 9(9) COMP-5.

       01  WS-NB                   PIC 9(9) COMP-5.
       01  WS-IDX                  PIC 9(9) COMP-5.
       01  WS-TMP                  PIC 9(9) COMP-5.
       01  WS-ROW0                 PIC 9(9) COMP-5.
       01  WS-COL0                 PIC 9(9) COMP-5.

       01  WS-QHEAD                PIC 9(9) COMP-5 VALUE 1.
       01  WS-QTAIL                PIC 9(9) COMP-5 VALUE 0.

       01  WS-REMOVED              PIC S9(18) COMP-5 VALUE 0.
       01  WS-OUT                  PIC Z(18)9.

       01  WS-GRID.
           05 WS-GRID-ROW OCCURS 2000.
              10 WS-GRID-LINE      PIC X(2000).

       01  WS-DEGREE.
           05 WS-DEG-ROW OCCURS 2000.
              10 WS-DEG OCCURS 2000 PIC 9 COMP-5.

       01  WS-QUEUE.
           05 WS-QIDX OCCURS 4000000 PIC 9(9) COMP-5.

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-FILENAME FROM COMMAND-LINE
           IF WS-FILENAME = SPACES
               MOVE "input.txt" TO WS-FILENAME
           END-IF

           *> Initialize grid to dots
           PERFORM VARYING WS-R FROM 1 BY 1 UNTIL WS-R > MAX-ROWS
               MOVE ALL "." TO WS-GRID-LINE(WS-R)
           END-PERFORM

           OPEN INPUT IN-FILE

           PERFORM UNTIL EOF
               READ IN-FILE INTO WS-LINE
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM NORMALIZE-LINE
                       IF WS-LEN > 0
                           ADD 1 TO WS-ROWS
                           IF WS-ROWS > MAX-ROWS
                               DISPLAY "ERROR: Too many rows (increase MAX-ROWS)."
                               STOP RUN
                           END-IF

                           IF WS-COLS = 0
                               MOVE WS-LEN TO WS-COLS
                               IF WS-COLS > MAX-COLS
                                   DISPLAY "ERROR: Too many columns (increase MAX-COLS)."
                                   STOP RUN
                               END-IF
                           ELSE
                               IF WS-LEN NOT = WS-COLS
                                   DISPLAY "ERROR: Ragged grid (lines not same length)."
                                   STOP RUN
                               END-IF
                           END-IF

                           *> Copy line into grid row (rest already '.')
                           MOVE WS-LINE(1:WS-COLS)
                             TO WS-GRID-LINE(WS-ROWS)(1:WS-COLS)
                       END-IF
               END-READ
           END-PERFORM

           CLOSE IN-FILE

           IF WS-ROWS = 0 OR WS-COLS = 0
               MOVE 0 TO WS-OUT
               DISPLAY FUNCTION TRIM(WS-OUT)
               STOP RUN
           END-IF

           IF (WS-ROWS * WS-COLS) > MAX-CELLS
               DISPLAY "ERROR: Grid exceeds MAX-CELLS (increase constants)."
               STOP RUN
           END-IF

           PERFORM INIT-DEGREES-AND-QUEUE
           PERFORM PEEL-PROCESS

           MOVE WS-REMOVED TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       NORMALIZE-LINE.
           *> Remove CR (Windows CRLF) and trim
           INSPECT WS-LINE REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-LINE) TO WS-LINE
           MOVE FUNCTION STORED-CHAR-LENGTH(WS-LINE) TO WS-LEN
           .

       INIT-DEGREES-AND-QUEUE.
           MOVE 1 TO WS-QHEAD
           MOVE 0 TO WS-QTAIL

           PERFORM VARYING WS-R FROM 1 BY 1 UNTIL WS-R > WS-ROWS
               PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > WS-COLS
                   IF WS-GRID-LINE(WS-R)(WS-C:1) = "@"
                       MOVE 0 TO WS-NB

                       IF WS-R > 1 AND WS-C > 1
                           IF WS-GRID-LINE(WS-R - 1)(WS-C - 1:1) = "@" ADD 1 TO WS-NB END-IF
                       END-IF
                       IF WS-R > 1
                           IF WS-GRID-LINE(WS-R - 1)(WS-C:1) = "@" ADD 1 TO WS-NB END-IF
                       END-IF
                       IF WS-R > 1 AND WS-C < WS-COLS
                           IF WS-GRID-LINE(WS-R - 1)(WS-C + 1:1) = "@" ADD 1 TO WS-NB END-IF
                       END-IF
                       IF WS-C > 1
                           IF WS-GRID-LINE(WS-R)(WS-C - 1:1) = "@" ADD 1 TO WS-NB END-IF
                       END-IF
                       IF WS-C < WS-COLS
                           IF WS-GRID-LINE(WS-R)(WS-C + 1:1) = "@" ADD 1 TO WS-NB END-IF
                       END-IF
                       IF WS-R < WS-ROWS AND WS-C > 1
                           IF WS-GRID-LINE(WS-R + 1)(WS-C - 1:1) = "@" ADD 1 TO WS-NB END-IF
                       END-IF
                       IF WS-R < WS-ROWS
                           IF WS-GRID-LINE(WS-R + 1)(WS-C:1) = "@" ADD 1 TO WS-NB END-IF
                       END-IF
                       IF WS-R < WS-ROWS AND WS-C < WS-COLS
                           IF WS-GRID-LINE(WS-R + 1)(WS-C + 1:1) = "@" ADD 1 TO WS-NB END-IF
                       END-IF

                       MOVE WS-NB TO WS-DEG(WS-R, WS-C)

                       IF WS-NB < 4
                           PERFORM ENQUEUE-RC
                       END-IF
                   ELSE
                       MOVE 0 TO WS-DEG(WS-R, WS-C)
                   END-IF
               END-PERFORM
           END-PERFORM
           .

       ENQUEUE-RC.
           COMPUTE WS-IDX = (WS-R - 1) * WS-COLS + WS-C
           ADD 1 TO WS-QTAIL
           MOVE WS-IDX TO WS-QIDX(WS-QTAIL)
           .

       PEEL-PROCESS.
           PERFORM UNTIL WS-QHEAD > WS-QTAIL
               MOVE WS-QIDX(WS-QHEAD) TO WS-IDX
               ADD 1 TO WS-QHEAD

               *> Decode IDX -> (R,C)
               COMPUTE WS-TMP = WS-IDX - 1
               DIVIDE WS-TMP BY WS-COLS GIVING WS-ROW0 REMAINDER WS-COL0
               COMPUTE WS-R = WS-ROW0 + 1
               COMPUTE WS-C = WS-COL0 + 1

               IF WS-GRID-LINE(WS-R)(WS-C:1) NOT = "@"
                   CONTINUE
               ELSE
                   MOVE "." TO WS-GRID-LINE(WS-R)(WS-C:1)
                   ADD 1 TO WS-REMOVED

                   *> Update neighbors (each loses 1 adjacent @)
                   IF WS-R > 1 AND WS-C > 1
                       COMPUTE WS-NR = WS-R - 1
                       COMPUTE WS-NC = WS-C - 1
                       PERFORM DEC-NEIGHBOR
                   END-IF

                   IF WS-R > 1
                       COMPUTE WS-NR = WS-R - 1
                       MOVE WS-C TO WS-NC
                       PERFORM DEC-NEIGHBOR
                   END-IF

                   IF WS-R > 1 AND WS-C < WS-COLS
                       COMPUTE WS-NR = WS-R - 1
                       COMPUTE WS-NC = WS-C + 1
                       PERFORM DEC-NEIGHBOR
                   END-IF

                   IF WS-C > 1
                       MOVE WS-R TO WS-NR
                       COMPUTE WS-NC = WS-C - 1
                       PERFORM DEC-NEIGHBOR
                   END-IF

                   IF WS-C < WS-COLS
                       MOVE WS-R TO WS-NR
                       COMPUTE WS-NC = WS-C + 1
                       PERFORM DEC-NEIGHBOR
                   END-IF

                   IF WS-R < WS-ROWS AND WS-C > 1
                       COMPUTE WS-NR = WS-R + 1
                       COMPUTE WS-NC = WS-C - 1
                       PERFORM DEC-NEIGHBOR
                   END-IF

                   IF WS-R < WS-ROWS
                       COMPUTE WS-NR = WS-R + 1
                       MOVE WS-C TO WS-NC
                       PERFORM DEC-NEIGHBOR
                   END-IF

                   IF WS-R < WS-ROWS AND WS-C < WS-COLS
                       COMPUTE WS-NR = WS-R + 1
                       COMPUTE WS-NC = WS-C + 1
                       PERFORM DEC-NEIGHBOR
                   END-IF
               END-IF
           END-PERFORM
           .

       DEC-NEIGHBOR.
           IF WS-GRID-LINE(WS-NR)(WS-NC:1) = "@"
               SUBTRACT 1 FROM WS-DEG(WS-NR, WS-NC)
               IF WS-DEG(WS-NR, WS-NC) = 3
                   *> Just became <4, enqueue once at the threshold crossing
                   COMPUTE WS-IDX = (WS-NR - 1) * WS-COLS + WS-NC
                   ADD 1 TO WS-QTAIL
                   MOVE WS-IDX TO WS-QIDX(WS-QTAIL)
               END-IF
           END-IF
           .
