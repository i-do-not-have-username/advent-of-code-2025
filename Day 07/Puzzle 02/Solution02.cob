       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day7part2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                   PIC X(10000).

       WORKING-STORAGE SECTION.
       78  MAX-ROWS                 VALUE 2500.
       78  MAX-COLS                 VALUE 2500.
       78  MAX-CELLS                VALUE 6250000.
       78  MAX-STACK                VALUE 500000.

       01  WS-FILENAME              PIC X(256) VALUE "input.txt".
       01  WS-EOF                   PIC X VALUE "N".
           88  EOF                  VALUE "Y".

       01  WS-BUF                   PIC X(10000).
       01  WS-LEN                   PIC 9(9) COMP-5.

       01  WS-ROWS                  PIC 9(9) COMP-5 VALUE 0.
       01  WS-COLS                  PIC 9(9) COMP-5 VALUE 0.
       01  WS-CELLS                 PIC 9(9) COMP-5 VALUE 0.

       01  WS-SFOUND                PIC X VALUE "N".
       01  WS-SROW                  PIC 9(9) COMP-5 VALUE 0.
       01  WS-SCOL                  PIC 9(9) COMP-5 VALUE 0.

       01  WS-R                     PIC 9(9) COMP-5.
       01  WS-C                     PIC 9(9) COMP-5.
       01  WS-I                     PIC 9(9) COMP-5.
       01  WS-CURNEXT               PIC 9(9) COMP-5.

       01  WS-IDX                   PIC 9(9) COMP-5.
       01  WS-CHILD-IDX             PIC 9(9) COMP-5.

       01  WS-ANSWER                PIC S9(18) COMP-5 VALUE 0.

       01  WS-TOP                   PIC 9(9) COMP-5 VALUE 0.
       01  WS-STAGE                 PIC 9     COMP-5.
       01  WS-KROW                  PIC 9(9) COMP-5.
       01  WS-LVAL                  PIC S9(18) COMP-5.
       01  WS-RVAL                  PIC S9(18) COMP-5.

       01  WS-OUT                   PIC Z(18)9.

       01  GRID.
           05 GRID-ROW OCCURS 2500.
              10 GRID-LINE          PIC X(2500).

       *> NEXT splitter row at or below (r,c); 0 = none
       01  NEXTSPLIT-TAB.
           05 NEXTSPLIT OCCURS 6250000 PIC 9(4) COMP-5.

       *> Memoized timelines for state (r,c):
       *>  -1 = unknown, -2 = in progress (cycle guard), >=0 = computed count
       01  MEMO-TAB.
           05 MEMO OCCURS 6250000 PIC S9(18) COMP-5.

       *> Explicit DFS stack frames
       01  STK-R    OCCURS 500000 PIC 9(4)  COMP-5.
       01  STK-C    OCCURS 500000 PIC 9(4)  COMP-5.
       01  STK-K    OCCURS 500000 PIC 9(4)  COMP-5.
       01  STK-STG  OCCURS 500000 PIC 9     COMP-5.
       01  STK-LV   OCCURS 500000 PIC S9(18) COMP-5.
       01  STK-RV   OCCURS 500000 PIC S9(18) COMP-5.

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-FILENAME FROM COMMAND-LINE
           IF WS-FILENAME = SPACES
               MOVE "input.txt" TO WS-FILENAME
           END-IF

           *> init grid to dots
           PERFORM VARYING WS-R FROM 1 BY 1 UNTIL WS-R > MAX-ROWS
               MOVE ALL "." TO GRID-LINE(WS-R)
           END-PERFORM

           OPEN INPUT IN-FILE
           PERFORM UNTIL EOF
               READ IN-FILE INTO WS-BUF
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM STORE-LINE
               END-READ
           END-PERFORM
           CLOSE IN-FILE

           IF WS-ROWS = 0 OR WS-COLS = 0
               MOVE 0 TO WS-OUT
               DISPLAY FUNCTION TRIM(WS-OUT)
               STOP RUN
           END-IF

           IF WS-SFOUND NOT = "Y"
               DISPLAY "ERROR: No S found"
               STOP RUN
           END-IF

           COMPUTE WS-CELLS = WS-ROWS * WS-COLS
           IF WS-CELLS > MAX-CELLS
               DISPLAY "ERROR: Grid too large (increase MAX-ROWS/MAX-COLS/MAX-CELLS)."
               STOP RUN
           END-IF

           *> initialize memo to -1 for used cells
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-CELLS
               MOVE -1 TO MEMO(WS-IDX)
           END-PERFORM

           PERFORM BUILD-NEXTSPLIT

           *> Start just below S
           COMPUTE WS-R = WS-SROW + 1
           MOVE WS-SCOL TO WS-C

           IF WS-R > WS-ROWS OR WS-C < 1 OR WS-C > WS-COLS
               MOVE 1 TO WS-ANSWER
           ELSE
               PERFORM COMPUTE-TIMELINES
               COMPUTE WS-IDX = (WS-R - 1) * WS-COLS + WS-C
               MOVE MEMO(WS-IDX) TO WS-ANSWER
           END-IF

           MOVE WS-ANSWER TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       STORE-LINE.
           INSPECT WS-BUF REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION STORED-CHAR-LENGTH(WS-BUF) TO WS-LEN

           IF WS-LEN = 0
               EXIT PARAGRAPH
           END-IF

           IF WS-ROWS >= MAX-ROWS
               DISPLAY "ERROR: Too many rows (increase MAX-ROWS)."
               STOP RUN
           END-IF
           IF WS-LEN > MAX-COLS
               DISPLAY "ERROR: Too many cols (increase MAX-COLS)."
               STOP RUN
           END-IF

           ADD 1 TO WS-ROWS
           MOVE ALL "." TO GRID-LINE(WS-ROWS)
           MOVE WS-BUF(1:WS-LEN) TO GRID-LINE(WS-ROWS)(1:WS-LEN)

           IF WS-COLS = 0
               MOVE WS-LEN TO WS-COLS
           ELSE
               IF WS-LEN NOT = WS-COLS
                   DISPLAY "ERROR: Ragged grid"
                   STOP RUN
               END-IF
           END-IF

           IF WS-SFOUND NOT = "Y"
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COLS OR WS-SFOUND = "Y"
                   IF GRID-LINE(WS-ROWS)(WS-I:1) = "S"
                       MOVE "Y" TO WS-SFOUND
                       MOVE WS-ROWS TO WS-SROW
                       MOVE WS-I    TO WS-SCOL
                   END-IF
               END-PERFORM
           END-IF
           .

       BUILD-NEXTSPLIT.
           *> For each column, scan bottom-up storing nearest '^' row
           PERFORM VARYING WS-C FROM 1 BY 1 UNTIL WS-C > WS-COLS
               MOVE 0 TO WS-CURNEXT
               PERFORM VARYING WS-R FROM WS-ROWS BY -1 UNTIL WS-R < 1
                   IF GRID-LINE(WS-R)(WS-C:1) = "^"
                       MOVE WS-R TO WS-CURNEXT
                   END-IF
                   COMPUTE WS-IDX = (WS-R - 1) * WS-COLS + WS-C
                   MOVE WS-CURNEXT TO NEXTSPLIT(WS-IDX)
               END-PERFORM
           END-PERFORM
           .

       COMPUTE-TIMELINES.
           *> Iterative DFS with memoization
           MOVE 0 TO WS-TOP
           PERFORM PUSH-FRAME

           PERFORM UNTIL WS-TOP = 0
               MOVE STK-R(WS-TOP)   TO WS-R
               MOVE STK-C(WS-TOP)   TO WS-C
               MOVE STK-STG(WS-TOP) TO WS-STAGE
               MOVE STK-K(WS-TOP)   TO WS-KROW
               MOVE STK-LV(WS-TOP)  TO WS-LVAL
               MOVE STK-RV(WS-TOP)  TO WS-RVAL

               COMPUTE WS-IDX = (WS-R - 1) * WS-COLS + WS-C

               EVALUATE WS-STAGE
                   WHEN 0
                       IF MEMO(WS-IDX) >= 0
                           PERFORM POP-FRAME
                       ELSE
                           IF MEMO(WS-IDX) = -2
                               DISPLAY "ERROR: cycle detected"
                               STOP RUN
                           END-IF

                           MOVE NEXTSPLIT(WS-IDX) TO WS-KROW
                           IF WS-KROW = 0
                               MOVE 1 TO MEMO(WS-IDX)
                               PERFORM POP-FRAME
                           ELSE
                               MOVE -2 TO MEMO(WS-IDX)

                               MOVE WS-KROW TO STK-K(WS-TOP)
                               MOVE 1       TO STK-STG(WS-TOP)
                               MOVE -1      TO STK-LV(WS-TOP)
                               MOVE -1      TO STK-RV(WS-TOP)

                               *> process LEFT
                               IF WS-C = 1
                                   MOVE 1 TO STK-LV(WS-TOP)
                               ELSE
                                   COMPUTE WS-CHILD-IDX = (WS-KROW - 1) * WS-COLS + (WS-C - 1)
                                   IF MEMO(WS-CHILD-IDX) = -2
                                       DISPLAY "ERROR: cycle detected"
                                       STOP RUN
                                   END-IF
                                   IF MEMO(WS-CHILD-IDX) >= 0
                                       MOVE MEMO(WS-CHILD-IDX) TO STK-LV(WS-TOP)
                                   ELSE
                                       MOVE WS-KROW TO WS-R
                                       COMPUTE WS-C = WS-C - 1
                                       PERFORM PUSH-FRAME
                                   END-IF
                               END-IF
                           END-IF
                       END-IF

                   WHEN 1
                       MOVE STK-K(WS-TOP) TO WS-KROW

                       *> ensure LEFT value captured
                       IF STK-LV(WS-TOP) = -1
                           COMPUTE WS-CHILD-IDX = (WS-KROW - 1) * WS-COLS + (WS-C - 1)
                           MOVE MEMO(WS-CHILD-IDX) TO STK-LV(WS-TOP)
                       END-IF

                       MOVE 2 TO STK-STG(WS-TOP)

                       *> process RIGHT
                       IF WS-C = WS-COLS
                           MOVE 1 TO STK-RV(WS-TOP)
                       ELSE
                           COMPUTE WS-CHILD-IDX = (WS-KROW - 1) * WS-COLS + (WS-C + 1)
                           IF MEMO(WS-CHILD-IDX) = -2
                               DISPLAY "ERROR: cycle detected"
                               STOP RUN
                           END-IF
                           IF MEMO(WS-CHILD-IDX) >= 0
                               MOVE MEMO(WS-CHILD-IDX) TO STK-RV(WS-TOP)
                           ELSE
                               MOVE WS-KROW TO WS-R
                               COMPUTE WS-C = WS-C + 1
                               PERFORM PUSH-FRAME
                           END-IF
                       END-IF

                   WHEN 2
                       MOVE STK-K(WS-TOP) TO WS-KROW

                       *> ensure RIGHT value captured
                       IF STK-RV(WS-TOP) = -1
                           COMPUTE WS-CHILD-IDX = (WS-KROW - 1) * WS-COLS + (WS-C + 1)
                           MOVE MEMO(WS-CHILD-IDX) TO STK-RV(WS-TOP)
                       END-IF

                       COMPUTE WS-LVAL = STK-LV(WS-TOP)
                       COMPUTE WS-RVAL = STK-RV(WS-TOP)
                       COMPUTE MEMO(WS-IDX) = WS-LVAL + WS-RVAL

                       PERFORM POP-FRAME

                   WHEN OTHER
                       DISPLAY "ERROR: bad stage"
                       STOP RUN
               END-EVALUATE
           END-PERFORM
           .

       PUSH-FRAME.
           IF WS-TOP >= MAX-STACK
               DISPLAY "ERROR: Stack overflow (increase MAX-STACK)."
               STOP RUN
           END-IF

           ADD 1 TO WS-TOP
           MOVE WS-R TO STK-R(WS-TOP)
           MOVE WS-C TO STK-C(WS-TOP)
           MOVE 0    TO STK-K(WS-TOP)
           MOVE 0    TO STK-STG(WS-TOP)
           MOVE 0    TO STK-LV(WS-TOP)
           MOVE 0    TO STK-RV(WS-TOP)
           .

       POP-FRAME.
           SUBTRACT 1 FROM WS-TOP
           .
