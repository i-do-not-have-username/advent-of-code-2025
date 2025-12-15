       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day7part1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                   PIC X(5000).

       WORKING-STORAGE SECTION.
       78  MAX-ROWS                 VALUE 5000.
       78  MAX-COLS                 VALUE 5000.
       78  MAX-STACK                VALUE 2000000.

       01  WS-FILENAME              PIC X(256) VALUE "input.txt".
       01  WS-EOF                   PIC X VALUE "N".
           88  EOF                  VALUE "Y".

       01  WS-BUF                   PIC X(5000).
       01  WS-LEN                   PIC 9(9) COMP-5.

       01  WS-ROWS                  PIC 9(9) COMP-5 VALUE 0.
       01  WS-COLS                  PIC 9(9) COMP-5 VALUE 0.

       01  WS-S-FOUND               PIC X VALUE "N".
       01  WS-SROW                  PIC 9(9) COMP-5 VALUE 0.
       01  WS-SCOL                  PIC 9(9) COMP-5 VALUE 0.

       01  WS-R                     PIC 9(9) COMP-5.
       01  WS-C                     PIC 9(9) COMP-5.
       01  WS-I                     PIC 9(9) COMP-5.

       01  WS-CH                    PIC X.

       01  WS-SPLITS                PIC S9(18) COMP-5 VALUE 0.

       *> Stack holds encoded IDX = (row-1)*cols + col
       01  WS-STK-TOP               PIC 9(9) COMP-5 VALUE 0.
       01  WS-STK-IDX               OCCURS 2000000 TIMES
                                   PIC S9(18) COMP-5.

       01  WS-IDX                   PIC S9(18) COMP-5.
       01  WS-IDX2                  PIC S9(18) COMP-5.
       01  WS-TMP                   PIC S9(18) COMP-5.
       01  WS-R0                    PIC S9(18) COMP-5.
       01  WS-C0                    PIC S9(18) COMP-5.

       01  WS-OUT                   PIC Z(18)9.

       01  WS-GRID.
           05 WS-GRID-ROW OCCURS 5000.
              10 WS-GRID-LINE       PIC X(5000).

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-FILENAME FROM COMMAND-LINE
           IF WS-FILENAME = SPACES
               MOVE "input.txt" TO WS-FILENAME
           END-IF

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

           IF WS-S-FOUND NOT = "Y"
               DISPLAY "ERROR: No 'S' found in input."
               STOP RUN
           END-IF

           *> Initial beam starts just below S (same column)
           COMPUTE WS-R = WS-SROW + 1
           MOVE WS-SCOL TO WS-C

           IF WS-R >= 1 AND WS-R <= WS-ROWS
               PERFORM PUSH-RC
           END-IF

           PERFORM UNTIL WS-STK-TOP = 0
               MOVE WS-STK-IDX(WS-STK-TOP) TO WS-IDX
               SUBTRACT 1 FROM WS-STK-TOP

               *> Decode WS-IDX -> (WS-R, WS-C)
               COMPUTE WS-TMP = WS-IDX - 1
               DIVIDE WS-TMP BY WS-COLS GIVING WS-R0 REMAINDER WS-C0
               COMPUTE WS-R = WS-R0 + 1
               COMPUTE WS-C = WS-C0 + 1

               PERFORM TRACE-BEAM
           END-PERFORM

           MOVE WS-SPLITS TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       STORE-LINE.
           INSPECT WS-BUF REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-BUF, TRAILING) TO WS-BUF
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
           MOVE ALL SPACE TO WS-GRID-LINE(WS-ROWS)
           MOVE WS-BUF(1:WS-LEN) TO WS-GRID-LINE(WS-ROWS)(1:WS-LEN)

           IF WS-COLS = 0
               MOVE WS-LEN TO WS-COLS
           ELSE
               IF WS-LEN NOT = WS-COLS
                   DISPLAY "ERROR: Ragged grid (lines not same length)."
                   STOP RUN
               END-IF
           END-IF

           *> Find S (first occurrence)
           IF WS-S-FOUND NOT = "Y"
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COLS OR WS-S-FOUND = "Y"
                   IF WS-GRID-LINE(WS-ROWS)(WS-I:1) = "S"
                       MOVE "Y" TO WS-S-FOUND
                       MOVE WS-ROWS TO WS-SROW
                       MOVE WS-I    TO WS-SCOL
                   END-IF
               END-PERFORM
           END-IF
           .

       PUSH-RC.
           *> Push current (WS-R, WS-C) if within bounds
           IF WS-C < 1 OR WS-C > WS-COLS
               EXIT PARAGRAPH
           END-IF
           IF WS-R < 1 OR WS-R > WS-ROWS
               EXIT PARAGRAPH
           END-IF

           IF WS-STK-TOP >= MAX-STACK
               DISPLAY "ERROR: Stack overflow (increase MAX-STACK)."
               STOP RUN
           END-IF

           COMPUTE WS-IDX2 = (WS-R - 1) * WS-COLS + WS-C
           ADD 1 TO WS-STK-TOP
           MOVE WS-IDX2 TO WS-STK-IDX(WS-STK-TOP)
           .

       TRACE-BEAM.
           *> Follow one beam straight down until it splits, merges, or exits
           PERFORM UNTIL WS-R < 1 OR WS-R > WS-ROWS OR WS-C < 1 OR WS-C > WS-COLS

               MOVE WS-GRID-LINE(WS-R)(WS-C:1) TO WS-CH

               *> Already visited/processed => merge; stop tracing
               IF WS-CH = "v" OR WS-CH = "x"
                   EXIT PERFORM
               END-IF

               IF WS-CH = "^"
                   *> Splitter encountered
                   MOVE "x" TO WS-GRID-LINE(WS-R)(WS-C:1)
                   ADD 1 TO WS-SPLITS

                   *> Spawn beams from immediate left/right of the splitter
                   IF WS-C > 1
                       MOVE WS-R TO WS-R
                       COMPUTE WS-C = WS-C - 1
                       PERFORM PUSH-RC
                       COMPUTE WS-C = WS-C + 1
                   END-IF

                   IF WS-C < WS-COLS
                       MOVE WS-R TO WS-R
                       COMPUTE WS-C = WS-C + 1
                       PERFORM PUSH-RC
                       COMPUTE WS-C = WS-C - 1
                   END-IF

                   EXIT PERFORM
               ELSE
                   *> Empty space (or 'S') => mark visited and continue downward
                   MOVE "v" TO WS-GRID-LINE(WS-R)(WS-C:1)
                   ADD 1 TO WS-R
               END-IF
           END-PERFORM
           .
