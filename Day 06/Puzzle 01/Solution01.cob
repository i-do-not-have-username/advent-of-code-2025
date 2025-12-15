       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day6part1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                   PIC X(20000).

       WORKING-STORAGE SECTION.
       78  MAX-ROWS                 VALUE 500.
       78  MAX-COLS                 VALUE 20000.

       01  WS-FILENAME              PIC X(256) VALUE "input.txt".
       01  WS-EOF                   PIC X VALUE "N".
           88  EOF                  VALUE "Y".

       01  WS-BUF                   PIC X(20000).
       01  WS-LEN                   PIC 9(9) COMP-5.

       01  WS-NROWS                 PIC 9(9) COMP-5 VALUE 0.
       01  WS-MAXLEN                PIC 9(9) COMP-5 VALUE 0.
       01  WS-OPROW                 PIC 9(9) COMP-5 VALUE 0.
       01  WS-LASTDATA              PIC 9(9) COMP-5 VALUE 0.

       01  WS-COL                   PIC 9(9) COMP-5.
       01  WS-STARTCOL              PIC 9(9) COMP-5.
       01  WS-ENDCOL                PIC 9(9) COMP-5.
       01  WS-WIDTH                 PIC 9(9) COMP-5.

       01  WS-R                     PIC 9(9) COMP-5.
       01  WS-K                     PIC 9(9) COMP-5.

       01  WS-ALLSPACE              PIC X VALUE "Y".
       01  WS-OP                    PIC X VALUE SPACE.
       01  WS-CH                    PIC X VALUE SPACE.

       01  WS-SUB                   PIC X(20000).
       01  WS-NUMSTR                PIC X(64).

       01  WS-N                     PIC S9(18) COMP-5.
       01  WS-RESULT                PIC S9(18) COMP-5.
       01  WS-TOTAL                 PIC S9(18) COMP-5 VALUE 0.
       01  WS-OUT                   PIC Z(18)9.

       01  WS-LINES.
           05 WS-LINE-ENTRY OCCURS 500.
              10 WS-LINE-TEXT       PIC X(20000).

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

           IF WS-NROWS < 2
               MOVE 0 TO WS-OUT
               DISPLAY FUNCTION TRIM(WS-OUT)
               STOP RUN
           END-IF

           MOVE WS-NROWS TO WS-OPROW
           COMPUTE WS-LASTDATA = WS-OPROW - 1

           PERFORM SCAN-PROBLEMS

           MOVE WS-TOTAL TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       STORE-LINE.
           *> Remove CR for Windows CRLF
           INSPECT WS-BUF REPLACING ALL X"0D" BY SPACE

           *> Keep leading spaces (alignment matters), remove trailing spaces only
           MOVE FUNCTION TRIM(WS-BUF, TRAILING) TO WS-BUF
           MOVE FUNCTION STORED-CHAR-LENGTH(WS-BUF) TO WS-LEN

           IF WS-LEN = 0
               EXIT PARAGRAPH
           END-IF

           IF WS-NROWS >= MAX-ROWS
               DISPLAY "ERROR: Too many rows (increase MAX-ROWS)."
               STOP RUN
           END-IF

           IF WS-LEN > MAX-COLS
               DISPLAY "ERROR: Line too wide (increase MAX-COLS)."
               STOP RUN
           END-IF

           ADD 1 TO WS-NROWS
           MOVE ALL SPACE TO WS-LINE-TEXT(WS-NROWS)
           MOVE WS-BUF(1:WS-LEN) TO WS-LINE-TEXT(WS-NROWS)(1:WS-LEN)

           IF WS-LEN > WS-MAXLEN
               MOVE WS-LEN TO WS-MAXLEN
           END-IF
           .

       SCAN-PROBLEMS.
           MOVE 1 TO WS-COL

           PERFORM UNTIL WS-COL > WS-MAXLEN
               PERFORM SKIP-BLANK-COLS
               IF WS-COL > WS-MAXLEN
                   EXIT PERFORM
               END-IF

               MOVE WS-COL TO WS-STARTCOL
               PERFORM FIND-SEGMENT-END

               PERFORM PROCESS-PROBLEM
           END-PERFORM
           .

       SKIP-BLANK-COLS.
           PERFORM UNTIL WS-COL > WS-MAXLEN
               PERFORM CHECK-COL-BLANK
               IF WS-ALLSPACE = "Y"
                   ADD 1 TO WS-COL
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

       FIND-SEGMENT-END.
           PERFORM UNTIL WS-COL > WS-MAXLEN
               PERFORM CHECK-COL-BLANK
               IF WS-ALLSPACE = "Y"
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-COL
           END-PERFORM

           COMPUTE WS-ENDCOL = WS-COL - 1
           COMPUTE WS-WIDTH = (WS-ENDCOL - WS-STARTCOL) + 1
           .

       CHECK-COL-BLANK.
           MOVE "Y" TO WS-ALLSPACE
           PERFORM VARYING WS-R FROM 1 BY 1
                   UNTIL WS-R > WS-NROWS OR WS-ALLSPACE = "N"
               IF WS-LINE-TEXT(WS-R)(WS-COL:1) NOT = SPACE
                   MOVE "N" TO WS-ALLSPACE
               END-IF
           END-PERFORM
           .

       PROCESS-PROBLEM.
           *> Find operator in op row within this segment
           MOVE SPACE TO WS-OP
           PERFORM VARYING WS-K FROM WS-STARTCOL BY 1
                   UNTIL WS-K > WS-ENDCOL OR WS-OP NOT = SPACE
               MOVE WS-LINE-TEXT(WS-OPROW)(WS-K:1) TO WS-CH
               IF WS-CH = "+" OR WS-CH = "*"
                   MOVE WS-CH TO WS-OP
               END-IF
           END-PERFORM

           IF WS-OP = SPACE
               EXIT PARAGRAPH
           END-IF

           IF WS-OP = "+"
               MOVE 0 TO WS-RESULT
           ELSE
               MOVE 1 TO WS-RESULT
           END-IF

           *> Accumulate numbers from rows 1..(oprow-1)
           PERFORM VARYING WS-R FROM 1 BY 1 UNTIL WS-R > WS-LASTDATA
               MOVE WS-LINE-TEXT(WS-R)(WS-STARTCOL:WS-WIDTH) TO WS-SUB
               MOVE FUNCTION TRIM(WS-SUB) TO WS-NUMSTR
               IF WS-NUMSTR NOT = SPACES
                   COMPUTE WS-N = FUNCTION NUMVAL(WS-NUMSTR)
                   IF WS-OP = "+"
                       ADD WS-N TO WS-RESULT
                   ELSE
                       MULTIPLY WS-N BY WS-RESULT
                   END-IF
               END-IF
           END-PERFORM

           ADD WS-RESULT TO WS-TOTAL
           .
