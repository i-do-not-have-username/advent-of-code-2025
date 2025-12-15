       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day6part2.

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
       78  MAX-ROWS                 VALUE 2000.
       78  MAX-COLS                 VALUE 20000.

       01  WS-FILENAME              PIC X(256) VALUE "input.txt".
       01  WS-EOF                   PIC X VALUE "N".
           88  EOF                  VALUE "Y".

       01  WS-BUF                   PIC X(20000).
       01  WS-LEN                   PIC 9(9) COMP-5.
       01  WS-NROWS                 PIC 9(9) COMP-5 VALUE 0.
       01  WS-MAXLEN                PIC 9(9) COMP-5 VALUE 0.

       01  WS-OPROW                 PIC 9(9) COMP-5.
       01  WS-LASTDATA              PIC 9(9) COMP-5.

       01  WS-COL                   PIC 9(9) COMP-5.
       01  WS-STARTCOL              PIC 9(9) COMP-5.
       01  WS-ENDCOL                PIC 9(9) COMP-5.
       01  WS-WIDTH                 PIC 9(9) COMP-5.

       01  WS-R                     PIC 9(9) COMP-5.
       01  WS-K                     PIC 9(9) COMP-5.
       01  WS-C                     PIC 9(9) COMP-5.
       01  WS-CSTART                PIC 9(9) COMP-5.

       01  WS-ALLSPACE              PIC X VALUE "Y".
       01  WS-OP                    PIC X VALUE SPACE.
       01  WS-CH                    PIC X VALUE SPACE.

       01  WS-DIGITS                PIC X(2000).
       01  WS-DIG-PTR               PIC 9(9) COMP-5.
       01  WS-NUM-LEN               PIC 9(9) COMP-5.

       01  WS-N                     PIC S9(31) COMP-3.
       01  WS-RESULT                PIC S9(31) COMP-3.
       01  WS-TOTAL                 PIC S9(31) COMP-3 VALUE 0.
       01  WS-OUT                   PIC Z(30)9.

       01  WS-LINES.
           05 WS-LINE-ENTRY OCCURS 2000.
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
           *> Remove CR (Windows CRLF)
           INSPECT WS-BUF REPLACING ALL X"0D" BY SPACE

           *> Compute trailing-trim length WITHOUT removing leading spaces
           MOVE FUNCTION STORED-CHAR-LENGTH(WS-BUF) TO WS-LEN
           PERFORM UNTIL WS-LEN = 0 OR WS-BUF(WS-LEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-LEN
           END-PERFORM

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
           COMPUTE WS-WIDTH  = (WS-ENDCOL - WS-STARTCOL) + 1
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
           *> Operator is on the bottom row within this segment
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

           *> Read numbers right-to-left by COLUMN inside the segment.
           MOVE WS-ENDCOL TO WS-CSTART

           PERFORM VARYING WS-C FROM WS-CSTART BY -1 UNTIL WS-C < WS-STARTCOL

               MOVE SPACES TO WS-DIGITS
               MOVE 1      TO WS-DIG-PTR

               *> Build the number for this column: top -> bottom, skipping spaces
               PERFORM VARYING WS-R FROM 1 BY 1 UNTIL WS-R > WS-LASTDATA
                   MOVE WS-LINE-TEXT(WS-R)(WS-C:1) TO WS-CH
                   IF WS-CH >= "0" AND WS-CH <= "9"
                       IF WS-DIG-PTR <= 2000
                           MOVE WS-CH TO WS-DIGITS(WS-DIG-PTR:1)
                           ADD 1 TO WS-DIG-PTR
                       ELSE
                           DISPLAY "ERROR: Too many digits in one column (increase WS-DIGITS)."
                           STOP RUN
                       END-IF
                   END-IF
               END-PERFORM

               COMPUTE WS-NUM-LEN = WS-DIG-PTR - 1

               IF WS-NUM-LEN > 0
                   COMPUTE WS-N = FUNCTION NUMVAL(WS-DIGITS(1:WS-NUM-LEN))
                   IF WS-OP = "+"
                       ADD WS-N TO WS-RESULT
                   ELSE
                       MULTIPLY WS-N BY WS-RESULT
                   END-IF
               END-IF

           END-PERFORM

           ADD WS-RESULT TO WS-TOTAL
           .
