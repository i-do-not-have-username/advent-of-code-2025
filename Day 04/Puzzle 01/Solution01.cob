       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day4part1.

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
       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                 VALUE "Y".

       01  WS-BUF                  PIC X(20000).
       01  WS-PREV                 PIC X(20000).
       01  WS-CURR                 PIC X(20000).
       01  WS-NEXT                 PIC X(20000).

       01  WS-COLS                 PIC 9(9) COMP-5 VALUE 0.

       01  WS-J                    PIC 9(9) COMP-5.
       01  WS-NB                   PIC 9(9) COMP-5.

       01  WS-TOTAL                PIC S9(18) COMP-5 VALUE 0.
       01  WS-OUT                  PIC Z(18)9.

       PROCEDURE DIVISION.
       MAIN.
           ACCEPT WS-FILENAME FROM COMMAND-LINE
           IF WS-FILENAME = SPACES
               MOVE "input.txt" TO WS-FILENAME
           END-IF

           MOVE ALL "." TO WS-PREV
           MOVE ALL "." TO WS-CURR
           MOVE ALL "." TO WS-NEXT

           OPEN INPUT IN-FILE

           *> Read first non-empty line into CURR
           PERFORM UNTIL EOF
               READ IN-FILE INTO WS-BUF
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM NORMALIZE-BUF
                       IF WS-COLS > 0
                           MOVE WS-BUF TO WS-CURR
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM

           IF EOF
               CLOSE IN-FILE
               MOVE WS-TOTAL TO WS-OUT
               DISPLAY FUNCTION TRIM(WS-OUT)
               STOP RUN
           END-IF

           *> Read second line into NEXT (or dots if none)
           READ IN-FILE INTO WS-BUF
               AT END
                   SET EOF TO TRUE
                   MOVE ALL "." TO WS-NEXT
               NOT AT END
                   PERFORM NORMALIZE-BUF
                   IF WS-COLS = 0
                       MOVE ALL "." TO WS-NEXT
                   ELSE
                       MOVE WS-BUF TO WS-NEXT
                   END-IF
           END-READ

           *> Process first row
           PERFORM PROCESS-CURR-ROW

           *> Main sliding-window loop
           PERFORM UNTIL EOF
               MOVE WS-CURR TO WS-PREV
               MOVE WS-NEXT TO WS-CURR

               READ IN-FILE INTO WS-BUF
                   AT END
                       SET EOF TO TRUE
                       MOVE ALL "." TO WS-NEXT
                   NOT AT END
                       PERFORM NORMALIZE-BUF
                       IF WS-COLS = 0
                           MOVE ALL "." TO WS-NEXT
                       ELSE
                           MOVE WS-BUF TO WS-NEXT
                       END-IF
               END-READ

               PERFORM PROCESS-CURR-ROW
           END-PERFORM

           CLOSE IN-FILE

           MOVE WS-TOTAL TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       NORMALIZE-BUF.
           *> Remove CR (Windows CRLF), trim, and set WS-COLS
           INSPECT WS-BUF REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-BUF) TO WS-BUF
           MOVE FUNCTION STORED-CHAR-LENGTH(WS-BUF) TO WS-COLS
           .

       PROCESS-CURR-ROW.
           IF WS-COLS <= 0
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-COLS
               IF WS-CURR(WS-J:1) = "@"
                   MOVE 0 TO WS-NB

                   *> Left-side neighbors (j-1)
                   IF WS-J > 1
                       IF WS-PREV(WS-J - 1:1) = "@" ADD 1 TO WS-NB END-IF
                       IF WS-CURR(WS-J - 1:1) = "@" ADD 1 TO WS-NB END-IF
                       IF WS-NEXT(WS-J - 1:1) = "@" ADD 1 TO WS-NB END-IF
                   END-IF

                   *> Vertical neighbors (j)
                   IF WS-PREV(WS-J:1) = "@" ADD 1 TO WS-NB END-IF
                   IF WS-NEXT(WS-J:1) = "@" ADD 1 TO WS-NB END-IF

                   *> Right-side neighbors (j+1)
                   IF WS-J < WS-COLS
                       IF WS-PREV(WS-J + 1:1) = "@" ADD 1 TO WS-NB END-IF
                       IF WS-CURR(WS-J + 1:1) = "@" ADD 1 TO WS-NB END-IF
                       IF WS-NEXT(WS-J + 1:1) = "@" ADD 1 TO WS-NB END-IF
                   END-IF

                   IF WS-NB < 4
                       ADD 1 TO WS-TOTAL
                   END-IF
               END-IF
           END-PERFORM
           .
