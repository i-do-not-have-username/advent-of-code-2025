       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day3part1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                  PIC X(200000).

       WORKING-STORAGE SECTION.
       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                 VALUE "Y".

       01  WS-LINE                 PIC X(200000).
       01  WS-LEN                  PIC 9(9) COMP-5.
       01  WS-START                PIC 9(9) COMP-5.

       01  WS-I                    PIC 9(9) COMP-5.
       01  WS-DIG                  PIC 9     COMP-5.
       01  WS-BEST-ONES            PIC 9     COMP-5.
       01  WS-BEST-PAIR            PIC 9(9)  COMP-5.
       01  WS-CAND                 PIC 9(9)  COMP-5.

       01  WS-TOTAL                PIC S9(18) COMP-5 VALUE 0.
       01  WS-OUT                  PIC Z(18)9.

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

           MOVE WS-TOTAL TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       PROCESS-LINE.
           INSPECT WS-LINE REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-LINE) TO WS-LINE

           MOVE FUNCTION STORED-CHAR-LENGTH(WS-LINE) TO WS-LEN
           IF WS-LEN < 2
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-BEST-ONES = FUNCTION NUMVAL(WS-LINE(WS-LEN:1))
           MOVE 0 TO WS-BEST-PAIR

           COMPUTE WS-START = WS-LEN - 1

           PERFORM VARYING WS-I FROM WS-START BY -1 UNTIL WS-I < 1
               COMPUTE WS-DIG  = FUNCTION NUMVAL(WS-LINE(WS-I:1))
               COMPUTE WS-CAND = (WS-DIG * 10) + WS-BEST-ONES

               IF WS-CAND > WS-BEST-PAIR
                   MOVE WS-CAND TO WS-BEST-PAIR
               END-IF

               IF WS-DIG > WS-BEST-ONES
                   MOVE WS-DIG TO WS-BEST-ONES
               END-IF
           END-PERFORM

           ADD WS-BEST-PAIR TO WS-TOTAL
           .
