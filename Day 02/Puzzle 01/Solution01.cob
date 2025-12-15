       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day1part2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                  PIC X(4096).

       WORKING-STORAGE SECTION.
       01  WS-FILENAME             PIC X(256)  VALUE "input.txt".
       01  WS-EOF                  PIC X       VALUE "N".
           88  EOF                             VALUE "Y".

       01  WS-LINE                 PIC X(4096).
       01  WS-TRIM                 PIC X(4096).
       01  WS-DIR                  PIC X.
       01  WS-NUM-STR              PIC X(4095).

       01  WS-DIAL                 PIC S9(9)  COMP-5 VALUE 50.

       01  WS-DIST                 PIC S9(18) COMP-5.
       01  WS-RESID                PIC S9(9)  COMP-5.
       01  WS-ADD                  PIC S9(18) COMP-5.
       01  WS-DELTA                PIC S9(9)  COMP-5.
       01  WS-TEMP                 PIC S9(9)  COMP-5.

       01  WS-COUNT                PIC S9(18) COMP-5 VALUE 0.

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

           DISPLAY WS-COUNT
           STOP RUN
           .

       PROCESS-LINE.
           MOVE WS-LINE TO WS-TRIM

           *> Strip CR for Windows CRLF inputs
           INSPECT WS-TRIM REPLACING ALL X"0D" BY SPACE

           MOVE FUNCTION TRIM(WS-TRIM) TO WS-TRIM
           IF WS-TRIM = SPACES
               EXIT PARAGRAPH
           END-IF

           MOVE WS-TRIM(1:1) TO WS-DIR
           MOVE FUNCTION TRIM(WS-TRIM(2:)) TO WS-NUM-STR
           COMPUTE WS-DIST = FUNCTION NUMVAL(WS-NUM-STR)

           *> Count how many k in [1..WS-DIST] satisfy:
           *>   (WS-DIAL + k) mod 100 = 0  for R
           *>   (WS-DIAL - k) mod 100 = 0  for L
           *> without looping even for huge distances.

           IF WS-DIR = "R" OR WS-DIR = "r"
               COMPUTE WS-RESID = FUNCTION MOD(100 - WS-DIAL, 100)
               IF WS-RESID < 0
                   COMPUTE WS-RESID = WS-RESID + 100
               END-IF
           ELSE
               COMPUTE WS-RESID = FUNCTION MOD(WS-DIAL, 100)
               IF WS-RESID < 0
                   COMPUTE WS-RESID = WS-RESID + 100
               END-IF
           END-IF

           IF WS-RESID = 0
               COMPUTE WS-ADD = WS-DIST / 100
           ELSE
               IF WS-DIST < WS-RESID
                   MOVE 0 TO WS-ADD
               ELSE
                   COMPUTE WS-ADD = 1 + ((WS-DIST - WS-RESID) / 100)
               END-IF
           END-IF

           ADD WS-ADD TO WS-COUNT

           *> Update dial position after the whole rotation (mod 100)
           COMPUTE WS-DELTA = FUNCTION MOD(WS-DIST, 100)
           IF WS-DELTA < 0
               COMPUTE WS-DELTA = WS-DELTA + 100
           END-IF

           IF WS-DIR = "R" OR WS-DIR = "r"
               COMPUTE WS-TEMP = WS-DIAL + WS-DELTA
           ELSE
               COMPUTE WS-TEMP = WS-DIAL - WS-DELTA
           END-IF

           COMPUTE WS-TEMP = FUNCTION MOD(WS-TEMP, 100)
           IF WS-TEMP < 0
               COMPUTE WS-TEMP = WS-TEMP + 100
           END-IF

           MOVE WS-TEMP TO WS-DIAL
           .
