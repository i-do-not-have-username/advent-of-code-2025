       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day1part1.

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
       01  WS-DIST                 PIC S9(9) COMP-5.
       01  WS-DIAL                 PIC S9(9) COMP-5 VALUE 50.
       01  WS-TEMP                 PIC S9(9) COMP-5.
       01  WS-COUNT                PIC 9(9)  COMP-5 VALUE 0.

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

           *> Handle Windows CRLF (strip carriage return)
           INSPECT WS-TRIM REPLACING ALL X"0D" BY SPACE

           MOVE FUNCTION TRIM(WS-TRIM) TO WS-TRIM
           IF WS-TRIM = SPACES
               EXIT PARAGRAPH
           END-IF

           MOVE WS-TRIM(1:1) TO WS-DIR
           MOVE FUNCTION TRIM(WS-TRIM(2:)) TO WS-NUM-STR
           COMPUTE WS-DIST = FUNCTION NUMVAL(WS-NUM-STR)

           IF WS-DIR = "L" OR WS-DIR = "l"
               COMPUTE WS-TEMP = WS-DIAL - WS-DIST
           ELSE
               COMPUTE WS-TEMP = WS-DIAL + WS-DIST
           END-IF

           *> Wrap into range 0..99 safely even for negatives
           COMPUTE WS-TEMP = FUNCTION MOD(WS-TEMP, 100)
           IF WS-TEMP < 0
               COMPUTE WS-TEMP = WS-TEMP + 100
           END-IF

           MOVE WS-TEMP TO WS-DIAL

           IF WS-DIAL = 0
               ADD 1 TO WS-COUNT
           END-IF
           .
