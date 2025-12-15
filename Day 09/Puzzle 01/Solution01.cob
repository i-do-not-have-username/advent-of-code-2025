       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day9part1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                  PIC X(256).

       WORKING-STORAGE SECTION.
       78  MAX-PTS                 VALUE 20000.

       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                         VALUE "Y".

       01  WS-LINE                 PIC X(256).
       01  WS-LEN                  PIC 9(9) COMP-5.

       01  WS-XSTR                 PIC X(64).
       01  WS-YSTR                 PIC X(64).

       01  WS-N                    PIC 9(9) COMP-5 VALUE 0.
       01  WS-I                    PIC 9(9) COMP-5.
       01  WS-J                    PIC 9(9) COMP-5.
       01  WS-JSTART               PIC 9(9) COMP-5.

       01  WS-DX                   PIC S9(18) COMP-5.
       01  WS-DY                   PIC S9(18) COMP-5.
       01  WS-WIDTH                PIC 9(18)  COMP-5.
       01  WS-HEIGHT               PIC 9(18)  COMP-5.
       01  WS-AREA                 PIC 9(18)  COMP-5.
       01  WS-MAX                  PIC 9(18)  COMP-5 VALUE 0.

       01  XARR OCCURS 20000       PIC S9(18) COMP-5.
       01  YARR OCCURS 20000       PIC S9(18) COMP-5.

       01  WS-OUT                  PIC Z(30)9.

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
                       PERFORM READ-POINT
               END-READ
           END-PERFORM
           CLOSE IN-FILE

           IF WS-N < 2
               MOVE 0 TO WS-OUT
               DISPLAY FUNCTION TRIM(WS-OUT)
               STOP RUN
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= WS-N
               COMPUTE WS-JSTART = WS-I + 1
               PERFORM VARYING WS-J FROM WS-JSTART BY 1 UNTIL WS-J > WS-N
                   COMPUTE WS-DX = XARR(WS-I) - XARR(WS-J)
                   IF WS-DX < 0
                       COMPUTE WS-DX = 0 - WS-DX
                   END-IF
                   COMPUTE WS-WIDTH = WS-DX + 1

                   COMPUTE WS-DY = YARR(WS-I) - YARR(WS-J)
                   IF WS-DY < 0
                       COMPUTE WS-DY = 0 - WS-DY
                   END-IF
                   COMPUTE WS-HEIGHT = WS-DY + 1

                   COMPUTE WS-AREA = WS-WIDTH * WS-HEIGHT
                   IF WS-AREA > WS-MAX
                       MOVE WS-AREA TO WS-MAX
                   END-IF
               END-PERFORM
           END-PERFORM

           MOVE WS-MAX TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       READ-POINT.
           INSPECT WS-LINE REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-LINE) TO WS-LINE
           MOVE FUNCTION STORED-CHAR-LENGTH(WS-LINE) TO WS-LEN
           IF WS-LEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-XSTR
           MOVE SPACES TO WS-YSTR

           UNSTRING WS-LINE DELIMITED BY ","
               INTO WS-XSTR WS-YSTR
           END-UNSTRING

           IF WS-N >= MAX-PTS
               DISPLAY "ERROR: Too many points; increase MAX-PTS."
               STOP RUN
           END-IF

           ADD 1 TO WS-N
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-XSTR)) TO XARR(WS-N)
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-YSTR)) TO YARR(WS-N)
           .
