       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day9part2.

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
       78  MAX-PTS                 VALUE 30000.

       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                         VALUE "Y".

       01  WS-LINE                 PIC X(256).
       01  WS-LEN                  PIC 9(9) COMP-5.

       01  WS-XSTR                 PIC X(64).
       01  WS-YSTR                 PIC X(64).

       01  N                       PIC 9(9) COMP-5 VALUE 0.

       01  I                       PIC 9(9) COMP-5.
       01  J                       PIC 9(9) COMP-5.
       01  JSTART                  PIC 9(9) COMP-5.
       01  K                       PIC 9(9) COMP-5.
       01  JJ                      PIC 9(9) COMP-5.

       01  TMP-IX                  PIC S9(18) COMP-5.
       01  TMP-IY                  PIC S9(18) COMP-5.

       01  X1                      PIC S9(18) COMP-5.
       01  Y1                      PIC S9(18) COMP-5.
       01  X2                      PIC S9(18) COMP-5.
       01  Y2                      PIC S9(18) COMP-5.

       01  MINX                    PIC S9(18) COMP-5.
       01  MAXX                    PIC S9(18) COMP-5.
       01  MINY                    PIC S9(18) COMP-5.
       01  MAXY                    PIC S9(18) COMP-5.

       01  DX                      PIC S9(18) COMP-5.
       01  DY                      PIC S9(18) COMP-5.
       01  WID                     PIC S9(18) COMP-5.
       01  HEI                     PIC S9(18) COMP-5.

       01  WS-AREA                 PIC S9(31) COMP-3.
       01  WS-BEST                 PIC S9(31) COMP-3 VALUE 0.

       01  OK-FLAG                 PIC X VALUE "Y".
       01  INSIDE-FLAG             PIC X VALUE "N".

       *> point arrays
       01  PX OCCURS 30000         PIC S9(18) COMP-5.
       01  PY OCCURS 30000         PIC S9(18) COMP-5.

       *> polygon boundary segments
       01  NV                      PIC 9(9) COMP-5 VALUE 0.
       01  NH                      PIC 9(9) COMP-5 VALUE 0.

       01  VX OCCURS 30000         PIC S9(18) COMP-5.
       01  VY1 OCCURS 30000        PIC S9(18) COMP-5.
       01  VY2 OCCURS 30000        PIC S9(18) COMP-5.

       01  HY OCCURS 30000         PIC S9(18) COMP-5.
       01  HX1 OCCURS 30000        PIC S9(18) COMP-5.
       01  HX2 OCCURS 30000        PIC S9(18) COMP-5.

       *> binary-search helpers
       01  BS-LOW                  PIC 9(9) COMP-5.
       01  BS-HIGH                 PIC 9(9) COMP-5.
       01  BS-MID                  PIC 9(9) COMP-5.
       01  VSTART                  PIC 9(9) COMP-5.
       01  VEND                    PIC 9(9) COMP-5.
       01  HSTART                  PIC 9(9) COMP-5.
       01  HEND                    PIC 9(9) COMP-5.

       *> point-in-polygon helpers
       01  QX                      PIC S9(18) COMP-5.
       01  QY                      PIC S9(18) COMP-5.
       01  CROSS                   PIC 9(9) COMP-5.
       01  XE                      PIC S9(18) COMP-5.
       01  YLO                     PIC S9(18) COMP-5.
       01  YHI                     PIC S9(18) COMP-5.

       01  OUTPIC                  PIC Z(30)9.

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

           IF N < 4
               MOVE 0 TO OUTPIC
               DISPLAY FUNCTION TRIM(OUTPIC)
               STOP RUN
           END-IF

           PERFORM BUILD-SEGMENTS
           PERFORM SORT-VERTICAL
           PERFORM SORT-HORIZONTAL

           *> try all pairs of red tiles as opposite corners
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= N
               COMPUTE JSTART = I + 1
               PERFORM VARYING J FROM JSTART BY 1 UNTIL J > N
                   MOVE PX(I) TO X1
                   MOVE PY(I) TO Y1
                   MOVE PX(J) TO X2
                   MOVE PY(J) TO Y2

                   IF X1 <= X2
                       MOVE X1 TO MINX
                       MOVE X2 TO MAXX
                   ELSE
                       MOVE X2 TO MINX
                       MOVE X1 TO MAXX
                   END-IF

                   IF Y1 <= Y2
                       MOVE Y1 TO MINY
                       MOVE Y2 TO MAXY
                   ELSE
                       MOVE Y2 TO MINY
                       MOVE Y1 TO MAXY
                   END-IF

                   COMPUTE DX = MAXX - MINX
                   COMPUTE DY = MAXY - MINY
                   COMPUTE WID = DX + 1
                   COMPUTE HEI = DY + 1
                   COMPUTE WS-AREA = WID * HEI

                   IF WS-AREA > WS-BEST
                       MOVE "Y" TO OK-FLAG
                       PERFORM RECT-VALIDATE
                       IF OK-FLAG = "Y"
                           MOVE WS-AREA TO WS-BEST
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM

           MOVE WS-BEST TO OUTPIC
           DISPLAY FUNCTION TRIM(OUTPIC)
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

           ADD 1 TO N
           IF N > MAX-PTS
               DISPLAY "ERROR: Too many points; increase MAX-PTS."
               STOP RUN
           END-IF

           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-XSTR)) TO PX(N)
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-YSTR)) TO PY(N)
           .

       BUILD-SEGMENTS.
           MOVE 0 TO NV
           MOVE 0 TO NH
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               IF I = N
                   MOVE 1 TO JJ
               ELSE
                   COMPUTE JJ = I + 1
               END-IF

               MOVE PX(I)  TO X1
               MOVE PY(I)  TO Y1
               MOVE PX(JJ) TO X2
               MOVE PY(JJ) TO Y2

               IF X1 = X2
                   ADD 1 TO NV
                   MOVE X1 TO VX(NV)
                   IF Y1 <= Y2
                       MOVE Y1 TO VY1(NV)
                       MOVE Y2 TO VY2(NV)
                   ELSE
                       MOVE Y2 TO VY1(NV)
                       MOVE Y1 TO VY2(NV)
                   END-IF
               ELSE
                   ADD 1 TO NH
                   MOVE Y1 TO HY(NH)
                   IF X1 <= X2
                       MOVE X1 TO HX1(NH)
                       MOVE X2 TO HX2(NH)
                   ELSE
                       MOVE X2 TO HX1(NH)
                       MOVE X1 TO HX2(NH)
                   END-IF
               END-IF
           END-PERFORM
           .

       SORT-VERTICAL.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > NV
               MOVE VX(I)  TO TMP-IX
               MOVE VY1(I) TO TMP-IY
               MOVE VY2(I) TO DY
               COMPUTE J = I - 1
               PERFORM UNTIL J < 1 OR VX(J) <= TMP-IX
                   MOVE VX(J)  TO VX(J + 1)
                   MOVE VY1(J) TO VY1(J + 1)
                   MOVE VY2(J) TO VY2(J + 1)
                   COMPUTE J = J - 1
               END-PERFORM
               MOVE TMP-IX TO VX(J + 1)
               MOVE TMP-IY TO VY1(J + 1)
               MOVE DY     TO VY2(J + 1)
           END-PERFORM
           .

       SORT-HORIZONTAL.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > NH
               MOVE HY(I)  TO TMP-IY
               MOVE HX1(I) TO TMP-IX
               MOVE HX2(I) TO DX
               COMPUTE J = I - 1
               PERFORM UNTIL J < 1 OR HY(J) <= TMP-IY
                   MOVE HY(J)  TO HY(J + 1)
                   MOVE HX1(J) TO HX1(J + 1)
                   MOVE HX2(J) TO HX2(J + 1)
                   COMPUTE J = J - 1
               END-PERFORM
               MOVE TMP-IY TO HY(J + 1)
               MOVE TMP-IX TO HX1(J + 1)
               MOVE DX     TO HX2(J + 1)
           END-PERFORM
           .

       RECT-VALIDATE.
           *> 1) reject if any boundary segment passes through rectangle interior
           PERFORM FIND-VSTART-GT-MINX
           PERFORM FIND-VEND-GE-MAXX
           PERFORM VARYING K FROM VSTART BY 1 UNTIL K >= VEND OR OK-FLAG = "N"
               IF VY1(K) < MAXY AND VY2(K) > MINY
                   MOVE "N" TO OK-FLAG
               END-IF
           END-PERFORM

           IF OK-FLAG = "N"
               EXIT PARAGRAPH
           END-IF

           PERFORM FIND-HSTART-GT-MINY
           PERFORM FIND-HEND-GE-MAXY
           PERFORM VARYING K FROM HSTART BY 1 UNTIL K >= HEND OR OK-FLAG = "N"
               IF HX1(K) < MAXX AND HX2(K) > MINX
                   MOVE "N" TO OK-FLAG
               END-IF
           END-PERFORM

           IF OK-FLAG = "N"
               EXIT PARAGRAPH
           END-IF

           *> 2) sample point inside rectangle must be inside/on boundary
           COMPUTE QX = (MINX + MAXX) / 2
           COMPUTE QY = (MINY + MAXY) / 2
           PERFORM POINT-IN-POLY
           IF INSIDE-FLAG NOT = "Y"
               MOVE "N" TO OK-FLAG
           END-IF
           .

       FIND-VSTART-GT-MINX.
           MOVE 1 TO BS-LOW
           COMPUTE BS-HIGH = NV + 1
           PERFORM UNTIL BS-LOW >= BS-HIGH
               COMPUTE BS-MID = (BS-LOW + BS-HIGH) / 2
               IF BS-MID <= NV AND VX(BS-MID) > MINX
                   MOVE BS-MID TO BS-HIGH
               ELSE
                   COMPUTE BS-LOW = BS-MID + 1
               END-IF
           END-PERFORM
           MOVE BS-LOW TO VSTART
           .

       FIND-VEND-GE-MAXX.
           MOVE 1 TO BS-LOW
           COMPUTE BS-HIGH = NV + 1
           PERFORM UNTIL BS-LOW >= BS-HIGH
               COMPUTE BS-MID = (BS-LOW + BS-HIGH) / 2
               IF BS-MID <= NV AND VX(BS-MID) >= MAXX
                   MOVE BS-MID TO BS-HIGH
               ELSE
                   COMPUTE BS-LOW = BS-MID + 1
               END-IF
           END-PERFORM
           MOVE BS-LOW TO VEND
           .

       FIND-HSTART-GT-MINY.
           MOVE 1 TO BS-LOW
           COMPUTE BS-HIGH = NH + 1
           PERFORM UNTIL BS-LOW >= BS-HIGH
               COMPUTE BS-MID = (BS-LOW + BS-HIGH) / 2
               IF BS-MID <= NH AND HY(BS-MID) > MINY
                   MOVE BS-MID TO BS-HIGH
               ELSE
                   COMPUTE BS-LOW = BS-MID + 1
               END-IF
           END-PERFORM
           MOVE BS-LOW TO HSTART
           .

       FIND-HEND-GE-MAXY.
           MOVE 1 TO BS-LOW
           COMPUTE BS-HIGH = NH + 1
           PERFORM UNTIL BS-LOW >= BS-HIGH
               COMPUTE BS-MID = (BS-LOW + BS-HIGH) / 2
               IF BS-MID <= NH AND HY(BS-MID) >= MAXY
                   MOVE BS-MID TO BS-HIGH
               ELSE
                   COMPUTE BS-LOW = BS-MID + 1
               END-IF
           END-PERFORM
           MOVE BS-LOW TO HEND
           .

       POINT-IN-POLY.
           MOVE "N" TO INSIDE-FLAG

           *> boundary check (vertical)
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > NV OR INSIDE-FLAG = "Y"
               IF QX = VX(K) AND QY >= VY1(K) AND QY <= VY2(K)
                   MOVE "Y" TO INSIDE-FLAG
               END-IF
           END-PERFORM
           IF INSIDE-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF

           *> boundary check (horizontal)
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > NH OR INSIDE-FLAG = "Y"
               IF QY = HY(K) AND QX >= HX1(K) AND QX <= HX2(K)
                   MOVE "Y" TO INSIDE-FLAG
               END-IF
           END-PERFORM
           IF INSIDE-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF

           *> ray casting to +X using vertical edges (even-odd)
           MOVE 0 TO CROSS
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > NV
               MOVE VX(K)  TO XE
               MOVE VY1(K) TO YLO
               MOVE VY2(K) TO YHI
               IF XE > QX AND QY >= YLO AND QY < YHI
                   ADD 1 TO CROSS
               END-IF
           END-PERFORM

           IF FUNCTION MOD(CROSS, 2) = 1
               MOVE "Y" TO INSIDE-FLAG
           END-IF
           .
