       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day8part1.

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
       78  MAX-PTS                VALUE 20000.
       78  K-EDGES                VALUE 1000.

       01  WS-FILENAME            PIC X(256) VALUE "input.txt".
       01  WS-EOF                 PIC X VALUE "N".
           88  EOF                        VALUE "Y".

       01  WS-LINE                PIC X(256).
       01  WS-LEN                 PIC 9(9) COMP-5.

       01  WS-XSTR                PIC X(64).
       01  WS-YSTR                PIC X(64).
       01  WS-ZSTR                PIC X(64).

       01  WS-N                   PIC 9(9) COMP-5 VALUE 0.

       01  WS-I                   PIC 9(9) COMP-5.
       01  WS-J                   PIC 9(9) COMP-5.
       01  WS-JSTART              PIC 9(9) COMP-5.

       01  WS-XI                  PIC S9(18) COMP-5.
       01  WS-YI                  PIC S9(18) COMP-5.
       01  WS-ZI                  PIC S9(18) COMP-5.
       01  WS-XJ                  PIC S9(18) COMP-5.
       01  WS-YJ                  PIC S9(18) COMP-5.
       01  WS-ZJ                  PIC S9(18) COMP-5.

       01  WS-DX                  PIC S9(18) COMP-5.
       01  WS-DY                  PIC S9(18) COMP-5.
       01  WS-DZ                  PIC S9(18) COMP-5.
       01  WS-DIST                PIC S9(18) COMP-5.

       01  WS-NEW-A               PIC 9(9)  COMP-5.
       01  WS-NEW-B               PIC 9(9)  COMP-5.
       01  WS-NEW-D               PIC S9(18) COMP-5.

       *> Heap management (max-heap: root is WORST kept edge)
       01  HEAP-SIZE              PIC 9(9) COMP-5 VALUE 0.
       01  HP-POS                 PIC 9(9) COMP-5.
       01  HP-PAR                 PIC 9(9) COMP-5.
       01  HP-L                   PIC 9(9) COMP-5.
       01  HP-R                   PIC 9(9) COMP-5.
       01  HP-BIG                 PIC 9(9) COMP-5.

       01  TMP-D                  PIC S9(18) COMP-5.
       01  TMP-A                  PIC 9(9)  COMP-5.
       01  TMP-B                  PIC 9(9)  COMP-5.

       01  WS-WORSE-FLAG          PIC X VALUE "N".
       01  WS-BETTER-FLAG         PIC X VALUE "N".
       01  WS-CMPI                PIC 9(9) COMP-5.
       01  WS-CMPJ                PIC 9(9) COMP-5.

       01  WS-DONE                PIC X VALUE "N".

       *> point coordinates
       01  PX OCCURS 20000        PIC S9(18) COMP-5.
       01  PY OCCURS 20000        PIC S9(18) COMP-5.
       01  PZ OCCURS 20000        PIC S9(18) COMP-5.

       *> heap arrays (size 1000)
       01  HDIST OCCURS 1000      PIC S9(18) COMP-5.
       01  HA    OCCURS 1000      PIC 9(9)  COMP-5.
       01  HB    OCCURS 1000      PIC 9(9)  COMP-5.

       *> Union-Find (SIZE is reserved; renamed to UF-SZ)
       01  UF-PARENT OCCURS 20000 PIC 9(9) COMP-5.
       01  UF-SZ     OCCURS 20000 PIC 9(9) COMP-5.

       01  F-X                   PIC 9(9) COMP-5.
       01  F-ROOT                PIC 9(9) COMP-5.
       01  F-NEXT                PIC 9(9) COMP-5.

       01  R1                    PIC 9(9) COMP-5.
       01  R2                    PIC 9(9) COMP-5.
       01  S1                    PIC 9(9) COMP-5.
       01  S2                    PIC 9(9) COMP-5.
       01  SWAP-TMP              PIC 9(9) COMP-5.

       01  TOP1                  PIC 9(9) COMP-5 VALUE 0.
       01  TOP2                  PIC 9(9) COMP-5 VALUE 0.
       01  TOP3                  PIC 9(9) COMP-5 VALUE 0.

       01  PROD                  PIC S9(31) COMP-3 VALUE 0.
       01  WS-OUT                PIC Z(30)9.

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

           IF WS-N < 3
               MOVE 0 TO WS-OUT
               DISPLAY FUNCTION TRIM(WS-OUT)
               STOP RUN
           END-IF

           *> Build heap of 1000 closest pairs (squared distance, tie by (a,b))
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= WS-N
               MOVE PX(WS-I) TO WS-XI
               MOVE PY(WS-I) TO WS-YI
               MOVE PZ(WS-I) TO WS-ZI

               COMPUTE WS-JSTART = WS-I + 1
               PERFORM VARYING WS-J FROM WS-JSTART BY 1 UNTIL WS-J > WS-N
                   MOVE PX(WS-J) TO WS-XJ
                   MOVE PY(WS-J) TO WS-YJ
                   MOVE PZ(WS-J) TO WS-ZJ

                   COMPUTE WS-DX = WS-XI - WS-XJ
                   COMPUTE WS-DY = WS-YI - WS-YJ
                   COMPUTE WS-DZ = WS-ZI - WS-ZJ

                   COMPUTE WS-DIST =
                       (WS-DX * WS-DX) + (WS-DY * WS-DY) + (WS-DZ * WS-DZ)

                   MOVE WS-I    TO WS-NEW-A
                   MOVE WS-J    TO WS-NEW-B
                   MOVE WS-DIST TO WS-NEW-D

                   PERFORM CONSIDER-EDGE
               END-PERFORM
           END-PERFORM

           *> Init Union-Find
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
               MOVE WS-I TO UF-PARENT(WS-I)
               MOVE 1    TO UF-SZ(WS-I)
           END-PERFORM

           *> Union all kept edges
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > HEAP-SIZE
               MOVE HA(WS-I) TO WS-NEW-A
               MOVE HB(WS-I) TO WS-NEW-B
               PERFORM UNION-AB
           END-PERFORM

           *> Find top 3 component sizes
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
               IF UF-PARENT(WS-I) = WS-I
                   MOVE UF-SZ(WS-I) TO S1
                   IF S1 > TOP1
                       MOVE TOP2 TO TOP3
                       MOVE TOP1 TO TOP2
                       MOVE S1   TO TOP1
                   ELSE
                       IF S1 > TOP2
                           MOVE TOP2 TO TOP3
                           MOVE S1   TO TOP2
                       ELSE
                           IF S1 > TOP3
                               MOVE S1 TO TOP3
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           COMPUTE PROD = TOP1 * TOP2 * TOP3
           MOVE PROD TO WS-OUT
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
           MOVE SPACES TO WS-ZSTR

           UNSTRING WS-LINE DELIMITED BY ","
               INTO WS-XSTR WS-YSTR WS-ZSTR
           END-UNSTRING

           IF WS-N >= MAX-PTS
               DISPLAY "ERROR: Too many points; increase MAX-PTS."
               STOP RUN
           END-IF

           ADD 1 TO WS-N
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-XSTR)) TO PX(WS-N)
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-YSTR)) TO PY(WS-N)
           MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-ZSTR)) TO PZ(WS-N)
           .

       *> Keep K smallest edges in a max-heap (root is worst among kept)
       CONSIDER-EDGE.
           IF HEAP-SIZE < K-EDGES
               ADD 1 TO HEAP-SIZE
               MOVE WS-NEW-D TO HDIST(HEAP-SIZE)
               MOVE WS-NEW-A TO HA(HEAP-SIZE)
               MOVE WS-NEW-B TO HB(HEAP-SIZE)
               MOVE HEAP-SIZE TO HP-POS
               PERFORM HEAP-UP
           ELSE
               PERFORM NEW-BETTER-THAN-ROOT
               IF WS-BETTER-FLAG = "Y"
                   MOVE WS-NEW-D TO HDIST(1)
                   MOVE WS-NEW-A TO HA(1)
                   MOVE WS-NEW-B TO HB(1)
                   MOVE 1 TO HP-POS
                   PERFORM HEAP-DOWN
               END-IF
           END-IF
           .

       NEW-BETTER-THAN-ROOT.
           MOVE "N" TO WS-BETTER-FLAG
           IF WS-NEW-D < HDIST(1)
               MOVE "Y" TO WS-BETTER-FLAG
           ELSE
               IF WS-NEW-D = HDIST(1)
                   IF WS-NEW-A < HA(1)
                       MOVE "Y" TO WS-BETTER-FLAG
                   ELSE
                       IF WS-NEW-A = HA(1)
                           IF WS-NEW-B < HB(1)
                               MOVE "Y" TO WS-BETTER-FLAG
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       HEAP-UP.
           PERFORM UNTIL HP-POS <= 1
               COMPUTE HP-PAR = HP-POS / 2
               MOVE HP-POS TO WS-CMPI
               MOVE HP-PAR TO WS-CMPJ
               PERFORM HEAP-ELEM-WORSE
               IF WS-WORSE-FLAG = "Y"
                   PERFORM SWAP-HEAP
                   MOVE HP-PAR TO HP-POS
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

       HEAP-DOWN.
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-DONE = "Y"
               COMPUTE HP-L = HP-POS * 2
               IF HP-L > HEAP-SIZE
                   MOVE "Y" TO WS-DONE
               ELSE
                   COMPUTE HP-R = HP-L + 1
                   MOVE HP-POS TO HP-BIG

                   MOVE HP-L   TO WS-CMPI
                   MOVE HP-BIG TO WS-CMPJ
                   PERFORM HEAP-ELEM-WORSE
                   IF WS-WORSE-FLAG = "Y"
                       MOVE HP-L TO HP-BIG
                   END-IF

                   IF HP-R <= HEAP-SIZE
                       MOVE HP-R   TO WS-CMPI
                       MOVE HP-BIG TO WS-CMPJ
                       PERFORM HEAP-ELEM-WORSE
                       IF WS-WORSE-FLAG = "Y"
                           MOVE HP-R TO HP-BIG
                       END-IF
                   END-IF

                   IF HP-BIG = HP-POS
                       MOVE "Y" TO WS-DONE
                   ELSE
                       MOVE HP-BIG TO HP-PAR
                       PERFORM SWAP-HEAP
                       MOVE HP-BIG TO HP-POS
                   END-IF
               END-IF
           END-PERFORM
           .

       HEAP-ELEM-WORSE.
           *> WS-WORSE-FLAG = "Y" if heap[WS-CMPI] is worse (greater) than heap[WS-CMPJ]
           MOVE "N" TO WS-WORSE-FLAG
           IF HDIST(WS-CMPI) > HDIST(WS-CMPJ)
               MOVE "Y" TO WS-WORSE-FLAG
           ELSE
               IF HDIST(WS-CMPI) = HDIST(WS-CMPJ)
                   IF HA(WS-CMPI) > HA(WS-CMPJ)
                       MOVE "Y" TO WS-WORSE-FLAG
                   ELSE
                       IF HA(WS-CMPI) = HA(WS-CMPJ)
                           IF HB(WS-CMPI) > HB(WS-CMPJ)
                               MOVE "Y" TO WS-WORSE-FLAG
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       *> Swap heap entries at indices HP-POS and HP-PAR (set by caller)
       SWAP-HEAP.
           MOVE HDIST(HP-POS) TO TMP-D
           MOVE HA(HP-POS)    TO TMP-A
           MOVE HB(HP-POS)    TO TMP-B

           MOVE HDIST(HP-PAR) TO HDIST(HP-POS)
           MOVE HA(HP-PAR)    TO HA(HP-POS)
           MOVE HB(HP-PAR)    TO HB(HP-POS)

           MOVE TMP-D TO HDIST(HP-PAR)
           MOVE TMP-A TO HA(HP-PAR)
           MOVE TMP-B TO HB(HP-PAR)
           .

       UNION-AB.
           MOVE WS-NEW-A TO F-X
           PERFORM FIND-ROOT
           MOVE F-ROOT TO R1

           MOVE WS-NEW-B TO F-X
           PERFORM FIND-ROOT
           MOVE F-ROOT TO R2

           IF R1 = R2
               EXIT PARAGRAPH
           END-IF

           MOVE UF-SZ(R1) TO S1
           MOVE UF-SZ(R2) TO S2

           IF S1 < S2
               MOVE R1 TO SWAP-TMP
               MOVE R2 TO R1
               MOVE SWAP-TMP TO R2
           END-IF

           MOVE R1 TO UF-PARENT(R2)
           COMPUTE UF-SZ(R1) = UF-SZ(R1) + UF-SZ(R2)
           .

       FIND-ROOT.
           MOVE F-X TO F-ROOT
           PERFORM UNTIL UF-PARENT(F-ROOT) = F-ROOT
               MOVE UF-PARENT(F-ROOT) TO F-ROOT
           END-PERFORM

           *> path compression
           PERFORM UNTIL UF-PARENT(F-X) = F-X
               MOVE UF-PARENT(F-X) TO F-NEXT
               MOVE F-ROOT TO UF-PARENT(F-X)
               MOVE F-NEXT TO F-X
           END-PERFORM
           .
