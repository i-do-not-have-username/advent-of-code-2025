       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day8part2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE
               ASSIGN TO DYNAMIC WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-WORK
               ASSIGN TO "sortwork".

       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                  PIC X(256).

       *> SORT work-file
       SD  SORT-WORK.
       01  SORT-REC.
           05 SR-DIST              PIC 9(18).          *> numeric key (DISPLAY)
           05 SR-A                 PIC 9(4).           *> 1..1001 fits
           05 SR-B                 PIC 9(4).

       WORKING-STORAGE SECTION.
       78  MAX-PTS                 VALUE 20000.

       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                         VALUE "Y".

       01  WS-LINE                 PIC X(256).
       01  WS-LEN                  PIC 9(9) COMP-5.

       01  WS-XSTR                 PIC X(64).
       01  WS-YSTR                 PIC X(64).
       01  WS-ZSTR                 PIC X(64).

       01  WS-N                    PIC 9(9) COMP-5 VALUE 0.

       01  WS-I                    PIC 9(9) COMP-5.
       01  WS-J                    PIC 9(9) COMP-5.
       01  WS-JSTART               PIC 9(9) COMP-5.

       01  WS-XI                   PIC S9(18) COMP-5.
       01  WS-YI                   PIC S9(18) COMP-5.
       01  WS-ZI                   PIC S9(18) COMP-5.
       01  WS-XJ                   PIC S9(18) COMP-5.
       01  WS-YJ                   PIC S9(18) COMP-5.
       01  WS-ZJ                   PIC S9(18) COMP-5.

       01  WS-DX                   PIC S9(18) COMP-5.
       01  WS-DY                   PIC S9(18) COMP-5.
       01  WS-DZ                   PIC S9(18) COMP-5.
       01  WS-DIST                 PIC 9(18)  COMP-5.

       *> points
       01  PX OCCURS 20000         PIC S9(18) COMP-5.
       01  PY OCCURS 20000         PIC S9(18) COMP-5.
       01  PZ OCCURS 20000         PIC S9(18) COMP-5.

       *> Union-Find (avoid reserved word SIZE)
       01  UF-PARENT OCCURS 20000  PIC 9(9) COMP-5.
       01  UF-SZ     OCCURS 20000  PIC 9(9) COMP-5.

       01  WS-COMPONENTS           PIC 9(9) COMP-5.
       01  LAST-A                  PIC 9(9) COMP-5 VALUE 0.
       01  LAST-B                  PIC 9(9) COMP-5 VALUE 0.

       01  F-X                     PIC 9(9) COMP-5.
       01  F-ROOT                  PIC 9(9) COMP-5.
       01  F-NEXT                  PIC 9(9) COMP-5.

       01  R1                      PIC 9(9) COMP-5.
       01  R2                      PIC 9(9) COMP-5.
       01  S1                      PIC 9(9) COMP-5.
       01  S2                      PIC 9(9) COMP-5.
       01  SWAP-TMP                PIC 9(9) COMP-5.

       01  PROD                    PIC S9(31) COMP-3 VALUE 0.
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

           *> init Union-Find
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
               MOVE WS-I TO UF-PARENT(WS-I)
               MOVE 1    TO UF-SZ(WS-I)
           END-PERFORM
           MOVE WS-N TO WS-COMPONENTS

           *> Sort all edges by (distance, a, b), then union until 1 component :contentReference[oaicite:4]{index=4}
           SORT SORT-WORK
               ON ASCENDING KEY SR-DIST SR-A SR-B
               INPUT PROCEDURE  IS BUILD-EDGES
               OUTPUT PROCEDURE IS KRUSKAL-PROCESS
           .

           COMPUTE PROD = PX(LAST-A) * PX(LAST-B)
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

       BUILD-EDGES.
           *> Generate all pairs and RELEASE into SORT-WORK :contentReference[oaicite:5]{index=5}
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

                   *> Put keys into SORT-REC (SR-DIST is DISPLAY numeric key)
                   MOVE WS-DIST TO SR-DIST
                   MOVE WS-I    TO SR-A
                   MOVE WS-J    TO SR-B
                   RELEASE SORT-REC
               END-PERFORM
           END-PERFORM
           .

       KRUSKAL-PROCESS.
           *> RETURN edges in sorted order and union until fully connected :contentReference[oaicite:6]{index=6}
           PERFORM UNTIL WS-COMPONENTS = 1
               RETURN SORT-WORK INTO SORT-REC
                   AT END
                       DISPLAY "ERROR: Ran out of edges before connected."
                       STOP RUN
               END-RETURN

               MOVE SR-A TO F-X
               PERFORM FIND-ROOT
               MOVE F-ROOT TO R1

               MOVE SR-B TO F-X
               PERFORM FIND-ROOT
               MOVE F-ROOT TO R2

               IF R1 NOT = R2
                   MOVE UF-SZ(R1) TO S1
                   MOVE UF-SZ(R2) TO S2
                   IF S1 < S2
                       MOVE R1 TO SWAP-TMP
                       MOVE R2 TO R1
                       MOVE SWAP-TMP TO R2
                   END-IF

                   MOVE R1 TO UF-PARENT(R2)
                   COMPUTE UF-SZ(R1) = UF-SZ(R1) + UF-SZ(R2)

                   SUBTRACT 1 FROM WS-COMPONENTS
                   MOVE SR-A TO LAST-A
                   MOVE SR-B TO LAST-B
               END-IF
           END-PERFORM
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
