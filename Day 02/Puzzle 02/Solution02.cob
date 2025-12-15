       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day2part2.

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
       01  WS-PTR                  PIC 9(9) COMP-5.

       01  WS-RANGE                PIC X(64).
       01  WS-START-STR            PIC X(32).
       01  WS-END-STR              PIC X(32).

       01  WS-A                    PIC S9(18) COMP-5.
       01  WS-B                    PIC S9(18) COMP-5.
       01  WS-TMP                  PIC S9(18) COMP-5.

       01  WS-TOTAL                PIC S9(18) COMP-5 VALUE 0.

       *> tables: 10^n (n=0..18) stored at index n+1
       01  WS-POW10-TAB.
           05 WS-POW10 OCCURS 19 PIC S9(18) COMP-5.

       *> Möbius mu(n) for n=0..18 stored at index n+1 (mu(0) unused -> 0)
       01  WS-MU-TAB.
           05 WS-MU    OCCURS 19 PIC S9(4) COMP-5.

       *> per-length / per-divisor work
       01  WS-L                    PIC 9(2) COMP-5.
       01  WS-P                    PIC 9(2) COMP-5.
       01  WS-Q                    PIC S9(18) COMP-5.
       01  WS-R                    PIC S9(18) COMP-5.

       01  WS-MINL                 PIC S9(18) COMP-5.
       01  WS-MAXL                 PIC S9(18) COMP-5.
       01  WS-LO                   PIC S9(18) COMP-5.
       01  WS-HI                   PIC S9(18) COMP-5.

       01  WS-SEG-SUM              PIC S9(18) COMP-5.
       01  WS-COEFF                PIC S9(4)  COMP-5.

       01  WS-NUMER                PIC S9(18) COMP-5.
       01  WS-DENOM                PIC S9(18) COMP-5.
       01  WS-G                    PIC S9(18) COMP-5.

       01  WS-XLOW                 PIC S9(18) COMP-5.
       01  WS-XHIGH                PIC S9(18) COMP-5.
       01  WS-MINX                 PIC S9(18) COMP-5.
       01  WS-MAXX                 PIC S9(18) COMP-5.

       01  WS-COUNT                PIC S9(18) COMP-5.
       01  WS-PAIR                 PIC S9(18) COMP-5.
       01  WS-HALF                 PIC S9(18) COMP-5.
       01  WS-REM2                 PIC S9(18) COMP-5.
       01  WS-SUMX                 PIC S9(18) COMP-5.
       01  WS-CONTRIB              PIC S9(18) COMP-5.

       01  WS-OUT                  PIC Z(18)9.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM INIT-TABLES

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

       INIT-TABLES.
           *> 10^0 .. 10^18
           MOVE 1 TO WS-POW10(1)
           PERFORM VARYING WS-L FROM 2 BY 1 UNTIL WS-L > 19
               COMPUTE WS-POW10(WS-L) = WS-POW10(WS-L - 1) * 10
           END-PERFORM

           *> mu(0)=0 (unused), mu(1..18) hard-coded
           MOVE 0  TO WS-MU(1)   *> mu(0)
           MOVE 1  TO WS-MU(2)   *> mu(1)
           MOVE -1 TO WS-MU(3)   *> mu(2)
           MOVE -1 TO WS-MU(4)   *> mu(3)
           MOVE 0  TO WS-MU(5)   *> mu(4)
           MOVE -1 TO WS-MU(6)   *> mu(5)
           MOVE 1  TO WS-MU(7)   *> mu(6)
           MOVE -1 TO WS-MU(8)   *> mu(7)
           MOVE 0  TO WS-MU(9)   *> mu(8)
           MOVE 0  TO WS-MU(10)  *> mu(9)
           MOVE 1  TO WS-MU(11)  *> mu(10)
           MOVE -1 TO WS-MU(12)  *> mu(11)
           MOVE 0  TO WS-MU(13)  *> mu(12)
           MOVE -1 TO WS-MU(14)  *> mu(13)
           MOVE 1  TO WS-MU(15)  *> mu(14)
           MOVE 1  TO WS-MU(16)  *> mu(15)
           MOVE 0  TO WS-MU(17)  *> mu(16)
           MOVE -1 TO WS-MU(18)  *> mu(17)
           MOVE 0  TO WS-MU(19)  *> mu(18)
           .

       PROCESS-LINE.
           *> strip CR (Windows CRLF), then trim
           INSPECT WS-LINE REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-LINE) TO WS-LINE

           MOVE FUNCTION STORED-CHAR-LENGTH(WS-LINE) TO WS-LEN
           IF WS-LEN = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-PTR

           PERFORM UNTIL WS-PTR > WS-LEN
               MOVE SPACES TO WS-RANGE

               UNSTRING WS-LINE
                   DELIMITED BY ","
                   INTO WS-RANGE
                   WITH POINTER WS-PTR
               END-UNSTRING

               MOVE FUNCTION TRIM(WS-RANGE) TO WS-RANGE
               IF WS-RANGE = SPACES
                   CONTINUE
               ELSE
                   PERFORM PROCESS-RANGE
               END-IF
           END-PERFORM
           .

       PROCESS-RANGE.
           MOVE SPACES TO WS-START-STR
           MOVE SPACES TO WS-END-STR

           UNSTRING WS-RANGE
               DELIMITED BY "-"
               INTO WS-START-STR WS-END-STR
           END-UNSTRING

           MOVE FUNCTION TRIM(WS-START-STR) TO WS-START-STR
           MOVE FUNCTION TRIM(WS-END-STR)   TO WS-END-STR

           IF WS-START-STR = SPACES OR WS-END-STR = SPACES
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-A = FUNCTION NUMVAL(WS-START-STR)
           COMPUTE WS-B = FUNCTION NUMVAL(WS-END-STR)

           IF WS-A > WS-B
               MOVE WS-A TO WS-TMP
               MOVE WS-B TO WS-A
               MOVE WS-TMP TO WS-B
           END-IF

           PERFORM ADD-INVALID-FOR-RANGE
           .

       ADD-INVALID-FOR-RANGE.
           *> split [WS-A..WS-B] by digit length L, then sum invalids per segment
           PERFORM VARYING WS-L FROM 1 BY 1 UNTIL WS-L > 18

               IF WS-L = 1
                   MOVE 0 TO WS-MINL
               ELSE
                   *> 10^(L-1) is at index (L-1)+1 = L
                   MOVE WS-POW10(WS-L) TO WS-MINL
               END-IF

               *> 10^L - 1 is at index L+1
               COMPUTE WS-MAXL = WS-POW10(WS-L + 1) - 1

               *> segment [LO..HI] = intersection
               MOVE WS-A TO WS-LO
               IF WS-LO < WS-MINL
                   MOVE WS-MINL TO WS-LO
               END-IF

               MOVE WS-B TO WS-HI
               IF WS-HI > WS-MAXL
                   MOVE WS-MAXL TO WS-HI
               END-IF

               IF WS-LO <= WS-HI
                   IF WS-L >= 2
                       PERFORM ADD-INVALID-IN-SEGMENT
                   END-IF
               END-IF

           END-PERFORM
           .

       ADD-INVALID-IN-SEGMENT.
           *> Sum of INVALID length-WS-L numbers in [WS-LO..WS-HI].
           *> Uses: invalidSum = - Σ_{p|L, p<L} mu(L/p) * g(p),
           *> where g(p) = sum of numbers formed by repeating a p-digit block.

           MOVE 0 TO WS-SEG-SUM

           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P >= WS-L
               DIVIDE WS-L BY WS-P GIVING WS-Q REMAINDER WS-R
               IF WS-R = 0
                   *> divisor: quotient = L/p = WS-Q
                   *> coeff = -mu(quotient)
                   COMPUTE WS-COEFF = 0 - WS-MU(WS-Q + 1)

                   IF WS-COEFF NOT = 0
                       *> G = (10^L - 1) / (10^P - 1)
                       COMPUTE WS-NUMER = WS-POW10(WS-L + 1) - 1
                       COMPUTE WS-DENOM = WS-POW10(WS-P + 1) - 1
                       DIVIDE WS-NUMER BY WS-DENOM GIVING WS-G

                       *> xLow = ceil(LO / G)
                       DIVIDE WS-LO BY WS-G GIVING WS-XLOW REMAINDER WS-R
                       IF WS-R > 0
                           ADD 1 TO WS-XLOW
                       END-IF

                       *> xHigh = floor(HI / G)
                       DIVIDE WS-HI BY WS-G GIVING WS-XHIGH

                       *> clamp X to valid p-digit blocks (no leading zero)
                       IF WS-P = 1
                           MOVE 1 TO WS-MINX
                       ELSE
                           *> 10^(p-1) is at index p
                           MOVE WS-POW10(WS-P) TO WS-MINX
                       END-IF
                       COMPUTE WS-MAXX = WS-POW10(WS-P + 1) - 1

                       IF WS-XLOW < WS-MINX
                           MOVE WS-MINX TO WS-XLOW
                       END-IF
                       IF WS-XHIGH > WS-MAXX
                           MOVE WS-MAXX TO WS-XHIGH
                       END-IF

                       IF WS-XLOW <= WS-XHIGH
                           PERFORM SUM-ARITHMETIC
                           *> contrib = coeff * G * sumX
                           MULTIPLY WS-SUMX BY WS-G GIVING WS-CONTRIB
                           MULTIPLY WS-CONTRIB BY WS-COEFF GIVING WS-CONTRIB
                           ADD WS-CONTRIB TO WS-SEG-SUM
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           ADD WS-SEG-SUM TO WS-TOTAL
           .

       SUM-ARITHMETIC.
           *> sumX = Σ_{k=XLOW..XHIGH} k
           COMPUTE WS-COUNT = WS-XHIGH - WS-XLOW + 1
           COMPUTE WS-PAIR  = WS-XLOW + WS-XHIGH

           DIVIDE WS-COUNT BY 2 GIVING WS-HALF REMAINDER WS-REM2
           IF WS-REM2 = 0
               MULTIPLY WS-HALF BY WS-PAIR GIVING WS-SUMX
           ELSE
               *> then WS-PAIR is even
               DIVIDE WS-PAIR BY 2 GIVING WS-HALF
               MULTIPLY WS-COUNT BY WS-HALF GIVING WS-SUMX
           END-IF
           .
