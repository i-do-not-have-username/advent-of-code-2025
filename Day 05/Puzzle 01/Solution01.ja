       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day5part1.

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
       78  MAX-RANGES              VALUE 20000.

       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                 VALUE "Y".

       01  WS-LINE                 PIC X(200000).
       01  WS-LEN                  PIC 9(9) COMP-5.

       01  WS-IN-IDS               PIC X VALUE "N".
           88  IN-RANGES           VALUE "N".
           88  IN-IDS              VALUE "Y".

       01  WS-RCNT                 PIC 9(9) COMP-5 VALUE 0.
       01  WS-MCNT                 PIC 9(9) COMP-5 VALUE 0.

       01  WS-START-STR            PIC X(64).
       01  WS-END-STR              PIC X(64).

       01  WS-A                    PIC S9(18) COMP-3.
       01  WS-B                    PIC S9(18) COMP-3.
       01  WS-ID                   PIC S9(18) COMP-3.

       01  WS-I                    PIC 9(9) COMP-5.
       01  WS-J                    PIC S9(9) COMP-5.
       01  WS-LOW                  PIC 9(9) COMP-5.
       01  WS-HIGH                 PIC 9(9) COMP-5.
       01  WS-MID                  PIC 9(9) COMP-5.
       01  WS-FOUND                PIC X.

       01  WS-KEY-START            PIC S9(18) COMP-3.
       01  WS-KEY-END              PIC S9(18) COMP-3.

       01  WS-FRESH-COUNT          PIC S9(18) COMP-3 VALUE 0.
       01  WS-OUT                  PIC Z(18)9.

       01  RANGE-TABLE.
           05 RANGE-ENTRY OCCURS 20000.
              10 R-START           PIC S9(18) COMP-3.
              10 R-END             PIC S9(18) COMP-3.

       01  MERGED-TABLE.
           05 MERGED-ENTRY OCCURS 20000.
              10 M-START           PIC S9(18) COMP-3.
              10 M-END             PIC S9(18) COMP-3.

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

           MOVE WS-FRESH-COUNT TO WS-OUT
           DISPLAY FUNCTION TRIM(WS-OUT)
           STOP RUN
           .

       PROCESS-LINE.
           *> Strip CR for Windows CRLF, then trim
           INSPECT WS-LINE REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-LINE) TO WS-LINE
           MOVE FUNCTION STORED-CHAR-LENGTH(WS-LINE) TO WS-LEN

           IF IN-RANGES
               IF WS-LEN = 0
                   PERFORM FINALIZE-RANGES
                   SET IN-IDS TO TRUE
                   EXIT PARAGRAPH
               ELSE
                   PERFORM ADD-RANGE
                   EXIT PARAGRAPH
               END-IF
           END-IF

           *> IN-IDS
           IF WS-LEN = 0
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-ID = FUNCTION NUMVAL(WS-LINE)
           PERFORM CHECK-ID
           IF WS-FOUND = "Y"
               ADD 1 TO WS-FRESH-COUNT
           END-IF
           .

       ADD-RANGE.
           MOVE SPACES TO WS-START-STR
           MOVE SPACES TO WS-END-STR

           UNSTRING WS-LINE
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
               MOVE WS-A TO WS-KEY-START
               MOVE WS-B TO WS-A
               MOVE WS-KEY-START TO WS-B
           END-IF

           IF WS-RCNT >= MAX-RANGES
               DISPLAY "ERROR: Too many ranges (increase MAX-RANGES)."
               STOP RUN
           END-IF

           ADD 1 TO WS-RCNT
           MOVE WS-A TO R-START(WS-RCNT)
           MOVE WS-B TO R-END(WS-RCNT)
           .

       FINALIZE-RANGES.
           IF WS-RCNT = 0
               MOVE 0 TO WS-MCNT
               EXIT PARAGRAPH
           END-IF

           PERFORM SORT-RANGES
           PERFORM MERGE-RANGES
           .

       SORT-RANGES.
           *> Insertion sort by R-START ascending
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > WS-RCNT
               MOVE R-START(WS-I) TO WS-KEY-START
               MOVE R-END(WS-I)   TO WS-KEY-END
               COMPUTE WS-J = WS-I - 1

               PERFORM UNTIL WS-J < 1 OR R-START(WS-J) <= WS-KEY-START
                   MOVE R-START(WS-J) TO R-START(WS-J + 1)
                   MOVE R-END(WS-J)   TO R-END(WS-J + 1)
                   SUBTRACT 1 FROM WS-J
               END-PERFORM

               MOVE WS-KEY-START TO R-START(WS-J + 1)
               MOVE WS-KEY-END   TO R-END(WS-J + 1)
           END-PERFORM
           .

       MERGE-RANGES.
           MOVE 0 TO WS-MCNT

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-RCNT
               IF WS-MCNT = 0
                   MOVE 1 TO WS-MCNT
                   MOVE R-START(WS-I) TO M-START(1)
                   MOVE R-END(WS-I)   TO M-END(1)
               ELSE
                   *> overlap/touch if start <= lastEnd + 1
                   IF R-START(WS-I) <= (M-END(WS-MCNT) + 1)
                       IF R-END(WS-I) > M-END(WS-MCNT)
                           MOVE R-END(WS-I) TO M-END(WS-MCNT)
                       END-IF
                   ELSE
                       ADD 1 TO WS-MCNT
                       MOVE R-START(WS-I) TO M-START(WS-MCNT)
                       MOVE R-END(WS-I)   TO M-END(WS-MCNT)
                   END-IF
               END-IF
           END-PERFORM
           .

       CHECK-ID.
           MOVE "N" TO WS-FOUND

           IF WS-MCNT = 0
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-LOW
           MOVE WS-MCNT TO WS-HIGH

           PERFORM UNTIL WS-LOW > WS-HIGH OR WS-FOUND = "Y"
               COMPUTE WS-MID = (WS-LOW + WS-HIGH) / 2

               IF WS-ID < M-START(WS-MID)
                   COMPUTE WS-HIGH = WS-MID - 1
               ELSE
                   IF WS-ID > M-END(WS-MID)
                       COMPUTE WS-LOW = WS-MID + 1
                   ELSE
                       MOVE "Y" TO WS-FOUND
                   END-IF
               END-IF
           END-PERFORM
           .
