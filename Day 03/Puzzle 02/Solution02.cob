       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. day3part2.

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
       78  K-DIGITS                VALUE 12.

       01  WS-FILENAME             PIC X(256) VALUE "input.txt".
       01  WS-EOF                  PIC X VALUE "N".
           88  EOF                 VALUE "Y".

       01  WS-LINE                 PIC X(200000).
       01  WS-LEN                  PIC 9(9) COMP-5.

       01  WS-POS                  PIC 9(9) COMP-5.
       01  WS-END                  PIC 9(9) COMP-5.
       01  WS-I                    PIC 9(9) COMP-5.
       01  WS-J                    PIC 9(9) COMP-5.

       01  WS-BESTCHAR             PIC X.
       01  WS-DIGCHAR              PIC X.
       01  WS-BESTIDX              PIC 9(9) COMP-5.

       01  WS-OUTSTR               PIC X(12).

       01  WS-BANKVAL              PIC S9(18) COMP-3 VALUE 0.
       01  WS-TOTAL                PIC S9(18) COMP-3 VALUE 0.
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
           *> Strip CR for Windows CRLF and trim spaces
           INSPECT WS-LINE REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(WS-LINE) TO WS-LINE

           MOVE FUNCTION STORED-CHAR-LENGTH(WS-LINE) TO WS-LEN
           IF WS-LEN < K-DIGITS
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-POS
           MOVE SPACES TO WS-OUTSTR

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > K-DIGITS

               *> Latest index we may choose for this digit
               *> END = LEN - (K - I)
               COMPUTE WS-END = WS-LEN - (K-DIGITS - WS-I)

               MOVE "0" TO WS-BESTCHAR
               MOVE 0   TO WS-BESTIDX

               PERFORM VARYING WS-J FROM WS-POS BY 1 UNTIL WS-J > WS-END
                   MOVE WS-LINE(WS-J:1) TO WS-DIGCHAR
                   IF WS-DIGCHAR > WS-BESTCHAR
                       MOVE WS-DIGCHAR TO WS-BESTCHAR
                       MOVE WS-J       TO WS-BESTIDX
                   END-IF
               END-PERFORM

               MOVE WS-BESTCHAR TO WS-OUTSTR(WS-I:1)
               COMPUTE WS-POS = WS-BESTIDX + 1

           END-PERFORM

           COMPUTE WS-BANKVAL = FUNCTION NUMVAL(WS-OUTSTR)
           ADD WS-BANKVAL TO WS-TOTAL
           .
