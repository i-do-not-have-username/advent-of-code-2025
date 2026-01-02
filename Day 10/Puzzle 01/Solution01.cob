       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY10PART1.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD            PIC X(1000).
       
       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-TOTAL-PRESSES        PIC 9(6) VALUE 0.
       01  WS-LINE                 PIC X(1000).
       01  WS-LINE-LEN             PIC 9(4).
       01  WS-POS                  PIC 9(4).
       01  WS-CHAR                 PIC X.
       
       01  WS-NUM-LIGHTS           PIC 9(3).
       01  WS-NUM-BUTTONS          PIC 9(3).
       01  WS-TARGET               OCCURS 100 TIMES PIC 9.
       01  WS-BUTTONS.
           05  WS-BUTTON           OCCURS 100 TIMES.
               10  WS-BTN-BITS     OCCURS 100 TIMES PIC 9.
       
       01  WS-MATRIX.
           05  WS-MAT-ROW          OCCURS 100 TIMES.
               10  WS-MAT-COL      OCCURS 101 TIMES PIC 9.
       
       01  WS-PIVOT-COLS           OCCURS 100 TIMES PIC 9(3).
       01  WS-NUM-PIVOTS           PIC 9(3).
       01  WS-FREE-VARS            OCCURS 100 TIMES PIC 9(3).
       01  WS-NUM-FREE             PIC 9(3).
       01  WS-IS-PIVOT             PIC 9.
       01  WS-CURRENT-ROW          PIC 9(3).
       01  WS-MASK                 PIC 9(10).
       01  WS-MAX-MASK             PIC 9(10).
       01  WS-MIN-WEIGHT           PIC 9(3).
       01  WS-WEIGHT               PIC 9(3).
       01  WS-BEST-SOL             OCCURS 100 TIMES PIC 9.
       01  WS-TEMP-SOL             OCCURS 100 TIMES PIC 9.
       01  WS-BIT                  PIC 9.
       01  WS-VAL                  PIC 9.
       01  WS-SOLUTION             OCCURS 100 TIMES PIC 9.
       01  WS-TEMP-NUM             PIC 9(4).
       01  WS-TEMP-STR             PIC X(10).
       01  WS-I                    PIC 9(3).
       01  WS-J                    PIC 9(3).
       01  WS-K                    PIC 9(3).
       01  WS-PIVOT                PIC 9(3).
       01  WS-RESULT               PIC 9(3).
       01  WS-IN-BRACKET           PIC 9.
       01  WS-IN-PAREN             PIC 9.
       01  WS-BUTTON-IDX           PIC 9(3).
       01  WS-DIGIT-STR            PIC X(10).
       01  WS-DIGIT-LEN            PIC 9(2).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE INTO WS-LINE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM.
           
           CLOSE INPUT-FILE.
           
           DISPLAY "Result: " WS-TOTAL-PRESSES.
           STOP RUN.
       
       PROCESS-LINE.
           PERFORM INITIALIZE-VARS.
           PERFORM PARSE-LINE.
           PERFORM BUILD-MATRIX.
           PERFORM SOLVE-MATRIX.
           PERFORM COUNT-PRESSES.
       
       INITIALIZE-VARS.
           MOVE 0 TO WS-NUM-LIGHTS.
           MOVE 0 TO WS-NUM-BUTTONS.
           MOVE 0 TO WS-IN-BRACKET.
           MOVE 0 TO WS-IN-PAREN.
           MOVE 0 TO WS-BUTTON-IDX.
           MOVE 0 TO WS-NUM-PIVOTS.
           MOVE 0 TO WS-NUM-FREE.
           MOVE 1 TO WS-CURRENT-ROW.
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               MOVE 0 TO WS-TARGET(WS-I)
               MOVE 0 TO WS-SOLUTION(WS-I)
               MOVE 0 TO WS-BEST-SOL(WS-I)
               MOVE 0 TO WS-TEMP-SOL(WS-I)
               MOVE 0 TO WS-PIVOT-COLS(WS-I)
               MOVE 0 TO WS-FREE-VARS(WS-I)
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 100
                   MOVE 0 TO WS-BTN-BITS(WS-I, WS-J)
                   MOVE 0 TO WS-MAT-COL(WS-I, WS-J)
               END-PERFORM
               MOVE 0 TO WS-MAT-COL(WS-I, 101)
           END-PERFORM.
       
       PARSE-LINE.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) 
               TO WS-LINE-LEN.
           MOVE 0 TO WS-DIGIT-LEN.
           MOVE SPACES TO WS-DIGIT-STR.
           
           PERFORM VARYING WS-POS FROM 1 BY 1 
               UNTIL WS-POS > WS-LINE-LEN
               
               MOVE WS-LINE(WS-POS:1) TO WS-CHAR
               
               EVALUATE TRUE
                   WHEN WS-CHAR = '['
                       MOVE 1 TO WS-IN-BRACKET
                       MOVE 0 TO WS-NUM-LIGHTS
                   
                   WHEN WS-CHAR = ']'
                       MOVE 0 TO WS-IN-BRACKET
                   
                   WHEN WS-CHAR = '('
                       MOVE 1 TO WS-IN-PAREN
                       ADD 1 TO WS-NUM-BUTTONS
                       MOVE WS-NUM-BUTTONS TO WS-BUTTON-IDX
                       MOVE 0 TO WS-DIGIT-LEN
                   
                   WHEN WS-CHAR = ')'
                       IF WS-DIGIT-LEN > 0
                           PERFORM PROCESS-DIGIT
                       END-IF
                       MOVE 0 TO WS-IN-PAREN
                       MOVE 0 TO WS-DIGIT-LEN
                   
                   WHEN WS-IN-BRACKET = 1
                       IF WS-CHAR = '.' OR WS-CHAR = '#'
                           ADD 1 TO WS-NUM-LIGHTS
                           IF WS-CHAR = '#'
                               MOVE 1 TO WS-TARGET(WS-NUM-LIGHTS)
                           ELSE
                               MOVE 0 TO WS-TARGET(WS-NUM-LIGHTS)
                           END-IF
                       END-IF
                   
                   WHEN WS-IN-PAREN = 1
                       IF WS-CHAR >= '0' AND WS-CHAR <= '9'
                           ADD 1 TO WS-DIGIT-LEN
                           MOVE WS-CHAR TO WS-DIGIT-STR(WS-DIGIT-LEN:1)
                       ELSE IF WS-CHAR = ','
                           IF WS-DIGIT-LEN > 0
                               PERFORM PROCESS-DIGIT
                           END-IF
                           MOVE 0 TO WS-DIGIT-LEN
                       END-IF
               END-EVALUATE
           END-PERFORM.
       
       PROCESS-DIGIT.
           MOVE FUNCTION NUMVAL(WS-DIGIT-STR(1:WS-DIGIT-LEN)) 
               TO WS-TEMP-NUM.
           ADD 1 TO WS-TEMP-NUM.
           MOVE 1 TO WS-BTN-BITS(WS-BUTTON-IDX, WS-TEMP-NUM).
           MOVE SPACES TO WS-DIGIT-STR.
       
       BUILD-MATRIX.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-LIGHTS
               PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > WS-NUM-BUTTONS
                   MOVE WS-BTN-BITS(WS-J, WS-I) 
                       TO WS-MAT-COL(WS-I, WS-J)
               END-PERFORM
               COMPUTE WS-K = WS-NUM-BUTTONS + 1
               MOVE WS-TARGET(WS-I) TO WS-MAT-COL(WS-I, WS-K)
           END-PERFORM.
       
       SOLVE-MATRIX.
           MOVE 0 TO WS-NUM-PIVOTS.
           MOVE 1 TO WS-CURRENT-ROW.
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-BUTTONS
               MOVE 0 TO WS-PIVOT
               
               PERFORM VARYING WS-J FROM WS-CURRENT-ROW BY 1 
                   UNTIL WS-J > WS-NUM-LIGHTS OR WS-PIVOT > 0
                   IF WS-MAT-COL(WS-J, WS-I) = 1
                       MOVE WS-J TO WS-PIVOT
                   END-IF
               END-PERFORM
               
               IF WS-PIVOT > 0
                   IF WS-PIVOT NOT = WS-CURRENT-ROW
                       PERFORM SWAP-ROWS
                   END-IF
                   
                   ADD 1 TO WS-NUM-PIVOTS
                   MOVE WS-I TO WS-PIVOT-COLS(WS-NUM-PIVOTS)
                   
                   PERFORM VARYING WS-J FROM 1 BY 1 
                       UNTIL WS-J > WS-NUM-LIGHTS
                       IF WS-J NOT = WS-CURRENT-ROW 
                           AND WS-MAT-COL(WS-J, WS-I) = 1
                           PERFORM ELIMINATE-ROW
                       END-IF
                   END-PERFORM
                   
                   ADD 1 TO WS-CURRENT-ROW
               END-IF
           END-PERFORM.
           
           MOVE 0 TO WS-NUM-FREE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-BUTTONS
               MOVE 0 TO WS-IS-PIVOT
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NUM-PIVOTS
                   IF WS-PIVOT-COLS(WS-J) = WS-I
                       MOVE 1 TO WS-IS-PIVOT
                   END-IF
               END-PERFORM
               IF WS-IS-PIVOT = 0
                   ADD 1 TO WS-NUM-FREE
                   MOVE WS-I TO WS-FREE-VARS(WS-NUM-FREE)
               END-IF
           END-PERFORM.
           
           COMPUTE WS-MAX-MASK = 2 ** WS-NUM-FREE.
           MOVE 999 TO WS-MIN-WEIGHT.
           
           PERFORM VARYING WS-MASK FROM 0 BY 1 
               UNTIL WS-MASK >= WS-MAX-MASK
               PERFORM TRY-SOLUTION
           END-PERFORM.
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-BUTTONS
               MOVE WS-BEST-SOL(WS-I) TO WS-SOLUTION(WS-I)
           END-PERFORM.
       
       SWAP-ROWS.
           COMPUTE WS-K = WS-NUM-BUTTONS + 1.
           PERFORM VARYING WS-J FROM 1 BY 1 
               UNTIL WS-J > WS-K
               MOVE WS-MAT-COL(WS-CURRENT-ROW, WS-J) TO WS-RESULT
               MOVE WS-MAT-COL(WS-PIVOT, WS-J) 
                   TO WS-MAT-COL(WS-CURRENT-ROW, WS-J)
               MOVE WS-RESULT TO WS-MAT-COL(WS-PIVOT, WS-J)
           END-PERFORM.
       
       TRY-SOLUTION.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-BUTTONS
               MOVE 0 TO WS-TEMP-SOL(WS-I)
           END-PERFORM.
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-FREE
               COMPUTE WS-BIT = FUNCTION MOD(
                   FUNCTION INTEGER(WS-MASK / (2 ** (WS-I - 1))), 2)
               MOVE WS-BIT TO WS-TEMP-SOL(WS-FREE-VARS(WS-I))
           END-PERFORM.
           
           COMPUTE WS-K = WS-NUM-BUTTONS + 1.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-PIVOTS
               MOVE WS-MAT-COL(WS-I, WS-K) TO WS-VAL
               PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > WS-NUM-BUTTONS
                   IF WS-J NOT = WS-PIVOT-COLS(WS-I)
                       IF WS-MAT-COL(WS-I, WS-J) = 1 
                           AND WS-TEMP-SOL(WS-J) = 1
                           IF WS-VAL = 0
                               MOVE 1 TO WS-VAL
                           ELSE
                               MOVE 0 TO WS-VAL
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               MOVE WS-VAL TO WS-TEMP-SOL(WS-PIVOT-COLS(WS-I))
           END-PERFORM.
           
           MOVE 0 TO WS-WEIGHT.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-BUTTONS
               ADD WS-TEMP-SOL(WS-I) TO WS-WEIGHT
           END-PERFORM.
           
           IF WS-WEIGHT < WS-MIN-WEIGHT
               MOVE WS-WEIGHT TO WS-MIN-WEIGHT
               PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > WS-NUM-BUTTONS
                   MOVE WS-TEMP-SOL(WS-I) TO WS-BEST-SOL(WS-I)
               END-PERFORM
           END-IF.
       
       ELIMINATE-ROW.
           COMPUTE WS-PIVOT = WS-NUM-BUTTONS + 1.
           PERFORM VARYING WS-K FROM 1 BY 1 
               UNTIL WS-K > WS-PIVOT
               IF WS-MAT-COL(WS-CURRENT-ROW, WS-K) = 
                   WS-MAT-COL(WS-J, WS-K)
                   MOVE 0 TO WS-MAT-COL(WS-J, WS-K)
               ELSE
                   MOVE 1 TO WS-MAT-COL(WS-J, WS-K)
               END-IF
           END-PERFORM.
       
       COUNT-PRESSES.
           MOVE 0 TO WS-RESULT.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-BUTTONS
               IF WS-SOLUTION(WS-I) = 1
                   ADD 1 TO WS-RESULT
               END-IF
           END-PERFORM.
           ADD WS-RESULT TO WS-TOTAL-PRESSES.
		   
