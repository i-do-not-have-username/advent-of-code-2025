       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY11PART1.
       
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
       01  WS-LINE                 PIC X(1000).
       01  WS-LINE-LEN             PIC 9(4).
       01  WS-POS                  PIC 9(4).
       01  WS-CHAR                 PIC X.
       
       01  WS-NUM-DEVICES          PIC 9(4) VALUE 0.
       01  WS-DEVICES.
           05  WS-DEVICE           OCCURS 1000 TIMES.
               10  WS-DEV-NAME     PIC X(10).
               10  WS-DEV-OUTPUTS  OCCURS 50 TIMES PIC X(10).
               10  WS-DEV-OUT-CNT  PIC 9(3).
       
       01  WS-DEVICE-NAME          PIC X(10).
       01  WS-OUTPUT-NAME          PIC X(10).
       01  WS-WORD                 PIC X(10).
       01  WS-WORD-LEN             PIC 9(3).
       01  WS-IN-DEVICE            PIC 9.
       
       01  WS-I                    PIC 9(4).
       01  WS-J                    PIC 9(4).
       01  WS-K                    PIC 9(4).
       01  WS-FOUND                PIC 9.
       01  WS-DEV-IDX              PIC 9(4).
       
       01  WS-PATH-COUNT           PIC 9(16) VALUE 0.
       01  WS-VISITED              OCCURS 1000 TIMES PIC 9.
       01  WS-START-IDX            PIC 9(4).
       01  WS-TARGET-IDX           PIC 9(4).
       01  WS-DAC-IDX              PIC 9(4).
       01  WS-FFT-IDX              PIC 9(4).
       
       01  WS-STACK.
           05  WS-STACK-ITEM       OCCURS 100 TIMES.
               10  WS-STACK-DEV    PIC 9(4).
               10  WS-STACK-NEXT   PIC 9(3).
               10  WS-STACK-DAC    PIC 9.
               10  WS-STACK-FFT    PIC 9.
       01  WS-STACK-PTR            PIC 9(3).
       01  WS-HAS-DAC              PIC 9.
       01  WS-HAS-FFT              PIC 9.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE INTO WS-LINE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-LINE
               END-READ
           END-PERFORM.
           
           CLOSE INPUT-FILE.
           
           PERFORM FIND-DEVICE-INDICES.
           PERFORM COUNT-PATHS.
           
           DISPLAY "Result: " WS-PATH-COUNT.
           STOP RUN.
       
       PARSE-LINE.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE)) TO WS-LINE-LEN.
           IF WS-LINE-LEN = 0
               EXIT PARAGRAPH
           END-IF.
           
           MOVE SPACES TO WS-DEVICE-NAME.
           MOVE 0 TO WS-WORD-LEN.
           MOVE 0 TO WS-IN-DEVICE.
           
           PERFORM VARYING WS-POS FROM 1 BY 1 UNTIL WS-POS > WS-LINE-LEN
               MOVE WS-LINE(WS-POS:1) TO WS-CHAR
               
               IF WS-CHAR = ':'
                   IF WS-WORD-LEN > 0
                       MOVE WS-WORD TO WS-DEVICE-NAME
                       PERFORM ADD-DEVICE
                       MOVE 1 TO WS-IN-DEVICE
                       MOVE 0 TO WS-WORD-LEN
                       MOVE SPACES TO WS-WORD
                   END-IF
               ELSE IF WS-CHAR = ' '
                   IF WS-WORD-LEN > 0 AND WS-IN-DEVICE = 1
                       MOVE WS-WORD TO WS-OUTPUT-NAME
                       PERFORM ADD-OUTPUT
                       MOVE 0 TO WS-WORD-LEN
                       MOVE SPACES TO WS-WORD
                   ELSE IF WS-WORD-LEN > 0
                       MOVE 0 TO WS-WORD-LEN
                       MOVE SPACES TO WS-WORD
                   END-IF
               ELSE
                   ADD 1 TO WS-WORD-LEN
                   MOVE WS-CHAR TO WS-WORD(WS-WORD-LEN:1)
               END-IF
           END-PERFORM.
           
           IF WS-WORD-LEN > 0 AND WS-IN-DEVICE = 1
               MOVE WS-WORD TO WS-OUTPUT-NAME
               PERFORM ADD-OUTPUT
           END-IF.
       
       ADD-DEVICE.
           MOVE 0 TO WS-FOUND.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-DEVICES
               IF WS-DEV-NAME(WS-I) = WS-DEVICE-NAME
                   MOVE 1 TO WS-FOUND
                   MOVE WS-I TO WS-DEV-IDX
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
           IF WS-FOUND = 0
               ADD 1 TO WS-NUM-DEVICES
               MOVE WS-NUM-DEVICES TO WS-DEV-IDX
               MOVE WS-DEVICE-NAME TO WS-DEV-NAME(WS-DEV-IDX)
               MOVE 0 TO WS-DEV-OUT-CNT(WS-DEV-IDX)
           END-IF.
       
       ADD-OUTPUT.
           PERFORM ENSURE-DEVICE-EXISTS.
           ADD 1 TO WS-DEV-OUT-CNT(WS-DEV-IDX)
           COMPUTE WS-K = WS-DEV-OUT-CNT(WS-DEV-IDX)
           MOVE WS-OUTPUT-NAME TO WS-DEV-OUTPUTS(WS-DEV-IDX, WS-K).
       
       ENSURE-DEVICE-EXISTS.
           MOVE 0 TO WS-FOUND.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-NUM-DEVICES
               IF WS-DEV-NAME(WS-J) = WS-OUTPUT-NAME
                   MOVE 1 TO WS-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
           IF WS-FOUND = 0
               ADD 1 TO WS-NUM-DEVICES
               MOVE WS-OUTPUT-NAME TO WS-DEV-NAME(WS-NUM-DEVICES)
               MOVE 0 TO WS-DEV-OUT-CNT(WS-NUM-DEVICES)
           END-IF.
       
       FIND-DEVICE-INDICES.
           MOVE 0 TO WS-START-IDX.
           MOVE 0 TO WS-TARGET-IDX.
           MOVE 0 TO WS-DAC-IDX.
           MOVE 0 TO WS-FFT-IDX.
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-DEVICES
               IF WS-DEV-NAME(WS-I) = 'svr'
                   MOVE WS-I TO WS-START-IDX
               END-IF
               IF WS-DEV-NAME(WS-I) = 'out'
                   MOVE WS-I TO WS-TARGET-IDX
               END-IF
               IF WS-DEV-NAME(WS-I) = 'dac'
                   MOVE WS-I TO WS-DAC-IDX
               END-IF
               IF WS-DEV-NAME(WS-I) = 'fft'
                   MOVE WS-I TO WS-FFT-IDX
               END-IF
           END-PERFORM.
       
       COUNT-PATHS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NUM-DEVICES
               MOVE 0 TO WS-VISITED(WS-I)
           END-PERFORM.
           
           MOVE 0 TO WS-PATH-COUNT.
           PERFORM DFS-ITERATIVE.
       
       DFS-ITERATIVE.
           MOVE 0 TO WS-STACK-PTR.
           
           ADD 1 TO WS-STACK-PTR.
           MOVE WS-START-IDX TO WS-STACK-DEV(WS-STACK-PTR).
           MOVE 1 TO WS-STACK-NEXT(WS-STACK-PTR).
           MOVE 0 TO WS-STACK-DAC(WS-STACK-PTR).
           MOVE 0 TO WS-STACK-FFT(WS-STACK-PTR).
           MOVE 1 TO WS-VISITED(WS-START-IDX).
           
           PERFORM UNTIL WS-STACK-PTR = 0
               MOVE WS-STACK-DEV(WS-STACK-PTR) TO WS-I
               MOVE WS-STACK-NEXT(WS-STACK-PTR) TO WS-K
               MOVE WS-STACK-DAC(WS-STACK-PTR) TO WS-HAS-DAC
               MOVE WS-STACK-FFT(WS-STACK-PTR) TO WS-HAS-FFT
               
               IF WS-I = WS-DAC-IDX
                   MOVE 1 TO WS-HAS-DAC
                   MOVE 1 TO WS-STACK-DAC(WS-STACK-PTR)
               END-IF
               
               IF WS-I = WS-FFT-IDX
                   MOVE 1 TO WS-HAS-FFT
                   MOVE 1 TO WS-STACK-FFT(WS-STACK-PTR)
               END-IF
               
               IF WS-I = WS-TARGET-IDX
                   IF WS-HAS-DAC = 1 AND WS-HAS-FFT = 1
                       ADD 1 TO WS-PATH-COUNT
                   END-IF
                   MOVE 0 TO WS-VISITED(WS-I)
                   SUBTRACT 1 FROM WS-STACK-PTR
               ELSE IF WS-K > WS-DEV-OUT-CNT(WS-I)
                   MOVE 0 TO WS-VISITED(WS-I)
                   SUBTRACT 1 FROM WS-STACK-PTR
               ELSE
                   MOVE 0 TO WS-DEV-IDX
                   
                   PERFORM VARYING WS-J FROM 1 BY 1 
                       UNTIL WS-J > WS-NUM-DEVICES
                       IF WS-DEV-NAME(WS-J) = 
                           WS-DEV-OUTPUTS(WS-I, WS-K)
                           MOVE WS-J TO WS-DEV-IDX
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
                   
                   ADD 1 TO WS-K
                   MOVE WS-K TO WS-STACK-NEXT(WS-STACK-PTR)
                   
                   IF WS-DEV-IDX > 0 AND WS-VISITED(WS-DEV-IDX) = 0
                       ADD 1 TO WS-STACK-PTR
                       MOVE WS-DEV-IDX TO WS-STACK-DEV(WS-STACK-PTR)
                       MOVE 1 TO WS-STACK-NEXT(WS-STACK-PTR)
                       MOVE WS-HAS-DAC TO WS-STACK-DAC(WS-STACK-PTR)
                       MOVE WS-HAS-FFT TO WS-STACK-FFT(WS-STACK-PTR)
                       MOVE 1 TO WS-VISITED(WS-DEV-IDX)
                   END-IF
               END-IF
           END-PERFORM.
		   
