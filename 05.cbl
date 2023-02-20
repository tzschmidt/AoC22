       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY05.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT DataFile ASSIGN TO "../data/05.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD DataFile.
               01 InputData PIC X(40).

           WORKING-STORAGE SECTION.
           01 EOF PIC 9.
           01 DataLine.
               05 Temp OCCURS 9 TIMES INDEXED BY I.
                   10 FILLER PIC X.
                   10 TempS PIC A.
                   10 FILLER PIC XX.

           01 Stacks.
               05 Stack OCCURS 9 TIMES INDEXED BY J.
                   10 Val OCCURS 99 TIMES PIC A.
           01 StackCounter.
               05 StackC OCCURS 9 TIMES PIC 99 VALUE 0.
           01 StackH PIC 99.
           01 L PIC 9 VALUE 0.
           01 StackMove OCCURS 3 TIMES PIC 99.
           01 StacksTop.
               05 StackTop OCCURS 9 TIMES PIC A.
           01 Part PIC 9.
           01 A PIC 99.

           01 Trash PIC X(20).



       PROCEDURE DIVISION.
       000-Main.
           PERFORM 200-GetStackHeigth
           MOVE 1 TO Part.
           PERFORM 100-Run
           PERFORM 600-CleanUp
           MOVE 2 TO Part
           PERFORM 100-Run
       STOP RUN
       .

       100-Run.
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                       NOT AT END
                       IF FUNCTION TRIM(DataLine)(1:1)="[" THEN
      *>                      get values and stack counter
                           PERFORM VARYING I FROM 1 BY 1 UNTIL I>9
                               MOVE TempS(I) TO Val(I,StackH - L)
                               IF TempS(I) NOT =" " THEN
                                   ADD 1 TO StackC(I)
                               END-IF
                           END-PERFORM
                           ADD 1 TO L
                       ELSE
                           IF DataLine(1:1)="m" THEN
                               IF Part=1 THEN
                                   PERFORM 300-MovePart1
                               ELSE
                                   PERFORM 500-MovePart2
                               END-iF
                           END-IF
                       END-IF
                   END-READ
               END-PERFORM.
           CLOSE DataFile.
           PERFORM 400-GetTop
           DISPLAY "Part" Part ": " StacksTop
           MOVE 0 TO EOF
       .

       200-GetStackHeigth.
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                       NOT AT END
                       IF FUNCTION TRIM(DataLine)(1:1)="[" THEN
                           ADD 1 TO StackH
                       ELSE
                           EXIT PERFORM
                       END-IF
                   END-READ
               END-PERFORM.
           CLOSE DataFile
           MOVE 0 TO EOF
       .

       300-MovePart1.
           UNSTRING DataLine(6:) DELIMITED BY " "
           INTO StackMove(1) Trash StackMove(2) Trash StackMove(3)
      *>      move value and adjust stack counter as often as required
           PERFORM StackMove(1) TIMES
               ADD 1 TO StackC(StackMove(3))
               MOVE Val(StackMove(2),StackC(StackMove(2)))
               TO Val(StackMove(3),StackC(StackMove(3)))
               MOVE " " TO Val(StackMove(2),StackC(StackMove(2)))
               SUBTRACT 1 FROM StackC(StackMove(2))
           END-PERFORM
       .

       400-GetTop.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J>9
               IF StackC(J) NOT =0 THEN
                   MOVE Val(J,StackC(J)) TO StackTop(J)
               END-IF
           END-PERFORM
       .

       500-MovePart2.
           UNSTRING DataLine(6:) DELIMITED BY " "
           INTO StackMove(1) Trash StackMove(2) Trash StackMove(3)
      *>      separately move values and adjust stack counter
      *>      keep relative positions of moved values
           ADD StackMove(1) TO StackC(StackMove(3))
           PERFORM VARYING A FROM StackMove(1) BY -1 UNTIL A=0
               MOVE Val(StackMove(2),StackC(StackMove(2)) - A + 1)
               TO Val(StackMove(3),StackC(StackMove(3)) - A + 1)
               MOVE " "
               TO Val(StackMove(2),StackC(StackMove(2)) - A + 1)
           END-PERFORM
           SUBTRACT StackMove(1) FROM StackC(StackMove(2))
       .

       600-CleanUp.
      *>      clean up everything for next use
           MOVE "000000000000000000" TO StackCounter
           MOVE " " TO Stacks
           MOVE 0 TO L
       .
