       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY01.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT DataFile ASSIGN TO "../data/01.txt"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
           FILE SECTION.
           FD DataFile.
               01 InputData PIC X(25).

           WORKING-STORAGE SECTION.
           01 EOF PIC 9.
           01 DataLine PIC X(20).
           01 CurrentCalories PIC 9(20).
      *>      Top 3 + 4th temp
           01 Top3.
               05 MaxCalories OCCURS 4 TIMES INDEXED BY I PIC 9(20)
               VALUE 0.
           01 SumCalories PIC 9(20) VALUE 0.

       PROCEDURE DIVISION.
       000-Main.
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                       PERFORM 100-Check
                       PERFORM 300-Array-Sum
                       NOT AT END
                           IF DataLine=SPACE THEN
                               PERFORM 100-Check
                           ELSE
                               ADD FUNCTION NUMVAL(DataLine)
                               TO CurrentCalories
                           END-IF
                   END-READ
               END-PERFORM.
           CLOSE DataFile.
           DISPLAY "PART 1: " MaxCalories(1)
           DISPLAY "PART 2: " SumCalories
       STOP RUN
       .

       100-Check.
           IF CurrentCalories > MaxCalories(3) THEN
               MOVE CurrentCalories TO MaxCalories(3)
               PERFORM 200-Update
           END-IF
           MOVE 0 TO CurrentCalories
           .

       200-Update.
           PERFORM VARYING I FROM 2 BY -1 UNTIL I=0
               IF MaxCalories(I + 1) > MaxCalories(I) THEN
                   MOVE MaxCalories(I) TO MaxCalories(4)
                   MOVE MaxCalories(I + 1) TO MaxCalories(I)
                   MOVE MaxCalories(4) TO MaxCalories(I + 1)
               END-IF
           END-PERFORM
           .

       300-Array-Sum.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I>3
               ADD MaxCalories(I) TO SumCalories
           END-PERFORM
           .
