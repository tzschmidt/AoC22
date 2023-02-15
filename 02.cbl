       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY02.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT DataFile ASSIGN TO "../data/02.txt"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
           FILE SECTION.
           FD DataFile.
               01 InputData PIC X(25).

           WORKING-STORAGE SECTION.
           01 EOF PIC 9.
           01 DataLine.
               05 Opp PIC 9.
               05 FILLER PIC X.
               05 Own PIC 9.
           01 PointsPart1 PIC 9(10) VALUE 0.
           01 PointsPart2 PIC 9(10) VALUE 0.


       PROCEDURE DIVISION.
       000-Main.
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                       NOT AT END
                       PERFORM 100-RepString
                       PERFORM 200-CalcPointsPart1
                       PERFORM 300-CalcPointsPart2
                   END-READ
               END-PERFORM.
           CLOSE DataFile.
           DISPLAY "Part 1: " PointsPart1
           DISPLAY "Part 2: " PointsPart2
       STOP RUN
       .

       100-RepString.
           INSPECT DataLine REPLACING ALL 'A' BY '1'
           INSPECT DataLine REPLACING ALL 'B' BY '2'
           INSPECT DataLine REPLACING ALL 'C' BY '3'
           INSPECT DataLine REPLACING ALL 'X' BY '1'
           INSPECT DataLine REPLACING ALL 'Y' BY '2'
           INSPECT DataLine REPLACING ALL 'Z' BY '3'
       .

       200-CalcPointsPart1.
           ADD Own TO PointsPart1
           IF Opp=Own THEN
               ADD 3 TO PointsPart1
           ELSE
               IF FUNCTION MOD(Opp, 3) + 1=Own THEN
                   ADD 6 TO PointsPart1
               END-IF
           END-IF
       .

       300-CalcPointsPart2.
           COMPUTE PointsPart2 = PointsPart2 + (Own - 1) * 3
           IF Opp=2 THEN
               ADD Own TO PointsPart2
           ELSE
               IF Opp=3 THEN
                   COMPUTE PointsPart2
                   = PointsPart2 + FUNCTION MOD(Own, 3) + 1
               ELSE
                   COMPUTE PointsPart2
                   = PointsPart2 + FUNCTION MOD(Own + 1, 3) + 1
               END-IF
           END-IF
       .
