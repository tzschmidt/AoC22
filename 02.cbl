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
           01 EndPoints PIC 9(10) VALUE 0.
           01 Points PIC 99 VALUE 0.


       PROCEDURE DIVISION.
       000-Main.
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                       NOT AT END
                       MOVE 0 TO Points
                       PERFORM 100-RepString
                       ADD Own TO Points
                       PERFORM 200-CalcPoints
                       ADD Points TO EndPoints
                   END-READ
               END-PERFORM.
           CLOSE DataFile.
           DISPLAY "Part 1:" EndPoints
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

       200-CalcPoints.
           IF Opp=Own THEN
               ADD 3 TO Points
           ELSE
               IF Opp + 1=Own OR (Opp=3 AND Own=1) THEN
                   ADD 6 TO Points
               END-IF
           END-IF
       .
