       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY06.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT DataFile ASSIGN TO "../data/06.txt"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
           FILE SECTION.
           FD DataFile.
               01 InputData PIC X(5000).

           WORKING-STORAGE SECTION.
           01 EOF PIC 9.
           01 DataLine PIC X(5000).
           01 DataLineLen PIC 9(5).
           01 FirstMarker PIC 9(5).
      *>      loop counter
           01 LC PIC 9(5) VALUE 1.
      *>      letter value
           01 LVal PIC 99.
      *>      occurence counter
           01 Dup.
               05 AllOcc PIC 9(26) VALUE 0.
               05 Occ REDEFINES AllOcc OCCURS 26 TIMES PIC 9.
           01 I PIC 99.
           01 Interval PIC 99.


       PROCEDURE DIVISION.
       000-Main.
      *>      read input
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                   END-READ
               END-PERFORM
           CLOSE DataFile
           INSPECT FUNCTION TRIM(DataLine)
           TALLYING DataLineLen FOR CHARACTERS
      *>      calc
           MOVE 4 TO Interval
           PERFORM 100-Run
           DISPLAY "Part1: " FirstMarker
           PERFORM 200-CleanUp
           MOVE 14 TO Interval
           PERFORM 100-Run
           DISPLAY "Part2: " FirstMarker
       STOP RUN.

       100-Run.
           PERFORM FOREVER
               MOVE 0 TO AllOcc
               PERFORM VARYING I FROM 0 BY 1 UNTIL I > Interval - 1
                   COMPUTE LVal = FUNCTION ORD(DataLine(LC + I:1)) - 97
                   IF Occ(LVal) = 0 THEN
                       MOVE 1 TO Occ(LVal)
                   ELSE
                       ADD 1 TO LC
                       MOVE 0 TO AllOcc
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               IF AllOcc > 0 THEN
                   COMPUTE FirstMarker = LC + Interval - 1
                   EXIT PERFORM
               END-IF
               IF LC + Interval - 1 > DataLineLen THEN
                   EXIT PERFORM
               END-IF
           END-PERFORM
       .

       200-CleanUp.
           MOVE 0 TO FirstMarker ALLOcc
           MOVE 1 TO LC
       .
