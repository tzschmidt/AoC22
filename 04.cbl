       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY04.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT DataFile ASSIGN TO "../data/04.txt"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
           FILE SECTION.
           FD DataFile.
               01 InputData PIC X(20).

           WORKING-STORAGE SECTION.
           01 EOF PIC 9.
           01 DataLine PIC X(20).
           01 Pair.
               05 Elf1 OCCURS 2 TIMES PIC 99.
               05 Elf2 OCCURS 2 TIMES PIC 99.
      *>      contain counter
           01 CC PIC 9(5) VALUE 0.
           01 IC PIC 9 VALUE 0.
      *>      overlap counter
           01 CO PIC 9(5) VALUE 0.


       PROCEDURE DIVISION.
       000-Main.
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                       NOT AT END
                       UNSTRING DataLine DELIMITED BY "," OR "-"
                       INTO Elf1(1) Elf1(2) Elf2(1) Elf2(2)
                       END-UNSTRING
                       PERFORM 100-CheckContain
                       PERFORM 200-CheckOverlap
                   END-READ
               END-PERFORM.
           CLOSE DataFile.
           DISPLAY "Part1: " CC
           DISPLAY "Part2: " CO
       STOP RUN
       .

       100-CheckContain.
      *>      T to check if same intervals => C-1
           MOVE 0 TO IC
      *>      elf2 in elf1
           IF Elf1(1) <= Elf2(1) THEN
               IF Elf1(2) >= Elf2(2) THEN
                   ADD 1 TO CC
                   ADD 1 TO IC
               END-IF
           END-IF
      *>      elf1 in elf2
           IF Elf2(1) <= Elf1(1) THEN
               IF Elf2(2) >= Elf1(2) THEN
                   ADD 1 TO CC
                   ADD 1 TO IC
               END-IF
           END-IF
           IF IC = 2 THEN
               SUBTRACT 1 FROM CC
           END-IF
       .

       200-CheckOverlap.
      *>      edge case
           IF Elf1(1) = Elf2(2) OR Elf1(2) = Elf2(1) THEN
               ADD 1 TO CO
           ELSE
      *>          elf1 starts in elf2
               IF Elf1(1) >= Elf2(1) AND Elf1(1) <= Elf2(2) THEN
                   ADD 1 TO CO
               ELSE
      *>              elf2 starts in elf1
                   IF Elf2(1) >= Elf1(1) AND Elf2(1) <= Elf1(2) THEN
                       ADD 1 TO CO
                   END-IF
                END-IF
           END-IF
       .
