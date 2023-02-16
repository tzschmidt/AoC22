       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY03.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT DataFile ASSIGN TO "../data/03.txt"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
           FILE SECTION.
           FD DataFile.
               01 InputData PIC X(100).

           WORKING-STORAGE SECTION.
           01 EOF PIC 9.
           01 DataLine PIC X(60).
           01 Part1.
               05 Half OCCURS 2 TIMES PIC X(30).
           01 Len PIC 9(5).
           01 PointsPart1 PIC 9(10) VALUE 0.

           01 Part2.
               05 Elf OCCURS 3 TIMES PIC X(60).
           01 Part2C PIC X(180).
           01 C PIC 9 VALUE 1.
           01 PointsPart2 PIC 9(10) VALUE 0.

           01 Res PIC X.
           01 Val PIC 999.
           01 BlankString PIC X(60).


       PROCEDURE DIVISION.
       000-Main.
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                       NOT AT END
                       MOVE FUNCTION TRIM(DataLine) TO Elf(C)
                       PERFORM 100-CalcPart1
                       IF C=3 THEN
                           MOVE 1 TO C
                           PERFORM 300-CalcPart2
                       ELSE
                           ADD 1 TO C
                   END-READ
               END-PERFORM.
           CLOSE DataFile.
           DISPLAY "Part1: " PointsPart1
           DISPLAY "Part2: " PointsPart2
       STOP RUN
       .

       100-CalcPart1.
           MOVE LENGTH OF FUNCTION TRIM(DataLine) TO Len
           MOVE FUNCTION TRIM(DataLine)(1:Len/2) TO Half(1)
           MOVE FUNCTION TRIM(DataLine)(Len/2 + 1:Len) TO Half(2)
           INSPECT Half(1) CONVERTING Half(2) TO BlankString(1:30)
           INSPECT DataLine CONVERTING Half(1) TO BlankString(1:30)
           MOVE FUNCTION TRIM(DataLine)(1:1) TO Res
           PERFORM 200-GetVal
           Add Val To PointsPart1
       .

       200-GetVal.
           MOVE FUNCTION ORD(Res) TO Val
      *>      lower-case
           IF Val>97 THEN
               COMPUTE Val = Val - 97
      *>      upper-case
           ELSE
               COMPUTE Val = Val - 65 + 26
           END-IF
       .

       300-CalcPart2.
           MOVE Part2 TO Part2C
           INSPECT Elf(2) CONVERTING ELF(1) TO BlankString
           INSPECT Elf(3) CONVERTING ELF(1) TO BlankString
           INSPECT Part2C CONVERTING Elf(2) TO BlankString
           INSPECT Part2C CONVERTING Elf(3) TO BlankString
           MOVE Part2C TO Part2
           INSPECT Elf(2) CONVERTING ELF(3) TO BlankString
           INSPECT Part2C CONVERTING Elf(2) TO BlankString
           MOVE Part2C TO Part2
           MOVE FUNCTION TRIM(Elf(2))(1:1) TO Res
           PERFORM 200-GetVal
           ADD Val TO PointsPart2
       .
