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
           01 FirstHalf PIC X(30).
           01 SecondHalf PIC X(30).
           01 Len PIC 9(5).
           01 Res PIC X.
           01 Points PIC 9(10) VALUE 0.
           01 Val PIC 999.
           01 BlankString PIC X(30).


       PROCEDURE DIVISION.
       000-Main.
           OPEN INPUT DataFile
               PERFORM UNTIL EOF=1
                   READ DataFile INTO DataLine
                       AT END MOVE 1 TO EOF
                       NOT AT END
                       MOVE LENGTH OF FUNCTION TRIM(DataLine) TO Len
                       MOVE FUNCTION TRIM(DataLine)(1:Len/2)
                       TO FirstHalf
                       MOVE FUNCTION TRIM(DataLine)(Len/2 + 1:Len)
                       TO SecondHalf
                       INSPECT FirstHalf CONVERTING SecondHalf
                       TO BlankString
                       INSPECT DataLine CONVERTING FirstHalf
                       TO BlankString
                       MOVE FUNCTION TRIM(DataLine)(1:1) TO Res
                       PERFORM 100-GetVal
                   END-READ
               END-PERFORM.
           CLOSE DataFile.
           DISPLAY "Part1: " Points
       STOP RUN
       .

       100-GetVal.
           MOVE FUNCTION ORD(Res) TO Val
      *>      lower-case
           IF Val>97 THEN
               COMPUTE Points = Points + Val - 97
      *>      upper-case
           ELSE
               COMPUTE Points = Points + Val - 65 + 26
           END-IF
       .
