       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler026.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  a       PIC 9(4)    COMP.
       01  d       PIC 9(4)    COMP.
       01  l       PIC 9(4)    COMP.
       01  max     PIC 9(4)    COMP.
       01  maxl    PIC 9(4)    COMP VALUE 0.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM VARYING d FROM 3 BY 2 UNTIL d > 1000
               IF FUNCTION MOD (d, 5) > 0
                   MOVE FUNCTION MOD (10, d) TO a
                   MOVE 1 to l

                   PERFORM UNTIL a <= 1
                       ADD 1 to l
                       MOVE FUNCTION MOD(a * 10, d) TO a
                   END-PERFORM

                   IF l > maxl
                       MOVE l TO maxl
                       MOVE d TO max
                   END-IF
               END-IF
           END-PERFORM

           MOVE max TO out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
