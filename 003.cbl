       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler003.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(12)   COMP    VALUE 600851475143.
       01  d   PIC 9(10)   COMP    VALUE 3.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM UNTIL n = 1
               IF FUNCTION MOD (n, d) = 0
                   DIVIDE n BY d giving n
               ELSE
                   ADD 2 to d
           END-PERFORM

           MOVE d TO out
           
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
