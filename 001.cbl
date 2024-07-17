       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler001.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(4).
       01  res PIC 9(10)   VALUE 0.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM VARYING n FROM 1 BY 1 UNTIL n = 1000
               IF FUNCTION MOD (n, 3) * FUNCTION MOD (n, 5) = 0
                   ADD n TO res
           END-PERFORM

           MOVE res TO out
           
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
