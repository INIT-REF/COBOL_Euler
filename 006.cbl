       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler006.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(3)    COMP.
       01  a   PIC 9(10)   COMP    VALUE 0.
       01  b   PIC 9(10)   COMP    VALUE 0.
       01  res PIC 9(10)   COMP    VALUE 0.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM VARYING n FROM 1 BY 1 UNTIL n > 100
               ADD n to a
               COMPUTE b = b + n * n
           END-PERFORM

           COMPUTE res = a * a - b
           MOVE res TO out
           
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
