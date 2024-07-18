       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler015.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(2)    COMP.
       01  res PIC 9(18)   COMP    VALUE 1.
       01  out PIC Z(18).

       PROCEDURE DIVISION.
           PERFORM VARYING n FROM 1 BY 1 UNTIL n > 20
               COMPUTE res = res * (20 + n) / n
           END-PERFORM

           MOVE res TO out
           
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
