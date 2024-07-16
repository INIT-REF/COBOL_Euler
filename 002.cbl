       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler002.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  f1  PIC 9(7)    VALUE 1.
       01  f2  PIC 9(7)    VALUE 2.
       01  res PIC 9(10)   VALUE 0.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM UNTIL f2 > 4000000
               IF FUNCTION MOD (f2, 2) = 0
                   ADD f2 TO res
               END-IF

               ADD f1 to f2
               COMPUTE f1 = f2 - f1
           END-PERFORM

           MOVE res TO out
           
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
