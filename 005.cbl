       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler005.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  a   PIC 9(10)   COMP    VALUE 1.
       01  b   PIC 9(10)   COMP.
       01  g1  PIC 9(10)   COMP.
       01  g2  PIC 9(10)   COMP.
       01  tmp PIC 9(10)   COMP.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM VARYING b FROM 2 BY 1 UNTIL b > 20
               MOVE a to g1
               MOVE b to g2

               PERFORM UNTIL g1 = 0
                   MOVE g1 to tmp
                   MOVE FUNCTION MOD (g2, g1) TO g1
                   MOVE tmp to g2
               END-PERFORM

               COMPUTE a = b * (a / g2)
           END-PERFORM

           MOVE a TO out
           
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
