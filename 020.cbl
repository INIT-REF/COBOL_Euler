       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler016.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 carry    PIC 9(3)    COMP    VALUE 0.
       01 tmp      PIC 9(3)    COMP.
       01 res      PIC 9(4)    COMP    VALUE 0.
       01 n        PIC 9(3)    COMP.
       01 fac100.
           05 digs PIC 9       COMP    VALUE 0 OCCURS 158 TIMES 
                                       INDEXED BY i.
       01 out      PIC Z(10).

       PROCEDURE DIVISION.
           MOVE 1 to digs(1)

           PERFORM VARYING n FROM 1 BY 1 UNTIL n > 100
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 158
                   COMPUTE tmp = n * digs(i) + carry
                   DIVIDE tmp BY 10 GIVING carry REMAINDER digs(i)
               END-PERFORM
           END-PERFORM

           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 158
               ADD digs(i) TO res
           END-PERFORM
 
           MOVE res to out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
