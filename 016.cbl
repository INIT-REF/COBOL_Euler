       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler016.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 carry    PIC 9       COMP    VALUE 0.
       01 tmp      PIC 9(2)    COMP.
       01 res      PIC 9(4)    COMP    VALUE 0.
       01 2p1000.
           05 digs PIC 9       COMP    VALUE 0 OCCURS 302 TIMES 
                                       INDEXED BY i.
       01 out      PIC Z(10).

       PROCEDURE DIVISION.
           MOVE 1 to digs(1)

           PERFORM 1000 TIMES
               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 302
                   COMPUTE tmp = 2 * digs(i) + carry
                   DIVIDE tmp BY 10 GIVING carry REMAINDER digs(i)
               END-PERFORM
           END-PERFORM

           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 302
               ADD digs(i) TO res
           END-PERFORM
 
           MOVE res to out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
