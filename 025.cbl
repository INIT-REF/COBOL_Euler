       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler025.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 n        PIC 9(4)    COMP    VALUE 2.
       01 carry    PIC 9(10)   COMP    VALUE 0.
       01 tmp      PIC 9(18)   COMP.
       01 f1.
           05 fib1 PIC 9(10)   COMP   VALUE 0 OCCURS 100 TIMES 
                                       INDEXED BY i.
       01 f2.
           05 fib2 PIC 9(10)   COMP   VALUE 0 OCCURS 100 TIMES 
                                       INDEXED BY j.
       01 ftmp.
           05 ft   PIC 9(10)   COMP   OCCURS 100 TIMES.
       01 out      PIC Z(10).

       PROCEDURE DIVISION.
           MOVE 1 to fib1(1)
           MOVE 1 to fib2(1)

           PERFORM UNTIL fib2(100) > 1000000000
               MOVE f2 to ftmp

               PERFORM VARYING i FROM 1 BY 1 UNTIL i > 100
                   SET j to i
                   COMPUTE tmp = fib1(i) + fib2(j) + carry
                   DIVIDE tmp BY 10000000000
                       GIVING carry REMAINDER fib2(j)
               END-PERFORM

               MOVE ftmp to f1
               ADD 1 to n
           END-PERFORM
 
           MOVE n to out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
