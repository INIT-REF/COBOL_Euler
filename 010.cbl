       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler010.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 n        PIC 9(4)    COMP.
       01 res      PIC 9(18)   COMP    VALUE 2.
       01 sieve.
           05 isp  PIC 9               VALUE 1 OCCURS 2000000 TIMES 
                                       INDEXED BY i.
       01 out      PIC Z(18).

       PROCEDURE DIVISION.
           PERFORM VARYING n FROM 2 BY 1 UNTIL n * n > 2000000
               SET i TO n

               IF isp(i) = 1
                   MULTIPLY n BY n GIVING i
                   PERFORM VARYING i FROM i BY n UNTIL i > 2000000
                       SET isp(i) TO 0
                   END-PERFORM
           
           END-PERFORM
           
           PERFORM VARYING i FROM 3 BY 2 UNTIL i > 2000000
               IF isp(i) = 1
                   ADD i TO res
           END-PERFORM

           MOVE res TO out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
