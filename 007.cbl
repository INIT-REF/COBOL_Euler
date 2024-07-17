       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler007.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 n        PIC 9(3)    COMP.
       01 cnt      PIC 9(5)    COMP    VALUE 1.
       01 sieve.
           05 isp  PIC 9               VALUE 1 OCCURS 115000 TIMES 
                                       INDEXED BY i.
       01 out      PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM VARYING n FROM 2 BY 1 UNTIL n * n > 115000
               SET i TO n

               IF isp(i) = 1
                   MULTIPLY n BY n GIVING i
                   PERFORM VARYING i FROM i BY n UNTIL i > 115000
                       SET isp(i) TO 0
                   END-PERFORM
           
           END-PERFORM

           SET i to 1
           
           PERFORM UNTIL cnt = 10001
               SET i UP BY 2
               ADD isp(i) TO cnt
           END-PERFORM

           MOVE i TO out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
