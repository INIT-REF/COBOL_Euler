       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler024.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 n        PIC 99      COMP.
       01 i        PIC 99      COMP.
       01 j        PIC 99      COMP.
       01 k        PIC 99      COMP.
       01 l        PIC 99      COMP.
       01 p        PIC 9(6)    COMP    VALUE 999999.
       01 perm     PIC X(10)           VALUE "0123456789".
       01 tmp      PIC X(10).
       01 res      PIC X(10).
       01 facts.
           05 fact PIC 9(6)    COMP OCCURS 10 TIMES.

       PROCEDURE DIVISION.
           MOVE 1 TO fact(1)
           MOVE 1 TO j

           PERFORM VARYING n FROM 1 BY 1 UNTIL n > 9
               MULTIPLY fact(n) by n GIVING fact(n + 1)
           END-PERFORM

           PERFORM VARYING n FROM 10 BY -1 UNTIL n = 0
               DIVIDE p BY fact(n) GIVING i REMAINDER p
               ADD 1 TO i
               MOVE perm(i:1) TO res(j:1)
               MOVE 1 TO k

               PERFORM VARYING l FROM 1 BY 1 UNTIL l > 10
                   IF l = i
                       CONTINUE
                   ELSE
                       MOVE perm(l:1) TO tmp(k:1)
                       ADD 1 TO k
                   END-IF
               END-PERFORM

               MOVE tmp to perm
               ADD 1 to j
           END-PERFORM
           
           DISPLAY res
           STOP RUN.
