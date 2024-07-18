       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler012.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(10)   COMP    VALUE 1.
       01  tri PIC 9(10)   COMP    VALUE 1.
       01  tmp PIC 9(10)   COMP.
       01  d   PIC 9(10)   COMP.
       01  exp PIC 9(10)   COMP.
       01  cnt PIC 9(3)    COMP.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM FOREVER
               ADD 1 TO n
               ADD n TO tri
               MOVE tri to tmp
               MOVE 1 TO cnt
               MOVE 1 TO exp
               
               PERFORM UNTIL FUNCTION MOD (tmp, 2) > 0
                   ADD 1 TO exp
                   DIVIDE tmp by 2 GIVING tmp
               END-PERFORM

               MULTIPLY exp BY cnt

               PERFORM VARYING d FROM 3 BY 2 UNTIL d * d > tmp
                   MOVE 1 TO exp

                   PERFORM UNTIL FUNCTION MOD (tmp, d) > 0
                       ADD 1 to exp
                       DIVIDE tmp BY d GIVING tmp
                   END-PERFORM

                   MULTIPLY exp BY cnt
               END-PERFORM

               IF tmp > 2
                   MULTIPLY 2 by cnt
               END-IF
               
               IF cnt > 500 GO TO DONE
           END-PERFORM.

       DONE.
           MOVE tri TO out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
