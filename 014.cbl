       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler014.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 n        PIC 9(7)    COMP.
       01 tmp      PIC 9(18)   COMP.
       01 l        PIC 9(3)    COMP.
       01 maxl     PIC 9(3)    COMP    VALUE 0.
       01 max      PIC 9(6)    COMP.
       01 cache.
           05 len  PIC 9(3)            VALUE 0 OCCURS 999999 TIMES.
       01 out      PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM VARYING n FROM 1 BY 1 UNTIL n = 1000000
               MOVE n TO tmp
               MOVE 1 TO l

               PERFORM UNTIL tmp = 1
                   IF FUNCTION MOD (tmp, 2) = 0
                       DIVIDE tmp BY 2 GIVING tmp
                   ELSE
                       COMPUTE tmp = 3 * tmp + 1
                   END-IF

                   IF tmp < 1000000 AND len(tmp) > 0
                       ADD len(tmp) to l
                       EXIT PERFORM
                   END-IF

                   ADD 1 to l
               END-PERFORM
               
               MOVE l to len(n)

               IF l > maxl
                   MOVE n TO max
                   MOVE l TO maxl
               END-IF
           END-PERFORM

           MOVE max TO out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
