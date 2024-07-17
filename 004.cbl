       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler004.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  a   PIC 9(4)    COMP.
       01  b   PIC 9(4)    COMP.
       01  p   PIC 9(6)    COMP.
       01  tmp PIC 9(6)    COMP.
       01  rev PIC 9(6)    COMP.
       01  max PIC 9(6)    VALUE 0.

       PROCEDURE DIVISION.
           PERFORM VARYING a FROM 100 BY 1 UNTIL a = 1000
               PERFORM VARYING b FROM a BY 1 UNTIL b = 1000
                   MULTIPLY a BY b GIVING p
                   MOVE p TO tmp

                   PERFORM UNTIL tmp = 0
                       COMPUTE rev = 10 * rev + FUNCTION MOD (tmp, 10)
                       DIVIDE tmp BY 10 GIVING tmp
                   END-PERFORM

                   IF rev = p AND p > max
                       MOVE p TO max
               END-PERFORM
           END-PERFORM
           
           DISPLAY max
           STOP RUN.
