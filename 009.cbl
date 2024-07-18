       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler009.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  r   PIC 9(3)    COMP    VALUE 0.
       01  rs2 PIC 9(6)    COMP.
       01  s   PIC 9(3)    COMP.
       01  t   PIC 9(3)    COMP.
       01  rem PIC 9(3)    COMP.
       01  prd PIC 9(10)   COMP.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM FOREVER
               ADD 2 TO r
               COMPUTE rs2 = r * r / 2

               PERFORM VARYING s FROM 1 BY 1 UNTIL s * s > rs2
                   DIVIDE rs2 BY s GIVING t REMAINDER rem
                   
                   IF rem = 0 AND 3 * r + 2 * s + 2 * t = 1000
                       GO TO DONE
               END-PERFORM
           END-PERFORM.
       
       DONE.
           COMPUTE prd = (r + s) * (r + t) * (r + s + t)
           MOVE prd TO out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
