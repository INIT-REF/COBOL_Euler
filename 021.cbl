       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler021.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(6)    COMP.
       01  ds1 PIC 9(6)    COMP.
       01  ds2 PIC 9(6)    COMP.
       01  res PIC 9(10)   COMP    VALUE 0.
       01  out PIC Z(10).

       PROCEDURE DIVISION.
           PERFORM VARYING n FROM 1 BY 1 UNTIL n > 10000
               CALL "Divsum" USING n, ds1
               CALL "Divsum" Using ds1, ds2
               
               IF n = ds2 AND n <> ds1 ADD n TO res
           END-PERFORM

           MOVE res TO out
           DISPLAY FUNCTION TRIM (out)
           STOP RUN.
       END PROGRAM Euler021.



       IDENTIFICATION DIVISION.
       PROGRAM-ID. Divsum.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  d   PIC 9(3)    COMP.
       01  q   PIC 9(6)    COMP.
       01  rem PIC 9(6)    COMP.

       LINKAGE SECTION.
       01  n   PIC 9(6)    COMP.
       01  ds  PIC 9(6)    COMP.
       
       PROCEDURE DIVISION USING n, ds.
           MOVE 1 to ds.

           PERFORM VARYING d FROM 2 BY 1 UNTIL d * d > n
               DIVIDE n BY d GIVING q REMAINDER rem
               IF rem = 0
                   IF q = d
                       ADD d to ds
                   ELSE
                       ADD d to ds
                       ADD q to ds
                   END-IF
               END-IF
           END-PERFORM.
       END PROGRAM Divsum.
