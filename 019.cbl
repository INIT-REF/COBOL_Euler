       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler019.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(3)    COMP    VALUE 1.
       01  d   PIC 9(2)    COMP.
       01  m   PIC 9(2)    COMP.
       01  y   PIC 9(4)    COMP.
       01  dc  PIC 9(5)    COMP    VALUE 2.
       01  res PIC 9(3)    COMP    VALUE 0.
       01  out PIC Z(18).
       01  str PIC X(35)           VALUE
           "31 28 31 30 31 30 31 31 30 31 30 31".
       01  dtb.
           05 dim PIC 99 OCCURS 12 TIMES INDEXED BY i.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 12
               UNSTRING str DELIMITED BY " " INTO dim(i) WITH POINTER n
           END-PERFORM

           PERFORM VARYING y FROM 1901 BY 1 UNTIL y > 2000
               MOVE 28 TO dim(2)

               IF FUNCTION MOD(y, 4) = 0
                   MOVE 29 TO dim(2)
               END-IF

               PERFORM VARYING m FROM 1 BY 1 UNTIL m > 12
                   PERFORM VARYING d FROM 1 BY 1 UNTIL d > dim(m)
                       ADD 1 TO dc

                       IF d = 1 AND FUNCTION MOD(dc, 7) = 0
                           ADD 1 TO res
                   END-PERFORM
               END-PERFORM
           END-PERFORM

           MOVE res TO out
           DISPLAY FUNCTION TRIM(out)
           STOP RUN.
