       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler001.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(3)    COMP VALUE 1.
       01  rw  PIC 9(2)    COMP.
       01  cl  PIC 9(2)    COMP.
       01  sub PIC 9(13)   COMP.
       01  out PIC Z(18).
       01  str PIC X(35)    VALUE "31 28 31 30 31 30 31 31 30 31 30 31".
       01  dtb.
           05 dim PIC 99 OCCURS 12 TIMES INDEXED BY i.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 12
               UNSTRING str DELIMITED BY " " INTO dim(i) WITH POINTER n
           END-PERFORM

           MOVE dim(11) TO out
           DISPLAY FUNCTION TRIM(out)
           STOP RUN.
