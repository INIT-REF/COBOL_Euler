       IDENTIFICATION DIVISION.
       PROGRAM-ID. Euler018.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  n   PIC 9(3)    COMP    VALUE 1.
       01  rw  PIC 9(2)    COMP.
       01  cl  PIC 9(2)    COMP.
       01  out PIC Z(18).
       01  str PIC X(364)          VALUE
           "75 95 64 17 47 82 18 35 87 10 20 04 82 47 65 19 01 23 75 03
      -    "34 88 02 77 73 07 63 67 99 65 04 28 06 16 70 92 41 41 26 56 
      -    "83 40 80 70 33 41 48 72 33 47 32 37 16 94 29 53 71 44 65 25 
      -    "43 91 52 97 51 14 70 11 33 28 77 73 17 78 39 68 17 57 91 71 
      -    "52 38 17 14 91 43 58 50 27 29 48 63 66 04 68 89 53 67 30 73 
      -  "16 69 87 40 31 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23 ".
       01 tri.
           05 tvals PIC 9(4) COMP OCCURS 120 TIMES INDEXED BY i.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 120
               UNSTRING str DELIMITED BY SPACE
               INTO tvals(i) WITH POINTER n
           END-PERFORM

           PERFORM WITH TEST AFTER VARYING rw FROM 13 BY -1 UNTIL rw = 0
               COMPUTE i = 1 + rw * (rw + 1) / 2

               PERFORM VARYING cl FROM 0 BY 1 UNTIL cl > rw
                   COMPUTE tvals(i) = tvals(i) +
                    FUNCTION MAX(tvals(i + rw + 1), tvals(i + rw + 2))
                   
                   SET i UP BY 1
               END-PERFORM
           END-PERFORM
           
           MOVE tvals(1) to out
           DISPLAY FUNCTION TRIM(out)
           STOP RUN.
