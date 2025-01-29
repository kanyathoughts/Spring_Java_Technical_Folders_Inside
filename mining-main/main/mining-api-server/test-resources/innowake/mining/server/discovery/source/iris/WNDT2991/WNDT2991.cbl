       IDENTIFICATION DIVISION.
       PROGRAM-ID. WNDT2991.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       BEGIN.
          EXEC SQL
            select f.title from film f where film_id = 
            (select max(film_id) as max  from film)
          END-EXEC.
          EXEC SQL
            select f.title from film f where film_id = 
            (select min(film_id) as min  from film)
          END-EXEC.
       GOBACK.
