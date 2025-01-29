
   HEXDUMP32: PROCEDURE (ADDRESS, LENGTH);
     DCL ADDRESS POINTER;
     DCL LENGTH FIXED BINARY(31);
     DCL OFFSET FIXED BINARY(15);
     
     DCL 1 OUT CHAR(82);
     DCL 1 OUTR BASED (ADDR(OUT)),
         2 ADR1 CHAR(4),
         2 SPC1 CHAR(3),

         2 HEX1 CHAR(8),
         2 SPC2 CHAR(1),
         2 HEX2 CHAR(8),
         2 SPC3 CHAR(2),
         2 HEX3 CHAR(8),
         2 SPC4 CHAR(1),
         2 HEX4 CHAR(8),
         
         2 SPC5 CHAR(3),
         
         2 HEX5 CHAR(8),
         2 SPC6 CHAR(1),
         2 HEX6 CHAR(8),
         2 SPC7 CHAR(2),
         2 HEX7 CHAR(8),
         2 SPC8 CHAR(1),
         2 HEX8 CHAR(8);

     DCL 1 H1X,
         2 H1A CHAR(25) INIT('       00----03 04----07 '),
         2 H1B CHAR(21) INIT(' 08----11 12----15   '),
         2 H1C CHAR(27) INIT('16----19 20----23  24----27'),
         2 H1D CHAR(9) INIT(' 28----31');
     DCL 1 H1 CHAR(82) BASED(ADDR(H1X));

     PUT SKIP LIST(H1);

     OFFSET = 0;
     DO WHILE (LENGTH > 0);
       DCL SUBLENGTH FIXED BINARY(31);
       SUBLENGTH = MIN(LENGTH, 32);

       OUT = '';

       /* RELATIVE ADDRESS */

       ADR1 = HEXIMAGE(ADDR(OFFSET), SIZE(OFFSET));
       SPC1 = ':';

       /* DATA IN HEX */

       DCL HEX CHAR(64);
       HEX = HEXIMAGE(ADDRESS + OFFSET, SUBLENGTH);
       HEX1 = SUBSTR(HEX,  1, 8);
       HEX2 = SUBSTR(HEX,  9, 8);
       HEX3 = SUBSTR(HEX, 17, 8);
       HEX4 = SUBSTR(HEX, 25, 8);
       HEX5 = SUBSTR(HEX, 33, 8);
       HEX6 = SUBSTR(HEX, 41, 8);
       HEX7 = SUBSTR(HEX, 49, 8);
       HEX8 = SUBSTR(HEX, 57, 8);

       PUT SKIP LIST(OUT);

       /* PREPARE NEXT ITERATION */
       LENGTH = LENGTH - SUBLENGTH;
       OFFSET = OFFSET + SUBLENGTH;
     END;
   END HEXDUMP32;