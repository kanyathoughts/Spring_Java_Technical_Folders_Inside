
   DUMP: PROCEDURE (ADDRESS, LENGTH);
     DCL ADDRESS POINTER;
     DCL LENGTH FIXED BINARY(31);
     DCL OFFSET FIXED BINARY(15);
     DCL 1 OUT CHAR(100);
     DCL 1 OUTR BASED (ADDR(OUT)),
         2 ADR1 CHAR(4),
         2 SPC1 CHAR(3),
         2 HEX1 CHAR(8),
         2 SPC2 CHAR(1),
         2 HEX2 CHAR(8),
         2 SPC3 CHAR(2),
         2 BIN1 CHAR(8),
         2 SPC5 CHAR(1),
         2 BIN2 CHAR(8),
         2 SPC6 CHAR(1),
         2 BIN3 CHAR(8),
         2 SPC7 CHAR(1),
         2 BIN4 CHAR(8),
         2 SPC8 CHAR(3),
         2 BIN5 CHAR(8),
         2 SPC9 CHAR(1),
         2 BIN6 CHAR(8),
         2 SPCA CHAR(1),
         2 BIN7 CHAR(8),
         2 SPCB CHAR(1),
         2 BIN8 CHAR(8);

     DCL 1 H1X,
         2 H1A CHAR(25) INIT('       |------HEX------| '),
         2 H1B CHAR(24) INIT(' |----------------------'),
         2 H1C CHAR(25) INIT('------------BIN----------'),
         2 H1D CHAR(25) INIT('------------------------|');
     DCL 1 H2X,
         2 H2A CHAR(25) INIT('       00----03 04----07 '),
         2 H2B CHAR(24) INIT(' 0------7 8-----15 16---'),
         2 H2C CHAR(25) INIT('-23 24----31   32----39 4'),
         2 H2D CHAR(25) INIT('0----47 48----55 56----63');
     DCL 1 H1 CHAR(99) BASED(ADDR(H1X));
     DCL 1 H2 CHAR(99) BASED(ADDR(H2X));

     PUT SKIP LIST(H1);
     PUT SKIP LIST(H2);

     OFFSET = 0;
     DO WHILE (LENGTH > 0);
       DCL SUBLENGTH FIXED BINARY(31);
       SUBLENGTH = MIN(LENGTH, 8);

       OUT = '';

       /* RELATIVE ADDRESS */

       ADR1 = HEXIMAGE(ADDR(OFFSET), SIZE(OFFSET));
       SPC1 = ':';

       /* DATA IN HEX */

       DCL HEX CHAR(16);
       HEX = HEXIMAGE(ADDRESS + OFFSET, SUBLENGTH);
       HEX1 = SUBSTR(HEX,  1,  8);
       HEX2 = SUBSTR(HEX,  9,  8);

       /* DATA IN BIN */

       DCL BIN CHAR(64);
       BIN = BINIMAGE(ADDRESS + OFFSET, SUBLENGTH);
       BIN1 = SUBSTR(BIN,  1,  8);
       BIN2 = SUBSTR(BIN,  9,  8);
       BIN3 = SUBSTR(BIN, 17,  8);
       BIN4 = SUBSTR(BIN, 25,  8);
       BIN5 = SUBSTR(BIN, 33,  8);
       BIN6 = SUBSTR(BIN, 41,  8);
       BIN7 = SUBSTR(BIN, 49,  8);
       BIN8 = SUBSTR(BIN, 57,  8);

       PUT SKIP LIST(OUT);

       /* PREPARE NEXT ITERATION */
       LENGTH = LENGTH - SUBLENGTH;
       OFFSET = OFFSET + SUBLENGTH;
     END;
   END DUMP;

   BINIMAGE: PROCEDURE (ADDRESS, LENGTH) RETURNS (CHAR(2048) VARYING);
     DCL ADDRESS POINTER;
     DCL LENGTH FIXED BINARY(31);
     DCL BIN_TEMP CHAR(64);
     DCL BIN_TEMP_CHARS(64) CHAR(1) BASED(ADDR(BIN_TEMP));
     DCL BIT_VIEW(64) BIT(1) BASED(ADDRESS);

     DCL SUBLENGTH FIXED BINARY(31);
     DCL BIT_COUNT FIXED BINARY(31);
     SUBLENGTH = MIN(LENGTH, 8);
       BIT_COUNT = SUBLENGTH * 8;

     DCL N FIXED BINARY (31);
     DO N = 1 TO BIT_COUNT;
       IF BIT_VIEW(N) THEN
         BIN_TEMP_CHARS(N) = '1';
       ELSE
         BIN_TEMP_CHARS(N) = '0';
     END;

     DCL RESULT CHAR(2048) VARYING;
     RESULT = SUBSTR(BIN_TEMP, 1, BIT_COUNT);
     RETURN (RESULT);
   END BINIMAGE;
