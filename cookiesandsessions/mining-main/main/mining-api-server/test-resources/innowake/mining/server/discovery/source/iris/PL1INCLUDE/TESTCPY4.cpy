 EXTPRC1: PROC(PARAM1,PARAM2);
   DCL PARAM1 CHAR(10);
   DCL PARAM2 FIXED BINARY(31);
   
   PUT SKIP LIST('IN EXT PROC');
   PUT SKIP LIST(PARAM1);
   PUT SKIP LIST(PARAM2);
   PARAM1 = 'MODIFIED';
 END EXTPRC1;
 
  EXTPRC2: PROC(PARAM1,PARAM2);
 DCL  M12428I    FILE RECORD INPUT ENV(VB RECSIZE(104));
 DCL  M12428O    FILE RECORD OUTPUT ENV(VB RECSIZE(104));
   DCL PARAM1 CHAR(10);
   DCL PARAM2 FIXED BINARY(31);
   
   PUT SKIP LIST('IN EXT PROC');
   PUT SKIP LIST(PARAM1);
   PUT SKIP LIST(PARAM2);
   PARAM1 = 'MODIFIED';
 END EXTPRC2;
 
  EXTPRC3: PROC(PARAM1,PARAM2);
   DCL PARAM1 CHAR(10);
   DCL PARAM2 FIXED BINARY(31);
   
   PUT SKIP LIST('IN EXT PROC');
   PUT SKIP LIST(PARAM1);
   PUT SKIP LIST(PARAM2);
   PARAM1 = 'MODIFIED';
 END EXTPRC3;