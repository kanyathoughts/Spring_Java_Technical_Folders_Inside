       IDENTIFICATION DIVISION.
       PROGRAM-ID. WCFD459B.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  HOST-VARS.
           10 ALPHA-SHORT PIC X(8).
     
       EXEC SQL
          INCLUDE EMPREC
       END-EXEC.
       
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       BEGIN.
      
      * some DECLARE CURSOR test
           EXEC SQL DECLARE CSR_GRIDRELS CURSOR FOR 
      * Q1
      SELECT 'BI' ,
             G1.GRID_ID ,
             G1.GRID_TYP ,
             GX.CNTR_NUM ,
             P1.CNTR_NUM ,
             P2.CNTR_PFX_ID
                    ||SUBSTR(P1.CNTR_NUM,2,5) ,
             P1.BG_DT ,
             P1.MKTR_FOCAL_SUBJ_ID ,
             P4.PRUORG_ID ,
             'B' ,
             '0001-01-01' ,
             ' ' ,
             GX.START_DT
      FROM   UTT_GEO_GRID G1 ,
             UTT_GRID_RELS GX ,
             UTT_OA P1 ,
             UTT_OA_DTL P2 ,
             UTT_PRU_ORG_OA P3 ,
             UTT_PRU_ORG P4 ,
             UTT_PERS P5
      WHERE  G1.GRID_ID = GX.GRID_ID
      AND    :WS-CURRENT-DATE-1 BETWEEN GX.START_DT 
      AND    GX.END_DT
      AND    GX.CNTR_NUM = P1.CNTR_NUM
      AND    ((
                           P1.FINAL_CD = 'A00'
                    AND    :WS-CURRENT-DATE-1 >= P1.BG_DT )
             OR     (
                           :WS-CURRENT-DATE-1 BETWEEN P1.BG_DT 
                           AND    P1.EN_DT ))
      AND    P5.SUBJ_ID = P1.MKTR_FOCAL_SUBJ_ID
      AND    P1.CNTR_NUM = P2.OA_CNTR_NUM
      AND    P2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  P2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    P2.CNTR_PFX_ID <> 'A'
      AND    P2.OA_CNTR_NUM = P3.OA_CNTR_NUM
      AND    P3.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_PRU_ORG_OA
                    WHERE  P3.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_LOC_IND IS NULL
                           OR     PRIR_LOC_IND = ' '))
      AND    P3.SUBJ_ID = P4.SUBJ_ID 
      * GET ACTIVE GRIDS CODED TO ACTIVE BROKER FIRMS & ACTIVE SOL.
      UNION 
      * Q2
      SELECT 'BS' ,
             G1.GRID_ID ,
             G1.GRID_TYP ,
             BX.PARNT_CNTR_NUM ,
             P1.CNTR_NUM ,
             P2.CNTR_PFX_ID
                    ||SUBSTR(P1.CNTR_NUM,2,5) ,
             P1.BG_DT ,
             P1.MKTR_FOCAL_SUBJ_ID ,
             P4.PRUORG_ID ,
             'S' ,
             '0001-01-01' ,
             ' ' ,
             GX.START_DT
      FROM   UTT_GEO_GRID G1 ,
             UTT_GRID_RELS GX ,
             UTT_OA B1 ,
             UTT_OA_DTL B2 ,
             UTT_NON_PRU_ORG B5 ,
             UTT_OA_REL BX ,
             UTT_OA P1 ,
             UTT_OA_DTL P2 ,
             UTT_PRU_ORG_OA P3 ,
             UTT_PRU_ORG P4 ,
             UTT_PERS P5
      WHERE  G1.GRID_ID = GX.GRID_ID
      AND    :WS-CURRENT-DATE-1 BETWEEN GX.START_DT 
      AND    GX.END_DT
      AND    GX.CNTR_NUM = B1.CNTR_NUM
      AND    ((
                           B1.FINAL_CD = 'A00'
                    AND    :WS-CURRENT-DATE-1 >= B1.BG_DT )
             OR     (
                           :WS-CURRENT-DATE-1 BETWEEN B1.BG_DT 
                           AND    B1.EN_DT ))
      AND    B1.MKTR_FOCAL_SUBJ_ID = B5.SUBJ_ID
      AND    B1.CNTR_NUM = B2.OA_CNTR_NUM
      AND    B2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  B2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    B2.CNTR_PFX_ID <> 'A'
      AND    B1.CNTR_NUM = BX.PARNT_CNTR_NUM
      AND    ((
                           BX.REL_FINAL_CD = 'A'
                    AND    :WS-CURRENT-DATE-1 >= BX.REL_BG_DT )
             OR     (
                           :WS-CURRENT-DATE-1 BETWEEN BX.REL_BG_DT 
                           AND    BX.REL_EN_DT))
      AND    BX.REL_TYP_CD = 'S'
      AND    BX.CHLD_CNTR_NUM = P1.CNTR_NUM
      AND    ((
                           P1.FINAL_CD = 'A00'
                    AND    :WS-CURRENT-DATE-1 >= P1.BG_DT )
             OR     (
                           :WS-CURRENT-DATE-1 BETWEEN P1.BG_DT 
                           AND    P1.EN_DT ))
      AND    P1.CNTR_NUM = P2.OA_CNTR_NUM
      AND    P2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  P2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    P2.CNTR_TYP_CD = 'S'
      AND    P2.OA_CNTR_NUM = P3.OA_CNTR_NUM
      AND    P3.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_PRU_ORG_OA
                    WHERE  P3.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_LOC_IND IS NULL
                           OR     PRIR_LOC_IND = ' '))
      AND    P3.SUBJ_ID = P4.SUBJ_ID
      AND    P5.SUBJ_ID = P1.MKTR_FOCAL_SUBJ_ID 
      * -- GET GRIDS CODED AT GA LEVEL AND RETRIEVE - BROKER INDIVID
      * WHO DO NOT HAVE A GRID AT THE BROKER INDIV LEVEL
      UNION
      * Q3
      SELECT 'GI' ,
             G1.GRID_ID ,
             G1.GRID_TYP ,
             GX.CNTR_NUM ,
             P1.CNTR_NUM ,
             P2.CNTR_PFX_ID
                    ||SUBSTR(P1.CNTR_NUM,2,5) ,
             P1.BG_DT ,
             P1.MKTR_FOCAL_SUBJ_ID ,
             P4.PRUORG_ID ,
             'B' ,
             AX.REL_BG_DT ,
             AX.PARNT_CNTR_NUM ,
             GX.START_DT
      FROM   UTT_GEO_GRID G1 ,
             UTT_GRID_RELS GX ,
             UTT_OA P1 ,
             UTT_OA_DTL P2 ,
             UTT_PRU_ORG_OA P3 ,
             UTT_PRU_ORG P4 ,
             UTT_PERS P5 ,
             UTT_OA_REL AX
      WHERE  G1.GRID_ID = GX.GRID_ID
      AND    :WS-CURRENT-DATE-1 BETWEEN GX.START_DT AND GX.END_DT
      AND    GX.CNTR_NUM = AX.PARNT_CNTR_NUM
      AND    AX.REL_TYP_CD = 'B'
      AND    ((
                           AX.REL_FINAL_CD = 'A'
                    AND    :WS-CURRENT-DATE-1 >= AX.REL_BG_DT)
             OR     (
             :WS-CURRENT-DATE-1 BETWEEN AX.REL_BG_DT AND
                               AX.REL_EN_DT))
      AND    AX.CHLD_CNTR_NUM = P1.CNTR_NUM
      AND    ((
                           P1.FINAL_CD = 'A00'
                    AND    :WS-CURRENT-DATE-1 >= P1.BG_DT )
             OR     (
            :WS-CURRENT-DATE-1 BETWEEN P1.BG_DT 
                           AND P1.EN_DT ))
      AND    0 =
             (
                    SELECT COUNT(*)
                    FROM   UTT_GRID_RELS
                    WHERE  P1.CNTR_NUM = CNTR_NUM
               AND    :WS-CURRENT-DATE-1 BETWEEN START_DT AND
                        END_DT)
      AND    P1.CNTR_NUM = P2.OA_CNTR_NUM
      AND    P2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  P2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    P2.CNTR_TYP_CD <> 'S'
      AND    P2.OA_CNTR_NUM = P3.OA_CNTR_NUM
      AND    P3.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_PRU_ORG_OA
                    WHERE  P3.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_LOC_IND IS NULL
                           OR     PRIR_LOC_IND = ' '))
      AND    P3.SUBJ_ID = P4.SUBJ_ID
      AND    P5.SUBJ_ID = P1.MKTR_FOCAL_SUBJ_ID 
      * GET GRIDS CODED AT GA LEVEL AND RETRIEVE - SOLICITORS 
      * WHOSE BROKER FIRM DOES NOT HAVE A GRID RELATIONSHIP
      UNION
      SELECT 'GS' ,
             G1.GRID_ID ,
             G1.GRID_TYP ,
             BX.PARNT_CNTR_NUM ,
             P1.CNTR_NUM ,
             P2.CNTR_PFX_ID
                    ||SUBSTR(P1.CNTR_NUM,2,5) ,
             P1.BG_DT ,
             P1.MKTR_FOCAL_SUBJ_ID ,
             P4.PRUORG_ID ,
             'S' ,
             AX.REL_BG_DT ,
             AX.PARNT_CNTR_NUM ,
             GX.START_DT
      FROM   UTT_GEO_GRID G1 ,
             UTT_GRID_RELS GX ,
             UTT_OA_REL AX ,
             UTT_OA A1 ,
             UTT_OA B1 ,
             UTT_OA_DTL B2 ,
             UTT_NON_PRU_ORG B5 ,
             UTT_OA_REL BX ,
             UTT_OA P1 ,
             UTT_OA_DTL P2 ,
             UTT_PRU_ORG_OA P3 ,
             UTT_PRU_ORG P4 ,
             UTT_PERS P5
      WHERE  G1.GRID_ID = GX.GRID_ID
      AND    :WS-CURRENT-DATE-1 BETWEEN GX.START_DT AND GX.END_DT
      AND    GX.CNTR_NUM = AX.PARNT_CNTR_NUM
      AND    AX.REL_TYP_CD = 'B'
      AND    ((
                           AX.REL_FINAL_CD = 'A'
                    AND    :WS-CURRENT-DATE-1 >= AX.REL_BG_DT )
             OR     (
             :WS-CURRENT-DATE-1 BETWEEN AX.REL_BG_DT AND
                               AX.REL_EN_DT))
      AND    AX.PARNT_CNTR_NUM = A1.CNTR_NUM
      AND    ((
                           A1.FINAL_CD = 'A00'
             AND    :WS-CURRENT-DATE-1 >= A1.BG_DT)
             OR     (
             :WS-CURRENT-DATE-1 BETWEEN A1.BG_DT AND
                               A1.EN_DT ))
      AND    AX.CHLD_CNTR_NUM = B1.CNTR_NUM
      AND    ((
                           B1.FINAL_CD = 'A00'
                    AND    :WS-CURRENT-DATE-1 >= B1.BG_DT )
             OR     (
             :WS-CURRENT-DATE-1 BETWEEN B1.BG_DT AND
                               B1.EN_DT ))
      AND    B5.SUBJ_ID = B1.MKTR_FOCAL_SUBJ_ID
      AND    0 =
             (
                    SELECT COUNT(*)
                    FROM   UTT_GRID_RELS
                    WHERE  B1.CNTR_NUM = CNTR_NUM
             AND    :WS-CURRENT-DATE-1 BETWEEN START_DT AND
                        END_DT)
      AND    B1.CNTR_NUM = B2.OA_CNTR_NUM
      AND    B2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  B2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    B2.CNTR_PFX_ID <> 'A'
      AND    B1.CNTR_NUM = BX.PARNT_CNTR_NUM
      AND    BX.REL_TYP_CD = 'S'
      AND    ((
                           BX.REL_FINAL_CD = 'A'
             AND    :WS-CURRENT-DATE-1 >= BX.REL_BG_DT )
             OR     (
             :WS-CURRENT-DATE-1 BETWEEN BX.REL_BG_DT 
                           AND    BX.REL_EN_DT))
      AND    BX.CHLD_CNTR_NUM = P1.CNTR_NUM
      AND    ((
                           P1.FINAL_CD = 'A00'
                    AND    :WS-CURRENT-DATE-1 >= P1.BG_DT )
             OR     (
             :WS-CURRENT-DATE-1 BETWEEN P1.BG_DT
                            AND P1.EN_DT ))
      AND    P1.CNTR_NUM = P2.OA_CNTR_NUM
      AND    P2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  P2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    P2.CNTR_TYP_CD = 'S'
      AND    P2.OA_CNTR_NUM = P3.OA_CNTR_NUM
      AND    P3.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_PRU_ORG_OA
                    WHERE  P3.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_LOC_IND IS NULL
                           OR     PRIR_LOC_IND = ' '))
      AND    P3.SUBJ_ID = P4.SUBJ_ID
      AND    P5.SUBJ_ID = P1.MKTR_FOCAL_SUBJ_ID 
      * GET GRIDS CODED AT GA LEVEL AND RETRIEVE                           
      * THE RESPONSIBLE PARTY - SOLICITORS
      UNION 
      * Q5
      SELECT 'GR' ,
             G1.GRID_ID ,
             G1.GRID_TYP ,
             GX.CNTR_NUM ,
             P1.CNTR_NUM ,
             P2.CNTR_PFX_ID
                    ||SUBSTR(P1.CNTR_NUM,2,5) ,
             P1.BG_DT ,
             P1.MKTR_FOCAL_SUBJ_ID ,
             P4.PRUORG_ID ,
             'S' ,
             AX.REL_BG_DT ,
             AX.PARNT_CNTR_NUM ,
             GX.START_DT
      FROM   UTT_GEO_GRID G1 ,
             UTT_GRID_RELS GX ,
             UTT_OA P1 ,
             UTT_OA_DTL P2 ,
             UTT_PRU_ORG_OA P3 ,
             UTT_PRU_ORG P4 ,
             UTT_PERS P5 ,
             UTT_OA_REL AX ,
             UTT_OA A1 ,
             UTT_OA_DTL A2
      WHERE  G1.GRID_ID = GX.GRID_ID
      AND    :WS-CURRENT-DATE-1 BETWEEN GX.START_DT AND GX.END_DT
      AND    GX.CNTR_NUM = AX.PARNT_CNTR_NUM
      AND    AX.REL_TYP_CD = 'S'
      AND    ((
                           AX.REL_FINAL_CD = 'A'
                    AND    :WS-CURRENT-DATE-1 >= AX.REL_BG_DT )
             OR     (
             :WS-CURRENT-DATE-1 BETWEEN AX.REL_BG_DT 
             AND    AX.REL_EN_DT))
      AND    AX.PARNT_CNTR_NUM = A1.CNTR_NUM
      AND    ((
                           A1.FINAL_CD = 'A00'
                    AND    :WS-CURRENT-DATE-1 >= A1.BG_DT )
             OR     (
             :WS-CURRENT-DATE-1 BETWEEN A1.BG_DT AND
                               A1.EN_DT ))
      AND    A1.CNTR_NUM = A2.OA_CNTR_NUM
      AND    A2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  A2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    A2.CNTR_PFX_ID = 'A'
      AND    AX.CHLD_CNTR_NUM = P1.CNTR_NUM
      AND    ((
                           P1.FINAL_CD = 'A00'
                    AND    :WS-CURRENT-DATE-1 >= P1.BG_DT )
             OR     (
                           :WS-CURRENT-DATE-1 BETWEEN P1.BG_DT
                             AND   P1.EN_DT ))
      AND    P1.CNTR_NUM = P2.OA_CNTR_NUM
      AND    P2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  P2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    P2.CNTR_TYP_CD = 'S'
      AND    P2.OA_CNTR_NUM = P3.OA_CNTR_NUM
      AND    P3.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_PRU_ORG_OA
                    WHERE  P3.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_LOC_IND IS NULL
                           OR     PRIR_LOC_IND = ' '))
      AND    P3.SUBJ_ID = P4.SUBJ_ID
      AND    P5.SUBJ_ID = P1.MKTR_FOCAL_SUBJ_ID 
      * PROTECTED RELS AT BROKER INDIV LEVEL 
      * WHO DO NOT HAVE A GRID AT THE GA LEVEL
      UNION 
      * Q6
      SELECT 'PR' ,
             G1.GRID_ID ,
             'PROT' ,
             P1.CNTR_NUM ,
             P1.CNTR_NUM ,
             P2.CNTR_PFX_ID
                    ||SUBSTR(P1.CNTR_NUM,2,5) ,
             P1.BG_DT ,
             P1.MKTR_FOCAL_SUBJ_ID ,
             P4.PRUORG_ID ,
             'B' ,
             '0001-01-01' ,
             ' ' ,
             PX.START_DT
      FROM   UTT_GEO_GRID G1 ,
             UTT_PROD_RELS PX ,
             UTT_OA P1 ,
             UTT_OA_DTL P2 ,
             UTT_PRU_ORG_OA P3 ,
             UTT_PRU_ORG P4 ,
             UTT_PERS P5
      WHERE  PX.CHILD_CNTR_NUM = P1.CNTR_NUM
      AND    PX.PROT_REL_IND = 'Y'
      AND    :WS-CURRENT-DATE-1 BETWEEN PX.START_DT AND
          PX.END_DT
      AND    PX.GRID_ID = G1.GRID_ID
      AND    :WS-CURRENT-DATE-1 BETWEEN G1.START_DT AND
          G1.END_DT
      AND    P1.CNTR_NUM = P2.OA_CNTR_NUM
      AND    P2.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_OA_DTL
                    WHERE  P2.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_CONTR_IND IS NULL
                           OR     PRIR_CONTR_IND = ' '))
      AND    P2.CNTR_TYP_CD <> 'S'
      AND    P2.OA_CNTR_NUM = P3.OA_CNTR_NUM
      AND    P3.CRT_TS =
             (
                    SELECT MAX(CRT_TS)
                    FROM   UTT_PRU_ORG_OA
                    WHERE  P3.OA_CNTR_NUM = OA_CNTR_NUM
                    AND    (
                                  PRIR_LOC_IND IS NULL
                           OR     PRIR_LOC_IND = ' '))
      AND    P3.SUBJ_ID = P4.SUBJ_ID
      AND    P5.SUBJ_ID = P1.MKTR_FOCAL_SUBJ_ID
      AND    0 =
             (
                    SELECT COUNT(*)
                    FROM   UTT_OA_REL X,
                           UTT_GRID_RELS Y
                    WHERE  PX.CHILD_CNTR_NUM = X.CHLD_CNTR_NUM
                    AND    X.REL_TYP_CD = 'B'
                    AND    ((
                    :WS-CURRENT-DATE-1 
                      BETWEEN X.REL_BG_DT AND    X.REL_EN_DT)
                           OR     (
                   :WS-CURRENT-DATE-1 >= X.REL_BG_DT
                                  AND    X.REL_FINAL_CD = 'A'))
                    AND    X.PARNT_CNTR_NUM = Y.CNTR_NUM
                    AND    :WS-CURRENT-DATE-1 BETWEEN Y.START_DT
                    AND    Y.END_DT
                    AND    Y.GRID_ID = PX.GRID_ID )
       END-EXEC
           
           EXEC SQL
            OPEN CSR_GRIDRELS
         END-EXEC
         
           GOBACK.

       END PROGRAM WCFD68A.

