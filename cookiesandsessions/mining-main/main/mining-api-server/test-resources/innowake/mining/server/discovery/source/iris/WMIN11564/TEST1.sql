SELECT COUNT(*)
     INTO v_cnt
     FROM TCSDBOWNER.DET_SEG DS,
    TCSDBOWNER.emp E           ,
    tcsdbowner.seg_code SC
           WHERE E.EMP_SK = DS.EMP_SK
           AND ds.seg_code_sk = sc.seg_code_sk
          AND TRIM(E.ID) = TRIM(in_crew_i)  
          AND to_date('30-12-1899', 'DD-MM-YYYY') + DS.NOM_DATE = TRUNC(in_a)        
      AND TRIM(sc.code) = 'SHIFT';