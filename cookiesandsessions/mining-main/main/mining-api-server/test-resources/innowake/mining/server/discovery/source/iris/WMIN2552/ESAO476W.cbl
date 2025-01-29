       IDENTIFICATION DIVISION.                                                 
201012 PROGRAM-ID.    ESAO476W IS INITIAL PROGRAM.                              
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
      *SOURCE-COMPUTER. IBM-4381 WITH DEBUGGING MODE.                           
       SOURCE-COMPUTER. IBM-4381.                                               
       DATA DIVISION.                                                           
      *-----------------------------------------------------------------        
       WORKING-STORAGE SECTION.                                                 
      *-----------------------------------------------------------------        
      *  DECLARE ACCOUNT LEVEL DATA CURSOR   -  RESULT SET #1                   
      *-----------------------------------------------------------------        
                                                                                
           EXEC SQL
                                                             
             DECLARE ACCT_CURSOR CURSOR FOR                          
               WITH TEMP1 AS (                                                   
                SELECT                                                          
RMAR13            SAGB.PO_ID                                                    
RMAR13           ,SAGB.SAG_ID                                                   
                 ,ASSET.ACCT_ID                                                 
RMA216           ,IFNULL(SUM(HLDGS.HLDG_VAL_AM), 0)                             
RMA216                                          AS ACCOUNT_BAL                   
                 ,ASSET.SUB_TYP                                                 
                 ,ASSET.VISTA_PLN_NM                                            
                 ,ASSET.ASSET_CASH_FLW_FL                                       
                 ,HLDGS.OUTINV_FNCLINST_ID                                      
                 ,ASSET.OH_ACCT_TYP_CD                                          
                 ,ASSET.ASSET_ID                                                
                 ,ASSET.PROFL_ACCT_SEQ_NO                                       
                 ,HLDGS.VISTA_PLN_ID                                            
                 ,IFNULL(ASSET.OH_TOT_TYP_CD,' ')                               
                                               AS OH_TOT_TYP_CD                 
                 ,PROFL.ADVC_PROFL_ID                                           
                 ,ASSET.OPTIMIZED_FL                                            
                 ,IFNULL(ASSET.PROJD_ANNL_CNTR_AM, 0)                           
                                     AS PROJD_ANNL_CNTR_AM                      
                 ,ASSET.TO_BE_MNGD_FL                                           
RMA4RE           ,HLDGS.VISTA_ACCT_ID                                           
                 ,HLDGS.OTH_FNCLINST_NM                                         
IIG4R8           ,COALESCE(ASSET.INST_SAG_ID, 0) AS INST_SAG_ID                 
                                                                                
RMAR13          FROM  VSAG_BUS_RLSHP           SAGB                             
                INNER JOIN VADVC_PROFL         PROFL                            
                        ON PROFL.SAG_ID         = SAGB.SAG_ID                   
                       AND PROFL.EFFTV_END_DT   > CURRENT DATE                  
                INNER JOIN VADVC_ASSET_PROFL   ASSET                            
                        ON ASSET.SAG_ID         = PROFL.SAG_ID                  
                       AND ASSET.EFFTV_END_DT   > CURRENT DATE                  
RMA216           LEFT JOIN VADVC_AST_SEL_POSN  HLDGS                            
                        ON HLDGS.SAG_ID    = ASSET.SAG_ID                       
                       AND HLDGS.ASSET_ID  = ASSET.ASSET_ID                     
                       AND HLDGS.EFFTV_END_DT   > CURRENT DATE                  
RMAR16                 AND (ASSET.SUB_TYP NOT IN ('OHLD','LMPO')                
RMAR16                  OR (ASSET.SUB_TYP     IN ('OHLD','LMPO')                
                       AND  HLDGS.SRT_ORD_SEQ_NO = 1))                          
RMAR13          WHERE SAGB.PO_ID           = :WS-PO-ID                          
WEBEXP            AND SAGB.PO_ROLE_CD      IN ('OW','OWNS')                     
RMAR13            AND SAGB.EFFTV_END_DT    > CURRENT_DATE                       
RMAR13          GROUP BY SAGB.PO_ID                                             
RMAR13                  ,SAGB.SAG_ID                                            
                        ,ASSET.ACCT_ID                                          
                        ,ASSET.SUB_TYP                                          
                        ,ASSET.VISTA_PLN_NM                                     
                        ,ASSET.ASSET_CASH_FLW_FL                                
                        ,HLDGS.OUTINV_FNCLINST_ID                               
                        ,ASSET.OH_ACCT_TYP_CD                                   
                        ,ASSET.ASSET_ID                                         
                        ,ASSET.PROFL_ACCT_SEQ_NO                                
                        ,HLDGS.VISTA_PLN_ID                                     
                        ,ASSET.OH_TOT_TYP_CD                                    
                        ,PROFL.ADVC_PROFL_ID                                    
                        ,ASSET.PROJD_ANNL_CNTR_AM                               
                        ,ASSET.OPTIMIZED_FL                                     
                        ,ASSET.TO_BE_MNGD_FL                                    
RMA4RE                  ,HLDGS.VISTA_ACCT_ID                                    
                        ,HLDGS.OTH_FNCLINST_NM                                  
IIG4R8                  ,ASSET.INST_SAG_ID                                      
                            )                                                   
RMAR13        SELECT TEMP1.PO_ID                                                
                    ,TEMP1.SAG_ID                                               
                    ,COALESCE(GARL.GOAL_ID, 0) AS GOAL_ID                       
                    ,TEMP1.ACCT_ID                                              
                    ,TEMP1.ACCOUNT_BAL                                          
                    ,CASE                                                       
                       WHEN TEMP1.OH_TOT_TYP_CD IN ('OWNR','SPSE',' ')          
RAGG                    AND TEMP1.SUB_TYP IN ('OHLD','LMPO','AGGR')             
                        THEN IFNULL(RTRIM(OHNM.AVT_ABBRV_NM),'--')              
                               || ' - ' ||                                      
                           IFNULL(RTRIM(ACTYP.AVT_NM),'--')                     
                               || ' - ' ||                                      
                           IFNULL(RTRIM(PRFMBR.MBR_FRST_NM),'--')               
                       WHEN TEMP1.OH_TOT_TYP_CD = 'JOIN'                        
RAGG                    AND TEMP1.SUB_TYP IN ('OHLD','LMPO','AGGR')             
                          THEN IFNULL(RTRIM(OHNM.AVT_ABBRV_NM),'--')            
                               || ' - ' ||                                      
                           IFNULL(RTRIM(ACTYP.AVT_NM),'--')                     
                               || ' - Joint'                                    
                       ELSE TEMP1.VISTA_PLN_NM                                  
                       END AS ASSET_NM                                          
                    ,TEMP1.ASSET_CASH_FLW_FL                                    
RMAR15* Existence of row in VADVC_SAG_ACCT_RLS is sufficient to                 
RMAR15* state that account is "MANAGED'/ Mock-Manage                            
RMAR15* Existence of row in VADVC_INST_ACCT_RL is sufficient to                 
RMAR15* state that IIG account is "MANAGED'/ Mock-Manage                        
                    ,CASE                                                       
                       WHEN SAGACCT.ACCT_ID IS NOT NULL                         
                          THEN 'Y'                                              
IIG4R8                 WHEN INSTACT.INST_SAG_ID IS NOT NULL                     
IIG4R8                    THEN 'Y'                                              
                       ELSE 'N'                                                 
                     END AS ISMNGD_IND                                          
RMAR15              ,TEMP1.SUB_TYP                                              
                    ,TEMP1.OPTIMIZED_FL                                         
                    ,TEMP1.PROFL_ACCT_SEQ_NO                                    
                    ,TEMP1.VISTA_PLN_ID                                         
                    ,TEMP1.OH_ACCT_TYP_CD                                       
                    ,TEMP1.OH_TOT_TYP_CD        AS MBR_TYP_CD                   
                    ,TEMP1.ASSET_ID                                             
                    ,TEMP1.PROJD_ANNL_CNTR_AM                                   
                    ,CASE                                                       
RAGG                   WHEN TEMP1.SUB_TYP IN ('OHLD','LMPO','AGGR')             
                          THEN TEMP1.VISTA_PLN_NM                               
                       ELSE ' '                                                 
                       END AS OH_FORMATTED_ACCT_NM                              
                    ,TEMP1.TO_BE_MNGD_FL                                        
                    ,TEMP1.OUTINV_FNCLINST_ID                                   
RMA4RE              ,TEMP1.VISTA_ACCT_ID                                        
                    ,TEMP1.OTH_FNCLINST_NM                                      
IIG4R8              ,TEMP1.INST_SAG_ID                                          
              FROM TEMP1                                                        
                LEFT  JOIN VADVC_GOAL_AST_RL       GARL                         
                       ON GARL.SAG_ID           = TEMP1.SAG_ID                  
                      AND GARL.ASSET_ID         = TEMP1.ASSET_ID                
                      AND GARL.ADVC_PROFL_ID    = TEMP1.ADVC_PROFL_ID           
                      AND GARL.EFFTV_END_DT > CURRENT DATE                      
                                                                                
                LEFT  JOIN VADVC_SAG_ACCT_RLS  SAGACCT                          
                        ON SAGACCT.SAG_ID       = TEMP1.SAG_ID                  
                       AND SAGACCT.ACCT_ID      = TEMP1.ACCT_ID                 
                       AND SAGACCT.EFFTV_END_DT > CURRENT DATE                  
                                                                                
IIG4R8          LEFT  JOIN VADVC_INST_ACCT_RL  INSTACT                          
IIG4R8                  ON INSTACT.SAG_ID       = TEMP1.SAG_ID                  
IIG4R8                 AND INSTACT.INST_SAG_ID  = TEMP1.INST_SAG_ID             
IIG4R8                 AND INSTACT.EFFTV_END_DT > CURRENT DATE                  
IIG4R8                                                                          
                LEFT  JOIN VOUT_INV_FNCL_INST  OHNM                             
                        ON OHNM.OUTINV_FNCLINST_ID =                            
                           TEMP1.OUTINV_FNCLINST_ID                             
                LEFT  JOIN VAVT_OH_ACCT_TYP    ACTYP                            
                        ON ACTYP.OH_ACCT_TYP_CD = TEMP1.OH_ACCT_TYP_CD          
                LEFT  JOIN VADVC_CLNT_MBR_INF  PRFMBR                           
WEBEXP                  ON PRFMBR.VGI_CLNT_ID   = :WS-PO-ID-OW                  
                       AND PRFMBR.MBR_TYP_CD    = TEMP1.OH_TOT_TYP_CD           
                       AND PRFMBR.EFFTV_END_DT  > CURRENT DATE                  
WEBEXP         ORDER BY GOAL_ID                                                 
                       ,PROFL_ACCT_SEQ_NO                                       
            FOR READ ONLY WITH UR                                               
           END-EXEC.                                                            
                                                                                
      *-----------------------------------------------------------------        
      *  DECLARE LEGACY BRKG SWEEP ACCT CURSOR - RESULT SET #8                  
      *-----------------------------------------------------------------        
           EXEC SQL                                                             
            DECLARE SWEEP_ACCT_CURSOR CURSOR FOR                    
              WITH TEMP1 AS (                                                   
                SELECT                                                          
RMAR13            SAGB.SAG_ID                                                   
                 ,ASSET.ACCT_ID                                                 
                 ,ASSET.ASSET_CASH_FLW_FL                                       
                 ,ASSET.ASSET_ID                                                
                 ,PROFL.ADVC_PROFL_ID                                           
                                                                                
RMAR15        FROM       VSAG                SAG                                
RMAR15        INNER JOIN VSAG_BUS_RLSHP      SAGB                               
RMAR15                ON SAGB.PO_ID        = :WS-PO-ID                          
RMAR15*              AND SAGB.PO_ROLE_CD   = 'OW'                               
WEBEXP               AND SAGB.PO_ROLE_CD  IN ('OW','OWNS')                      
RMAR15               AND SAGB.SAG_ID          = SAG.SAG_ID                      
RMAR15*              AND SAGB.EFFTV_BGN_DT   <= CURRENT_DATE                    
RMAR15               AND SAGB.EFFTV_END_DT    > CURRENT_DATE                    
                                                                                
                INNER JOIN VADVC_PROFL         PROFL                            
                        ON PROFL.SAG_ID         = SAGB.SAG_ID                   
                INNER JOIN VADVC_ASSET_PROFL   ASSET                            
                        ON ASSET.SAG_ID         = PROFL.SAG_ID                  
                       AND ASSET.SUB_TYP        = 'BRKG'                        
                       AND ASSET.EFFTV_END_DT   > CURRENT DATE                  
                                                                                
RMAR15          WHERE SAG.SERV_ID          = 413                                
RMAR15            AND SAG.EFFTV_END_DT    > CURRENT DATE                        
                                                                                
RMAR13          GROUP BY SAGB.SAG_ID                                            
                        ,ASSET.ACCT_ID                                          
                        ,ASSET.ASSET_CASH_FLW_FL                                
                        ,ASSET.ASSET_ID                                         
                        ,PROFL.ADVC_PROFL_ID                                    
                            )                                                   
              SELECT TEMP1.SAG_ID                                               
                    ,TEMP1.ACCT_ID            AS BRKG_ACCT                      
                    ,TEMP1.ASSET_CASH_FLW_FL  AS BRKG_CASH_FL                   
                    ,SWEEPADVC.ACCT_ID        AS SWEEP_ACCT                     
                    ,SWEEPADVC.ASSET_CASH_FLW_FL AS SWEEP_CASH_FL               
              FROM TEMP1                                                        
                INNER JOIN VADVC_GOAL_AST_RL       GARL                         
                       ON GARL.SAG_ID           = TEMP1.SAG_ID                  
                      AND GARL.ASSET_ID         = TEMP1.ASSET_ID                
                      AND GARL.ADVC_PROFL_ID    = TEMP1.ADVC_PROFL_ID           
                      AND GARL.EFFTV_END_DT > CURRENT DATE                      
                                                                                
                INNER JOIN VSAG_ACCT_RLSHP     BRKGACCT                         
                       ON BRKGACCT.RLSHP_ID         = TEMP1.ACCT_ID             
                      AND BRKGACCT.RLSHP_TYP_CD     = 'ACCT'                    
                                                                                
                INNER JOIN VSAG                BRKGSAG                          
                        ON BRKGSAG.SAG_ID       = BRKGACCT.SAG_ID               
                       AND BRKGSAG.SERV_ID      = '9'                           
RMAR13                 AND BRKGSAG.EFFTV_BGN_DT <= CURRENT DATE                 
                       AND BRKGSAG.EFFTV_END_DT > CURRENT DATE                  
                                                                                
                INNER JOIN VSAG_RLSHP          SWEEPSAG                         
                        ON SWEEPSAG.MBR_SAG_ID  = BRKGSAG.SAG_ID                
                       AND SWEEPSAG.SAG_RLSHP_TYP_CD = 9                        
                                                                                
                INNER JOIN VSAG_ACCT_RLSHP     SWEEPACCT                        
                       ON SWEEPACCT.SAG_ID      = SWEEPSAG.OWNR_SAG_ID          
                      AND BRKGACCT.RLSHP_TYP_CD = 'ACCT'                        
                                                                                
                INNER JOIN VADVC_ASSET_PROFL   SWEEPADVC                        
                       ON SWEEPADVC.SAG_ID        = TEMP1.SAG_ID                
                      AND SWEEPADVC.ACCT_ID       = SWEEPACCT.RLSHP_ID          
                      AND SWEEPADVC.EFFTV_END_DT  > CURRENT DATE                
                                                                                
              FOR FETCH ONLY WITH UR                                            
           END-EXEC                                                             
      *                                                                          
       LINKAGE SECTION.                                                         
      *-----------------------------------------------------------------        
      *-----------------------------------------------------------------        
       PROCEDURE DIVISION.                                               
           EXIT.                                                                
