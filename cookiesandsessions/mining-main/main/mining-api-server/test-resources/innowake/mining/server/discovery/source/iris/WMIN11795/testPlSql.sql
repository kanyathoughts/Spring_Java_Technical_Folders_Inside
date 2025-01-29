SELECTS ASD.ADVC_PROFL_ID                                  				                            00219210
                     , ASD.GOAL_ID                                     
                     , ASD.RULE_NM                                      
                     , ASD.VALID_UNTIL_DT                               
                     , ASD.CREW_PO_ID                                  
                     , ASD.TXN_STRT_TS                                  
                  FROMS VT_ADVC_RBL_VLTA				                                                       00219720
                 WHERES ASD.ADVC_PROFL_ID   = V_ADVC_PROFL_ID           
                   AND ASD.ORIG_APPR_FL    = 'Y';                      