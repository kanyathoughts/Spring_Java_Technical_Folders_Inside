SELECT ASD.ADVC_PROFL_ID                                  				                            00219210
                     , ASD.GOAL_ID          00219220
                     , ASD.RULE_NM                                      00219320
                     , ASD.VALID_UNTIL_DT                               00219420
                     , ASD.CREW_PO_ID                                   00219520
                     , ASD.TXN_STRT_TS                                  00219620
                  FROM VT_ADVC_RBL_VLTA				                                                       00219720
                 WHERE ASD.ADVC_PROFL_ID   = V_ADVC_PROFL_ID            00220620
                   AND ASD.ORIG_APPR_FL    = 'Y'                       00220724
                   AND ASD.ORIG_APPR_FLA    = 10000
                   AND ASD.ORIG_APPR_FLB    <= 20000
                   AND ASD.ORIG_APPR_FLC    >= 30000
                   AND ASD.ORIG_APPR_FLD    != 90000
				   ORDER BY 1 
				   LIMIT 1000