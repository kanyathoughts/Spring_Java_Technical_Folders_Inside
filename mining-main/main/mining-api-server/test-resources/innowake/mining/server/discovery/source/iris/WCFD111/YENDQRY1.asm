  SELECT *
     FROM BQSFMIS1.TEMP_GL
     WHERE (CL_PPY_BALANCE <>0.00
       OR   CL_PY_BALANCE <>0.00
       OR   CL_M01_BALANCE <>0.00
       OR   CL_M02_BALANCE <>0.00
       OR   CL_M03_BALANCE <>0.00
       OR   CL_M04_BALANCE <>0.00
       OR   CL_M05_BALANCE <>0.00
       OR   CL_M06_BALANCE <>0.00
       OR   CL_M07_BALANCE <>0.00
       OR   CL_M08_BALANCE <>0.00
       OR   CL_M09_BALANCE <>0.00
       OR   CL_M10_BALANCE <>0.00
       OR   CL_M11_BALANCE <>0.00
       OR   CL_M12_BALANCE <>0.00
       OR   CL_M13_BALANCE <>0.00
       OR   CL_CUM_BALANCE <>0.00)
       AND (CL_DEPARTMENT NOT IN('J01','J02','J03','J04','J05','J06'))
       AND (CL_TRANS_YY       IN('17','18'))
      WITH UR;
  SELECT *
     FROM BQSFMIS1.TEMP_GL
     WHERE CL_PPY_BALANCE <>0.00
       OR   CL_PY_BALANCE <>0.00
       OR   CL_M01_BALANCE <>0.00
       OR   CL_M02_BALANCE <>0.00
       OR   CL_M03_BALANCE <>0.00
       OR   CL_M04_BALANCE <>0.00
       OR   CL_M05_BALANCE <>0.00
       OR   CL_M06_BALANCE <>0.00
       OR   CL_M07_BALANCE <>0.00
       OR   CL_M08_BALANCE <>0.00
       OR   CL_M09_BALANCE <>0.00
       OR   CL_M10_BALANCE <>0.00
       OR   CL_M11_BALANCE <>0.00
       OR   CL_M12_BALANCE <>0.00
       OR   CL_M13_BALANCE <>0.00
       OR   CL_CUM_BALANCE <>0.00
       AND ((CL_DEPARTMENT IN ('J01','J02','J04','J05','J06') AND
            CL_FUND_DETAIL < 'AAAA')  OR
           (CL_DEPARTMENT = 'J03' AND
            CL_GL_ACCOUNT NOT IN('0504','0505') AND
            CL_FUND_DETAIL < 'AAAA'))
       AND (CL_TRANS_YY       IN('17','18'))
      WITH UR;
  SELECT *
     FROM BQSFMIS1.TEMP_GL
     WHERE (CL_PPY_BALANCE <>0.00
       OR   CL_PY_BALANCE <>0.00
       OR   CL_M01_BALANCE <>0.00
       OR   CL_M02_BALANCE <>0.00
       OR   CL_M03_BALANCE <>0.00
       OR   CL_M04_BALANCE <>0.00
       OR   CL_M05_BALANCE <>0.00
       OR   CL_M06_BALANCE <>0.00
       OR   CL_M07_BALANCE <>0.00
       OR   CL_M08_BALANCE <>0.00
       OR   CL_M09_BALANCE <>0.00
       OR   CL_M10_BALANCE <>0.00
       OR   CL_M11_BALANCE <>0.00
       OR   CL_M12_BALANCE <>0.00
       OR   CL_M13_BALANCE <>0.00
       OR   CL_CUM_BALANCE <>0.00)
       AND (CL_APPN_YEAR < 'AA')
       AND (CL_TRANS_YY       IN('17','18'))
      WITH UR;
  SELECT *
     FROM BQSFMIS1.TEMP_GL
     WHERE (CL_PPY_BALANCE <>0.00
       OR   CL_PY_BALANCE <>0.00
       OR   CL_M01_BALANCE <>0.00
       OR   CL_M02_BALANCE <>0.00
       OR   CL_M03_BALANCE <>0.00
       OR   CL_M04_BALANCE <>0.00
       OR   CL_M05_BALANCE <>0.00
       OR   CL_M06_BALANCE <>0.00
       OR   CL_M07_BALANCE <>0.00
       OR   CL_M08_BALANCE <>0.00
       OR   CL_M09_BALANCE <>0.00
       OR   CL_M10_BALANCE <>0.00
       OR   CL_M11_BALANCE <>0.00
       OR   CL_M12_BALANCE <>0.00
       OR   CL_M13_BALANCE <>0.00
       OR   CL_CUM_BALANCE <>0.00)
       AND  (CL_TRANS_YY  < 'AA')
       AND  (CL_TRANS_YY       IN('17','18'))
      WITH UR;
  SELECT *
     FROM BQSFMIS1.TEMP_GL
     WHERE (CL_PPY_BALANCE <>0.00
       OR   CL_PY_BALANCE <>0.00
       OR   CL_M01_BALANCE <>0.00
       OR   CL_M02_BALANCE <>0.00
       OR   CL_M03_BALANCE <>0.00
       OR   CL_M04_BALANCE <>0.00
       OR   CL_M05_BALANCE <>0.00
       OR   CL_M06_BALANCE <>0.00
       OR   CL_M07_BALANCE <>0.00
       OR   CL_M08_BALANCE <>0.00
       OR   CL_M09_BALANCE <>0.00
       OR   CL_M10_BALANCE <>0.00
       OR   CL_M11_BALANCE <>0.00
       OR   CL_M12_BALANCE <>0.00
       OR   CL_M13_BALANCE <>0.00
       OR   CL_CUM_BALANCE <>0.00)
       AND  (CL_GL_ACCOUNT < 'AAAA')
       AND  (CL_TRANS_YY       IN('17','18'))
      WITH UR;
