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
      
      * SQL TEST 1 (TABLE: CUSTOMER_SECURE)
           EXEC SQL
             INSERT INTO CUSTOMER_SECURE
                       ( customerNumber,
                         customerPass,
                         state_indicator,
                         pass_changes   )
                VALUES ( :DB2-CUSTOMERNUM-INT,
                         :D2-CUSTSECR-PASS,
                         :D2-CUSTSECR-STATE,
                         :DB2-CUSTOMERCNT-INT)
           END-EXEC
             
      * SQL TEST 2 (TABLE: POLICY)
      *    get value of assigned Timestamp
           EXEC SQL
             SELECT LASTCHANGED
               INTO :CA-LASTCHANGED
               FROM POLICY
               WHERE POLICYNUMBER = :DB2-POLICYNUM-INT
           END-EXEC.
           
      * SQL TEST 3 (TABLE: CUST.POLICY)
           EXEC SQL
             DELETE
               FROM CUST.POLICY
               WHERE ( CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT AND
                       POLICYNUMBER  = :DB2-POLICYNUM-INT      )
           END-EXEC
      
      * SQL TEST 4 (TABLE: POLICY, CLAIM)
           EXEC SQL
             DECLARE CusClaim_Cursor Insensitive Scroll Cursor For
             SELECT
                   POLICY.CustomerNumber,
                   ClaimNumber,
                   CLAIM.PolicyNumber,
                   ClaimDate,
                   Paid,
                   Value,
                   Cause,
                   Observations
             FROM  POLICY,CLAIM
             WHERE ( POLICY.POLICYNUMBER =
                        CLAIM.POLICYNUMBER   AND
                     POLICY.CustomerNumber =
                        :DB2-CUSTOMERNUM-INT)
           END-EXEC.
      
      * SQL TEST 5 (TABLE: CUSTOMER)
           EXEC SQL
               UPDATE CUSTOMER
                 SET
                   FIRSTNAME     = :CA-FIRST-NAME,
                   LASTNAME      = :CA-LAST-NAME,
                   DATEOFBIRTH   = :CA-DOB,
                   HOUSENAME     = :CA-HOUSE-NAME,
                   HOUSENUMBER   = :CA-HOUSE-NUM,
                   POSTCODE      = :CA-POSTCODE,
                   PHONEMOBILE   = :CA-PHONE-MOBILE,
                   PHONEHOME     = :CA-PHONE-HOME,
                   EMAILADDRESS  = :CA-EMAIL-ADDRESS
                 WHERE
                     CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT
             END-EXEC

      * SQL TEST 6 (TABLE: CURS.POLICY)
             EXEC SQL
             DECLARE POLICY_CURSOR CURSOR WITH HOLD FOR
               SELECT ISSUEDATE,
                      EXPIRYDATE,
                      LASTCHANGED,
                      BROKERID,
                      BROKERSREFERENCE
               FROM CURS.POLICY
               WHERE ( CUSTOMERNUMBER = :DB2-CUSTOMERNUM-INT AND
                       POLICYNUMBER = :DB2-POLICYNUM-INT )
               FOR UPDATE OF ISSUEDATE,
                             EXPIRYDATE,
                             LASTCHANGED,
                             BROKERID,
                             BROKERSREFERENCE
           END-EXEC.
     
      * SQL TEST 7 (TABLE: ENDOWMENT) 
           EXEC SQL
               UPDATE ENDOWMENT
                 SET
                   WITHPROFITS   = :CA-E-WITH-PROFITS,
                     EQUITIES    = :CA-E-EQUITIES,
                     MANAGEDFUND = :CA-E-MANAGED-FUND,
                     FUNDNAME    = :CA-E-FUND-NAME,
                     TERM        = :DB2-E-TERM-SINT,
                     SUMASSURED  = :DB2-E-SUMASSURED-INT,
                     LIFEASSURED = :CA-E-LIFE-ASSURED
      *---> STEW     PADDINGDATA = :WS-VARY-FIELD
                 WHERE
                     POLICYNUMBER = :DB2-POLICYNUM-INT
             END-EXEC
           
           GOBACK.

       END PROGRAM WCFD68A.

