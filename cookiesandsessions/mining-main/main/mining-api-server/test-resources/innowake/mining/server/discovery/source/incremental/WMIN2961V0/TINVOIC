      ******************************************************************
      * DCLGEN TABLE(TINVOIC)                                          *
      ******************************************************************
           EXEC SQL DECLARE TINVOIC            TABLE
           ( PO_NBR             INTEGER         NOT NULL
            ,IN_ID              CHAR(0022)      NOT NULL
            ,PAYT_REQ_SEQ_NBR   INTEGER         NOT NULL
            ,IN_TYP_CDE         CHAR(0002)      NOT NULL
            ,ENTR_MTHD_CDE      CHAR(0002)      NOT NULL
            ,CRE_BY_USR_ID      CHAR(0008)      NOT NULL
            ,BAR_CDE            CHAR(0009)      NOT NULL
            ,SPLIT_TERMS_IND    CHAR(0001)      NOT NULL
            ,DTL_ENTR_IND       CHAR(0001)      NOT NULL WITH DEFAULT
            ,INVC_CART_CT       SMALLINT        NOT NULL WITH DEFAULT
            ,CRE_DTE            DATE            NOT NULL
            ,PPD_RVRSL_IND      CHAR(0001)      NOT NULL WITH DEFAULT
            ,INVC_DTE           DATE            NOT NULL
            ,GRO_COST_AMT       DECIMAL(010, 2) NOT NULL
            ,INVC_NET_COST_AMT  DECIMAL(010, 2) NOT NULL WITH DEFAULT
            ,RMIT_VND_ID        CHAR(0009)      NOT NULL
            ,STR_NBR            SMALLINT        NOT NULL
            ,HOLD_IND           CHAR(0001)      NOT NULL
            ,DUE_DTE_SRC_CDE    CHAR(0001)      NOT NULL WITH DEFAULT
            ,CALC_DUE_DTE       DATE            NOT NULL WITH DEFAULT
            ,PAYT_DTE           DATE            NOT NULL
            ,AP_PRC_DTE         DATE            NOT NULL WITH DEFAULT
            ,AP_PRC_CDE         CHAR(0001)      NOT NULL WITH DEFAULT
            ,PAYT_RLSE_TMST     TIMESTAMP       NOT NULL
            ,ST_CDE             CHAR(0002)      NOT NULL
            ,ST_CHG_TMST        TIMESTAMP       NOT NULL WITH DEFAULT
            ,DC_NBR             SMALLINT        NOT NULL WITH DEFAULT
            ,MTCH_LVL_CDE       CHAR(0001)      NOT NULL WITH DEFAULT
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION
      ******************************************************************
       01  DCLTINVOIC.
           10 IN-PO-NBR                 PIC  S9(9) COMP.
           10 IN-INVC-ID                PIC   X(22).
           10 IN-PAYT-REQ-SEQ-NBR       PIC  S9(9) COMP.
           10 IN-INVC-TYP-CDE           PIC   X(2).
           10 IN-ENTR-MTHD-CDE          PIC   X(2).
           10 IN-CRE-BY-USR-ID          PIC   X(8).
           10 IN-BAR-CDE                PIC   X(9).
           10 IN-SPLIT-TERMS-IND        PIC   X(1).
           10 IN-DTL-ENTR-IND           PIC   X(1).
           10 IN-INVC-CART-CT           PIC  S9(4) COMP.
           10 IN-CRE-DTE                PIC   X(10).
           10 IN-PPD-RVRSL-IND          PIC   X(1).
           10 IN-INVC-DTE               PIC   X(10).
           10 IN-GRO-COST-AMT           PIC  S9(8)V9(2) COMP-3.
           10 IN-INVC-NET-COST-AMT      PIC  S9(8)V9(2) COMP-3.
           10 IN-RMIT-VND-ID            PIC   X(9).
           10 IN-STR-NBR                PIC  S9(4) COMP.
           10 IN-HOLD-IND               PIC   X(1).
           10 IN-DUE-DTE-SRC-CDE        PIC   X(1).
           10 IN-CALC-DUE-DTE           PIC   X(10).
           10 IN-PAYT-DTE               PIC   X(10).
           10 IN-AP-PRC-DTE             PIC   X(10).
           10 IN-AP-PRC-CDE             PIC   X(1).
           10 IN-PAYT-RLSE-TMST         PIC   X(26).
           10 IN-STATUS-CDE             PIC   X(2).
           10 IN-STATUS-CHG-TMST        PIC   X(26).
           10 IN-DC-NBR                 PIC  S9(4) COMP.
           10 IN-MTCH-LVL-CDE           PIC   X(1).
