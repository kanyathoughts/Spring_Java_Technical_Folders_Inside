            EXEC SQL
            DECLARE VIPISS_ID TABLE
               (DEPTNO    CHAR(3)           NOT NULL,
               DEPTNAME  VARCHAR(36)       NOT NULL,
               MGRNO     CHAR(6)                   ,
               ADMRDEPT  CHAR(3)           NOT NULL,
               LOCATION  CHAR(16)                  ) 
            END-EXEC.
            EXEC SQL
	           DECLARE VIP_ID TABLE
               (DEPTNO    CHAR(3)           NOT NULL,
               DEPTNAME  VARCHAR(36)       NOT NULL,
               MGRNO     CHAR(6)                   ,
               ADMRDEPT  CHAR(3)           NOT NULL,
               LOCATION  CHAR(16)                  ) 
            END-EXEC.