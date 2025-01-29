@RUN,/W    XXXECL,308201,PRODUCTION 
@SYM       PRINT$,,PRBAT
@FURPUR 
@ . 
@.  
@CAT,P     DG-XXXECL(+1).,///100000 
@CAT,P     TR-XXXECL(+1).,///100000 
@CAT,P     ER-XXXECL(+1).,///100000 
@CAT,P     CR-XXXECL(+1).   
@ASG,AX    DG-XXXECL.   
@CHG,V     DG-XXXECL.   
@ASG,AX    TR-XXXECL.   
@CHG,V     TR-XXXECL.   
@ASG,AX    ER-XXXECL.   
@CHG,V     ER-XXXECL.   
@ASG,AX    CR-XXXECL.   
@CHG,V     CR-XXXECL.   
@ASG,A     FCSO-CSEN-02.
@ASG,A     CIDX-EXT.
@ASG,CP    FCSO-ERR(+1).,///100000  
@ASG,CP    ITRG-CSEI(+1).,///100000 
@USE       DIAG$.,DG-XXXECL.
@USE       TRACE$.,TR-XXXECL.   
@USE       ERPT.,ER-XXXECL. 
@USE       CNTL.,CR-XXXECL. 
@USE       FCSO,FCSO-CSEN-02.   
@USE       CIDX.,CIDX-EXT.  
@USE       FCSOERR.,FCSO-ERR(+1).   
@USE       ITRG.,ITRG-CSEI(+1). 
@.  
@ASG,T     TPF$.,///1536
@COPY,A    G$BPF.PABCSEI/BP                  . COPY ABSOLUTE TO TPF$
@PRT,TL    TPF$.
@XQT       PABCSEI/BP   
@BRKPT     TRACE$   
@SYM       TRACE$.,,PRBAT   
@SYM       CNTL.,,PRBAT 
@SYM       ERPT.,,PRBAT 
@.  
@TEST      TEP/1/W                            . IS SWITCH-24 SET?   
@JUMP      FATAL                              . YES - THEN SEND FATAL MSG   
@JUMP      SW23                               . NO  - THEN TEST SWITCH-23   
@.  
@FATAL: 
@.  
@.  
@FREE      DG-XXXECL.   
@FREE      TR-XXXECL.   
@FREE      ER-XXXECL.   
@FREE      CR-XXXECL.   
@FREE      FCSO-CSEN-02.
@FREE      CIDX-EXT.
@FREE      FCSO-ERR(+1).
@FREE      ITRG-CSEI(+1).   
@JUMP      FINI 
@.  
@SW23:  
@TEST      TEP/2/W                            . IS SWITCH-23 SET?   
@JUMP      WARN                               . YES - THEN SEND WARNING MSG 
@JUMP      NOERR                              . NO  - THEN SEND GOOD MSG
@.  
@WARN:  
@.  
@.  
@FREE      DG-XXXECL.   
@FREE      TR-XXXECL.   
@FREE      ER-XXXECL.   
@FREE      CR-XXXECL.   
@FREE      FCSO-CSEN-02.
@FREE      CIDX-EXT.
@FREE      FCSO-ERR(+1).
@FREE      ITRG-CSEI(+1).   
@JUMP      FINI 
@.  
@NOERR: 
@.  
@.  
@FREE      DG-XXXECL.   
@FREE      TR-XXXECL.   
@FREE      ER-XXXECL.   
@FREE      CR-XXXECL.   
@FREE      FCSO-CSEN-02.
@FREE      CIDX-EXT.
@FREE      FCSO-ERR(+1).
@FREE      ITRG-CSEI(+1).   
@PRT,F     FCSO-ERR.
@PRT,F     ITRG-CSEI.   
@PRT,F     FCSO-CSEN-02.
@PRT,F     CIDX-EXT.
@.  
@.  
@FINI:  
@FURPUR 
@FIN