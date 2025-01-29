 START
         B     D(X,B)
         CLC   PFUNK,=C'15'
         LA    R1,X'40'
MYDATA   DS CL8
NAME1    LA      R1
         LA    9,L'MYMVCL*20
         LA    10,MYBRANCH
         LA    11,L'MYBRANCH*20
         TMY   O'MYMVCL,I2
         LA    11,L'&VARSYM*20
         B     *+10