         BEGIN NAME=SCSF,VERSION=DW
         PRINT NOGEN
         #SPM  PRINT=NOMOD,LEVEL=NO
         PS1PS  REG=R1,SUFFIX=1            BASE PNIVR
         API0AP REG=R2                     API0AP WORK BLOCK
         ADP2WA REG=R3                     WORK BLOCK
         SS0SA  REG=R4                     SSR BASE REGISTER
         PS1PS  REG=R6                     PNIS BASE REGISTER
         PS1PS  REG=R7,SUFFIX=F            PNIS ITEM BASE
         SW0SW  REG=R15                    BASE SW0SW BLOCK
         LTORG
         FINIS SCSF
         END

