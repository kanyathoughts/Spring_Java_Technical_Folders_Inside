       01 MMRS71BI.
            02 FILLER PIC X(12).
            02 USERTXTL COMP PIC S9(4).
            02 USERTXTF PIC X.
            02 FILLER REDEFINES USERTXTF.
                03 USERTXTA PIC X.
            02 FILLER PIC X(4).
            02 USERTXTI PIC X(19).
            02 USERIDL COMP PIC S9(4).
            02 USERIDF PIC X.
            02 FILLER REDEFINES USERIDF.
                03 USERIDA PIC X.
            02 FILLER PIC X(4).
            02 USERIDI PIC X(50).
            02 PWTXTL COMP PIC S9(4).
            02 PWTXTF PIC X.
            02 FILLER REDEFINES PWTXTF.
                03 PWTXTA PIC X.
            02 FILLER PIC X(4).
            02 PWTXTI PIC X(19).
            02 PWDATAL COMP PIC S9(4).
            02 PWDATAF PIC X.
            02 FILLER REDEFINES PWDATAF.
                03 PWDATAA PIC X.
            02 FILLER PIC X(4).
            02 PWDATAI PIC X(20).
        01 MMRS71BO REDEFINES MMRS71BI.
            02 FILLER PIC X(12).
            02 FILLER PIC X(3).
            02 USERTXTC PIC X.
            02 USERTXTP PIC X.
            02 USERTXTH PIC X.
            02 USERTXTV PIC X.
            02 USERTXTO PIC X(19).
            02 FILLER PIC X(3).
            02 USERIDC PIC X.
            02 USERIDP PIC X.
            02 USERIDH PIC X.
            02 USERIDV PIC X.
            02 USERIDO PIC X(50).
            02 FILLER PIC X(3).
            02 PWTXTC PIC X.
            02 PWTXTP PIC X.
            02 PWTXTH PIC X.
            02 PWTXTV PIC X.
            02 PWTXTO PIC X(19).
            02 FILLER PIC X(3).
            02 PWDATAC PIC X.
            02 PWDATAP PIC X.
            02 PWDATAH PIC X.
            02 PWDATAV PIC X.
            02 PWDATAO PIC X(20).
