       01 MMRS71CI.
            02 FILLER PIC X(12).
            02 WHATTXTL COMP PIC S9(4).
            02 WHATTXTF PIC X.
            02 FILLER REDEFINES WHATTXTF.
                03 WHATTXTA PIC X.
            02 FILLER PIC X(4).
            02 WHATTXTI PIC X(25).
            02 WHATINL COMP PIC S9(4).
            02 WHATINF PIC X.
            02 FILLER REDEFINES WHATINF.
                03 WHATINA PIC X.
            02 FILLER PIC X(4).
            02 WHATINI PIC X(50).
        01 MMRS71CO REDEFINES MMRS71CI.
            02 FILLER PIC X(12).
            02 FILLER PIC X(3).
            02 WHATTXTC PIC X.
            02 WHATTXTP PIC X.
            02 WHATTXTH PIC X.
            02 WHATTXTV PIC X.
            02 WHATTXTO PIC X(25).
            02 FILLER PIC X(3).
            02 WHATINC PIC X.
            02 WHATINP PIC X.
            02 WHATINH PIC X.
            02 WHATINV PIC X.
            02 WHATINO PIC X(50).
