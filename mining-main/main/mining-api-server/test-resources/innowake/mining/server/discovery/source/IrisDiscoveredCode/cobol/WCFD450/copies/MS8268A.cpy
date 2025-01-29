       01 M8268AI.
            02 FILLER PIC X(12).
            02 FIELD01L COMP PIC S9(4).
            02 FIELD01F PIC X.
            02 FILLER REDEFINES FIELD01F.
                03 FIELD01A PIC X.
            02 FILLER PIC X(2).
            02 FIELD01I PIC X(40).
        01 M8268AO REDEFINES M8268AI.
            02 FILLER PIC X(12).
            02 FILLER PIC X(3).
            02 FIELD01C PIC X.
            02 FIELD01H PIC X.
            02 FIELD01O PIC X(40).
