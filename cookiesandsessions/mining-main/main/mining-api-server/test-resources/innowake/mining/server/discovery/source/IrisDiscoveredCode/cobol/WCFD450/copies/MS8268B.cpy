       01 M8268BI.
            02 FILLER PIC X(12).
            02 FIELD02L COMP PIC S9(4).
            02 FIELD02F PIC X.
            02 FILLER REDEFINES FIELD02F.
                03 FIELD02A PIC X.
            02 FIELD02I PIC X(40).
            02 FIELD03L COMP PIC S9(4).
            02 FIELD03F PIC X.
            02 FILLER REDEFINES FIELD03F.
                03 FIELD03A PIC X.
            02 FIELD03I PIC X(45).
            02 FIELDA1L COMP PIC S9(4).
            02 FIELDA1F PIC X.
            02 FILLER REDEFINES FIELDA1F.
                03 FIELDA1A PIC X.
            02 FIELDA1I PIC X(40).
            02 FIELDA2L COMP PIC S9(4).
            02 FIELDA2F PIC X.
            02 FILLER REDEFINES FIELDA2F.
                03 FIELDA2A PIC X.
            02 FIELDA2I PIC X(45).
            02 FIELDA3L COMP PIC S9(4).
            02 FIELDA3F PIC X.
            02 FILLER REDEFINES FIELDA3F.
                03 FIELDA3A PIC X.
            02 FIELDA3I PIC X(50).
        01 M8268BO REDEFINES M8268BI.
            02 FILLER PIC X(12).
            02 FILLER PIC X(3).
            02 FIELD02O PIC X(40).
            02 FILLER PIC X(3).
            02 FIELD03O PIC X(45).
            02 FILLER PIC X(3).
            02 FIELDA1O PIC X(40).
            02 FILLER PIC X(3).
            02 FIELDA2O PIC X(45).
            02 FILLER PIC X(3).
            02 FIELDA3O PIC X(50).
