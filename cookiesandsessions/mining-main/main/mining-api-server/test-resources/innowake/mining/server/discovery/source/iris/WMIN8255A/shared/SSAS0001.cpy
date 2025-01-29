        01 VENDOR-SSA-UNQUALIFIED.
           05 SEGMENT-NAME    PIC X(008) VALUE 'INVENSEG'.
           05 FILLER          PIC X(992) VALUE SPACE.

        01 VENDOR-SSA-QUALIFIED.
           05 FILLER          PIC X(009)  VALUE 'INVENSEG('.
           05 FILLER          PIC X(010)  VALUE 'INVENCOD ='.
           05 VENDOR-CODE     PIC X(003).
           05 FILLER          PIC X(001)  VALUE ')'.
           05 FILLER          PIC X(977) VALUE SPACE.

        01 ITEM-SSA-UNQUALIFIED.
           05 SEGMENT-NAME    PIC X(008) VALUE 'INITMSEG'.
           05 FILLER          PIC X(992) VALUE SPACE.

        01 ITEM-SSA-QUALIFIED.
           05 FILLER          PIC X(009)  VALUE 'INITMSEG('.
           05 FILLER          PIC X(010)  VALUE 'INITMNUM ='.
           05 ITEM-CODE       PIC X(005).
           05 FILLER          PIC X(001)  VALUE ')'.
           05 FILLER          PIC X(975) VALUE SPACE.

        01 LOCATION-SSA-UNQUALIFIED.
           05 SEGMENT-NAME    PIC X(008) VALUE 'INLOCSEG'.
           05 FILLER          PIC X(992) VALUE SPACE.

        01 LOCATION-SSA-QUALIFIED.
           05 FILLER          PIC X(009)  VALUE 'INLOCSEG('.
           05 FILLER          PIC X(010)  VALUE 'INLOCLOC ='.
           05 LOCATION-CODE   PIC X(003).
           05 FILLER          PIC X(001)  VALUE ')'.
           05 FILLER          PIC X(975) VALUE SPACE.
