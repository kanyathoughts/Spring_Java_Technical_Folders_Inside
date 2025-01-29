    DCL 1 DLI_FUNC1,
          2 FUNC_GU   CHAR(4) INIT('GU  '),
          2 FUNC_GN   CHAR(4) INIT('GN  ');
    DCL 1 DLI_FUNC2,
          2 FUNC_GNP  CHAR(4) VALUE('GNP '),
          2 DLI_NESTED,
            3 FUNC_ISRT CHAR(4) VALUE('ISRT');
    DCL FUNC_REPL CHAR(4) BASED INIT('REPL');
    DCL 1 DLI_FUNC4 BASED,
          2 FUNC_GHN CHAR(4) INIT('GHN '); 