CREATE PROCEDURE name
    EXTERNAL NAME bla LANGUAGE COBOL;
    
CREATE PROCEDURE name
    EXTERNAL NAME bla LANGUAGE COBOL
    DYNAMIC RESULT SETS 0;
    
CREATE PROCEDURE name para1 para2 OUT para3
    EXTERNAL LANGUAGE PLI;
    
CREATE PROCEDURE name
    NO COLLID
    EXTERNAL NAME bla LANGUAGE C
    COMMIT ON RETURN NO;