PROGRAM program_lower
 CALL program_upper
 CALL LIB$SPAWN ( '@COM:CLUR04.COM '+ Merge,,,,,,Completion_Stat)
 CALL LIB$SPAWN ( '@COM:CLuR04.COM '+ Merge,,,,,,Completion_Stat)
END PROGRAM