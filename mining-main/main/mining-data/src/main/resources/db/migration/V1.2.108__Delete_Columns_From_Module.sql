DROP PROPERTY Module.linesOfCode IF EXISTS FORCE;
DROP PROPERTY Module.linesOfComment IF EXISTS FORCE;
DROP PROPERTY Module.complexity IF EXISTS FORCE;
DROP PROPERTY Module.linesOfDeadCode IF EXISTS FORCE;

UPDATE Module REMOVE linesOfCode, linesOfComment, complexity, linesOfDeadCode;