DROP SEQUENCE Client_Sequence
DROP SEQUENCE Module_Sequence
DROP SEQUENCE ModuleUid_Sequence
DROP SEQUENCE Project_Sequence
DROP SEQUENCE AnnotationCategory_Sequence
DROP SEQUENCE Annotation_Sequence
DROP SEQUENCE Reference_Sequence
DROP SEQUENCE DataDictionaryEntry_Sequence
DROP SEQUENCE Taxonomy_Sequence
DROP SEQUENCE SourceObject_Sequence
DROP SEQUENCE TaxonomyCategory_Sequence

CREATE SEQUENCE Client_Sequence TYPE ORDERED START 0 INCREMENT 1
CREATE SEQUENCE Module_Sequence TYPE ORDERED START 1999 INCREMENT 1
CREATE SEQUENCE ModuleUid_Sequence TYPE ORDERED START 0 INCREMENT 1
CREATE SEQUENCE Project_Sequence TYPE ORDERED START 0 INCREMENT 1
CREATE SEQUENCE AnnotationCategory_Sequence TYPE ORDERED START 1000 INCREMENT 1
CREATE SEQUENCE Annotation_Sequence TYPE ORDERED START 0 INCREMENT 1
CREATE SEQUENCE Reference_Sequence TYPE ORDERED START 0 INCREMENT 1
CREATE SEQUENCE DataDictionaryEntry_Sequence TYPE ORDERED START 0 INCREMENT 1
CREATE SEQUENCE Taxonomy_Sequence TYPE ORDERED START 0 INCREMENT 1
CREATE SEQUENCE SourceObject_Sequence TYPE ORDERED START 0 INCREMENT 1
CREATE SEQUENCE TaxonomyCategory_Sequence TYPE ORDERED START 2 INCREMENT 1

###################################################################################################################################
# DELETE FROMES
###################################################################################################################################
DELETE VERTEX Client WHERE id<>0
DELETE VERTEX Project WHERE id<>0
DELETE VERTEX Module WHERE projectLink.id<>0
DELETE VERTEX AstNode
DELETE VERTEX TaxonomyCategory WHERE projectLink.id<>0

DELETE FROM Reference UNSAFE 
DELETE FROM Calls UNSAFE
DELETE FROM Includes UNSAFE
DELETE FROM References UNSAFE
DELETE FROM ReadsWrites UNSAFE
DELETE FROM None UNSAFE
DELETE FROM HasTaxonomy UNSAFE
DELETE FROM Taxonomy UNSAFE 
DELETE FROM TaxonomyEnum UNSAFE 
DELETE FROM AnnotationCategory WHERE projectLink.id<>0 UNSAFE
DELETE FROM Annotation UNSAFE
DELETE FROM HasAnnotation UNSAFE
DELETE FROM DataDictionaryEntry UNSAFE
DELETE FROM HasDataDictionaryEntry UNSAFE
DELETE FROM DataDictionaryOtherScopeEnum UNSAFE
DELETE FROM SqlHistory UNSAFE 
DELETE FROM SourceAttachment UNSAFE 
DELETE FROM SourceObject UNSAFE 
DELETE FROM Statement UNSAFE 
DELETE FROM SqlStatement UNSAFE
DELETE FROM ExcelSheetErrors UNSAFE 
DELETE FROM ExcelSheetDeadCode UNSAFE 
DELETE FROM ExcelSheetConditionalOutlines UNSAFE 
DELETE FROM ExcelSheetUndiscovered UNSAFE
DELETE FROM EntryPoint UNSAFE
DELETE FROM ReturnPoint UNSAFE
DELETE FROM HaltPoint UNSAFE

###################################################################################################################################
# TEST CLIENTS
###################################################################################################################################
CREATE PROPERTY Client.customClientMandatoryProperty IF NOT EXISTS String
ALTER PROPERTY Client.customClientMandatoryProperty CUSTOM label="Custom Client Mandatory Property"
ALTER PROPERTY Client.customClientMandatoryProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientMandatoryProperty description "A custom Mandatory property for the Client class"
ALTER PROPERTY Client.customClientMandatoryProperty MANDATORY TRUE

UPDATE Client SET customClientMandatoryProperty="A value for the custom client Mandatory property (SYSTEM)" WHERE name="SYSTEM"
INSERT INTO Client SET id=sequence('Client_Sequence').next(), name="Demo Client 1", customClientMandatoryProperty="A value for the custom client Mandatory property (Demo Client 1)"
INSERT INTO Client SET id=sequence('Client_Sequence').next(), name="Demo Client 2", customClientMandatoryProperty="A value for the custom client Mandatory property (Demo Client 2)"

###################################################################################################################################
# TEST PROJECTS
###################################################################################################################################
INSERT INTO Project SET id=sequence('Project_Sequence').next(), name="Demo Project A", clientLink=(SELECT FROM Client WHERE id=1)
INSERT INTO Project SET id=sequence('Project_Sequence').next(), name="Demo Project B", clientLink=(SELECT FROM Client WHERE id=1)
INSERT INTO Project SET id=sequence('Project_Sequence').next(), name="Demo Project C", clientLink=(SELECT FROM Client WHERE id=2)
INSERT INTO Project SET id=sequence('Project_Sequence').next(), name="Demo Project D", clientLink=(SELECT FROM Client WHERE id=2)

###################################################################################################################################
## TEST TAXONOMY CATEGORY
###################################################################################################################################
INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name="Technical Taxonomies", projectLink=(SELECT FROM Project WHERE id=1);
INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name="Business Taxonomies", projectLink=(SELECT FROM Project WHERE id=1);
INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name="Technical Taxonomies", projectLink=(SELECT FROM Project WHERE id=2);
INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name="Business Taxonomies", projectLink=(SELECT FROM Project WHERE id=2);
INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name="Technical Taxonomies", projectLink=(SELECT FROM Project WHERE id=3);
INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name="Business Taxonomies", projectLink=(SELECT FROM Project WHERE id=3);
INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name="Technical Taxonomies", projectLink=(SELECT FROM Project WHERE id=4);
INSERT INTO TaxonomyCategory SET id=sequence('TaxonomyCategory_Sequence').next(), name="Business Taxonomies", projectLink=(SELECT FROM Project WHERE id=4);	

###################################################################################################################################
## UPDATE TEST PROJECTS
###################################################################################################################################
UPDATE PROJECT SET technicalTaxonomyCategoryLink=(Select from TaxonomyCategory where name="Technical Taxonomies" and projectLink=(SELECT FROM Project WHERE id=$current.id));
UPDATE PROJECT SET defaultTaxonomyCategoryLink=(Select from TaxonomyCategory where name="Business Taxonomies" and projectLink=(SELECT FROM Project WHERE id=$current.id));

###################################################################################################################################
# UPSERT TEST OBJECTTYPES
###################################################################################################################################
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="NATURAL"), typeLink=(SELECT FROM TypeEnum WHERE name="PROGRAM"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="NATURAL") AND typeLink=(SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="NATURAL"), typeLink=(SELECT FROM TypeEnum WHERE name="COPYCODE"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="NATURAL") AND typeLink=(SELECT FROM TypeEnum WHERE name="COPYCODE") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="NATURAL"), typeLink=(SELECT FROM TypeEnum WHERE name="LDA"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="NATURAL") AND typeLink=(SELECT FROM TypeEnum WHERE name="LDA") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="NATURAL"), typeLink=(SELECT FROM TypeEnum WHERE name="PDA"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="NATURAL") AND typeLink=(SELECT FROM TypeEnum WHERE name="PDA") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="JCL"), typeLink=(SELECT FROM TypeEnum WHERE name="EXEC_PGM"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE_SECTION"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink=(SELECT FROM TypeEnum WHERE name="EXEC_PGM") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE_SECTION") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="JCL"), typeLink=(SELECT FROM TypeEnum WHERE name="JOB"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink=(SELECT FROM TypeEnum WHERE name="JOB") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="JCL"), typeLink=(SELECT FROM TypeEnum WHERE name="PROC"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink=(SELECT FROM TypeEnum WHERE name="PROC") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="JCL"), typeLink=(SELECT FROM TypeEnum WHERE name="INCLUDE"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink=(SELECT FROM TypeEnum WHERE name="INCLUDE") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="COBOL"), typeLink=(SELECT FROM TypeEnum WHERE name="PROGRAM"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink=(SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="COBOL"), typeLink=(SELECT FROM TypeEnum WHERE name="COPYBOOK"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink=(SELECT FROM TypeEnum WHERE name="COPYBOOK") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="COBOL"), typeLink=(SELECT FROM TypeEnum WHERE name="BMS_MAPSET"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink=(SELECT FROM TypeEnum WHERE name="BMS_MAPSET") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="BASIC"), typeLink=(SELECT FROM TypeEnum WHERE name="OBJECT"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="BASIC") AND typeLink=(SELECT FROM TypeEnum WHERE name="OBJECT") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="BASIC"), typeLink=(SELECT FROM TypeEnum WHERE name="PROGRAM"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="BASIC") AND typeLink=(SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="VMS"), typeLink=(SELECT FROM TypeEnum WHERE name="DCL"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="VMS") AND typeLink=(SELECT FROM TypeEnum WHERE name="DCL") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="VMS"), typeLink=(SELECT FROM TypeEnum WHERE name="IFDL_FORM"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="VMS") AND typeLink=(SELECT FROM TypeEnum WHERE name="IFDL_FORM") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="NONE"), typeLink=(SELECT FROM TypeEnum WHERE name="UNKNOWN"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="NONE") AND typeLink=(SELECT FROM TypeEnum WHERE name="UNKNOWN") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")
UPDATE ObjectType SET technologyLink=(SELECT FROM TechnologyEnum WHERE name="BASIC"), typeLink=(SELECT FROM TypeEnum WHERE name="FUNCTION"), storageLink=(SELECT FROM StorageEnum WHERE name="FILE"), originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM") UPSERT RETURN AFTER @rid WHERE technologyLink=(SELECT FROM TechnologyEnum WHERE name="BASIC") AND typeLink=(SELECT FROM TypeEnum WHERE name="FUNCTION") AND storageLink=(SELECT FROM StorageEnum WHERE name="FILE") AND originLink=(SELECT FROM OriginEnum WHERE name="CUSTOM")

###################################################################################################################################
# TEST MODULES
###################################################################################################################################
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="PRG1", path="src-natural/LibA/PRG1.nsp", projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="NATURAL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="QBGPSLP1MMRS710A.STEP01.MMRS7102", projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="EXEC_PGM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE_SECTION") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"

INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="MMRS7101", path="src/cobol/programs/MMRS7101.cbl", description="A test description for MMRS7101", sourceAttachmentLink=(INSERT INTO SourceObject SET content="dummy source content", contentHash='1', name="MMRS7101", path="src/cobol/programs/MMRS7101.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO SourceMetrics SET id=sequence('SourceMetrics_Sequence').next(), codeLines=107, commentLines=46, complexityMcCabe=3;
CREATE EDGE HasAdditionalInfo FROM (select from Module where id=(SELECT sequence('Module_Sequence').current())) TO (select from SourceMetrics where id=(SELECT sequence('SourceMetrics_Sequence').current()));

INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="MMRS7102", path="src/cobol/programs/MMRS7102.cbl", description="A test description for MMRS7102", projectLink=(SELECT FROM Project WHERE id=2), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="MMRS7103", path="src/cobol/programs/MMRS7103.cbl", description="A test description for MMRS7102", projectLink=(SELECT FROM Project WHERE id=2), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="PRGA", path="src/cobol/programs/PRGA.cbl", description="A test program", sourceAttachmentLink=(INSERT INTO SourceObject SET content="       PROCEDURE DIVISION.\n           DISPLAY '1'\n           .\n", contentHash='1', name="PRGA", path="src/cobol/programs/PRGA.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="PRGB", path="src/cobol/programs/PRGB.cbl", description="A test program", sourceAttachmentLink=(INSERT INTO SourceObject SET content="       PROCEDURE DIVISION.\n           DISPLAY '1'\n           COPY CC1\n           DISPLAY '2'\n           COPY CC2\n           DISPLAY '3'\n           .\n", contentHash='1', name="PRGB", path="src/cobol/programs/PRGB.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="CC1", path="src/cobol/programs/CC1.cpy", description="A test copy", sourceAttachmentLink=(INSERT INTO SourceObject SET content="           DISPLAY 'CC1'\n           COPY CC2\n", contentHash='1', name="CC1", path="src/cobol/programs/CC1.cpy", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="COPYBOOK") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="CC2", path="src/cobol/programs/CC2.cpy", description="A test copy", sourceAttachmentLink=(INSERT INTO SourceObject SET content="           DISPLAY 'CC2'\n", contentHash='1', name="CC2", path="src/cobol/programs/CC2.cpy", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="COPYBOOK") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="PRGC", path="src/cobol/programs/PRGC.cbl", description="A test program", sourceAttachmentLink=(INSERT INTO SourceObject SET content="       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7101:'.\n       PROCEDURE DIVISION.\n           PERFORM MY-FLOW-01 THRU MY-FLOW-02.\n       MY-FLOW-01.\n           DISPLAY MY-PROGRAM-NAME 'my-flow-01   Label 01    '.\n       MY-FLOW-02.\n           DISPLAY MY-PROGRAM-NAME 'my-flow-02   Label 02    '.\n", contentHash='1', name="PRGC", path="src/cobol/programs/PRGC.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="PRGD", path="src/cobol/programs/PRGD.cbl", description="A test program", sourceAttachmentLink=(INSERT INTO SourceObject SET content="       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01  MY-HEX-SHOW-CHARS PIC X(16) VALUE '0123456789ABCDEF'.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR PIC X(1) OCCURS 16.\n       01  REDEFINES MY-HEX-SHOW-CHARS.\n              05 MY-HEX-SHOW-CHAR2 PIC X(1) OCCURS 16.\n       PROCEDURE DIVISION\n       .\n", contentHash='1', name="PRGD", path="src/cobol/programs/PRGD.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="IOSCOPE", path="src/cobol/programs/IOSCOPE.cbl", description="A test program", sourceAttachmentLink=(INSERT INTO SourceObject SET content="******** A                                       \n        IDENTIFICATION DIVISION.                  \n        PROGRAM-ID. A.                            \n        ENVIRONMENT DIVISION.                     \n        CONFIGURATION SECTION.                    \n        INPUT-OUTPUT SECTION.                     \n        FILE-CONTROL.                             \n            SELECT KSDSFILE ASSIGN TO  \"KSD\"    \n                ORGANIZATION  IS  INDEXED         \n                ACCESS  MODE  IS  DYNAMIC         \n                RECORD  KEY   IS  PKEY            \n                FILE STATUS   IS  WS-FS.          \n        DATA DIVISION.                            \n        FILE SECTION.                             \n        FD KSDSFILE RECORD CONTAINS 80 CHARACTERS.\n        01  KSDSFILE-REC.                         \n            05  PKEY                PIC X(6).     \n            05  INFO                PIC X(74).    \n        WORKING-STORAGE SECTION.                  \n        01  MY-PROGRAM-NAME PIC X(10) VALUE 'A'.  \n        LINKAGE SECTION.                          \n        PROCEDURE DIVISION.                       \n                                                  \n                                                  \n", contentHash='1', name="IOSCOPE", path="src/cobol/programs/IOSCOPE.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="UISCOPE", path="src/cobol/maps/UISCOPE.map", description="A test map", sourceAttachmentLink=(INSERT INTO SourceObject SET content="AI0103M DFHMSD TYPE=&SYSPARM,LANG=COBOL,TIOAPFX=YES,MODE=INOUT,        X\n               STORAGE=AUTO,CTRL=FREEKB                                 \nAI01003 DFHMDI SIZE=(24,80),LINE=1,COLUMN=1,JUSTIFY=LEFT,              X\n               MAPATTS=(COLOR,HILIGHT)                                  \n        DFHMDF POS=(2,3),LENGTH=7,INITIAL='AI01003',ATTRB=ASKIP         \n        DFHMDF POS=(2,65),LENGTH=7,INITIAL='AI01-03',                  X\n               ATTRB=ASKIP                                              \nPRDDS   DFHMDF POS=(4,5),LENGTH=50,ATTRB=PROT                           \n        DFHMDF POS=(4,57),LENGTH=1,ATTRB=ASKIP                          \nISSCO2  DFHMDF POS=(5,18),LENGTH=30,ATTRB=PROT                          \n        DFHMDF POS=(5,51),LENGTH=1,ATTRB=ASKIP                          \nAPPTST2 DFHMDF POS=(6,10),LENGTH=20,ATTRB=PROT                          \n        DFHMDF POS=(6,32),LENGTH=1,ATTRB=ASKIP                          \n        DFHMDF POS=(9,5),LENGTH=15,ATTRB=ASKIP,                        X\n               INITIAL='---------------'                                \n        DFHMDF POS=(9,35),LENGTH=12,ATTRB=ASKIP,                       X\n               INITIAL='------------'                                   \n", contentHash='1', name="UISCOPE", path="src/cobol/maps/UISCOPE.map", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="BMS_MAPSET") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="DBSCOPE", path="src/cobol/programs/DBSCOPE.cbl", description="A test program", sourceAttachmentLink=(INSERT INTO SourceObject SET content="******** A                                                \n        IDENTIFICATION DIVISION.                           \n        PROGRAM-ID. A.                                     \n        ENVIRONMENT DIVISION.                              \n        CONFIGURATION SECTION.                             \n        DATA DIVISION.                                     \n        WORKING-STORAGE SECTION.                           \n        01  DCL.                                           \n            10 COORDINATOR-TYPE     PIC X(1).              \n            10 ANNUAL-DELIV-FEE     PIC S9(4) USAGE COMP.  \n            10 FREE-DELIVERY-IND    PIC X(1).              \n         01  W-SUBJECT-ID           PIC S9(9) COMP.        \n        LINKAGE SECTION.                                   \n        PROCEDURE DIVISION.                                \n        0000-INITIALIZE.                                   \n            EXEC SQL                                       \n                SELECT TABLE1.COORDINATOR_TYPE,            \n                   TABLE1.ANNUAL_DELIV_FEE,                \n                   TABLE1.FREE_DELIVERY_IND                \n                   INTO :DCL.COORDINATOR-TYPE,             \n                        :DCL.ANNUAL-DELIV-FEE,             \n                        :DCL.FREE-DELIVERY-IND             \n                   FROM TABLE1                             \n                   WHERE TABLE1.SUBJECT_ID = :W-SUBJECT-ID \n            END-EXEC.                                      \n                                                           \n ", contentHash='1', name="DBSCOPE", path="src/cobol/programs/DBSCOPE.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="PARAM", path="src/cobol/programs/PARAM.cbl", description="A test program", sourceAttachmentLink=(INSERT INTO SourceObject SET content="******** A                                                       \n        IDENTIFICATION DIVISION.                                  \n        PROGRAM-ID. A.                                            \n        ENVIRONMENT DIVISION.                                     \n        CONFIGURATION SECTION.                                    \n        DATA DIVISION.                                            \n        WORKING-STORAGE SECTION.                                  \n         01  W-SUBJECT-ID           PIC S9(9) COMP.               \n        LINKAGE SECTION.                                          \n        01  DFHCOMMAREA.                                          \n            02  LK-FILLER.                                        \n                03  LK-FILLER03                     PIC X(1)      \n                   OCCURS 1 TO 25000 TIMES DEPENDING ON EIBCALEN. \n        PROCEDURE DIVISION.                                       \n        0000-INITIALIZE.                                          \n                                                                  \n ", contentHash='1', name="PARAM", path="src/cobol/programs/PARAM.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="EXECSQL", path="src/cobol/programs/EXECSQL.cbl", description="A Cobol program with some EXEC SQL statements.", sourceAttachmentLink=(INSERT INTO SourceObject SET content="       IDENTIFICATION DIVISION.\n       PROGRAM-ID.    EXECSQL.\n       ENVIRONMENT DIVISION.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n           EXEC SQL DECLARE MMRS00C_AWA_VSAMK TABLE\n             (\n               KSDS_PRIMARY_INDEX   VARCHAR(10)\n             )\n           END-EXEC\n      *    ************************************************************\n           EXEC SQL\n             DECLARE   C-VSAMK    CURSOR FOR\n               SELECT    KSDS_PRIMARY_INDEX\n               FROM      MMRS00C_AWA_VSAMK AS VSAMK\n               ORDER BY  VSAMK.KSDS_PRIMARY_INDEX ASC\n               FOR       FETCH ONLY\n           END-EXEC\n       PROCEDURE DIVISION.\n           GOBACK.\n", contentHash='1', name="EXECSQL", path="src/cobol/programs/EXECSQL.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE id=1) RETURN @rid), projectLink=(SELECT FROM Project WHERE id=1), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"

INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="PROG1", path="src/cobol/programs/PROG1.cbl", description="A test program including CC1 from A", sourceAttachmentLink=(INSERT INTO SourceObject SET content="       PROCEDURE DIVISION.\n           DISPLAY 'PROG1'\n           COPY CC1\n           .\n", contentHash='1', name="PROG1", path="src/cobol/programs/PROG1.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="PROG2", path="src/cobol/programs/PROG2.cbl", description="A test program including CC1 from B", sourceAttachmentLink=(INSERT INTO SourceObject SET content="       PROCEDURE DIVISION.\n           DISPLAY 'PROG2'\n           COPY CC1\n           .\n", contentHash='1', name="PROG2", path="src/cobol/programs/PROG2.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROGRAM") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="CC1", path="src/cobol/copies/A/CC1.cpy", description="A test copy for PROG1", sourceAttachmentLink=(INSERT INTO SourceObject SET content="           DISPLAY 'CC1 from A'\n           COPY CC2\n", contentHash='1', name="CC1", path="src/cobol/copies/A/CC1.cpy", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="COPYBOOK") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="CC1", path="src/cobol/copies/B/CC1.cpy", description="A test copy for PROG2", sourceAttachmentLink=(INSERT INTO SourceObject SET content="           DISPLAY 'CC1 from B'\n", contentHash='1', name="CC1", path="src/cobol/copies/B/CC1.cpy", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="COPYBOOK") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="CC2", path="src/cobol/copies/A/CC2.cpy", description="A nested test copy for src/cobol/copies/A/CC1.cpy", sourceAttachmentLink=(INSERT INTO SourceObject SET content="           DISPLAY 'CC2 from A'\n", contentHash='1', name="CC2", path="src/cobol/copies/A/CC2.cpy", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="COPYBOOK") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"

INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="JOB1", path="src/jobs/JOB1.job", description="A test job including MYPROC from A", sourceAttachmentLink=(INSERT INTO SourceObject SET content="//PDSCRTJ3 JOB IW,ACCOUNT,CLASS=1,MSGCLASS=0,NOTIFY=CSIP1\n//MYBE INCLUDE MEMBER=MYB\n//S1       EXEC MYPROC\n", contentHash='1', name="JOB1", path="src/jobs/JOB1.job", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='JCL'),typeLink = (SELECT FROM TypeEnum WHERE name='JOB'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="JOB") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="JOB2", path="src/jobs/JOB2.job", description="A test job including MYPROC from B", sourceAttachmentLink=(INSERT INTO SourceObject SET content="//PDSCRTJ3 JOB IW,ACCOUNT,CLASS=1,MSGCLASS=0,NOTIFY=CSIP1\n//MYBE INCLUDE MEMBER=MYB\n//S1       EXEC MYPROC\n", contentHash='1', name="JOB2", path="src/jobs/JOB2.job", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='JCL'),typeLink = (SELECT FROM TypeEnum WHERE name='JOB'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="JOB") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="MYPROC", path="src/procs/A/MYPROC.proc", description="A test proc for JOB1", sourceAttachmentLink=(INSERT INTO SourceObject SET content="//PDSCRTP3 PROC\n//PDSCRTS1 EXEC PGM=IEFBR14\n//TEMPLIB1 DD  DISP=(NEW,CATLG)\n//PDSCRTS2 EXEC PGM=IEFBR14\n//TEMPLIB1 DD  DISP=(NEW,CATLG)\n//S34       EXEC MYNEST\n", contentHash='1', name="MYPROC", path="src/procs/A/MYPROC.proc", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='JCL'),typeLink = (SELECT FROM TypeEnum WHERE name='JOB'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROC") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="MYPROC", path="src/procs/B/MYPROC.proc", description="A test proc for JOB2", sourceAttachmentLink=(INSERT INTO SourceObject SET content="//PDSCRTP3 PROC\n//PDSCRTS1 EXEC PGM=IEFBR14\n//TEMPLIB1 DD  DISP=(NEW,CATLG)\n", contentHash='1', name="MYPROC", path="src/procs/B/MYPROC.proc", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='JCL'),typeLink = (SELECT FROM TypeEnum WHERE name='JOB'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROC") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="MYNEST", path="src/procs/A/MYNEST.proc", description="A nested test proc for JOB1", sourceAttachmentLink=(INSERT INTO SourceObject SET content="//NESTSTEP1  EXEC  PGM=IEBGENER\n", contentHash='1', name="MYNEST", path="src/procs/A/MYNEST.proc", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='JCL'),typeLink = (SELECT FROM TypeEnum WHERE name='JOB'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="PROC") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="MYB", path="src/incs/A/MYB.inc", description="A test include member for JOB1.", sourceAttachmentLink=(INSERT INTO SourceObject SET content="//GENSTEP001  EXEC  PGM=IEBGENER\n//GENSTEP002  EXEC  PGM=IEBGENER\n", contentHash='1', name="MYB", path="src/incs/A/MYB.inc", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='JCL'),typeLink = (SELECT FROM TypeEnum WHERE name='JOB'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="INCLUDE") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"
INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="MYB", path="src/incs/B/MYB.inc", description="A test include member for JOB2.", sourceAttachmentLink=(INSERT INTO SourceObject SET content="//GENSTEP001  EXEC  PGM=IEBGENER\n", contentHash='1', name="MYB", path="src/incs/B/MYB.inc", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='JCL'),typeLink = (SELECT FROM TypeEnum WHERE name='JOB'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="JCL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="INCLUDE") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"

INSERT INTO Module SET id=sequence('Module_Sequence').next(), uid=sequence('ModuleUid_Sequence').next(), name="DIFTYPE", path="/src/test/resources/innowake/mining/data/plugin/test/annotations/database/differenttype/DIFTYPE.cpy", description="A test copy for MMRS7120", sourceAttachmentLink=(INSERT INTO SourceObject SET content="           EXEC SQL DECLARE VSAMK TABLE (KSDS VARCHAR(10)) END-EXEC\n", contentHash='1', name="MMRS7101", path="src/cobol/programs/MMRS7101.cbl", id=sequence('SourceObject_Sequence').next(), contentRevision=1, metaDataRevision=1, technologyLink=(SELECT FROM TechnologyEnum WHERE name='COBOL'),typeLink = (SELECT FROM TypeEnum WHERE name='PROGRAM'), projectLink=(SELECT FROM Project WHERE name='Demo Project C') RETURN @rid), projectLink=(SELECT FROM Project WHERE name='Demo Project C'), objectTypeLink=(SELECT FROM ObjectType WHERE technologyLink IN (SELECT FROM TechnologyEnum WHERE name="COBOL") AND typeLink IN (SELECT FROM TypeEnum WHERE name="COPYBOOK") AND storageLink IN (SELECT FROM StorageEnum WHERE name="FILE") AND originLink IN (SELECT FROM OriginEnum WHERE name="CUSTOM")), identificationLink=(SELECT FROM IdentificationEnum WHERE name="IDENTIFIED"), creator="DISCOVERY"

###################################################################################################################################
# TEST TAXONOMY
###################################################################################################################################
INSERT INTO TaxonomyEnum SET name="DataDomain", projectLink=(SELECT FROM Project WHERE id=1), categoryLink = projectLink.defaultTaxonomyCategoryLink;
INSERT INTO TaxonomyEnum SET name="BusinessProcess", projectLink=(SELECT FROM Project WHERE id=1), categoryLink = projectLink.defaultTaxonomyCategoryLink;
INSERT INTO TaxonomyEnum SET name="BusinessSubsystem", projectLink=(SELECT FROM Project WHERE id=1), categoryLink = projectLink.defaultTaxonomyCategoryLink;
INSERT INTO Taxonomy SET id=sequence('Taxonomy_Sequence').next(), name="Employee domain", projectLink=(SELECT FROM Project WHERE id=1), typeLink=(SELECT FROM TaxonomyEnum WHERE name="DataDomain" AND projectLink.id=1)
INSERT INTO Taxonomy SET id=sequence('Taxonomy_Sequence').next(), name="Create Invoices", projectLink=(SELECT FROM Project WHERE id=1), typeLink=(SELECT FROM TaxonomyEnum WHERE name="BusinessProcess" AND projectLink.id=1)
INSERT INTO Taxonomy SET id=sequence('Taxonomy_Sequence').next(), name="ARB100", projectLink=(SELECT FROM Project WHERE id=1), typeLink=(SELECT FROM TaxonomyEnum WHERE name="BusinessSubsystem" AND projectLink.id=1)

###################################################################################################################################
# TEST ANNOTATION CATEGORY
###################################################################################################################################
INSERT INTO AnnotationCategory SET id=sequence('AnnotationCategory_Sequence').next(), name="Annotation Category A", projectLink=(SELECT FROM Project WHERE id=1)
INSERT INTO AnnotationCategory SET id=sequence('AnnotationCategory_Sequence').next(), name="Annotation Category B", projectLink=(SELECT FROM Project WHERE id=1)
INSERT INTO AnnotationCategory SET id=sequence('AnnotationCategory_Sequence').next(), name="Annotation Category C", projectLink=(SELECT FROM Project WHERE id=2)
INSERT INTO AnnotationCategory SET id=sequence('AnnotationCategory_Sequence').next(), name="Annotation Category D", projectLink=(SELECT FROM Project WHERE id=2)
INSERT INTO AnnotationCategory SET id=sequence('AnnotationCategory_Sequence').next(), name="Annotation Category E", projectLink=(SELECT FROM Project WHERE id=3)

###################################################################################################################################
# TEST ANNOTATION
###################################################################################################################################
INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Annotation 1", projectLink=(SELECT FROM Project WHERE id=1), stateLink=(SELECT FROM WorkingStateEnum WHERE name="CANDIDATE"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="RULE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category A"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="abcd" RETURN @rid), createdByUserId="admin"
INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Annotation 2", projectLink=(SELECT FROM Project WHERE id=1), stateLink=(SELECT FROM WorkingStateEnum WHERE name="CANDIDATE"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="RULE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category A"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="1234" RETURN @rid), createdByUserId="admin"
INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Annotation 3", projectLink=(SELECT FROM Project WHERE id=1), stateLink=(SELECT FROM WorkingStateEnum WHERE name="CANDIDATE"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="RULE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category A"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="efgh" RETURN @rid), createdByUserId="admin"
INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Annotation 4", projectLink=(SELECT FROM Project WHERE id=1), stateLink=(SELECT FROM WorkingStateEnum WHERE name="CANDIDATE"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="RULE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category A"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="5678" RETURN @rid), createdByUserId="admin"
INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Annotation 5", projectLink=(SELECT FROM Project WHERE id=1), stateLink=(SELECT FROM WorkingStateEnum WHERE name="IN_ANALYSIS"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="RULE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category B"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="5678" RETURN @rid), createdByUserId="admin"
INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Annotation 6", projectLink=(SELECT FROM Project WHERE id=1), stateLink=(SELECT FROM WorkingStateEnum WHERE name="REJECTED"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="RULE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category C"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="5678" RETURN @rid), createdByUserId="admin"
INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Database Annotation 1", projectLink=(SELECT FROM Project WHERE id=1), stateLink=(SELECT FROM WorkingStateEnum WHERE name="CANDIDATE"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="DATABASE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category B"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="IDENTIFICATION" RETURN @rid), createdByUserId="system_user", updatedByUserId="admin"
INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Database Annotation 2", projectLink=(SELECT FROM Project WHERE id=1), stateLink=(SELECT FROM WorkingStateEnum WHERE name="CANDIDATE"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="DATABASE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category B"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="GOBACK." RETURN @rid), createdByUserId="system_user", updatedByUserId="system_user"

INSERT INTO Annotation SET id=sequence('Annotation_Sequence').next(), name="Rule annotation 1", projectLink=(SELECT FROM Project WHERE name='Demo Project C'), stateLink=(SELECT FROM WorkingStateEnum WHERE name="CANDIDATE"), typeLink=(SELECT FROM AnnotationTypeEnum WHERE name="RULE"), categoryLink=(SELECT FROM AnnotationCategory WHERE name="Annotation Category E"), sourceAttachmentLink=(INSERT INTO SourceAttachment SET content="IRRELEVANT CONTENT" RETURN @rid), createdByUserId="system_user", updatedByUserId="system_user"

###################################################################################################################################
# TEST HAS_ANNOTATION
###################################################################################################################################
CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE id=2000) TO (SELECT FROM Annotation WHERE id=1) SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:100, length:4 }
CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE id=2000) TO (SELECT FROM Annotation WHERE id=2) SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:200, length:4 }
CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE id=2000) TO (SELECT FROM Annotation WHERE id=3) SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:300, length:4 }
CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE id=2000) TO (SELECT FROM Annotation WHERE id=4) SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:400, length:4 }
CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE id=2001) TO (SELECT FROM Annotation WHERE id=5) SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:500, length:5 }
CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE id=2002) TO (SELECT FROM Annotation WHERE id=6) SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:600, length:6 }
CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE name='EXECSQL') TO (SELECT FROM Annotation WHERE name="Database Annotation 1") SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:7, length:14 }
CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE name='EXECSQL') TO (SELECT FROM Annotation WHERE name="Database Annotation 2") SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:677, length:7 }

CREATE EDGE HasAnnotation FROM (SELECT FROM Module WHERE name='DIFTYPE') TO (SELECT FROM Annotation WHERE name="Rule annotation 1") SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:11, length:57 }

###################################################################################################################################
# TEST DATA DICTIONARY ENTRY
###################################################################################################################################
INSERT INTO DataDictionaryEntry SET id=sequence('DataDictionaryEntry_Sequence').next(), dataElementName="MY-PROGRAM-NAME", description="This is an english description of the data element name MY-PROGRAM-NAME", scopeLink=(SELECT FROM DataDictionaryVariableScopeEnum WHERE name IN ["SQL_DATABASE", "CICS_UI"]), format="PICX", createdByUserId="admin", length=15, scopeAttributes={"SQL_DATABASE":{"attributes":{"tables":"insert table names here"},"@class":"DataDictionaryEntryScopeAttributes"},"CICS_UI":{"attributes":{"mapname":"mapnome","mapset":"mapsot"},"@class":"DataDictionaryEntryScopeAttributes"}}
INSERT INTO DataDictionaryEntry SET id=sequence('DataDictionaryEntry_Sequence').next(), dataElementName="MY-BIN-FIELDS", description="This is an english description of the data element name MY-BIN-FIELDS", scopeLink=(SELECT FROM DataDictionaryVariableScopeEnum WHERE name IN ["FILE"]), format="GROUP", createdByUserId="admin", length=13, scopeAttributes={"FILE":{"attributes":{"dataset":"insert your favorite dataset names here"},"@class":"DataDictionaryEntryScopeAttributes"}}
INSERT INTO DataDictionaryEntry SET id=sequence('DataDictionaryEntry_Sequence').next(), dataElementName="MY-HEX-ORIGIN-LEN", description="This is an english description of the data element name MY-HEX-ORIGIN-LEN", format="PIC9", createdByUserId="admin", length=17

###################################################################################################################################
# TEST DATA DICTIONARY OTHER SCOPE ENUM
###################################################################################################################################
INSERT INTO DataDictionaryOtherScopeEnum SET name="SCOPE_1", projectLink=(SELECT FROM Project WHERE id=1) 
INSERT INTO DataDictionaryOtherScopeEnum SET name="SCOPE_2", projectLink=(SELECT FROM Project WHERE id=1) 

###################################################################################################################################
# TEST HasDataDictionaryEntry
###################################################################################################################################
CREATE EDGE HasDataDictionaryEntry FROM (SELECT FROM Module WHERE path="src/cobol/programs/MMRS7101.cbl") TO (SELECT FROM DataDictionaryEntry WHERE dataElementName="MY-PROGRAM-NAME") SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:1005, length:15 }
CREATE EDGE HasDataDictionaryEntry FROM (SELECT FROM Module WHERE path="src/cobol/programs/MMRS7101.cbl") TO (SELECT FROM DataDictionaryEntry WHERE dataElementName="MY-BIN-FIELDS") SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:1250, length:13 }
CREATE EDGE HasDataDictionaryEntry FROM (SELECT FROM Module WHERE path="src/cobol/programs/MMRS7101.cbl") TO (SELECT FROM DataDictionaryEntry WHERE dataElementName="MY-HEX-ORIGIN-LEN") SET id=sequence('Reference_Sequence').next(), fromModuleLocation={ offset:1862, length:17 }

###################################################################################################################################
# TEST CUSTOM PROPERTIES CLIENT
###################################################################################################################################
CREATE PROPERTY Client.customClientProperty IF NOT EXISTS String
ALTER PROPERTY Client.customClientProperty CUSTOM label="Custom Client Property"
ALTER PROPERTY Client.customClientProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientProperty description "A custom property for the Client class"
UPDATE Client SET customClientProperty="A value for the custom client property" WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientPropertyWithMinMax IF NOT EXISTS String
ALTER PROPERTY Client.customClientPropertyWithMinMax CUSTOM label="Custom Client Property With Min Max"
ALTER PROPERTY Client.customClientPropertyWithMinMax CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientPropertyWithMinMax description "A custom property with min max for the Client class"
ALTER PROPERTY Client.customClientPropertyWithMinMax MIN 5
ALTER PROPERTY Client.customClientPropertyWithMinMax MAX 10
UPDATE Client SET customClientPropertyWithMinMax="12345" WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientReadOnlyProperty IF NOT EXISTS String
ALTER PROPERTY Client.customClientReadOnlyProperty CUSTOM label="Custom Client Read Only Property"
ALTER PROPERTY Client.customClientReadOnlyProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientReadOnlyProperty description "A custom read only property for the Client class"
ALTER PROPERTY Client.customClientReadOnlyProperty READONLY TRUE
UPDATE Client SET customClientReadOnlyProperty="A value for the custom client read only property" WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientBooleanProperty IF NOT EXISTS Boolean
ALTER PROPERTY Client.customClientBooleanProperty CUSTOM label="Custom Client Boolean Property"
ALTER PROPERTY Client.customClientBooleanProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientBooleanProperty description "A custom Boolean property for the Client class"
UPDATE Client SET customClientBooleanProperty=true WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientIntegerProperty IF NOT EXISTS Integer
ALTER PROPERTY Client.customClientIntegerProperty CUSTOM label="Custom Client Integer Property"
ALTER PROPERTY Client.customClientIntegerProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientIntegerProperty description "A custom Integer property for the Client class"
UPDATE Client SET customClientIntegerProperty=1 WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientShortProperty IF NOT EXISTS Short
ALTER PROPERTY Client.customClientShortProperty CUSTOM label="Custom Client Short Property"
ALTER PROPERTY Client.customClientShortProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientShortProperty description "A custom Short property for the Client class"
UPDATE Client SET customClientShortProperty=1 WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientLongProperty IF NOT EXISTS Long
ALTER PROPERTY Client.customClientLongProperty CUSTOM label="Custom Client Long Property"
ALTER PROPERTY Client.customClientLongProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientLongProperty description "A custom Long property for the Client class"
UPDATE Client SET customClientLongProperty=1 WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientFloatProperty IF NOT EXISTS Float
ALTER PROPERTY Client.customClientFloatProperty CUSTOM label="Custom Client Float Property"
ALTER PROPERTY Client.customClientFloatProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientFloatProperty description "A custom Float property for the Client class"
UPDATE Client SET customClientFloatProperty=1.1 WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientDoubleProperty IF NOT EXISTS Double
ALTER PROPERTY Client.customClientDoubleProperty CUSTOM label="Custom Client Double Property"
ALTER PROPERTY Client.customClientDoubleProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientDoubleProperty description "A custom Double property for the Client class"
UPDATE Client SET customClientDoubleProperty=1.1 WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientDateProperty IF NOT EXISTS Date
ALTER PROPERTY Client.customClientDateProperty CUSTOM label="Custom Client Date Property"
ALTER PROPERTY Client.customClientDateProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientDateProperty description "A custom Date property for the Client class"
UPDATE Client SET customClientDateProperty="2019-07-09" WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientDatetimeProperty IF NOT EXISTS Datetime
ALTER PROPERTY Client.customClientDatetimeProperty CUSTOM label="Custom Client Datetime Property"
ALTER PROPERTY Client.customClientDatetimeProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientDatetimeProperty description "A custom Datetime property for the Client class"
UPDATE Client SET customClientDatetimeProperty="2019-07-09 01:23:45" WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientByteProperty IF NOT EXISTS Byte
ALTER PROPERTY Client.customClientByteProperty CUSTOM label="Custom Client Byte Property"
ALTER PROPERTY Client.customClientByteProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientByteProperty description "A custom Byte property for the Client class"
UPDATE Client SET customClientByteProperty="1" WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientDecimalProperty IF NOT EXISTS Decimal
ALTER PROPERTY Client.customClientDecimalProperty CUSTOM label="Custom Client Decimal Property"
ALTER PROPERTY Client.customClientDecimalProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientDecimalProperty description "A custom Decimal property for the Client class"
UPDATE Client SET customClientDecimalProperty="1.1" WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientReferenceProperty IF NOT EXISTS LINK Client
ALTER PROPERTY Client.customClientReferenceProperty CUSTOM label="Custom Client Reference Property"
ALTER PROPERTY Client.customClientReferenceProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientReferenceProperty description "A custom Reference property for the Client class"
UPDATE Client SET customClientReferenceProperty=(SELECT FROM Client WHERE name="Demo Client 2") WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientListProperty IF NOT EXISTS LINKLIST Client
ALTER PROPERTY Client.customClientListProperty CUSTOM label="Custom Client List Property"
ALTER PROPERTY Client.customClientListProperty CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientListProperty description "A custom List property for the Client class"
UPDATE Client SET customClientListProperty=(SELECT FROM Client WHERE name="SYSTEM") WHERE name="Demo Client 1"

CREATE PROPERTY Client.customClientListPropertyWithMax IF NOT EXISTS LINKLIST Client
ALTER PROPERTY Client.customClientListPropertyWithMax CUSTOM label="Custom Client List PropertyWithMax"
ALTER PROPERTY Client.customClientListPropertyWithMax CUSTOM pluginVisible=true
ALTER PROPERTY Client.customClientListPropertyWithMax description "A custom List property with max validation for the Client class"
ALTER PROPERTY Client.customClientListPropertyWithMax MAX 1
UPDATE Client SET customClientListPropertyWithMax=(SELECT FROM Client WHERE name="SYSTEM") WHERE name="Demo Client 1"

###################################################################################################################################
# TEST CUSTOM PROPERTIES PROJECT
###################################################################################################################################
CREATE PROPERTY Project.customProjectProperty IF NOT EXISTS String
ALTER PROPERTY Project.customProjectProperty CUSTOM label="Custom Project Property"
ALTER PROPERTY Project.customProjectProperty CUSTOM pluginVisible=true
ALTER PROPERTY Project.customProjectProperty description "A custom property for the Project class"
UPDATE Project SET customProjectProperty="A value for the custom project property" WHERE name="Demo Project A"

###################################################################################################################################
# TEST CUSTOM PROPERTIES DataDictionaryEntry
###################################################################################################################################
CREATE PROPERTY DataDictionaryEntry.customDataDictionaryEntryProperty IF NOT EXISTS String
ALTER PROPERTY DataDictionaryEntry.customDataDictionaryEntryProperty CUSTOM label="Custom DataDictionaryEntry Property"
ALTER PROPERTY DataDictionaryEntry.customDataDictionaryEntryProperty CUSTOM pluginVisible=true
ALTER PROPERTY DataDictionaryEntry.customDataDictionaryEntryProperty description "A custom property for the DataDictionaryEntry class"
UPDATE DataDictionaryEntry SET customDataDictionaryEntryProperty="A value for the custom DataDictionaryEntry property" WHERE dataElementName="FOO"

###################################################################################################################################
# TEST CUSTOM PROPERTIES TaxonomyEnum
###################################################################################################################################
CREATE PROPERTY TaxonomyEnum.customTaxonomyEnumProperty IF NOT EXISTS String
ALTER PROPERTY TaxonomyEnum.customTaxonomyEnumProperty CUSTOM label="Custom TaxonomyEnum Property"
ALTER PROPERTY TaxonomyEnum.customTaxonomyEnumProperty CUSTOM pluginVisible=true
ALTER PROPERTY TaxonomyEnum.customTaxonomyEnumProperty description "A custom property for the TaxonomyEnum class"
UPDATE TaxonomyEnum SET customTaxonomyEnumProperty="A value for the custom TaxonomyEnum property" WHERE name="DataDomain"

###################################################################################################################################
# TEST CUSTOM PROPERTIES AnnotationTypeEnum
###################################################################################################################################
CREATE PROPERTY AnnotationTypeEnum.customAnnotationTypeEnumProperty IF NOT EXISTS String
ALTER PROPERTY AnnotationTypeEnum.customAnnotationTypeEnumProperty CUSTOM label="Custom AnnotationTypeEnum Property"
ALTER PROPERTY AnnotationTypeEnum.customAnnotationTypeEnumProperty CUSTOM pluginVisible=true
ALTER PROPERTY AnnotationTypeEnum.customAnnotationTypeEnumProperty description "A custom property for the AnnotationTypeEnum class"
UPDATE AnnotationTypeEnum SET customAnnotationTypeEnumProperty="A value for the custom AnnotationTypeEnum property" WHERE name="RULE"

###################################################################################################################################
# TEST CUSTOM PROPERTIES Taxonomy
###################################################################################################################################
CREATE PROPERTY Taxonomy.customTaxonomyProperty IF NOT EXISTS String
ALTER PROPERTY Taxonomy.customTaxonomyProperty CUSTOM label="Custom Taxonomy Property"
ALTER PROPERTY Taxonomy.customTaxonomyProperty CUSTOM pluginVisible=true
ALTER PROPERTY Taxonomy.customTaxonomyProperty description "A custom property for the Taxonomy class"
UPDATE Taxonomy SET customTaxonomyProperty="A value for the custom Taxonomy property" WHERE name="Employee domain"

###################################################################################################################################
# TEST CUSTOM PROPERTIES ANNOTATION
###################################################################################################################################
CREATE PROPERTY Annotation.dependentAnnotations IF NOT EXISTS LINKLIST Annotation
ALTER PROPERTY Annotation.dependentAnnotations CUSTOM label="Dependent Annotations"
ALTER PROPERTY Annotation.dependentAnnotations CUSTOM pluginVisible=true
ALTER PROPERTY Annotation.dependentAnnotations description "Choose zero, one or more dependend annotations."
UPDATE Annotation SET dependentAnnotations=(SELECT FROM Annotation WHERE name="Annotation 2" OR name="Annotation 3") WHERE name="Annotation 1"

CREATE PROPERTY Annotation.customMetaInfo IF NOT EXISTS String
ALTER PROPERTY Annotation.customMetaInfo MANDATORY FALSE
ALTER PROPERTY Annotation.customMetaInfo MIN 5
ALTER PROPERTY Annotation.customMetaInfo MAX 30
ALTER PROPERTY Annotation.customMetaInfo READONLY TRUE
ALTER PROPERTY Annotation.customMetaInfo description "This is some more custom meta information"
ALTER PROPERTY Annotation.customMetaInfo CUSTOM label="Some custom meta information"
ALTER PROPERTY Annotation.customMetaInfo CUSTOM pluginVisible=false
ALTER PROPERTY Annotation.customMetaInfo CUSTOM dataSource="Custom datasource URL"
UPDATE Annotation SET customMetaInfo="some custom meta value" WHERE name="Annotation 1"

CREATE PROPERTY Annotation.customAnnotationProperty IF NOT EXISTS String
ALTER PROPERTY Annotation.customAnnotationProperty CUSTOM label="Custom Annotation Property"
ALTER PROPERTY Annotation.customAnnotationProperty CUSTOM pluginVisible=true
ALTER PROPERTY Annotation.customAnnotationProperty description "A custom property for the Annotation class"
UPDATE Annotation SET customAnnotationProperty="A value for the custom Annotation property" WHERE name="Annotation 1"

CREATE PROPERTY Annotation.referencedTables IF NOT EXISTS LINKLIST Module (MANDATORY FALSE)
ALTER PROPERTY Annotation.referencedTables CUSTOM label="Referenced Tables"
ALTER PROPERTY Annotation.referencedTables CUSTOM pluginVisible=true
ALTER PROPERTY Annotation.referencedTables description "Contains a list of all referenced tables in the query" 

###################################################################################################################################
# TEST CUSTOM PROPERTIES AnnotationCategory
###################################################################################################################################
CREATE PROPERTY AnnotationCategory.customAnnotationCategoryProperty IF NOT EXISTS String
ALTER PROPERTY AnnotationCategory.customAnnotationCategoryProperty CUSTOM label="Custom AnnotationCategory Property"
ALTER PROPERTY AnnotationCategory.customAnnotationCategoryProperty CUSTOM pluginVisible=true
ALTER PROPERTY AnnotationCategory.customAnnotationCategoryProperty description "A custom property for the AnnotationCategory class"
UPDATE AnnotationCategory SET customAnnotationCategoryProperty="A value for the custom AnnotationCategory property" WHERE name="Annotation Category A"

###################################################################################################################################
# TEST CUSTOM PROPERTIES MODULE
###################################################################################################################################
CREATE PROPERTY Module.customMetaInfo1 IF NOT EXISTS String
ALTER PROPERTY Module.customMetaInfo1 MANDATORY FALSE
ALTER PROPERTY Module.customMetaInfo1 MIN 5
ALTER PROPERTY Module.customMetaInfo1 MAX 26
ALTER PROPERTY Module.customMetaInfo1 READONLY TRUE
ALTER PROPERTY Module.customMetaInfo1 description "This is some more custom meta information 1"
ALTER PROPERTY Module.customMetaInfo1 CUSTOM label="Some custom meta information 1"
ALTER PROPERTY Module.customMetaInfo1 CUSTOM pluginVisible=false
ALTER PROPERTY Module.customMetaInfo1 CUSTOM dataSource="Custom datasource URL 1"

CREATE PROPERTY Module.customMetaInfo2 IF NOT EXISTS String
ALTER PROPERTY Module.customMetaInfo2 description "This is some more custom meta information 2"
ALTER PROPERTY Module.customMetaInfo2 CUSTOM label="Some custom meta information 2"
ALTER PROPERTY Module.customMetaInfo2 CUSTOM pluginVisible=false
ALTER PROPERTY Module.customMetaInfo2 CUSTOM dataSource="Custom datasource URL 2"

UPDATE Module SET customMetaInfo1="some custom meta 1 value" WHERE name="IDCAMS"
UPDATE Module SET customMetaInfo2="some custom meta 2 value" WHERE name="IDCAMS"

###################################################################################################################################
# TEST CUSTOM PROPERTIES Reference
###################################################################################################################################
CREATE PROPERTY Reference.customReferenceProperty IF NOT EXISTS String
ALTER PROPERTY Reference.customReferenceProperty CUSTOM label="Custom Reference Property"
ALTER PROPERTY Reference.customReferenceProperty CUSTOM pluginVisible=true
ALTER PROPERTY Reference.customReferenceProperty description "A custom property for the Reference class"

###################################################################################################################################
# TEST OAUTHTOKEN UPDATION FOR NEW USER POST LOGIN
###################################################################################################################################
DELETE FROM oauth_access_token WHERE user_name = "test-admin";
DELETE FROM User WHERE userId="test-admin"
INSERT INTO User SET userId="test-admin", password="$2a$10$.1stH06O0tY5jQQDG2vfn.aMmoXLhYgR8jrLNnpoKNSi3gHou9sva"

###################################################################################################################################
# TEST HAS TAXONOMY
###################################################################################################################################
CREATE EDGE HasTaxonomy FROM (SELECT FROM Module WHERE id=2000) TO (SELECT FROM Taxonomy WHERE name="ARB100") SET id=sequence('Reference_Sequence').next()
CREATE EDGE HasTaxonomy FROM (SELECT FROM Module WHERE id=2000) TO (SELECT FROM Taxonomy WHERE name="Employee domain") SET id=sequence('Reference_Sequence').next()
CREATE EDGE HasTaxonomy FROM (SELECT FROM Module WHERE id=2001) TO (SELECT FROM Taxonomy WHERE name="Employee domain") SET id=sequence('Reference_Sequence').next()

###################################################################################################################################
# TEST HAS DEPENDENCYGRAPHLINKS
###################################################################################################################################
CREATE EDGE Calls FROM (SELECT FROM Module WHERE id=2000) TO (SELECT FROM Module WHERE id=2001) SET id=sequence('Reference_Sequence').next()
CREATE EDGE Calls FROM (SELECT FROM Module WHERE id=2000) TO (SELECT FROM Module WHERE id=2002) SET id=sequence('Reference_Sequence').next()
CREATE EDGE Calls FROM (SELECT FROM Module WHERE id=2003) TO (SELECT FROM Module WHERE id=1) SET id=sequence('Reference_Sequence').next()
CREATE EDGE Calls FROM (SELECT FROM Module WHERE id=2002) TO (SELECT FROM Module WHERE id=1) SET id=sequence('Reference_Sequence').next()

###################################################################################################################################
# TEST INCLUDES
###################################################################################################################################
CREATE EDGE Includes FROM (SELECT FROM Module WHERE path='src/cobol/programs/PRGB.cbl' AND projectLink.name='Demo Project A') TO (SELECT FROM Module WHERE path='src/cobol/programs/CC1.cpy' AND projectLink.name='Demo Project A') SET id=sequence('Reference_Sequence').next()
CREATE EDGE Includes FROM (SELECT FROM Module WHERE path='src/cobol/programs/PRGB.cbl' AND projectLink.name='Demo Project A') TO (SELECT FROM Module WHERE path='src/cobol/programs/CC2.cpy' AND projectLink.name='Demo Project A') SET id=sequence('Reference_Sequence').next()
CREATE EDGE Includes FROM (SELECT FROM Module WHERE path='src/cobol/programs/CC1.cpy' AND projectLink.name='Demo Project A') TO (SELECT FROM Module WHERE path='src/cobol/programs/CC2.cpy' AND projectLink.name='Demo Project A') SET id=sequence('Reference_Sequence').next()

CREATE EDGE Includes FROM (SELECT FROM Module WHERE path='src/cobol/programs/PROG1.cbl' AND projectLink.name='Demo Project C') TO (SELECT FROM Module WHERE path='src/cobol/copies/A/CC1.cpy' AND projectLink.name='Demo Project C') SET id=sequence('Reference_Sequence').next()
CREATE EDGE Includes FROM (SELECT FROM Module WHERE path='src/cobol/copies/A/CC1.cpy' AND projectLink.name='Demo Project C') TO (SELECT FROM Module WHERE path='src/cobol/copies/A/CC2.cpy' AND projectLink.name='Demo Project C') SET id=sequence('Reference_Sequence').next()
CREATE EDGE Includes FROM (SELECT FROM Module WHERE path='src/cobol/programs/PROG2.cbl' AND projectLink.name='Demo Project C') TO (SELECT FROM Module WHERE path='src/cobol/copies/B/CC1.cpy' AND projectLink.name='Demo Project C') SET id=sequence('Reference_Sequence').next()

CREATE EDGE Calls FROM (SELECT FROM Module WHERE path='src/jobs/JOB1.job' AND projectLink.name='Demo Project C') TO (SELECT FROM Module WHERE path='src/procs/A/MYPROC.proc' AND projectLink.name='Demo Project C') SET id=sequence('Reference_Sequence').next()
CREATE EDGE Calls FROM (SELECT FROM Module WHERE path='src/procs/A/MYPROC.proc' AND projectLink.name='Demo Project C') TO (SELECT FROM Module WHERE path='src/procs/A/MYNEST.proc' AND projectLink.name='Demo Project C') SET id=sequence('Reference_Sequence').next()
CREATE EDGE Includes FROM (SELECT FROM Module WHERE path='src/jobs/JOB1.job' AND projectLink.name='Demo Project C') TO (SELECT FROM Module WHERE path='src/incs/A/MYB.inc' AND projectLink.name='Demo Project C') SET id=sequence('Reference_Sequence').next()
CREATE EDGE Calls FROM (SELECT FROM Module WHERE path='src/jobs/JOB2.job' AND projectLink.name='Demo Project C') TO (SELECT FROM Module WHERE path='src/procs/B/MYPROC.proc' AND projectLink.name='Demo Project C') SET id=sequence('Reference_Sequence').next()
CREATE EDGE Includes FROM (SELECT FROM Module WHERE path='src/jobs/JOB2.job' AND projectLink.name='Demo Project C') TO (SELECT FROM Module WHERE path='src/incs/B/MYB.inc' AND projectLink.name='Demo Project C') SET id=sequence('Reference_Sequence').next()
