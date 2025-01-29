-- WMIN-2597 Schema changes required to store custom properties per project
CREATE CLASS AdditionalInfo EXTENDS V ABSTRACT;
CREATE CLASS HasAdditionalInfo EXTENDS E;
CREATE PROPERTY AdditionalInfo.in_HasAdditionalInfo LINKLIST HasAdditionalInfo;
 
CREATE CLASS CustomProperties EXTENDS AdditionalInfo ABSTRACT;
 
CREATE CLASS MiningEntity EXTENDS V ABSTRACT;
CREATE PROPERTY MiningEntity.out_HasAdditionalInfo LINKLIST HasAdditionalInfo;
 
CREATE PROPERTY HasAdditionalInfo.out LINK MiningEntity (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY HasAdditionalInfo.in LINK AdditionalInfo (NOTNULL, MANDATORY TRUE);
 
ALTER CLASS Client SUPERCLASS MiningEntity;
ALTER CLASS Project SUPERCLASS MiningEntity;
ALTER CLASS SourceAttachment SUPERCLASS MiningEntity;
ALTER CLASS Module SUPERCLASS MiningEntity;
ALTER CLASS Statement SUPERCLASS MiningEntity;
ALTER CLASS Annotation SUPERCLASS MiningEntity;
ALTER CLASS AnnotationCategory SUPERCLASS MiningEntity;
ALTER CLASS DataDictionaryEntry SUPERCLASS MiningEntity;
ALTER CLASS Taxonomy SUPERCLASS MiningEntity;
ALTER CLASS TaxonomyEnum SUPERCLASS MiningEntity;
ALTER CLASS AnnotationTypeEnum SUPERCLASS MiningEntity;
 
CREATE PROPERTY Project.customPropertyClasses EMBEDDEDMAP EMBEDDEDSET (NOTNULL, DEFAULT {});