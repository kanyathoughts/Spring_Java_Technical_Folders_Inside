import { gql } from 'apollo-angular';
import { Injectable } from '@angular/core';
import * as Apollo from 'apollo-angular';
export type Maybe<T> = T | null;
export type InputMaybe<T> = Maybe<T>;
export type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]?: Maybe<T[SubKey]> };
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]: Maybe<T[SubKey]> };
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string;
  String: string;
  Boolean: boolean;
  Int: number;
  Float: number;
  /** An RFC-3339 compliant DateTime Scalar also accepting java.time.Instant */
  DateTime: any;
  /** EntityId containing either a UUID or numerical id */
  EntityId: any;
  /** A JSON scalar */
  JSON: any;
  /** A 64-bit signed integer */
  Long: any;
  /** Milliseconds from the epoch of 1970-01-01T00:00:00Z */
  Timestamp: any;
  /** A universally unique identifier compliant UUID Scalar */
  UUID: any;
};

export type Annotation = {
  __typename?: 'Annotation';
  /** Offset: Module location offset */
  annotationOffset?: Maybe<Scalars['Int']>;
  categoryId?: Maybe<Scalars['Long']>;
  /** Category: Category of the Annotation */
  categoryName?: Maybe<Scalars['String']>;
  createdByUserId?: Maybe<Scalars['String']>;
  /** Created By: Name of the user who created the Annotation */
  createdByUserName?: Maybe<Scalars['String']>;
  customProperties?: Maybe<Scalars['JSON']>;
  dataDictionaryEntries?: Maybe<Array<Maybe<Scalars['UUID']>>>;
  /** English Translation: Annotation english translation */
  englishTranslation?: Maybe<Scalars['String']>;
  functionalGroups?: Maybe<Array<Maybe<AnnotationFunctionalGroup>>>;
  /** Annotation Id: The unique id of the Annotation */
  id?: Maybe<Scalars['Long']>;
  length?: Maybe<Scalars['Int']>;
  linkedDataDictionaries?: Maybe<Array<Maybe<DataDictionaryEntry>>>;
  linkedDataDictionaryEntries?: Maybe<Array<Maybe<DataDictionaryEntry>>>;
  location?: Maybe<ModuleLocation>;
  module?: Maybe<Module>;
  /** Module Name */
  moduleName?: Maybe<Scalars['String']>;
  moduleNid?: Maybe<Scalars['Long']>;
  modulePath?: Maybe<Scalars['String']>;
  moduleUid?: Maybe<Scalars['UUID']>;
  /** Annotation Description: The Description of the Annotation */
  name?: Maybe<Scalars['String']>;
  offset?: Maybe<Scalars['Int']>;
  project?: Maybe<Scalars['EntityId']>;
  projectNid?: Maybe<Scalars['Long']>;
  projectUid?: Maybe<Scalars['UUID']>;
  /** Identification Reason: Reason for Identification as Business Rule Candidate */
  reasons?: Maybe<Array<Maybe<Scalars['String']>>>;
  source?: Maybe<Scalars['UUID']>;
  /** Source Code: Source Code attached to the Annotation */
  sourceAttachment?: Maybe<Scalars['String']>;
  /** State: State of the Annotation */
  state?: Maybe<WorkingState>;
  /** Annotation Type: Type of the Annotation */
  type?: Maybe<AnnotationType>;
  uid?: Maybe<Scalars['UUID']>;
  updatedByUserId?: Maybe<Scalars['String']>;
  /** Modified By: Name of the user who last modified the Annotation */
  updatedByUserName?: Maybe<Scalars['String']>;
};


export type AnnotationLinkedDataDictionariesArgs = {
  isBusiness?: InputMaybe<Scalars['Boolean']>;
};

export type AnnotationFunctionalGroup = {
  __typename?: 'AnnotationFunctionalGroup';
  /** Sequence Number: Index of the Annotation inside of the Functional Block */
  annotationSequenceNumber?: Maybe<Scalars['Int']>;
  /** Functional Block Name: Name of the Functional Block to which the Annotation belongs */
  name?: Maybe<Scalars['String']>;
  /** Functional Group Id: Id of the Functional Group to which the Annotation belongs */
  uid?: Maybe<Scalars['UUID']>;
};

export enum AnnotationType {
  Database = 'DATABASE',
  DeadCode = 'DEAD_CODE',
  Exclude = 'EXCLUDE',
  Functional = 'FUNCTIONAL',
  Rule = 'RULE'
}

export type Client = {
  __typename?: 'Client';
  customProperties?: Maybe<Scalars['JSON']>;
  hasLogo?: Maybe<Scalars['Boolean']>;
  id?: Maybe<Scalars['Long']>;
  name?: Maybe<Scalars['String']>;
  uid?: Maybe<Scalars['UUID']>;
};

export enum Creator {
  Api = 'API',
  Discovery = 'DISCOVERY',
  SchedulerInfo = 'SCHEDULER_INFO'
}

export type CustomProperty = {
  __typename?: 'CustomProperty';
  /** Auto Completion Key: The auto completion key of the custom property */
  autoCompletionKey?: Maybe<Scalars['String']>;
  /** customCategory: The name of the custom Category */
  customCategory?: Maybe<Scalars['String']>;
  /** Custom View Index: The custom view index of the custom property */
  customViewIndex?: Maybe<Scalars['Int']>;
  customViewNames?: Maybe<Array<Maybe<Scalars['String']>>>;
  dataSource?: Maybe<Scalars['String']>;
  /** Data Type: The data type of the custom property */
  dataType?: Maybe<Innowake_Mining_Shared_Model_CustomPropertyDataType>;
  /** Description: The description of the custom property */
  description?: Maybe<Scalars['String']>;
  /** Field Type: The field type of the custom property */
  fieldType?: Maybe<Innowake_Mining_Shared_Model_CustomPropertyFieldType>;
  /** Label: The label of the custom property */
  label?: Maybe<Scalars['String']>;
  /** Name: The name of the custom property */
  name?: Maybe<Scalars['String']>;
  showWhen?: Maybe<Array<Maybe<Map_String_Java_Lang_Object>>>;
  validationErrorMessage?: Maybe<Scalars['String']>;
  validationRegex?: Maybe<Scalars['String']>;
};

export type DataDictionaryEntry = {
  __typename?: 'DataDictionaryEntry';
  /** Access Type: The identified access type of the data dictionary entry field */
  accessType?: Maybe<Scalars['JSON']>;
  annotations?: Maybe<Array<Maybe<Scalars['EntityId']>>>;
  createdByUserId?: Maybe<Scalars['String']>;
  /** Created By: The user Name for the user who created the data dictionary entry field */
  createdByUserName?: Maybe<Scalars['String']>;
  customProperties?: Maybe<Scalars['JSON']>;
  /** Defined Location: The defined location of the Data Field */
  definedLocation?: Maybe<Innowake_Mining_Shared_Model_DefinedLocation>;
  /** Field Description: Field Description of Data Dictionary */
  description?: Maybe<Scalars['String']>;
  /** Field Level: The level of the COBOL data dictionary entry field */
  fieldLevel?: Maybe<Scalars['Long']>;
  /** Field Transformation: Line of code where the data dictionary entry field final value is computed or a value moved into it */
  fieldTransformation?: Maybe<Scalars['String']>;
  /** Field Type: Whether the source field is a GROUP or an ELEMENTARY field. */
  fieldType?: Maybe<Scalars['String']>;
  /** Field Format: The format of the Data Field. */
  format?: Maybe<Scalars['String']>;
  groupPath?: Maybe<Scalars['String']>;
  /** Entry ID: ID of the Data Dictionary Entry. */
  id?: Maybe<Scalars['Long']>;
  indentation?: Maybe<Scalars['Long']>;
  /** Initial Value: Any initial information about the data dictionary entry field */
  initialValue?: Maybe<Scalars['String']>;
  /** Business: Indicates whether the data dictionary field is business-related */
  isBusiness?: Maybe<Scalars['Boolean']>;
  isCandidate?: Maybe<Scalars['Boolean']>;
  /** Referenced: Indicates whether a data dictionary entry field is referenced within the module code */
  isReferenced?: Maybe<Scalars['Boolean']>;
  /** Length: The length of the Data Field */
  length?: Maybe<Scalars['Long']>;
  /** Linked Annotations: The linked annotations of the data dictionary entry field */
  linkedAnnotations?: Maybe<Array<Maybe<Annotation>>>;
  location?: Maybe<ModuleLocation>;
  module?: Maybe<Module>;
  moduleNid?: Maybe<Scalars['Long']>;
  moduleUid?: Maybe<Scalars['UUID']>;
  /** Field Name: The name of the Data Field. */
  name?: Maybe<Scalars['String']>;
  /** Group Field: The parent group of the data dictionary entry field */
  parentGroup?: Maybe<Scalars['String']>;
  /** PIC Clause: The PIC clause of the Data Field */
  picClause?: Maybe<Scalars['String']>;
  /** Scope: The identified scope or usage of the data dictionary entry field */
  scopeNames?: Maybe<Array<Maybe<Scalars['String']>>>;
  /** Scope: The identified scope or usage of the data dictionary entry field */
  scopes?: Maybe<Scalars['JSON']>;
  /** Source Input: Any manually defined source/input information about the data dictionary entry field */
  sourceInput?: Maybe<Scalars['String']>;
  /** State: State of the data dictionary entry */
  state?: Maybe<WorkingState>;
  /** Target Output: Any manually defined target/output information about the data dictionary entry field */
  targetOutput?: Maybe<Scalars['String']>;
  /** Translated Field Value: The custom translation of this DataDictionaryEntry */
  translatedFieldValue?: Maybe<Scalars['String']>;
  uid?: Maybe<Scalars['UUID']>;
  updatedByUserId?: Maybe<Scalars['String']>;
  /** Modified By: Name of the user who last modified the Annotation */
  updatedByUserName?: Maybe<Scalars['String']>;
  /** Field Usage: Shows the internals of the usage clause on a Cobol field declaration */
  usage?: Maybe<Scalars['String']>;
};

export type DependencyInformation = {
  __typename?: 'DependencyInformation';
  /** AMP */
  ACCESSES_AMP?: Maybe<Scalars['String']>;
  /** AVGREC */
  ACCESSES_AVGREC?: Maybe<Scalars['String']>;
  /** BLKSIZE */
  ACCESSES_BLKSIZE?: Maybe<Scalars['String']>;
  /** CALL_TYPE */
  ACCESSES_CALL_TYPE?: Maybe<Scalars['String']>;
  /** DATACLAS */
  ACCESSES_DATACLAS?: Maybe<Scalars['String']>;
  /** DB_ACCESS_OPERATION */
  ACCESSES_DB_ACCESS_OPERATION?: Maybe<Scalars['String']>;
  /** DB_ACCESS_TYPE */
  ACCESSES_DB_ACCESS_TYPE?: Maybe<Scalars['String']>;
  /** DCB */
  ACCESSES_DCB?: Maybe<Scalars['String']>;
  /** DISP */
  ACCESSES_DISP?: Maybe<Scalars['String']>;
  /** DSNTYPE */
  ACCESSES_DSNTYPE?: Maybe<Scalars['String']>;
  /** DSORG */
  ACCESSES_DSORG?: Maybe<Scalars['String']>;
  /** FILE_ACCESS_OPERATION */
  ACCESSES_FILE_ACCESS_OPERATION?: Maybe<Scalars['String']>;
  /** FILE_ACCESS_TYPE */
  ACCESSES_FILE_ACCESS_TYPE?: Maybe<Scalars['String']>;
  /** FILE_ALIAS */
  ACCESSES_FILE_ALIAS?: Maybe<Scalars['String']>;
  /** ID_NAME */
  ACCESSES_ID_NAME?: Maybe<Scalars['String']>;
  /** LABEL */
  ACCESSES_LABEL?: Maybe<Scalars['String']>;
  /** LRECL */
  ACCESSES_LRECL?: Maybe<Scalars['String']>;
  /** MGMTCLAS */
  ACCESSES_MGMTCLAS?: Maybe<Scalars['String']>;
  /** QUEUE_ACCESS */
  ACCESSES_QUEUE_ACCESS?: Maybe<Scalars['String']>;
  /** RECFM */
  ACCESSES_RECFM?: Maybe<Scalars['String']>;
  /** RETPD */
  ACCESSES_RETPD?: Maybe<Scalars['String']>;
  /** SPACE */
  ACCESSES_SPACE?: Maybe<Scalars['String']>;
  /** STORCLAS */
  ACCESSES_STORCLAS?: Maybe<Scalars['String']>;
  /** UNIT */
  ACCESSES_UNIT?: Maybe<Scalars['String']>;
  /** VOL */
  ACCESSES_VOL?: Maybe<Scalars['String']>;
  /** * */
  ACCESSES__?: Maybe<Scalars['String']>;
  /** CALL_TYPE */
  CALLS_CALL_TYPE?: Maybe<Scalars['String']>;
  /** DB_ACCESS_OPERATION */
  CALLS_DB_ACCESS_OPERATION?: Maybe<Scalars['String']>;
  /** DB_ACCESS_TYPE */
  CALLS_DB_ACCESS_TYPE?: Maybe<Scalars['String']>;
  /** DISP */
  CALLS_DISP?: Maybe<Scalars['String']>;
  /** DSN */
  CALLS_DSN?: Maybe<Scalars['String']>;
  /** ID_NAME */
  CALLS_ID_NAME?: Maybe<Scalars['String']>;
  /** INBOUND */
  CALLS_INBOUND?: Maybe<Scalars['String']>;
  /** OUTBOUND */
  CALLS_OUTBOUND?: Maybe<Scalars['String']>;
  /** OUTBOUND_TARGETS */
  CALLS_OUTBOUND_TARGETS?: Maybe<Scalars['String']>;
  /** STATEMENT */
  CALLS_STATEMENT?: Maybe<Scalars['String']>;
  /** TYPE_REFERENCE_TYPE */
  CALLS_TYPE_REFERENCE_TYPE?: Maybe<Scalars['String']>;
  /** * */
  CALLS__?: Maybe<Scalars['String']>;
  /** CALL_TYPE */
  INCLUDES_CALL_TYPE?: Maybe<Scalars['String']>;
  /** SEND_RECEIVE_ACCESS_TYPE */
  INCLUDES_SEND_RECEIVE_ACCESS_TYPE?: Maybe<Scalars['String']>;
  /** CALL_TYPE */
  REFERENCES_CALL_TYPE?: Maybe<Scalars['String']>;
  /** DB_ACCESS_OPERATION */
  REFERENCES_DB_ACCESS_OPERATION?: Maybe<Scalars['String']>;
  /** DB_ACCESS_TYPE */
  REFERENCES_DB_ACCESS_TYPE?: Maybe<Scalars['String']>;
  /** IMS_DBD_NAME */
  REFERENCES_IMS_DBD_NAME?: Maybe<Scalars['String']>;
  /** IMS_DBD_SEGMENT_COMPRTN */
  REFERENCES_IMS_DBD_SEGMENT_COMPRTN?: Maybe<Scalars['String']>;
  /** IMS_DBD_SEGMENT_PARENT */
  REFERENCES_IMS_DBD_SEGMENT_PARENT?: Maybe<Scalars['String']>;
  /** IMS_PCB_PROCOPT */
  REFERENCES_IMS_PCB_PROCOPT?: Maybe<Scalars['String']>;
  /** IMS_PCB_PROCSEQ */
  REFERENCES_IMS_PCB_PROCSEQ?: Maybe<Scalars['String']>;
  /** IMS_PCB_SENSEG */
  REFERENCES_IMS_PCB_SENSEG?: Maybe<Scalars['String']>;
  /** IMS_PCB_TYPE */
  REFERENCES_IMS_PCB_TYPE?: Maybe<Scalars['String']>;
  /** IMS_REFERENCE_TYPE */
  REFERENCES_IMS_REFERENCE_TYPE?: Maybe<Scalars['String']>;
  /** IMS_SEGMENTS */
  REFERENCES_IMS_SEGMENTS?: Maybe<Scalars['String']>;
  /** OUTBOUND */
  REFERENCES_OUTBOUND?: Maybe<Scalars['String']>;
  /** OUTBOUND_TARGETS */
  REFERENCES_OUTBOUND_TARGETS?: Maybe<Scalars['String']>;
  /** STATEMENT */
  REFERENCES_STATEMENT?: Maybe<Scalars['String']>;
  /** TYPE_REFERENCE_TYPE */
  REFERENCES_TYPE_REFERENCE_TYPE?: Maybe<Scalars['String']>;
  /** Direction: Direction of the dependency (incoming or outgoing) */
  direction?: Maybe<Innowake_Mining_Shared_Model_RelationshipDirection>;
  /** Location of the dependency in the source module */
  fromModuleLocation?: Maybe<ModuleLocation>;
  /** Properties of the dependency */
  properties?: Maybe<Scalars['JSON']>;
  /** Relationship Type: Relationship of the dependency (Calls or Includes or Read writes or References) */
  relationship?: Maybe<RelationshipType>;
  relationshipId?: Maybe<Scalars['UUID']>;
  /** Target of the dependency */
  target?: Maybe<Innowake_Mining_Shared_Entities_ModuleBasePojo>;
  /** Module id of the dependency target */
  targetId?: Maybe<Scalars['Long']>;
  /** Target Name: Name of the dependency target for outgoing dependencies or source for incoming dependencies */
  targetName?: Maybe<Scalars['String']>;
  /** Location of the dependency target in the target module */
  toModuleLocation?: Maybe<ModuleLocation>;
};

export type FieldInfo = {
  __typename?: 'FieldInfo';
  /** Auto Increment: Whether the value for this column is generated via auto increment */
  autoIncrement?: Maybe<Scalars['Boolean']>;
  /** Comment: Comment describing the field or column */
  comment?: Maybe<Scalars['String']>;
  /** Data Type: Data Type of the Field or Column */
  dataType?: Maybe<Scalars['String']>;
  id?: Maybe<Scalars['UUID']>;
  module?: Maybe<Scalars['EntityId']>;
  moduleNid?: Maybe<Scalars['Long']>;
  moduleUid?: Maybe<Scalars['UUID']>;
  /** Field/Column Name: Name of the data field or table column */
  name?: Maybe<Scalars['String']>;
  /** Ordinal: Index of the data field or table column */
  ordinal?: Maybe<Scalars['Int']>;
  /** Primary Key: The index of the column in the table's primary key */
  primaryKey?: Maybe<Scalars['String']>;
  properties?: Maybe<Scalars['JSON']>;
  /** Reference: Fields or columns referenced by this column */
  reference?: Maybe<Scalars['String']>;
  /** Field/Column Length: The length of Field/Columns of the Tables */
  size?: Maybe<Scalars['Long']>;
};

export type FilterObject_Annotations = {
  _and?: InputMaybe<Array<InputMaybe<FilterObject_Annotations>>>;
  _not?: InputMaybe<FilterObject_Annotations>;
  _or?: InputMaybe<Array<InputMaybe<FilterObject_Annotations>>>;
  content_categoryName?: InputMaybe<FilterObject_Annotations_Content_CategoryName>;
  content_functionalGroups_name?: InputMaybe<FilterObject_Annotations_Content_FunctionalGroups_Name>;
  content_id?: InputMaybe<FilterObject_Annotations_Content_Id>;
  content_moduleName?: InputMaybe<FilterObject_Annotations_Content_ModuleName>;
  content_module_id?: InputMaybe<FilterObject_Annotations_Content_Module_Id>;
  content_module_name?: InputMaybe<FilterObject_Annotations_Content_Module_Name>;
  content_module_technology?: InputMaybe<FilterObject_Annotations_Content_Module_Technology>;
  content_module_type?: InputMaybe<FilterObject_Annotations_Content_Module_Type>;
  content_name?: InputMaybe<FilterObject_Annotations_Content_Name>;
  content_reasons?: InputMaybe<FilterObject_Annotations_Content_Reasons>;
  content_sourceAttachment?: InputMaybe<FilterObject_Annotations_Content_SourceAttachment>;
  content_state?: InputMaybe<FilterObject_Annotations_Content_State>;
  content_type?: InputMaybe<FilterObject_Annotations_Content_Type>;
};

export type FilterObject_Annotations_Content_CategoryName = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
  is?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Annotations_Content_FunctionalGroups_Name = {
  eq?: InputMaybe<Scalars['String']>;
  is?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Annotations_Content_Id = {
  eq?: InputMaybe<Scalars['Long']>;
  gt?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['Long']>>>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Annotations_Content_ModuleName = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Annotations_Content_Module_Id = {
  eq?: InputMaybe<Scalars['Long']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['Long']>>>;
};

export type FilterObject_Annotations_Content_Module_Name = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Annotations_Content_Module_Technology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Annotations_Content_Module_Type = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Annotations_Content_Name = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Annotations_Content_Reasons = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
  is?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Annotations_Content_SourceAttachment = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Annotations_Content_State = {
  eq?: InputMaybe<WorkingState>;
  in?: InputMaybe<Array<InputMaybe<WorkingState>>>;
};

export type FilterObject_Annotations_Content_Type = {
  eq?: InputMaybe<AnnotationType>;
  in?: InputMaybe<Array<InputMaybe<AnnotationType>>>;
};

export type FilterObject_DataDictionaries = {
  _and?: InputMaybe<Array<InputMaybe<FilterObject_DataDictionaries>>>;
  _not?: InputMaybe<FilterObject_DataDictionaries>;
  _or?: InputMaybe<Array<InputMaybe<FilterObject_DataDictionaries>>>;
  content_definedLocation?: InputMaybe<FilterObject_DataDictionaries_Content_DefinedLocation>;
  content_description?: InputMaybe<FilterObject_DataDictionaries_Content_Description>;
  content_fieldTransformation?: InputMaybe<FilterObject_DataDictionaries_Content_FieldTransformation>;
  content_format?: InputMaybe<FilterObject_DataDictionaries_Content_Format>;
  content_id?: InputMaybe<FilterObject_DataDictionaries_Content_Id>;
  content_initialValue?: InputMaybe<FilterObject_DataDictionaries_Content_InitialValue>;
  content_isBusiness?: InputMaybe<FilterObject_DataDictionaries_Content_IsBusiness>;
  content_isReferenced?: InputMaybe<FilterObject_DataDictionaries_Content_IsReferenced>;
  content_module_id?: InputMaybe<FilterObject_DataDictionaries_Content_Module_Id>;
  content_module_name?: InputMaybe<FilterObject_DataDictionaries_Content_Module_Name>;
  content_name?: InputMaybe<FilterObject_DataDictionaries_Content_Name>;
  content_parentGroup?: InputMaybe<FilterObject_DataDictionaries_Content_ParentGroup>;
  content_picClause?: InputMaybe<FilterObject_DataDictionaries_Content_PicClause>;
  content_scopeNames?: InputMaybe<FilterObject_DataDictionaries_Content_ScopeNames>;
  content_scopes?: InputMaybe<FilterObject_DataDictionaries_Content_Scopes>;
  content_sourceInput?: InputMaybe<FilterObject_DataDictionaries_Content_SourceInput>;
  content_state?: InputMaybe<FilterObject_DataDictionaries_Content_State>;
  content_targetOutput?: InputMaybe<FilterObject_DataDictionaries_Content_TargetOutput>;
  content_usage?: InputMaybe<FilterObject_DataDictionaries_Content_Usage>;
};

export type FilterObject_DataDictionaries_Content_DefinedLocation = {
  eq?: InputMaybe<Innowake_Mining_Shared_Model_DefinedLocation>;
  in?: InputMaybe<Array<InputMaybe<Innowake_Mining_Shared_Model_DefinedLocation>>>;
};

export type FilterObject_DataDictionaries_Content_Description = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_FieldTransformation = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_Format = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_DataDictionaries_Content_Id = {
  eq?: InputMaybe<Scalars['Long']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['Long']>>>;
};

export type FilterObject_DataDictionaries_Content_InitialValue = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_IsBusiness = {
  is?: InputMaybe<Scalars['Boolean']>;
};

export type FilterObject_DataDictionaries_Content_IsReferenced = {
  in?: InputMaybe<Array<InputMaybe<Scalars['Boolean']>>>;
  is?: InputMaybe<Scalars['Boolean']>;
};

export type FilterObject_DataDictionaries_Content_Module_Id = {
  eq?: InputMaybe<Scalars['Long']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['Long']>>>;
};

export type FilterObject_DataDictionaries_Content_Module_Name = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_Name = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_ParentGroup = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_PicClause = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_ScopeNames = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_DataDictionaries_Content_Scopes = {
  eq?: InputMaybe<Scalars['JSON']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['JSON']>>>;
};

export type FilterObject_DataDictionaries_Content_SourceInput = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_State = {
  in?: InputMaybe<Array<InputMaybe<WorkingState>>>;
};

export type FilterObject_DataDictionaries_Content_TargetOutput = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_DataDictionaries_Content_Usage = {
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_DnaModulesInCluster = {
  _and?: InputMaybe<Array<InputMaybe<FilterObject_DnaModulesInCluster>>>;
  _not?: InputMaybe<FilterObject_DnaModulesInCluster>;
  _or?: InputMaybe<Array<InputMaybe<FilterObject_DnaModulesInCluster>>>;
  content_module_id?: InputMaybe<FilterObject_DnaModulesInCluster_Content_Module_Id>;
  content_module_name?: InputMaybe<FilterObject_DnaModulesInCluster_Content_Module_Name>;
};

export type FilterObject_DnaModulesInCluster_Content_Module_Id = {
  eq?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_DnaModulesInCluster_Content_Module_Name = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_FunctionalBlocks = {
  _and?: InputMaybe<Array<InputMaybe<FilterObject_FunctionalBlocks>>>;
  _not?: InputMaybe<FilterObject_FunctionalBlocks>;
  _or?: InputMaybe<Array<InputMaybe<FilterObject_FunctionalBlocks>>>;
  content_blocksWithDeletedUB?: InputMaybe<FilterObject_FunctionalBlocks_Content_BlocksWithDeletedUb>;
  content_children?: InputMaybe<FilterObject_FunctionalBlocks_Content_Children>;
  content_deepName?: InputMaybe<FilterObject_FunctionalBlocks_Content_DeepName>;
  content_generatedFrom_annotationId?: InputMaybe<FilterObject_FunctionalBlocks_Content_GeneratedFrom_AnnotationId>;
  content_lowerBoundAccessTypes?: InputMaybe<FilterObject_FunctionalBlocks_Content_LowerBoundAccessTypes>;
  content_name?: InputMaybe<FilterObject_FunctionalBlocks_Content_Name>;
  content_outdatedBlock?: InputMaybe<FilterObject_FunctionalBlocks_Content_OutdatedBlock>;
  content_parents?: InputMaybe<FilterObject_FunctionalBlocks_Content_Parents>;
  content_peers?: InputMaybe<FilterObject_FunctionalBlocks_Content_Peers>;
  content_referencedDataDictionaries?: InputMaybe<FilterObject_FunctionalBlocks_Content_ReferencedDataDictionaries>;
  content_referencedDataDictionaryNames?: InputMaybe<FilterObject_FunctionalBlocks_Content_ReferencedDataDictionaryNames>;
  content_resolvedModuleParts_moduleId?: InputMaybe<FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_ModuleId>;
  content_resolvedModuleParts_module_name?: InputMaybe<FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_Module_Name>;
  content_resolvedModuleParts_module_technology?: InputMaybe<FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_Module_Technology>;
  content_resolvedModuleParts_module_type?: InputMaybe<FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_Module_Type>;
  content_resolvedModuleParts_referencedTaxonomies_id?: InputMaybe<FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_ReferencedTaxonomies_Id>;
  content_status?: InputMaybe<FilterObject_FunctionalBlocks_Content_Status>;
  content_type?: InputMaybe<FilterObject_FunctionalBlocks_Content_Type>;
  content_uid?: InputMaybe<FilterObject_FunctionalBlocks_Content_Uid>;
};

export type FilterObject_FunctionalBlocks_Content_BlocksWithDeletedUb = {
  eq?: InputMaybe<Scalars['Boolean']>;
};

export type FilterObject_FunctionalBlocks_Content_Children = {
  eq?: InputMaybe<FilterObject_FunctionalBlocks>;
};

export type FilterObject_FunctionalBlocks_Content_DeepName = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_FunctionalBlocks_Content_GeneratedFrom_AnnotationId = {
  eq?: InputMaybe<Scalars['EntityId']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
};

export type FilterObject_FunctionalBlocks_Content_LowerBoundAccessTypes = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_FunctionalBlocks_Content_Name = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_FunctionalBlocks_Content_OutdatedBlock = {
  eq?: InputMaybe<Scalars['Boolean']>;
};

export type FilterObject_FunctionalBlocks_Content_Parents = {
  eq?: InputMaybe<FilterObject_FunctionalBlocks>;
  notEq?: InputMaybe<FilterObject_FunctionalBlocks>;
};

export type FilterObject_FunctionalBlocks_Content_Peers = {
  eq?: InputMaybe<FilterObject_FunctionalBlocks>;
  notEq?: InputMaybe<FilterObject_FunctionalBlocks>;
};

export type FilterObject_FunctionalBlocks_Content_ReferencedDataDictionaries = {
  in?: InputMaybe<Array<InputMaybe<Scalars['UUID']>>>;
};

export type FilterObject_FunctionalBlocks_Content_ReferencedDataDictionaryNames = {
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_ModuleId = {
  eq?: InputMaybe<Scalars['EntityId']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
};

export type FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_Module_Name = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_Module_Technology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_Module_Type = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_FunctionalBlocks_Content_ResolvedModuleParts_ReferencedTaxonomies_Id = {
  eq?: InputMaybe<Scalars['EntityId']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
};

export type FilterObject_FunctionalBlocks_Content_Status = {
  eq?: InputMaybe<Innowake_Mining_Shared_Entities_Functionalblocks_FunctionalBlockStatus>;
  notEq?: InputMaybe<Innowake_Mining_Shared_Entities_Functionalblocks_FunctionalBlockStatus>;
};

export type FilterObject_FunctionalBlocks_Content_Type = {
  eq?: InputMaybe<FunctionalBlockType>;
  in?: InputMaybe<Array<InputMaybe<FunctionalBlockType>>>;
};

export type FilterObject_FunctionalBlocks_Content_Uid = {
  eq?: InputMaybe<Scalars['UUID']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['UUID']>>>;
  notEq?: InputMaybe<Scalars['UUID']>;
};

export type FilterObject_ModuleDependencies = {
  _and?: InputMaybe<Array<InputMaybe<FilterObject_ModuleDependencies>>>;
  _not?: InputMaybe<FilterObject_ModuleDependencies>;
  _or?: InputMaybe<Array<InputMaybe<FilterObject_ModuleDependencies>>>;
  content_direction?: InputMaybe<FilterObject_ModuleDependencies_Content_Direction>;
  content_properties?: InputMaybe<FilterObject_ModuleDependencies_Content_Properties>;
  content_relationship?: InputMaybe<FilterObject_ModuleDependencies_Content_Relationship>;
};

export type FilterObject_ModuleDependencies_Content_Direction = {
  eq?: InputMaybe<Innowake_Mining_Shared_Model_RelationshipDirection>;
  in?: InputMaybe<Array<InputMaybe<Innowake_Mining_Shared_Model_RelationshipDirection>>>;
};

export type FilterObject_ModuleDependencies_Content_Properties = {
  eq?: InputMaybe<Scalars['JSON']>;
};

export type FilterObject_ModuleDependencies_Content_Relationship = {
  eq?: InputMaybe<RelationshipType>;
  in?: InputMaybe<Array<InputMaybe<RelationshipType>>>;
};

export type FilterObject_Modules = {
  _and?: InputMaybe<Array<InputMaybe<FilterObject_Modules>>>;
  _not?: InputMaybe<FilterObject_Modules>;
  _or?: InputMaybe<Array<InputMaybe<FilterObject_Modules>>>;
  content_annotationCount?: InputMaybe<FilterObject_Modules_Content_AnnotationCount>;
  content_creator?: InputMaybe<FilterObject_Modules_Content_Creator>;
  content_dependencyCount?: InputMaybe<FilterObject_Modules_Content_DependencyCount>;
  content_errorCount?: InputMaybe<FilterObject_Modules_Content_ErrorCount>;
  content_id?: InputMaybe<FilterObject_Modules_Content_Id>;
  content_identification?: InputMaybe<FilterObject_Modules_Content_Identification>;
  content_inAccessesTechnology?: InputMaybe<FilterObject_Modules_Content_InAccessesTechnology>;
  content_inAccessesType?: InputMaybe<FilterObject_Modules_Content_InAccessesType>;
  content_inArtificialTechnology?: InputMaybe<FilterObject_Modules_Content_InArtificialTechnology>;
  content_inArtificialType?: InputMaybe<FilterObject_Modules_Content_InArtificialType>;
  content_inCallsTechnology?: InputMaybe<FilterObject_Modules_Content_InCallsTechnology>;
  content_inCallsType?: InputMaybe<FilterObject_Modules_Content_InCallsType>;
  content_inContainsTechnology?: InputMaybe<FilterObject_Modules_Content_InContainsTechnology>;
  content_inContainsType?: InputMaybe<FilterObject_Modules_Content_InContainsType>;
  content_inIncludesTechnology?: InputMaybe<FilterObject_Modules_Content_InIncludesTechnology>;
  content_inIncludesType?: InputMaybe<FilterObject_Modules_Content_InIncludesType>;
  content_inNoneTechnology?: InputMaybe<FilterObject_Modules_Content_InNoneTechnology>;
  content_inNoneType?: InputMaybe<FilterObject_Modules_Content_InNoneType>;
  content_inPrecedesTechnology?: InputMaybe<FilterObject_Modules_Content_InPrecedesTechnology>;
  content_inPrecedesType?: InputMaybe<FilterObject_Modules_Content_InPrecedesType>;
  content_inReferencesTechnology?: InputMaybe<FilterObject_Modules_Content_InReferencesTechnology>;
  content_inReferencesType?: InputMaybe<FilterObject_Modules_Content_InReferencesType>;
  content_inboundAccessesCount?: InputMaybe<FilterObject_Modules_Content_InboundAccessesCount>;
  content_inboundArtificialCount?: InputMaybe<FilterObject_Modules_Content_InboundArtificialCount>;
  content_inboundCallsCount?: InputMaybe<FilterObject_Modules_Content_InboundCallsCount>;
  content_inboundContainsCount?: InputMaybe<FilterObject_Modules_Content_InboundContainsCount>;
  content_inboundDependencyCount?: InputMaybe<FilterObject_Modules_Content_InboundDependencyCount>;
  content_inboundIncludesCount?: InputMaybe<FilterObject_Modules_Content_InboundIncludesCount>;
  content_inboundNoneCount?: InputMaybe<FilterObject_Modules_Content_InboundNoneCount>;
  content_inboundPrecedesCount?: InputMaybe<FilterObject_Modules_Content_InboundPrecedesCount>;
  content_inboundReferencesCount?: InputMaybe<FilterObject_Modules_Content_InboundReferencesCount>;
  content_name?: InputMaybe<FilterObject_Modules_Content_Name>;
  content_numberOfBusinessRules?: InputMaybe<FilterObject_Modules_Content_NumberOfBusinessRules>;
  content_numberOfCloseDatabaseOperation?: InputMaybe<FilterObject_Modules_Content_NumberOfCloseDatabaseOperation>;
  content_numberOfDeclareDatabaseOperation?: InputMaybe<FilterObject_Modules_Content_NumberOfDeclareDatabaseOperation>;
  content_numberOfErrorProcessingRules?: InputMaybe<FilterObject_Modules_Content_NumberOfErrorProcessingRules>;
  content_numberOfFieldComputationRules?: InputMaybe<FilterObject_Modules_Content_NumberOfFieldComputationRules>;
  content_numberOfReadDatabaseOperation?: InputMaybe<FilterObject_Modules_Content_NumberOfReadDatabaseOperation>;
  content_numberOfTechnicalRules?: InputMaybe<FilterObject_Modules_Content_NumberOfTechnicalRules>;
  content_numberOfValidationRules?: InputMaybe<FilterObject_Modules_Content_NumberOfValidationRules>;
  content_numberOfWriteDatabaseOperation?: InputMaybe<FilterObject_Modules_Content_NumberOfWriteDatabaseOperation>;
  content_outAccessesTechnology?: InputMaybe<FilterObject_Modules_Content_OutAccessesTechnology>;
  content_outAccessesType?: InputMaybe<FilterObject_Modules_Content_OutAccessesType>;
  content_outArtificialTechnology?: InputMaybe<FilterObject_Modules_Content_OutArtificialTechnology>;
  content_outArtificialType?: InputMaybe<FilterObject_Modules_Content_OutArtificialType>;
  content_outCallsTechnology?: InputMaybe<FilterObject_Modules_Content_OutCallsTechnology>;
  content_outCallsType?: InputMaybe<FilterObject_Modules_Content_OutCallsType>;
  content_outContainsTechnology?: InputMaybe<FilterObject_Modules_Content_OutContainsTechnology>;
  content_outContainsType?: InputMaybe<FilterObject_Modules_Content_OutContainsType>;
  content_outIncludesTechnology?: InputMaybe<FilterObject_Modules_Content_OutIncludesTechnology>;
  content_outIncludesType?: InputMaybe<FilterObject_Modules_Content_OutIncludesType>;
  content_outNoneTechnology?: InputMaybe<FilterObject_Modules_Content_OutNoneTechnology>;
  content_outNoneType?: InputMaybe<FilterObject_Modules_Content_OutNoneType>;
  content_outPrecedesTechnology?: InputMaybe<FilterObject_Modules_Content_OutPrecedesTechnology>;
  content_outPrecedesType?: InputMaybe<FilterObject_Modules_Content_OutPrecedesType>;
  content_outReferencesTechnology?: InputMaybe<FilterObject_Modules_Content_OutReferencesTechnology>;
  content_outReferencesType?: InputMaybe<FilterObject_Modules_Content_OutReferencesType>;
  content_outboundAccessesCount?: InputMaybe<FilterObject_Modules_Content_OutboundAccessesCount>;
  content_outboundArtificialCount?: InputMaybe<FilterObject_Modules_Content_OutboundArtificialCount>;
  content_outboundCallsCount?: InputMaybe<FilterObject_Modules_Content_OutboundCallsCount>;
  content_outboundContainsCount?: InputMaybe<FilterObject_Modules_Content_OutboundContainsCount>;
  content_outboundDependencyCount?: InputMaybe<FilterObject_Modules_Content_OutboundDependencyCount>;
  content_outboundIncludesCount?: InputMaybe<FilterObject_Modules_Content_OutboundIncludesCount>;
  content_outboundNoneCount?: InputMaybe<FilterObject_Modules_Content_OutboundNoneCount>;
  content_outboundPrecedesCount?: InputMaybe<FilterObject_Modules_Content_OutboundPrecedesCount>;
  content_outboundReferencesCount?: InputMaybe<FilterObject_Modules_Content_OutboundReferencesCount>;
  content_reachabilityBlockNames_name?: InputMaybe<FilterObject_Modules_Content_ReachabilityBlockNames_Name>;
  content_representation?: InputMaybe<FilterObject_Modules_Content_Representation>;
  content_requiresReview?: InputMaybe<FilterObject_Modules_Content_RequiresReview>;
  content_sourceMetrics_codeLines?: InputMaybe<FilterObject_Modules_Content_SourceMetrics_CodeLines>;
  content_sourceMetrics_commentLines?: InputMaybe<FilterObject_Modules_Content_SourceMetrics_CommentLines>;
  content_sourceMetrics_complexityMcCabe?: InputMaybe<FilterObject_Modules_Content_SourceMetrics_ComplexityMcCabe>;
  content_sourceMetrics_deadCodeLines?: InputMaybe<FilterObject_Modules_Content_SourceMetrics_DeadCodeLines>;
  content_sourceMetrics_physicalLines?: InputMaybe<FilterObject_Modules_Content_SourceMetrics_PhysicalLines>;
  content_storage?: InputMaybe<FilterObject_Modules_Content_Storage>;
  content_taxonomies_id?: InputMaybe<FilterObject_Modules_Content_Taxonomies_Id>;
  content_taxonomies_name?: InputMaybe<FilterObject_Modules_Content_Taxonomies_Name>;
  content_taxonomyCount?: InputMaybe<FilterObject_Modules_Content_TaxonomyCount>;
  content_technology?: InputMaybe<FilterObject_Modules_Content_Technology>;
  content_type?: InputMaybe<FilterObject_Modules_Content_Type>;
};

export type FilterObject_Modules_Content_AnnotationCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_Creator = {
  eq?: InputMaybe<Creator>;
  in?: InputMaybe<Array<InputMaybe<Creator>>>;
};

export type FilterObject_Modules_Content_DependencyCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_ErrorCount = {
  eq?: InputMaybe<Scalars['Int']>;
  gte?: InputMaybe<Scalars['Int']>;
  lte?: InputMaybe<Scalars['Int']>;
};

export type FilterObject_Modules_Content_Id = {
  eq?: InputMaybe<Scalars['Long']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['Long']>>>;
};

export type FilterObject_Modules_Content_Identification = {
  eq?: InputMaybe<Identification>;
  in?: InputMaybe<Array<InputMaybe<Identification>>>;
};

export type FilterObject_Modules_Content_InAccessesTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_InAccessesType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_InArtificialTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_InArtificialType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_InCallsTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_InCallsType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_InContainsTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_InContainsType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_InIncludesTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_InIncludesType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_InNoneTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_InNoneType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_InPrecedesTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_InPrecedesType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_InReferencesTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_InReferencesType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_InboundAccessesCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_InboundArtificialCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_InboundCallsCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_InboundContainsCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_InboundDependencyCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_InboundIncludesCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_InboundNoneCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_InboundPrecedesCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_InboundReferencesCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_Name = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_Modules_Content_NumberOfBusinessRules = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_NumberOfCloseDatabaseOperation = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_NumberOfDeclareDatabaseOperation = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_NumberOfErrorProcessingRules = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_NumberOfFieldComputationRules = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_NumberOfReadDatabaseOperation = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_NumberOfTechnicalRules = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_NumberOfValidationRules = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_NumberOfWriteDatabaseOperation = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutAccessesTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_OutAccessesType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_OutArtificialTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_OutArtificialType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_OutCallsTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_OutCallsType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_OutContainsTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_OutContainsType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_OutIncludesTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_OutIncludesType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_OutNoneTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_OutNoneType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_OutPrecedesTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_OutPrecedesType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_OutReferencesTechnology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_OutReferencesType = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_Modules_Content_OutboundAccessesCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutboundArtificialCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutboundCallsCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutboundContainsCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutboundDependencyCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutboundIncludesCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutboundNoneCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutboundPrecedesCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_OutboundReferencesCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_ReachabilityBlockNames_Name = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Modules_Content_Representation = {
  eq?: InputMaybe<Innowake_Mining_Shared_Entities_ModulePojo__Representation>;
};

export type FilterObject_Modules_Content_RequiresReview = {
  eq?: InputMaybe<Scalars['Boolean']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['Boolean']>>>;
  is?: InputMaybe<Scalars['Boolean']>;
};

export type FilterObject_Modules_Content_SourceMetrics_CodeLines = {
  eq?: InputMaybe<Scalars['Int']>;
  gte?: InputMaybe<Scalars['Int']>;
  lte?: InputMaybe<Scalars['Int']>;
};

export type FilterObject_Modules_Content_SourceMetrics_CommentLines = {
  eq?: InputMaybe<Scalars['Int']>;
  gte?: InputMaybe<Scalars['Int']>;
  lte?: InputMaybe<Scalars['Int']>;
};

export type FilterObject_Modules_Content_SourceMetrics_ComplexityMcCabe = {
  eq?: InputMaybe<Scalars['Int']>;
  gt?: InputMaybe<Scalars['Int']>;
  gte?: InputMaybe<Scalars['Int']>;
  lt?: InputMaybe<Scalars['Int']>;
  lte?: InputMaybe<Scalars['Int']>;
};

export type FilterObject_Modules_Content_SourceMetrics_DeadCodeLines = {
  eq?: InputMaybe<Scalars['Int']>;
  gte?: InputMaybe<Scalars['Int']>;
  lte?: InputMaybe<Scalars['Int']>;
};

export type FilterObject_Modules_Content_SourceMetrics_PhysicalLines = {
  eq?: InputMaybe<Scalars['Int']>;
  gte?: InputMaybe<Scalars['Int']>;
  lte?: InputMaybe<Scalars['Int']>;
};

export type FilterObject_Modules_Content_Storage = {
  eq?: InputMaybe<Storage>;
};

export type FilterObject_Modules_Content_Taxonomies_Id = {
  eq?: InputMaybe<Scalars['EntityId']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
  notEq?: InputMaybe<Scalars['EntityId']>;
  notIn?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
};

export type FilterObject_Modules_Content_Taxonomies_Name = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
  notEq?: InputMaybe<Scalars['String']>;
  notIn?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FilterObject_Modules_Content_TaxonomyCount = {
  eq?: InputMaybe<Scalars['Long']>;
  gte?: InputMaybe<Scalars['Long']>;
  lte?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Modules_Content_Technology = {
  eq?: InputMaybe<Technology>;
  in?: InputMaybe<Array<InputMaybe<Technology>>>;
  notEq?: InputMaybe<Technology>;
  notIn?: InputMaybe<Array<InputMaybe<Technology>>>;
};

export type FilterObject_Modules_Content_Type = {
  eq?: InputMaybe<Type>;
  in?: InputMaybe<Array<InputMaybe<Type>>>;
};

export type FilterObject_ReachabilityData = {
  _and?: InputMaybe<Array<InputMaybe<FilterObject_ReachabilityData>>>;
  _not?: InputMaybe<FilterObject_ReachabilityData>;
  _or?: InputMaybe<Array<InputMaybe<FilterObject_ReachabilityData>>>;
  content_lowerBoundTaxonomies?: InputMaybe<FilterObject_ReachabilityData_Content_LowerBoundTaxonomies>;
  content_pathTaxonomies?: InputMaybe<FilterObject_ReachabilityData_Content_PathTaxonomies>;
  content_upperBoundTaxonomies?: InputMaybe<FilterObject_ReachabilityData_Content_UpperBoundTaxonomies>;
};

export type FilterObject_ReachabilityData_Content_LowerBoundTaxonomies = {
  eq?: InputMaybe<Scalars['EntityId']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
};

export type FilterObject_ReachabilityData_Content_PathTaxonomies = {
  eq?: InputMaybe<Scalars['EntityId']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
};

export type FilterObject_ReachabilityData_Content_UpperBoundTaxonomies = {
  eq?: InputMaybe<Scalars['EntityId']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
};

export type FilterObject_Statements = {
  _and?: InputMaybe<Array<InputMaybe<FilterObject_Statements>>>;
  _not?: InputMaybe<FilterObject_Statements>;
  _or?: InputMaybe<Array<InputMaybe<FilterObject_Statements>>>;
  content_halsteadComplexity?: InputMaybe<FilterObject_Statements_Content_HalsteadComplexity>;
  content_module_id?: InputMaybe<FilterObject_Statements_Content_Module_Id>;
  content_module_name?: InputMaybe<FilterObject_Statements_Content_Module_Name>;
  content_module_path?: InputMaybe<FilterObject_Statements_Content_Module_Path>;
  content_taxonomy?: InputMaybe<FilterObject_Statements_Content_Taxonomy>;
  content_technology?: InputMaybe<FilterObject_Statements_Content_Technology>;
  content_text?: InputMaybe<FilterObject_Statements_Content_Text>;
  content_textLength?: InputMaybe<FilterObject_Statements_Content_TextLength>;
  content_type?: InputMaybe<FilterObject_Statements_Content_Type>;
};

export type FilterObject_Statements_Content_HalsteadComplexity = {
  eq?: InputMaybe<Scalars['JSON']>;
  gt?: InputMaybe<Scalars['JSON']>;
  gte?: InputMaybe<Scalars['JSON']>;
  lt?: InputMaybe<Scalars['JSON']>;
  lte?: InputMaybe<Scalars['JSON']>;
};

export type FilterObject_Statements_Content_Module_Id = {
  eq?: InputMaybe<Scalars['Long']>;
};

export type FilterObject_Statements_Content_Module_Name = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Statements_Content_Module_Path = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Statements_Content_Taxonomy = {
  eq?: InputMaybe<Scalars['EntityId']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['EntityId']>>>;
};

export type FilterObject_Statements_Content_Technology = {
  eq?: InputMaybe<Technology>;
};

export type FilterObject_Statements_Content_Text = {
  eq?: InputMaybe<Scalars['String']>;
};

export type FilterObject_Statements_Content_TextLength = {
  eq?: InputMaybe<Scalars['Int']>;
  gt?: InputMaybe<Scalars['Int']>;
  gte?: InputMaybe<Scalars['Int']>;
  lt?: InputMaybe<Scalars['Int']>;
  lte?: InputMaybe<Scalars['Int']>;
};

export type FilterObject_Statements_Content_Type = {
  eq?: InputMaybe<Scalars['String']>;
  in?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
  notEq?: InputMaybe<Scalars['String']>;
  notIn?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type FunctionalBlock = {
  __typename?: 'FunctionalBlock';
  blocksWithDeletedUB?: Maybe<Scalars['Boolean']>;
  children?: Maybe<Paged_FunctionalBlock>;
  childrenDeep?: Maybe<Paged_FunctionalBlock>;
  customProperties?: Maybe<Scalars['JSON']>;
  deepName?: Maybe<Scalars['String']>;
  description?: Maybe<Scalars['String']>;
  flags?: Maybe<Scalars['JSON']>;
  generatedFrom?: Maybe<GeneratedFrom>;
  id?: Maybe<Scalars['Long']>;
  links?: Maybe<Array<Maybe<FunctionalBlockLink>>>;
  lowerBoundAccessTypes?: Maybe<Array<Maybe<Scalars['String']>>>;
  moduleParts?: Maybe<Array<Maybe<Innowake_Mining_Shared_Entities_Functionalblocks_ModulePart>>>;
  name?: Maybe<Scalars['String']>;
  outdatedBlock?: Maybe<Scalars['Boolean']>;
  parents?: Maybe<Paged_FunctionalBlock>;
  peers?: Maybe<Paged_FunctionalBlock>;
  project?: Maybe<Project>;
  reachabilityData?: Maybe<Paged_ReachabilityData>;
  /** Referenced Data Dictionaries: Data Dictionaries referenced by the Functional Block */
  referencedDataDictionaries?: Maybe<Array<Maybe<Scalars['UUID']>>>;
  /** Referenced Data Dictionaries Names: Data Dictionaries Names referenced by the Functional Block. */
  referencedDataDictionaryNames?: Maybe<Array<Maybe<Scalars['String']>>>;
  resolvedModuleParts?: Maybe<Array<Maybe<ResolvedModulePart>>>;
  status?: Maybe<Innowake_Mining_Shared_Entities_Functionalblocks_FunctionalBlockStatus>;
  type?: Maybe<Array<Maybe<FunctionalBlockType>>>;
  uid?: Maybe<Scalars['UUID']>;
  updated?: Maybe<Scalars['DateTime']>;
  updatedTimestamp?: Maybe<Scalars['Timestamp']>;
};


export type FunctionalBlockChildrenArgs = {
  filterObject?: InputMaybe<FilterObject_FunctionalBlocks>;
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
};


export type FunctionalBlockChildrenDeepArgs = {
  filterObject?: InputMaybe<FilterObject_FunctionalBlocks>;
  maxDepth?: InputMaybe<Scalars['Int']>;
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
};


export type FunctionalBlockParentsArgs = {
  filterObject?: InputMaybe<FilterObject_FunctionalBlocks>;
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
};


export type FunctionalBlockPeersArgs = {
  filterObject?: InputMaybe<FilterObject_FunctionalBlocks>;
  page?: InputMaybe<Scalars['Int']>;
  peerType?: InputMaybe<FunctionalBlockType>;
  size?: InputMaybe<Scalars['Int']>;
};


export type FunctionalBlockReachabilityDataArgs = {
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
};

export type FunctionalBlockLink = {
  __typename?: 'FunctionalBlockLink';
  childA?: Maybe<FunctionalBlock>;
  childB?: Maybe<FunctionalBlock>;
  condition?: Maybe<Innowake_Mining_Shared_Entities_Functionalblocks_FunctionalBlockLinkCondition>;
  conditionLabel?: Maybe<Scalars['String']>;
  flags?: Maybe<Scalars['JSON']>;
  parent?: Maybe<Scalars['UUID']>;
  uid?: Maybe<Scalars['UUID']>;
};

export enum FunctionalBlockType {
  CallChain = 'CALL_CHAIN',
  FunctionalCondition = 'FUNCTIONAL_CONDITION',
  FunctionalGroup = 'FUNCTIONAL_GROUP',
  FunctionalStatement = 'FUNCTIONAL_STATEMENT',
  FunctionalUnit = 'FUNCTIONAL_UNIT',
  MergeParent = 'MERGE_PARENT',
  Module = 'MODULE',
  RaAccessModule = 'RA_ACCESS_MODULE',
  RaBottomUp = 'RA_BOTTOM_UP',
  RaLowerBound = 'RA_LOWER_BOUND',
  RaTopDown = 'RA_TOP_DOWN',
  RaUpperBound = 'RA_UPPER_BOUND',
  Reachability = 'REACHABILITY',
  ReachabilityNetwork = 'REACHABILITY_NETWORK',
  Structural = 'STRUCTURAL'
}

export type GeneratedFrom = {
  __typename?: 'GeneratedFrom';
  annotation?: Maybe<Annotation>;
  annotationId?: Maybe<Scalars['EntityId']>;
  contentChanged?: Maybe<Scalars['DateTime']>;
  contentChangedTimestamp?: Maybe<Scalars['Timestamp']>;
  dependencyChanged?: Maybe<Scalars['DateTime']>;
  dependencyChangedTimestamp?: Maybe<Scalars['Timestamp']>;
  missing?: Maybe<Scalars['Boolean']>;
  missingSince?: Maybe<Scalars['DateTime']>;
  missingSinceTimestamp?: Maybe<Scalars['Timestamp']>;
  module?: Maybe<Module>;
  moduleContentHash?: Maybe<Scalars['String']>;
  moduleDependencyHash?: Maybe<Scalars['String']>;
  moduleLinkHash?: Maybe<Scalars['String']>;
};

export enum Identification {
  Identified = 'IDENTIFIED',
  Missing = 'MISSING'
}

export type Map_String_Array_Of_String = {
  __typename?: 'MAP_STRING_ARRAY_OF_STRING';
  key?: Maybe<Scalars['String']>;
  value?: Maybe<Array<Maybe<Scalars['String']>>>;
};

export type Map_String_String = {
  __typename?: 'MAP_STRING_STRING';
  key?: Maybe<Scalars['String']>;
  value?: Maybe<Scalars['String']>;
};

export type Map_String_Java_Lang_Object = {
  __typename?: 'MAP_STRING_java_lang_Object';
  key?: Maybe<Scalars['String']>;
};

export type ModelAlgorithmOption = {
  __typename?: 'ModelAlgorithmOption';
  name?: Maybe<Scalars['String']>;
  title?: Maybe<Scalars['String']>;
  value?: Maybe<Scalars['String']>;
};

export type ModelCluster = {
  __typename?: 'ModelCluster';
  clusterDescription?: Maybe<Scalars['String']>;
  clusterIndex?: Maybe<Scalars['Int']>;
  clusterTitle?: Maybe<Scalars['String']>;
  communityId?: Maybe<Scalars['String']>;
  moduleCount?: Maybe<Scalars['Int']>;
  modules?: Maybe<Paged_ModuleInDnaCluster>;
};


export type ModelClusterModulesArgs = {
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
  sortBy?: InputMaybe<Array<InputMaybe<Scalars['String']>>>;
};

export type ModelClustering = {
  __typename?: 'ModelClustering';
  algorithm?: Maybe<Array<Maybe<Map_String_String>>>;
  clusters?: Maybe<Array<Maybe<ModelCluster>>>;
  options?: Maybe<Array<Maybe<ModelAlgorithmOption>>>;
};

export type ModelDna = {
  __typename?: 'ModelDna';
  clusterings?: Maybe<Array<Maybe<ModelClustering>>>;
  moduleCount?: Maybe<Scalars['Int']>;
};

export type Module = {
  __typename?: 'Module';
  /** Type-Technology is Supported: If the module type-technology combination is supported or not. */
  actuallySupported?: Maybe<Scalars['Boolean']>;
  /** Number of Annotations: Number of all Annotations of a Module */
  annotationCount?: Maybe<Scalars['Long']>;
  /** Complexity Level: Complexity Ranking of the Module */
  complexityLevel?: Maybe<Innowake_Mining_Shared_Model_ComplexityLevel>;
  content?: Maybe<Scalars['String']>;
  contentHash?: Maybe<Scalars['String']>;
  /** Creator: How the Module was created */
  creator?: Maybe<Creator>;
  customProperties?: Maybe<Scalars['JSON']>;
  dataDictionaryEntryCount?: Maybe<Scalars['Int']>;
  /** Dependencies: List of Dependencies of the Module */
  dependencies?: Maybe<Array<Maybe<Innowake_Mining_Server_Graphql_Controller_ModulesGraphQlController__ModuleRelation>>>;
  /** Number of dependencies: Number of all direct incoming or outbound dependencies of the Module */
  dependencyCount?: Maybe<Scalars['Long']>;
  dependencyHash?: Maybe<Scalars['String']>;
  /** Module Description: Text describing the functionality of the Module */
  description?: Maybe<Scalars['String']>;
  /** Number of Errors: Number of errors for the Module */
  errorCount?: Maybe<Scalars['Int']>;
  /** Module Errors */
  errors?: Maybe<Scalars['Int']>;
  fieldInfos?: Maybe<Array<Maybe<FieldInfo>>>;
  /** Module Id: The unique id of the Module */
  id?: Maybe<Scalars['Long']>;
  /** File Identified / File Missing: Whether the Module is included in the code base or missing */
  identification?: Maybe<Identification>;
  /** Filter for the Technology of inbound accesses dependencies of the Module */
  inAccessesTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of inbound accesses dependencies of the Module */
  inAccessesType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of inbound artificial dependencies of the Module */
  inArtificialTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of inbound artificial dependencies of the Module */
  inArtificialType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of inbound calls dependencies of the Module */
  inCallsTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of inbound calls dependencies of the Module */
  inCallsType?: Maybe<Array<Maybe<Type>>>;
  /** In Codebase: Whether the source file of this Module is present in the codebase or Module is contained within another Module. */
  inCodebase?: Maybe<Scalars['Boolean']>;
  /** Filter for the Technology of inbound contains dependencies of the Module */
  inContainsTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of inbound contains dependencies of the Module */
  inContainsType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of inbound includes dependencies of the Module */
  inIncludesTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of inbound includes dependencies of the Module */
  inIncludesType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of inbound none dependencies of the Module */
  inNoneTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of inbound none dependencies of the Module */
  inNoneType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of inbound precedes dependencies of the Module */
  inPrecedesTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of inbound precedes dependencies of the Module */
  inPrecedesType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of inbound references dependencies of the Module */
  inReferencesTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of inbound references dependencies of the Module */
  inReferencesType?: Maybe<Array<Maybe<Type>>>;
  /** Number of inbound accesses dependencies: Number of direct inbound accesses dependencies of the Module */
  inboundAccessesCount?: Maybe<Scalars['Long']>;
  /** Number of inbound artificial dependencies: Number of direct inbound artificial dependencies of the Module */
  inboundArtificialCount?: Maybe<Scalars['Long']>;
  /** Number of inbound calls dependencies: Number of direct inbound calls dependencies of the Module */
  inboundCallsCount?: Maybe<Scalars['Long']>;
  /** Number of inbound contains dependencies: Number of direct inbound contains dependencies of the Module */
  inboundContainsCount?: Maybe<Scalars['Long']>;
  /** Number of inbound dependencies: Number of all direct incoming or outbound dependencies of the Module */
  inboundDependencyCount?: Maybe<Scalars['Long']>;
  /** Number of inbound includes dependencies: Number of direct inbound includes dependencies of the Module */
  inboundIncludesCount?: Maybe<Scalars['Long']>;
  /** Number of inbound none dependencies: Number of direct inbound none dependencies of the Module */
  inboundNoneCount?: Maybe<Scalars['Long']>;
  /** Number of inbound precedes dependencies: Number of direct inbound precedes dependencies of the Module */
  inboundPrecedesCount?: Maybe<Scalars['Long']>;
  /** Number of inbound references dependencies: Number of direct inbound references dependencies of the Module */
  inboundReferencesCount?: Maybe<Scalars['Long']>;
  info?: Maybe<Map_String_Java_Lang_Object>;
  /** Link Hash: Unique link hash of the Module */
  linkHash?: Maybe<Scalars['String']>;
  location?: Maybe<ModuleLocation>;
  /** Last Scan: The date and time when the code metrics and dependencies for this Module were last updated */
  metricsDate?: Maybe<Scalars['DateTime']>;
  metricsDateTimestamp?: Maybe<Scalars['Timestamp']>;
  /** Missing Dependencies: List of all Dependency modules that are missing in the codebase */
  missingDependencies?: Maybe<Array<Maybe<Module>>>;
  /** Last modified: The date when this Module was last modified (either through code scanning or manually) */
  modifiedDate?: Maybe<Scalars['DateTime']>;
  modifiedDateTimestamp?: Maybe<Scalars['Timestamp']>;
  /** Module Name: The module for which the data dictionary entry field was created */
  name?: Maybe<Scalars['String']>;
  /** Percentage of annotated Statements: Displays the amount of Statements covered by Annotations */
  numberOfAnnotatedStatementNodes?: Maybe<Scalars['Float']>;
  /** Number of Business Rules: Number of all annotation with Type - Rule and Category - Business Rule */
  numberOfBusinessRules?: Maybe<Scalars['Long']>;
  /** Number of Close Database Operations: Number of all annotation with Type - Database and Category - Close */
  numberOfCloseDatabaseOperation?: Maybe<Scalars['Long']>;
  /** Number of Declare Database Operations: Number of all annotation with Type - Database and Category - Declare */
  numberOfDeclareDatabaseOperation?: Maybe<Scalars['Long']>;
  /** Number of Error Processing Rules: Number of all annotation with Type - Rule and Category - Error Processing Rule */
  numberOfErrorProcessingRules?: Maybe<Scalars['Long']>;
  /** Number of Field Computation Rules: Number of all annotation with Type - Rule and Category - Field Computation Rule */
  numberOfFieldComputationRules?: Maybe<Scalars['Long']>;
  /** Number of Read Database Operations: Number of all annotation with Type - Database and Category - Read */
  numberOfReadDatabaseOperation?: Maybe<Scalars['Long']>;
  /** Number of Technical Rules: Number of all annotation with Type - Rule and Category - Technical Rule */
  numberOfTechnicalRules?: Maybe<Scalars['Long']>;
  /** Number of Validation Rules: Number of all annotation with Type - Rule and Category - Validation Rule */
  numberOfValidationRules?: Maybe<Scalars['Long']>;
  /** Number of Write Database Operations: Number of all annotation with Type - Database and Category - Write */
  numberOfWriteDatabaseOperation?: Maybe<Scalars['Long']>;
  origin?: Maybe<Origin>;
  /** Filter for the Technology of outbound accesses dependencies of the Module */
  outAccessesTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of outbound accesses dependencies of the Module */
  outAccessesType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of outbound artificial dependencies of the Module */
  outArtificialTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of outbound artificial dependencies of the Module */
  outArtificialType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of outbound calls dependencies of the Module */
  outCallsTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of outbound calls dependencies of the Module */
  outCallsType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of outbound contains dependencies of the Module */
  outContainsTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of outbound contains dependencies of the Module */
  outContainsType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of outbound includes dependencies of the Module */
  outIncludesTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of outbound includes dependencies of the Module */
  outIncludesType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of outbound none dependencies of the Module */
  outNoneTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of outbound none dependencies of the Module */
  outNoneType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of outbound precedes dependencies of the Module */
  outPrecedesTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of outbound precedes dependencies of the Module */
  outPrecedesType?: Maybe<Array<Maybe<Type>>>;
  /** Filter for the Technology of outbound references dependencies of the Module */
  outReferencesTechnology?: Maybe<Array<Maybe<Technology>>>;
  /** Filter for the Type of outbound references dependencies of the Module */
  outReferencesType?: Maybe<Array<Maybe<Type>>>;
  /** Number of outbound accesses dependencies: Number of direct outbound accesses dependencies of the Module */
  outboundAccessesCount?: Maybe<Scalars['Long']>;
  /** Number of outbound artificial dependencies: Number of direct outbound artificial dependencies of the Module */
  outboundArtificialCount?: Maybe<Scalars['Long']>;
  /** Number of outbound calls dependencies: Number of direct outbound calls dependencies of the Module */
  outboundCallsCount?: Maybe<Scalars['Long']>;
  /** Number of outbound contains dependencies: Number of direct outbound contains dependencies of the Module */
  outboundContainsCount?: Maybe<Scalars['Long']>;
  /** Number of outbound dependencies: Number of all direct dependencies of the Module */
  outboundDependencyCount?: Maybe<Scalars['Long']>;
  /** Number of outbound includes dependencies: Number of direct outbound includes dependencies of the Module */
  outboundIncludesCount?: Maybe<Scalars['Long']>;
  /** Number of outbound none dependencies: Number of direct outbound none dependencies of the Module */
  outboundNoneCount?: Maybe<Scalars['Long']>;
  /** Number of outbound precedes dependencies: Number of direct outbound precedes dependencies of the Module */
  outboundPrecedesCount?: Maybe<Scalars['Long']>;
  /** Number of outbound references dependencies: Number of direct outbound references dependencies of the Module */
  outboundReferencesCount?: Maybe<Scalars['Long']>;
  parent?: Maybe<Scalars['EntityId']>;
  parentNid?: Maybe<Scalars['Long']>;
  parentPath?: Maybe<Scalars['String']>;
  parentUid?: Maybe<Scalars['UUID']>;
  /** Path: Path of the file containing the Module */
  path?: Maybe<Scalars['String']>;
  project?: Maybe<Scalars['EntityId']>;
  projectId?: Maybe<Scalars['Long']>;
  projectNid?: Maybe<Scalars['Long']>;
  projectUid?: Maybe<Scalars['UUID']>;
  reachabilityBlocks?: Maybe<Array<Maybe<FunctionalBlock>>>;
  /** Representation */
  representation?: Maybe<Innowake_Mining_Shared_Entities_ModulePojo__Representation>;
  /** Requires Review: Whether the annotations and other metadata of this Module require review by a user */
  requiresReview?: Maybe<Scalars['Boolean']>;
  source?: Maybe<Scalars['UUID']>;
  sourceCodeAvailable?: Maybe<Scalars['Boolean']>;
  sourceMetrics?: Maybe<SourceMetrics>;
  /** SQL Statement Count */
  sqlStatements?: Maybe<Scalars['Int']>;
  /** Statement Count */
  statements?: Maybe<Scalars['Int']>;
  /** Storage */
  storage?: Maybe<Storage>;
  /** Taxonomies: Taxonomies of the Module */
  taxonomies?: Maybe<Array<Maybe<Taxonomy>>>;
  /** Number of assigned taxonomies: Number of all Taxonomies of the Module */
  taxonomyCount?: Maybe<Scalars['Long']>;
  /** Technology: Technology of the Module */
  technology?: Maybe<Technology>;
  /** Type: Type of the Module */
  type?: Maybe<Type>;
  /** Module UID */
  uid?: Maybe<Scalars['UUID']>;
};


export type ModuleAnnotationCountArgs = {
  categoryName?: InputMaybe<Scalars['String']>;
  state?: InputMaybe<WorkingState>;
  type?: InputMaybe<AnnotationType>;
};


export type ModuleDependenciesArgs = {
  direction?: InputMaybe<Innowake_Mining_Shared_Model_RelationshipDirection>;
  filterObject?: InputMaybe<FilterObject_Modules>;
  properties?: InputMaybe<Scalars['JSON']>;
  type?: InputMaybe<Array<InputMaybe<RelationshipType>>>;
};


export type ModuleDependencyCountArgs = {
  direction?: InputMaybe<Innowake_Mining_Shared_Model_RelationshipDirection>;
  type?: InputMaybe<RelationshipType>;
};


export type ModuleInboundDependencyCountArgs = {
  type?: InputMaybe<RelationshipType>;
};


export type ModuleNumberOfBusinessRulesArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleNumberOfCloseDatabaseOperationArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleNumberOfDeclareDatabaseOperationArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleNumberOfErrorProcessingRulesArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleNumberOfFieldComputationRulesArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleNumberOfReadDatabaseOperationArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleNumberOfTechnicalRulesArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleNumberOfValidationRulesArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleNumberOfWriteDatabaseOperationArgs = {
  state?: InputMaybe<WorkingState>;
};


export type ModuleOutboundDependencyCountArgs = {
  type?: InputMaybe<RelationshipType>;
};


export type ModuleTaxonomyCountArgs = {
  type?: InputMaybe<Scalars['String']>;
};

export type ModuleInDnaCluster = {
  __typename?: 'ModuleInDNACluster';
  /** DNA Cluster Index: The index of the DNA Cluster this Module belongs to */
  clusterIndex?: Maybe<Scalars['Int']>;
  /** DNA Community UUID: The UUID of the DNA Community */
  communityId?: Maybe<Scalars['UUID']>;
  /** Module: The Module in the Cluster */
  module?: Maybe<Module>;
};

export type ModuleLocation = {
  __typename?: 'ModuleLocation';
  length?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
};

export type ModuleRelationship = {
  __typename?: 'ModuleRelationship';
  dependencyAttributes?: Maybe<Scalars['String']>;
  dependencyBinding?: Maybe<Innowake_Mining_Shared_Model_Binding>;
  dependencyDefinition?: Maybe<Scalars['UUID']>;
  direction?: Maybe<Innowake_Mining_Shared_Model_RelationshipDirection>;
  dstLocation?: Maybe<ModuleLocation>;
  dstModule?: Maybe<Scalars['UUID']>;
  dstModuleDetails?: Maybe<Innowake_Mining_Shared_Entities_ModuleBasePojo>;
  id?: Maybe<Scalars['UUID']>;
  properties?: Maybe<Scalars['JSON']>;
  relationship?: Maybe<RelationshipType>;
  srcLocation?: Maybe<ModuleLocation>;
  srcModule?: Maybe<Scalars['UUID']>;
  srcModuleDetails?: Maybe<Innowake_Mining_Shared_Entities_ModuleBasePojo>;
  validIfReachedFrom?: Maybe<Array<Maybe<Scalars['UUID']>>>;
};

export type ModuleRelationshipBase = {
  __typename?: 'ModuleRelationshipBase';
  dstLocation?: Maybe<ModuleLocation>;
  dstModule?: Maybe<Scalars['UUID']>;
  id?: Maybe<Scalars['UUID']>;
  properties?: Maybe<Scalars['JSON']>;
  relationship?: Maybe<RelationshipType>;
  srcLocation?: Maybe<ModuleLocation>;
  srcModule?: Maybe<Scalars['UUID']>;
};

export enum Origin {
  Custom = 'CUSTOM',
  Environment = 'ENVIRONMENT'
}

export type Paged_Annotation = {
  __typename?: 'PAGED_Annotation';
  content?: Maybe<Array<Maybe<Annotation>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Paged_DataDictionaryEntry = {
  __typename?: 'PAGED_DataDictionaryEntry';
  content?: Maybe<Array<Maybe<DataDictionaryEntry>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Paged_DependencyInformation = {
  __typename?: 'PAGED_DependencyInformation';
  content?: Maybe<Array<Maybe<DependencyInformation>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Paged_FunctionalBlock = {
  __typename?: 'PAGED_FunctionalBlock';
  aggregations?: Maybe<Array<Maybe<Paged_FunctionalBlock_Aggregations_Aggregations>>>;
  content?: Maybe<Array<Maybe<FunctionalBlock>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Paged_FunctionalBlock_Aggregations_Aggregations = {
  __typename?: 'PAGED_FunctionalBlock_aggregations_aggregations';
  fields?: Maybe<Paged_FunctionalBlock_Aggregations_Aggregations_Fields>;
  groupBy?: Maybe<Paged_FunctionalBlock_Aggregations_Aggregations_GroupBy>;
};

export type Paged_FunctionalBlock_Aggregations_Aggregations_Project_Id_Operators = {
  __typename?: 'PAGED_FunctionalBlock_aggregations_aggregations_PROJECT_ID_operators';
  COUNT?: Maybe<Scalars['Long']>;
  LIST?: Maybe<Array<Maybe<Scalars['JSON']>>>;
};

export type Paged_FunctionalBlock_Aggregations_Aggregations_Referenced_Module_Technology_Operators = {
  __typename?: 'PAGED_FunctionalBlock_aggregations_aggregations_REFERENCED_MODULE_TECHNOLOGY_operators';
  COUNT?: Maybe<Scalars['Long']>;
  LIST?: Maybe<Array<Maybe<Scalars['JSON']>>>;
};

export type Paged_FunctionalBlock_Aggregations_Aggregations_Referenced_Module_Type_Operators = {
  __typename?: 'PAGED_FunctionalBlock_aggregations_aggregations_REFERENCED_MODULE_TYPE_operators';
  COUNT?: Maybe<Scalars['Long']>;
  LIST?: Maybe<Array<Maybe<Scalars['JSON']>>>;
};

export type Paged_FunctionalBlock_Aggregations_Aggregations_Type_Operators = {
  __typename?: 'PAGED_FunctionalBlock_aggregations_aggregations_TYPE_operators';
  COUNT?: Maybe<Scalars['Long']>;
  LIST?: Maybe<Array<Maybe<Scalars['JSON']>>>;
};

export type Paged_FunctionalBlock_Aggregations_Aggregations_Uid_Operators = {
  __typename?: 'PAGED_FunctionalBlock_aggregations_aggregations_UID_operators';
  COUNT?: Maybe<Scalars['Long']>;
  LIST?: Maybe<Array<Maybe<Scalars['JSON']>>>;
};

export type Paged_FunctionalBlock_Aggregations_Aggregations_Fields = {
  __typename?: 'PAGED_FunctionalBlock_aggregations_aggregations_fields';
  PROJECT_ID?: Maybe<Paged_FunctionalBlock_Aggregations_Aggregations_Project_Id_Operators>;
  REFERENCED_MODULE_TECHNOLOGY?: Maybe<Paged_FunctionalBlock_Aggregations_Aggregations_Referenced_Module_Technology_Operators>;
  REFERENCED_MODULE_TYPE?: Maybe<Paged_FunctionalBlock_Aggregations_Aggregations_Referenced_Module_Type_Operators>;
  TYPE?: Maybe<Paged_FunctionalBlock_Aggregations_Aggregations_Type_Operators>;
  UID?: Maybe<Paged_FunctionalBlock_Aggregations_Aggregations_Uid_Operators>;
};

export type Paged_FunctionalBlock_Aggregations_Aggregations_GroupBy = {
  __typename?: 'PAGED_FunctionalBlock_aggregations_aggregations_groupBy';
  PROJECT_ID?: Maybe<Scalars['JSON']>;
  REFERENCED_MODULE_TECHNOLOGY?: Maybe<Scalars['JSON']>;
  REFERENCED_MODULE_TYPE?: Maybe<Scalars['JSON']>;
  TYPE?: Maybe<Scalars['JSON']>;
  UID?: Maybe<Scalars['JSON']>;
};

export type Paged_Module = {
  __typename?: 'PAGED_Module';
  content?: Maybe<Array<Maybe<Module>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Paged_ModuleInDnaCluster = {
  __typename?: 'PAGED_ModuleInDNACluster';
  content?: Maybe<Array<Maybe<ModuleInDnaCluster>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Paged_ReachabilityData = {
  __typename?: 'PAGED_ReachabilityData';
  content?: Maybe<Array<Maybe<ReachabilityData>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Paged_SchedulerImport = {
  __typename?: 'PAGED_SchedulerImport';
  content?: Maybe<Array<Maybe<SchedulerImport>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Paged_Statement = {
  __typename?: 'PAGED_Statement';
  content?: Maybe<Array<Maybe<Statement>>>;
  limit?: Maybe<Scalars['Int']>;
  number?: Maybe<Scalars['Int']>;
  offset?: Maybe<Scalars['Int']>;
  size?: Maybe<Scalars['Int']>;
  totalElements?: Maybe<Scalars['Int']>;
  totalPages?: Maybe<Scalars['Int']>;
};

export type Project = {
  __typename?: 'Project';
  autoCompletionMap?: Maybe<Scalars['JSON']>;
  client?: Maybe<Client>;
  clientNid?: Maybe<Scalars['Long']>;
  clientUid?: Maybe<Scalars['UUID']>;
  customProperties?: Maybe<Scalars['JSON']>;
  customPropertyClasses?: Maybe<Scalars['JSON']>;
  defaultTaxonomyCategoryId?: Maybe<Scalars['Long']>;
  id?: Maybe<Scalars['Long']>;
  markedDeleted?: Maybe<Scalars['Boolean']>;
  metaDataBackupId?: Maybe<Scalars['String']>;
  metricsBaseRevision?: Maybe<Scalars['Long']>;
  metricsDate?: Maybe<Scalars['DateTime']>;
  metricsDateTimestamp?: Maybe<Scalars['Timestamp']>;
  metricsVersion?: Maybe<Scalars['String']>;
  name?: Maybe<Scalars['String']>;
  searchOrders?: Maybe<Scalars['JSON']>;
  sourceCodeRevision?: Maybe<Scalars['Long']>;
  technicalTaxonomyCategoryId?: Maybe<Scalars['Long']>;
  uid?: Maybe<Scalars['UUID']>;
};

export type Query = {
  __typename?: 'Query';
  annotations?: Maybe<Paged_Annotation>;
  client?: Maybe<Client>;
  customProperties?: Maybe<Array<Maybe<CustomProperty>>>;
  dataDictionaries?: Maybe<Paged_DataDictionaryEntry>;
  dnaData?: Maybe<ModelDna>;
  dnaModulesInCluster?: Maybe<Paged_ModuleInDnaCluster>;
  fieldInfos?: Maybe<Array<Maybe<FieldInfo>>>;
  functionalBlock?: Maybe<FunctionalBlock>;
  functionalBlocks?: Maybe<Paged_FunctionalBlock>;
  moduleDependencies?: Maybe<Paged_DependencyInformation>;
  modules?: Maybe<Paged_Module>;
  project?: Maybe<Project>;
  reachabilityData?: Maybe<Paged_ReachabilityData>;
  schedulerImports?: Maybe<Paged_SchedulerImport>;
  statements?: Maybe<Paged_Statement>;
  taxonomyCategories?: Maybe<Innowake_Mining_Server_Graphql_Controller_TaxonomyCategoriesGraphQlController__AggregationResult>;
};


export type QueryAnnotationsArgs = {
  filterObject?: InputMaybe<FilterObject_Annotations>;
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['Long']>;
  size?: InputMaybe<Scalars['Int']>;
  sortObject?: InputMaybe<Array<InputMaybe<SortObject_Annotations>>>;
  useOpenSearch?: InputMaybe<Scalars['Boolean']>;
};


export type QueryClientArgs = {
  clientId?: InputMaybe<Scalars['Long']>;
};


export type QueryCustomPropertiesArgs = {
  entityName?: InputMaybe<Scalars['String']>;
  projectId?: InputMaybe<Scalars['Long']>;
};


export type QueryDataDictionariesArgs = {
  filterObject?: InputMaybe<FilterObject_DataDictionaries>;
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['Long']>;
  size?: InputMaybe<Scalars['Int']>;
  sortObject?: InputMaybe<Array<InputMaybe<SortObject_DataDictionaries>>>;
};


export type QueryDnaDataArgs = {
  projectId?: InputMaybe<Scalars['EntityId']>;
  snapshotId?: InputMaybe<Scalars['String']>;
};


export type QueryDnaModulesInClusterArgs = {
  algorithm?: InputMaybe<Scalars['String']>;
  clusterIndex?: InputMaybe<Scalars['Int']>;
  filterObject?: InputMaybe<FilterObject_DnaModulesInCluster>;
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['EntityId']>;
  size?: InputMaybe<Scalars['Int']>;
  sortObject?: InputMaybe<Array<InputMaybe<SortObject_DnaModulesInCluster>>>;
  updatedTime?: InputMaybe<Scalars['String']>;
};


export type QueryFieldInfosArgs = {
  moduleId?: InputMaybe<Scalars['EntityId']>;
  projectId?: InputMaybe<Scalars['EntityId']>;
};


export type QueryFunctionalBlockArgs = {
  projectId?: InputMaybe<Scalars['Long']>;
  uid?: InputMaybe<Scalars['UUID']>;
};


export type QueryFunctionalBlocksArgs = {
  filterObject?: InputMaybe<FilterObject_FunctionalBlocks>;
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['Long']>;
  size?: InputMaybe<Scalars['Int']>;
  sortObject?: InputMaybe<Array<InputMaybe<SortObject_FunctionalBlocks>>>;
};


export type QueryModuleDependenciesArgs = {
  filterObject?: InputMaybe<FilterObject_ModuleDependencies>;
  moduleId?: InputMaybe<Scalars['Long']>;
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['Long']>;
  size?: InputMaybe<Scalars['Int']>;
};


export type QueryModulesArgs = {
  filterObject?: InputMaybe<FilterObject_Modules>;
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['Long']>;
  size?: InputMaybe<Scalars['Int']>;
  sortObject?: InputMaybe<Array<InputMaybe<SortObject_Modules>>>;
  useOpenSearch?: InputMaybe<Scalars['Boolean']>;
};


export type QueryProjectArgs = {
  projectId?: InputMaybe<Scalars['EntityId']>;
};


export type QueryReachabilityDataArgs = {
  filterObject?: InputMaybe<FilterObject_ReachabilityData>;
  functionalBlocks?: InputMaybe<Array<InputMaybe<Scalars['UUID']>>>;
  includeIntermediateModulesPerAccessModule?: InputMaybe<Scalars['Boolean']>;
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['Long']>;
  size?: InputMaybe<Scalars['Int']>;
  sortObject?: InputMaybe<Array<InputMaybe<SortObject_ReachabilityData>>>;
};


export type QuerySchedulerImportsArgs = {
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['Long']>;
  size?: InputMaybe<Scalars['Int']>;
};


export type QueryStatementsArgs = {
  filterObject?: InputMaybe<FilterObject_Statements>;
  page?: InputMaybe<Scalars['Int']>;
  projectId?: InputMaybe<Scalars['Long']>;
  size?: InputMaybe<Scalars['Int']>;
};


export type QueryTaxonomyCategoriesArgs = {
  projectId?: InputMaybe<Scalars['EntityId']>;
};

export type ReachabilityData = {
  __typename?: 'ReachabilityData';
  accessModuleIds?: Maybe<Array<Maybe<Scalars['EntityId']>>>;
  accessModules?: Maybe<Array<Maybe<Module>>>;
  accessTypes?: Maybe<Array<Maybe<Scalars['String']>>>;
  /** Data Access: Access types by which the upper bound accesses the lower bound */
  dataAccessType?: Maybe<Array<Maybe<Scalars['String']>>>;
  functionalBlock?: Maybe<Scalars['UUID']>;
  intermediateModules?: Maybe<Array<Maybe<Scalars['EntityId']>>>;
  intermediateModulesData?: Maybe<Array<Maybe<Module>>>;
  lowerBoundModuleId?: Maybe<Scalars['EntityId']>;
  lowerBoundModules?: Maybe<Module>;
  moduleTaxonomies?: Maybe<Array<Maybe<Scalars['String']>>>;
  uid?: Maybe<Scalars['UUID']>;
  upperBoundModuleId?: Maybe<Scalars['EntityId']>;
  upperBoundModules?: Maybe<Module>;
};


export type ReachabilityDataModuleTaxonomiesArgs = {
  moduleType?: InputMaybe<Scalars['String']>;
};

export enum RelationshipType {
  Accesses = 'ACCESSES',
  Artificial = 'ARTIFICIAL',
  Calls = 'CALLS',
  Contains = 'CONTAINS',
  Includes = 'INCLUDES',
  None = 'NONE',
  Precedes = 'PRECEDES',
  References = 'REFERENCES'
}

export type ResolvedModulePart = {
  __typename?: 'ResolvedModulePart';
  location?: Maybe<ModuleLocation>;
  module?: Maybe<Module>;
  moduleId?: Maybe<Scalars['EntityId']>;
  referencedTaxonomies?: Maybe<Array<Maybe<Taxonomy>>>;
};

export type SchedulerImport = {
  __typename?: 'SchedulerImport';
  /** Description: The description of the import */
  description?: Maybe<Scalars['String']>;
  /** Identifier: The scheduler import identifier */
  identifier?: Maybe<Scalars['String']>;
  /** Uploaded On: The date when the import was done */
  importedOn?: Maybe<Scalars['DateTime']>;
  importedOnTimestamp?: Maybe<Scalars['Timestamp']>;
  /** Importer: The importer used to import the file */
  importerUsed?: Maybe<Scalars['String']>;
  /** Operations Selected: The operations selected while importing the file */
  operationsSelected?: Maybe<Array<Maybe<Scalars['String']>>>;
  project?: Maybe<Scalars['UUID']>;
  properties?: Maybe<Array<Maybe<Map_String_Java_Lang_Object>>>;
  /** Scheduler Type: The scheduler type */
  schedulerType?: Maybe<Scalars['String']>;
  schedulerVersion?: Maybe<Scalars['String']>;
  source?: Maybe<Scalars['UUID']>;
  uid?: Maybe<Scalars['UUID']>;
};

export type SearchOrder = {
  __typename?: 'SearchOrder';
  source?: Maybe<SearchOrderCandidate>;
  targets?: Maybe<Array<Maybe<SearchOrderCandidate>>>;
};

export type SearchOrderCandidate = {
  __typename?: 'SearchOrderCandidate';
  containedIn?: Maybe<SearchOrderCandidate>;
  name?: Maybe<Scalars['String']>;
  path?: Maybe<Scalars['String']>;
  pathPattern?: Maybe<Scalars['String']>;
  type?: Maybe<Scalars['String']>;
};

export enum SortDirection {
  Asc = 'ASC',
  Ascending = 'ASCENDING',
  Desc = 'DESC',
  Descending = 'DESCENDING'
}

export type SortObject_Annotations = {
  content_annotationOffset?: InputMaybe<SortDirection>;
  content_categoryName?: InputMaybe<SortDirection>;
  content_functionalGroups_name?: InputMaybe<SortDirection>;
  content_id?: InputMaybe<SortDirection>;
  content_moduleName?: InputMaybe<SortDirection>;
  content_module_id?: InputMaybe<SortDirection>;
  content_module_name?: InputMaybe<SortDirection>;
  content_name?: InputMaybe<SortDirection>;
  content_reasons?: InputMaybe<SortDirection>;
  content_state?: InputMaybe<SortDirection>;
  content_type?: InputMaybe<SortDirection>;
};

export type SortObject_DataDictionaries = {
  content_fieldLevel?: InputMaybe<SortDirection>;
  content_id?: InputMaybe<SortDirection>;
  content_length?: InputMaybe<SortDirection>;
  content_module_name?: InputMaybe<SortDirection>;
  content_name?: InputMaybe<SortDirection>;
  content_parentGroup?: InputMaybe<SortDirection>;
  content_translatedFieldValue?: InputMaybe<SortDirection>;
};

export type SortObject_DnaModulesInCluster = {
  content_module_id?: InputMaybe<SortDirection>;
  content_module_name?: InputMaybe<SortDirection>;
  content_module_path?: InputMaybe<SortDirection>;
};

export type SortObject_FunctionalBlocks = {
  content_name?: InputMaybe<SortDirection>;
  content_updated?: InputMaybe<SortDirection>;
};

export type SortObject_Modules = {
  content_annotationCount?: InputMaybe<SortDirection>;
  content_dependencyCount?: InputMaybe<SortDirection>;
  content_errorCount?: InputMaybe<SortDirection>;
  content_id?: InputMaybe<SortDirection>;
  content_inboundAccessesCount?: InputMaybe<SortDirection>;
  content_inboundArtificialCount?: InputMaybe<SortDirection>;
  content_inboundCallsCount?: InputMaybe<SortDirection>;
  content_inboundContainsCount?: InputMaybe<SortDirection>;
  content_inboundDependencyCount?: InputMaybe<SortDirection>;
  content_inboundIncludesCount?: InputMaybe<SortDirection>;
  content_inboundNoneCount?: InputMaybe<SortDirection>;
  content_inboundPrecedesCount?: InputMaybe<SortDirection>;
  content_inboundReferencesCount?: InputMaybe<SortDirection>;
  content_modifiedDate?: InputMaybe<SortDirection>;
  content_name?: InputMaybe<SortDirection>;
  content_numberOfBusinessRules?: InputMaybe<SortDirection>;
  content_numberOfCloseDatabaseOperation?: InputMaybe<SortDirection>;
  content_numberOfDeclareDatabaseOperation?: InputMaybe<SortDirection>;
  content_numberOfErrorProcessingRules?: InputMaybe<SortDirection>;
  content_numberOfFieldComputationRules?: InputMaybe<SortDirection>;
  content_numberOfReadDatabaseOperation?: InputMaybe<SortDirection>;
  content_numberOfTechnicalRules?: InputMaybe<SortDirection>;
  content_numberOfValidationRules?: InputMaybe<SortDirection>;
  content_numberOfWriteDatabaseOperation?: InputMaybe<SortDirection>;
  content_outboundAccessesCount?: InputMaybe<SortDirection>;
  content_outboundArtificialCount?: InputMaybe<SortDirection>;
  content_outboundCallsCount?: InputMaybe<SortDirection>;
  content_outboundContainsCount?: InputMaybe<SortDirection>;
  content_outboundDependencyCount?: InputMaybe<SortDirection>;
  content_outboundIncludesCount?: InputMaybe<SortDirection>;
  content_outboundNoneCount?: InputMaybe<SortDirection>;
  content_outboundPrecedesCount?: InputMaybe<SortDirection>;
  content_outboundReferencesCount?: InputMaybe<SortDirection>;
  content_path?: InputMaybe<SortDirection>;
  content_reachabilityBlockNames_name?: InputMaybe<SortDirection>;
  content_requiresReview?: InputMaybe<SortDirection>;
  content_sourceMetrics_codeLines?: InputMaybe<SortDirection>;
  content_sourceMetrics_commentLines?: InputMaybe<SortDirection>;
  content_sourceMetrics_complexityMcCabe?: InputMaybe<SortDirection>;
  content_sourceMetrics_deadCodeLines?: InputMaybe<SortDirection>;
  content_sourceMetrics_physicalLines?: InputMaybe<SortDirection>;
  content_technology?: InputMaybe<SortDirection>;
  content_type?: InputMaybe<SortDirection>;
};

export type SortObject_ReachabilityData = {
  content_lowerBoundModuleName_name?: InputMaybe<SortDirection>;
  content_lowerBoundModules?: InputMaybe<SortDirection>;
  content_upperBoundModuleName_name?: InputMaybe<SortDirection>;
  content_upperBoundModules?: InputMaybe<SortDirection>;
};

export type SourceMetrics = {
  __typename?: 'SourceMetrics';
  /** Source Lines of Code: Number of source code lines */
  codeLines?: Maybe<Scalars['Int']>;
  /** Comment Lines of Code: Number of lines containing a comment */
  commentLines?: Maybe<Scalars['Int']>;
  /** Program Complexity: Cyclomatic Complexity of the Module */
  complexityMcCabe?: Maybe<Scalars['Int']>;
  /** Lines of Dead Code: Number of lines containing dead code */
  deadCodeLines?: Maybe<Scalars['Int']>;
  module?: Maybe<Scalars['EntityId']>;
  moduleNid?: Maybe<Scalars['Long']>;
  moduleUid?: Maybe<Scalars['UUID']>;
  /** Physical Lines of Code: Number of lines of code in source file including code lines, empty lines and comment lines */
  physicalLines?: Maybe<Scalars['Int']>;
};

export type Statement = {
  __typename?: 'Statement';
  /** Custom Complexity */
  customComplexity?: Maybe<Scalars['JSON']>;
  customProperties?: Maybe<Scalars['JSON']>;
  /** Distinct Tables */
  distinctTables?: Maybe<Scalars['JSON']>;
  /** Halstead Complexity */
  halsteadComplexity?: Maybe<Scalars['JSON']>;
  /** Halstead Difficulty */
  halsteadDifficulty?: Maybe<Scalars['JSON']>;
  id?: Maybe<Scalars['Long']>;
  module?: Maybe<Module>;
  properties?: Maybe<Array<Maybe<Map_String_Java_Lang_Object>>>;
  /** Length */
  sqlLength?: Maybe<Scalars['JSON']>;
  /** Tables */
  tables?: Maybe<Scalars['JSON']>;
  taxonomy?: Maybe<Scalars['EntityId']>;
  /** Technology */
  technology?: Maybe<Technology>;
  /** Text */
  text?: Maybe<Scalars['String']>;
  /** Length */
  textLength?: Maybe<Scalars['Int']>;
  /** Statement Type */
  type?: Maybe<Scalars['String']>;
  uid?: Maybe<Scalars['UUID']>;
};

export enum StatementType {
  Adaprep = 'ADAPREP',
  AllocateCursor = 'ALLOCATE_CURSOR',
  AlterCheckConstraint = 'ALTER_CHECK_CONSTRAINT',
  AlterConstraint = 'ALTER_CONSTRAINT',
  AlterForeignKey = 'ALTER_FOREIGN_KEY',
  AlterFunction = 'ALTER_FUNCTION',
  AlterIndex = 'ALTER_INDEX',
  AlterPrimaryKey = 'ALTER_PRIMARY_KEY',
  AlterProcedure = 'ALTER_PROCEDURE',
  AlterTable = 'ALTER_TABLE',
  AlterView = 'ALTER_VIEW',
  AssociateLocator = 'ASSOCIATE_LOCATOR',
  Call = 'CALL',
  Close = 'CLOSE',
  Commit = 'COMMIT',
  Conditional = 'CONDITIONAL',
  CreateCheckConstraint = 'CREATE_CHECK_CONSTRAINT',
  CreateConstraint = 'CREATE_CONSTRAINT',
  CreateForeignKey = 'CREATE_FOREIGN_KEY',
  CreateFunction = 'CREATE_FUNCTION',
  CreateIndex = 'CREATE_INDEX',
  CreatePrimaryKey = 'CREATE_PRIMARY_KEY',
  CreateProcedure = 'CREATE_PROCEDURE',
  CreateSynonym = 'CREATE_SYNONYM',
  CreateTable = 'CREATE_TABLE',
  CreateTrigger = 'CREATE_TRIGGER',
  CreateView = 'CREATE_VIEW',
  DbAccessType = 'DB_ACCESS_TYPE',
  DeclareAlias = 'DECLARE_ALIAS',
  DeclareSchema = 'DECLARE_SCHEMA',
  DeclareTable = 'DECLARE_TABLE',
  DeclareTempTable = 'DECLARE_TEMP_TABLE',
  DeclareTransaction = 'DECLARE_TRANSACTION',
  Delete = 'DELETE',
  Display = 'DISPLAY',
  DropCheckConstraint = 'DROP_CHECK_CONSTRAINT',
  DropConstraint = 'DROP_CONSTRAINT',
  DropForeignKey = 'DROP_FOREIGN_KEY',
  DropFunction = 'DROP_FUNCTION',
  DropIndex = 'DROP_INDEX',
  DropPrimaryKey = 'DROP_PRIMARY_KEY',
  DropProcedure = 'DROP_PROCEDURE',
  DropTable = 'DROP_TABLE',
  DropView = 'DROP_VIEW',
  Entry = 'ENTRY',
  Exec = 'EXEC',
  Execute = 'EXECUTE',
  ExecuteImmediate = 'EXECUTE_IMMEDIATE',
  ExecAdabas = 'EXEC_ADABAS',
  ExecCics = 'EXEC_CICS',
  ExecProc = 'EXEC_PROC',
  ExecRexx = 'EXEC_REXX',
  ExecSql = 'EXEC_SQL',
  ExecUnknown = 'EXEC_UNKNOWN',
  Fetch = 'FETCH',
  Ftp = 'FTP',
  Geterror = 'GETERROR',
  Grant = 'GRANT',
  Insert = 'INSERT',
  LockTable = 'LOCK_TABLE',
  Merge = 'MERGE',
  Open = 'OPEN',
  Prepare = 'PREPARE',
  RdbDatabase = 'RDB_DATABASE',
  Rollback = 'ROLLBACK',
  Select = 'SELECT',
  Set = 'SET',
  Unknown = 'UNKNOWN',
  Update = 'UPDATE',
  Values = 'VALUES',
  Whenever = 'WHENEVER'
}

export enum Storage {
  Database = 'DATABASE',
  File = 'FILE',
  FileSection = 'FILE_SECTION',
  Network = 'NETWORK',
  Queue = 'QUEUE',
  System = 'SYSTEM',
  Undefined = 'UNDEFINED'
}

export type Taxonomy = {
  __typename?: 'Taxonomy';
  customProperties?: Maybe<Scalars['JSON']>;
  id?: Maybe<Scalars['EntityId']>;
  name?: Maybe<Scalars['String']>;
  project?: Maybe<Scalars['EntityId']>;
  projectNid?: Maybe<Scalars['Long']>;
  projectUid?: Maybe<Scalars['UUID']>;
  taxonomyReferenceCount?: Maybe<Scalars['Long']>;
  type?: Maybe<TaxonomyEnum>;
  uid?: Maybe<Scalars['UUID']>;
};

export type TaxonomyCategory = {
  __typename?: 'TaxonomyCategory';
  id?: Maybe<Scalars['Long']>;
  name?: Maybe<Scalars['String']>;
  project?: Maybe<Scalars['EntityId']>;
  projectNid?: Maybe<Scalars['Long']>;
  projectUid?: Maybe<Scalars['UUID']>;
};

export type TaxonomyEnum = {
  __typename?: 'TaxonomyEnum';
  category?: Maybe<TaxonomyCategory>;
  id?: Maybe<Scalars['UUID']>;
  name?: Maybe<Scalars['String']>;
  project?: Maybe<Scalars['EntityId']>;
  projectNid?: Maybe<Scalars['Long']>;
  projectUid?: Maybe<Scalars['UUID']>;
};

export enum Technology {
  Assembler = 'ASSEMBLER',
  Basic = 'BASIC',
  Binary = 'BINARY',
  C = 'C',
  Cics = 'CICS',
  Cobol = 'COBOL',
  Cpp = 'CPP',
  Csd = 'CSD',
  Easytrieve = 'EASYTRIEVE',
  Ecl = 'ECL',
  Ims = 'IMS',
  Java = 'JAVA',
  Jcl = 'JCL',
  Mark4 = 'MARK4',
  Natural = 'NATURAL',
  None = 'NONE',
  Oracle = 'ORACLE',
  Pl1 = 'PL1',
  Resource = 'RESOURCE',
  Scheduler = 'SCHEDULER',
  Service = 'SERVICE',
  Sql = 'SQL',
  Unknown = 'UNKNOWN',
  Vb = 'VB',
  Vms = 'VMS',
  Windows = 'WINDOWS',
  Xml = 'XML'
}

export enum Type {
  ActivexDocument = 'ACTIVEX_DOCUMENT',
  Adapter = 'ADAPTER',
  Adaptview = 'ADAPTVIEW',
  AltPcb = 'ALT_PCB',
  Annotation = 'ANNOTATION',
  Application = 'APPLICATION',
  BmsMap = 'BMS_MAP',
  BmsMapset = 'BMS_MAPSET',
  CdoFile = 'CDO_FILE',
  CdoRecord = 'CDO_RECORD',
  Cfg = 'CFG',
  Class = 'CLASS',
  CompilationUnit = 'COMPILATION_UNIT',
  Controlcard = 'CONTROLCARD',
  Copybook = 'COPYBOOK',
  Copycode = 'COPYCODE',
  Copylib = 'COPYLIB',
  Copyproc = 'COPYPROC',
  Cpm = 'CPM',
  Dbd = 'DBD',
  DbdComprtn = 'DBD_COMPRTN',
  DbdDataset = 'DBD_DATASET',
  DbdSegment = 'DBD_SEGMENT',
  Dcl = 'DCL',
  Ddm = 'DDM',
  DesignerFile = 'DESIGNER_FILE',
  Dialog = 'DIALOG',
  DialogPrivRes = 'DIALOG_PRIV_RES',
  Dll = 'DLL',
  EclJob = 'ECL_JOB',
  Enum = 'ENUM',
  ErrorMessage = 'ERROR_MESSAGE',
  Event = 'EVENT',
  Exec = 'EXEC',
  ExecPgm = 'EXEC_PGM',
  Export = 'EXPORT',
  Extract = 'EXTRACT',
  File = 'FILE',
  FmsForm = 'FMS_FORM',
  Form = 'FORM',
  Function = 'FUNCTION',
  Gda = 'GDA',
  GdgFile = 'GDG_FILE',
  Hdamparm = 'HDAMPARM',
  Header = 'HEADER',
  Help = 'HELP',
  IfdlForm = 'IFDL_FORM',
  Include = 'INCLUDE',
  Index = 'INDEX',
  Info = 'INFO',
  InlineProc = 'INLINE_PROC',
  Instream = 'INSTREAM',
  Interface = 'INTERFACE',
  Job = 'JOB',
  Jsf = 'JSF',
  Jsp = 'JSP',
  Lda = 'LDA',
  Lib = 'LIB',
  List = 'LIST',
  Listcat = 'LISTCAT',
  Macro = 'MACRO',
  MacroFile = 'MACRO_FILE',
  Mainprogram = 'MAINPROGRAM',
  Map = 'MAP',
  Method = 'METHOD',
  Mfs = 'MFS',
  Mid = 'MID',
  Mod = 'MOD',
  Module = 'MODULE',
  Object = 'OBJECT',
  Ocx = 'OCX',
  Package = 'PACKAGE',
  Pcb = 'PCB',
  Pda = 'PDA',
  Pgm = 'PGM',
  Proc = 'PROC',
  Process = 'PROCESS',
  Program = 'PROGRAM',
  Project = 'PROJECT',
  Psb = 'PSB',
  RapControlcard = 'RAP_CONTROLCARD',
  RdbDatabase = 'RDB_DATABASE',
  Record = 'RECORD',
  Schema = 'SCHEMA',
  Script = 'SCRIPT',
  ServiceRequestId = 'SERVICE_REQUEST_ID',
  Sqlmod = 'SQLMOD',
  SqlmodProcedure = 'SQLMOD_PROCEDURE',
  StoredProcedure = 'STORED_PROCEDURE',
  Subprogram = 'SUBPROGRAM',
  Subroutine = 'SUBROUTINE',
  Synonym = 'SYNONYM',
  Table = 'TABLE',
  Tdfxtrct = 'TDFXTRCT',
  Tdq = 'TDQ',
  Text = 'TEXT',
  TpfdfDataset = 'TPFDF_DATASET',
  Transaction = 'TRANSACTION',
  Trigger = 'TRIGGER',
  Tsq = 'TSQ',
  Type = 'TYPE',
  Unknown = 'UNKNOWN',
  UserControl = 'USER_CONTROL',
  Utility = 'UTILITY',
  VaxMacro = 'VAX_MACRO',
  VaxMacroEntry = 'VAX_MACRO_ENTRY',
  View = 'VIEW',
  VsamFile = 'VSAM_FILE',
  Workspace = 'WORKSPACE',
  Xhtml = 'XHTML'
}

export enum WorkingState {
  Approved = 'APPROVED',
  Candidate = 'CANDIDATE',
  ForReview = 'FOR_REVIEW',
  Invalid = 'INVALID',
  InAnalysis = 'IN_ANALYSIS',
  Rejected = 'REJECTED'
}

export type Innowake_Mining_Server_Graphql_Controller_ModulesGraphQlController__ModuleRelation = {
  __typename?: 'innowake_mining_server_graphql_controller_ModulesGraphQlController__ModuleRelation';
  direction?: Maybe<Innowake_Mining_Shared_Model_RelationshipDirection>;
  dstLocation?: Maybe<ModuleLocation>;
  dstModule?: Maybe<Scalars['UUID']>;
  id?: Maybe<Scalars['UUID']>;
  module?: Maybe<Module>;
  properties?: Maybe<Scalars['JSON']>;
  relationship?: Maybe<RelationshipType>;
  srcLocation?: Maybe<ModuleLocation>;
  srcModule?: Maybe<Scalars['UUID']>;
  type?: Maybe<RelationshipType>;
};

export type Innowake_Mining_Server_Graphql_Controller_TaxonomyCategoriesGraphQlController__AggregationResult = {
  __typename?: 'innowake_mining_server_graphql_controller_TaxonomyCategoriesGraphQlController__AggregationResult';
  categories?: Maybe<Array<Maybe<Innowake_Mining_Server_Graphql_Controller_TaxonomyCategoriesGraphQlController__CategoryAggregate>>>;
};

export type Innowake_Mining_Server_Graphql_Controller_TaxonomyCategoriesGraphQlController__CategoryAggregate = {
  __typename?: 'innowake_mining_server_graphql_controller_TaxonomyCategoriesGraphQlController__CategoryAggregate';
  distinctAssignments?: Maybe<Scalars['Long']>;
  id?: Maybe<Scalars['Long']>;
  name?: Maybe<Scalars['String']>;
  types?: Maybe<Array<Maybe<Innowake_Mining_Server_Graphql_Controller_TaxonomyCategoriesGraphQlController__TypeAggregate>>>;
};

export type Innowake_Mining_Server_Graphql_Controller_TaxonomyCategoriesGraphQlController__TermAggregate = {
  __typename?: 'innowake_mining_server_graphql_controller_TaxonomyCategoriesGraphQlController__TermAggregate';
  assignments?: Maybe<Scalars['Long']>;
  id?: Maybe<Scalars['Long']>;
  name?: Maybe<Scalars['String']>;
};

export type Innowake_Mining_Server_Graphql_Controller_TaxonomyCategoriesGraphQlController__TypeAggregate = {
  __typename?: 'innowake_mining_server_graphql_controller_TaxonomyCategoriesGraphQlController__TypeAggregate';
  distinctAssignments?: Maybe<Scalars['Long']>;
  id?: Maybe<Scalars['UUID']>;
  name?: Maybe<Scalars['String']>;
  terms?: Maybe<Array<Maybe<Innowake_Mining_Server_Graphql_Controller_TaxonomyCategoriesGraphQlController__TermAggregate>>>;
};

export type Innowake_Mining_Shared_Access_BinaryString = {
  __typename?: 'innowake_mining_shared_access_BinaryString';
  empty?: Maybe<Scalars['Boolean']>;
};

export type Innowake_Mining_Shared_Access_BinaryValue = {
  __typename?: 'innowake_mining_shared_access_BinaryValue';
  empty?: Maybe<Scalars['Boolean']>;
};

export type Innowake_Mining_Shared_Entities_ModuleBasePojo = {
  __typename?: 'innowake_mining_shared_entities_ModuleBasePojo';
  id?: Maybe<Scalars['Long']>;
  identification?: Maybe<Identification>;
  info?: Maybe<Array<Maybe<Map_String_Java_Lang_Object>>>;
  linkHash?: Maybe<Scalars['String']>;
  moduleType?: Maybe<Innowake_Mining_Shared_Model_ModuleType>;
  name?: Maybe<Scalars['String']>;
  path?: Maybe<Scalars['String']>;
  representation?: Maybe<Innowake_Mining_Shared_Entities_ModulePojo__Representation>;
  technology?: Maybe<Technology>;
  type?: Maybe<Type>;
  uid?: Maybe<Scalars['UUID']>;
};

export enum Innowake_Mining_Shared_Entities_ModulePojo__Representation {
  Physical = 'PHYSICAL',
  Virtual = 'VIRTUAL'
}

export type Innowake_Mining_Shared_Entities_Functionalblocks_FunctionalBlockLinkCondition = {
  __typename?: 'innowake_mining_shared_entities_functionalblocks_FunctionalBlockLinkCondition';
  label?: Maybe<Scalars['String']>;
  uid?: Maybe<Scalars['UUID']>;
};

export enum Innowake_Mining_Shared_Entities_Functionalblocks_FunctionalBlockStatus {
  Active = 'ACTIVE',
  Inactive = 'INACTIVE'
}

export type Innowake_Mining_Shared_Entities_Functionalblocks_ModulePart = {
  __typename?: 'innowake_mining_shared_entities_functionalblocks_ModulePart';
  location?: Maybe<ModuleLocation>;
  moduleLinkHash?: Maybe<Scalars['String']>;
};

export enum Innowake_Mining_Shared_Entities_Scheduler_SchedulerType {
  Ca7 = 'CA7',
  ControlM = 'CONTROL_M'
}

export enum Innowake_Mining_Shared_Model_Binding {
  Early = 'EARLY',
  Late = 'LATE',
  Unknown = 'UNKNOWN'
}

export enum Innowake_Mining_Shared_Model_ComplexityLevel {
  High = 'HIGH',
  Low = 'LOW',
  Medium = 'MEDIUM',
  Unknown = 'UNKNOWN',
  VeryHigh = 'VERY_HIGH'
}

export enum Innowake_Mining_Shared_Model_CustomPropertyDataType {
  Boolean = 'BOOLEAN',
  Byte = 'BYTE',
  Date = 'DATE',
  Datetime = 'DATETIME',
  Decimal = 'DECIMAL',
  Double = 'DOUBLE',
  Embeddedlist = 'EMBEDDEDLIST',
  Embeddedmap = 'EMBEDDEDMAP',
  Float = 'FLOAT',
  Integer = 'INTEGER',
  Linklist = 'LINKLIST',
  Long = 'LONG',
  Reference = 'REFERENCE',
  Short = 'SHORT',
  String = 'STRING'
}

export enum Innowake_Mining_Shared_Model_CustomPropertyFieldType {
  Default = 'DEFAULT',
  Number = 'NUMBER',
  Select = 'SELECT',
  Tag = 'TAG'
}

export enum Innowake_Mining_Shared_Model_DefinedLocation {
  Begin = 'BEGIN',
  Copybook = 'COPYBOOK',
  Package = 'PACKAGE',
  Procedure = 'PROCEDURE',
  Program = 'PROGRAM',
  Subprogram = 'SUBPROGRAM',
  Subroutine = 'SUBROUTINE'
}

export enum Innowake_Mining_Shared_Model_ModuleType {
  AssemblerMacro = 'ASSEMBLER_MACRO',
  AssemblerProgram = 'ASSEMBLER_PROGRAM',
  BasicFunction = 'BASIC_FUNCTION',
  BasicObject = 'BASIC_OBJECT',
  BasicProgram = 'BASIC_PROGRAM',
  BasicSubroutine = 'BASIC_SUBROUTINE',
  Binary = 'BINARY',
  CdoFile = 'CDO_FILE',
  CdoRecord = 'CDO_RECORD',
  CicsBmsMap = 'CICS_BMS_MAP',
  CicsBmsMapset = 'CICS_BMS_MAPSET',
  CicsTdq = 'CICS_TDQ',
  CicsTsq = 'CICS_TSQ',
  CobolCopybook = 'COBOL_COPYBOOK',
  CobolCopylib = 'COBOL_COPYLIB',
  CobolCopyproc = 'COBOL_COPYPROC',
  CobolProgram = 'COBOL_PROGRAM',
  CppHeader = 'CPP_HEADER',
  CppProgram = 'CPP_PROGRAM',
  CsdExtract = 'CSD_EXTRACT',
  CsdFile = 'CSD_FILE',
  CsdList = 'CSD_LIST',
  CsdProgram = 'CSD_PROGRAM',
  CsdTransaction = 'CSD_TRANSACTION',
  CFunction = 'C_FUNCTION',
  CHeader = 'C_HEADER',
  CProgram = 'C_PROGRAM',
  Dcl = 'DCL',
  EasytrieveInstream = 'EASYTRIEVE_INSTREAM',
  EasytrieveMacroFile = 'EASYTRIEVE_MACRO_FILE',
  EasytrieveProgram = 'EASYTRIEVE_PROGRAM',
  Ecl = 'ECL',
  EclJob = 'ECL_JOB',
  FmsForm = 'FMS_FORM',
  IfdlForm = 'IFDL_FORM',
  ImsAltPcb = 'IMS_ALT_PCB',
  ImsDbd = 'IMS_DBD',
  ImsDbdComprtn = 'IMS_DBD_COMPRTN',
  ImsDbdDataset = 'IMS_DBD_DATASET',
  ImsDbdSegment = 'IMS_DBD_SEGMENT',
  ImsHdamparm = 'IMS_HDAMPARM',
  ImsHelptxt = 'IMS_HELPTXT',
  ImsMfs = 'IMS_MFS',
  ImsMfsMid = 'IMS_MFS_MID',
  ImsMfsMod = 'IMS_MFS_MOD',
  ImsPcb = 'IMS_PCB',
  ImsPsb = 'IMS_PSB',
  ImsSysgenApplication = 'IMS_SYSGEN_APPLICATION',
  ImsSysgenExport = 'IMS_SYSGEN_EXPORT',
  ImsSysgenTransaction = 'IMS_SYSGEN_TRANSACTION',
  ImsTdfxtrct = 'IMS_TDFXTRCT',
  JavaAnnotation = 'JAVA_ANNOTATION',
  JavaCompilationUnit = 'JAVA_COMPILATION_UNIT',
  JavaEnum = 'JAVA_ENUM',
  JavaInterface = 'JAVA_INTERFACE',
  JavaJsf = 'JAVA_JSF',
  JavaJsp = 'JAVA_JSP',
  JavaMethod = 'JAVA_METHOD',
  JavaPackage = 'JAVA_PACKAGE',
  JavaType = 'JAVA_TYPE',
  JclCfg = 'JCL_CFG',
  JclControlcard = 'JCL_CONTROLCARD',
  JclExec = 'JCL_EXEC',
  JclExecPgm = 'JCL_EXEC_PGM',
  JclInclude = 'JCL_INCLUDE',
  JclInfo = 'JCL_INFO',
  JclInlineProc = 'JCL_INLINE_PROC',
  JclJob = 'JCL_JOB',
  JclPgm = 'JCL_PGM',
  JclProc = 'JCL_PROC',
  Mark4Program = 'MARK4_PROGRAM',
  NaturalAdapter = 'NATURAL_ADAPTER',
  NaturalAdaptview = 'NATURAL_ADAPTVIEW',
  NaturalClass = 'NATURAL_CLASS',
  NaturalCopycode = 'NATURAL_COPYCODE',
  NaturalCopycodeReporting = 'NATURAL_COPYCODE_REPORTING',
  NaturalCpm = 'NATURAL_CPM',
  NaturalDdm = 'NATURAL_DDM',
  NaturalDialog = 'NATURAL_DIALOG',
  NaturalDialogPrivRes = 'NATURAL_DIALOG_PRIV_RES',
  NaturalErrorMessage = 'NATURAL_ERROR_MESSAGE',
  NaturalFunction = 'NATURAL_FUNCTION',
  NaturalFunctionReporting = 'NATURAL_FUNCTION_REPORTING',
  NaturalGda = 'NATURAL_GDA',
  NaturalHelp = 'NATURAL_HELP',
  NaturalHelpReporting = 'NATURAL_HELP_REPORTING',
  NaturalIwGda = 'NATURAL_IW_GDA',
  NaturalIwLda = 'NATURAL_IW_LDA',
  NaturalIwPda = 'NATURAL_IW_PDA',
  NaturalLda = 'NATURAL_LDA',
  NaturalMap = 'NATURAL_MAP',
  NaturalMapReporting = 'NATURAL_MAP_REPORTING',
  NaturalPda = 'NATURAL_PDA',
  NaturalProgram = 'NATURAL_PROGRAM',
  NaturalProgramReporting = 'NATURAL_PROGRAM_REPORTING',
  NaturalSubprogram = 'NATURAL_SUBPROGRAM',
  NaturalSubprogramReporting = 'NATURAL_SUBPROGRAM_REPORTING',
  NaturalSubroutine = 'NATURAL_SUBROUTINE',
  NaturalSubroutineReporting = 'NATURAL_SUBROUTINE_REPORTING',
  NaturalText = 'NATURAL_TEXT',
  Pl1Copybook = 'PL1_COPYBOOK',
  Pl1Function = 'PL1_FUNCTION',
  Pl1Mainprogram = 'PL1_MAINPROGRAM',
  Pl1Program = 'PL1_PROGRAM',
  Pl1Subroutine = 'PL1_SUBROUTINE',
  RapControlcard = 'RAP_CONTROLCARD',
  RdbDatabase = 'RDB_DATABASE',
  ResourceFile = 'RESOURCE_FILE',
  ResourceGdgFile = 'RESOURCE_GDG_FILE',
  ResourceLib = 'RESOURCE_LIB',
  ResourceListcat = 'RESOURCE_LISTCAT',
  ResourceRecord = 'RESOURCE_RECORD',
  ResourceTpfdfDataset = 'RESOURCE_TPFDF_DATASET',
  ResourceVsamFile = 'RESOURCE_VSAM_FILE',
  Service = 'SERVICE',
  Sqlmod = 'SQLMOD',
  SqlmodProc = 'SQLMOD_PROC',
  SqlIndex = 'SQL_INDEX',
  SqlSchema = 'SQL_SCHEMA',
  SqlScript = 'SQL_SCRIPT',
  SqlStoredProcedure = 'SQL_STORED_PROCEDURE',
  SqlSynonym = 'SQL_SYNONYM',
  SqlTable = 'SQL_TABLE',
  SqlTemporaryTable = 'SQL_TEMPORARY_TABLE',
  SqlTrigger = 'SQL_TRIGGER',
  SqlView = 'SQL_VIEW',
  Unknown = 'UNKNOWN',
  UnknownProgram = 'UNKNOWN_PROGRAM',
  UnknownUtility = 'UNKNOWN_UTILITY',
  VaxMacro = 'VAX_MACRO',
  VaxMacroEntry = 'VAX_MACRO_ENTRY',
  VbActivexDocument = 'VB_ACTIVEX_DOCUMENT',
  VbClass = 'VB_CLASS',
  VbControl = 'VB_CONTROL',
  VbDesignerFile = 'VB_DESIGNER_FILE',
  VbForm = 'VB_FORM',
  VbModule = 'VB_MODULE',
  VbProject = 'VB_PROJECT',
  VbWorkspace = 'VB_WORKSPACE',
  WindowsDll = 'WINDOWS_DLL',
  WindowsOcx = 'WINDOWS_OCX',
  Xml = 'XML',
  XmlXhtml = 'XML_XHTML'
}

export enum Innowake_Mining_Shared_Model_RelationshipDirection {
  Both = 'BOTH',
  In = 'IN',
  Out = 'OUT'
}

export type FaChildrenDeepQueryVariables = Exact<{
  projectId?: InputMaybe<Scalars['Long']>;
  uid?: InputMaybe<Scalars['UUID']>;
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
  filterObject?: InputMaybe<FilterObject_FunctionalBlocks>;
}>;


export type FaChildrenDeepQuery = { __typename?: 'Query', functionalBlock?: { __typename?: 'FunctionalBlock', childrenDeep?: { __typename?: 'PAGED_FunctionalBlock', totalElements?: number | null, content?: Array<{ __typename?: 'FunctionalBlock', name?: string | null, uid?: any | null, description?: string | null, type?: Array<FunctionalBlockType | null> | null, parents?: { __typename?: 'PAGED_FunctionalBlock', content?: Array<{ __typename?: 'FunctionalBlock', name?: string | null, uid?: any | null } | null> | null } | null, generatedFrom?: { __typename?: 'GeneratedFrom', annotation?: { __typename?: 'Annotation', name?: string | null, id?: any | null, sourceAttachment?: string | null, categoryName?: string | null, type?: AnnotationType | null, state?: WorkingState | null, createdByUserName?: string | null, dataDictionaryEntries?: Array<any | null> | null, location?: { __typename?: 'ModuleLocation', offset?: number | null, length?: number | null } | null, module?: { __typename?: 'Module', id?: any | null, name?: string | null } | null } | null } | null } | null> | null } | null } | null };

export type FaTreeQueryVariables = Exact<{
  projectId?: InputMaybe<Scalars['Long']>;
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
  filterObject?: InputMaybe<FilterObject_FunctionalBlocks>;
  sortObject?: InputMaybe<SortObject_FunctionalBlocks>;
  childrenFilterObject?: InputMaybe<FilterObject_FunctionalBlocks>;
  useChildrenDeep: Scalars['Boolean'];
}>;


export type FaTreeQuery = { __typename?: 'Query', functionalBlocks?: { __typename?: 'PAGED_FunctionalBlock', totalElements?: number | null, content?: Array<{ __typename?: 'FunctionalBlock', uid?: any | null, name?: string | null, type?: Array<FunctionalBlockType | null> | null, flags?: any | null, childrenDeep?: { __typename?: 'PAGED_FunctionalBlock', content?: Array<{ __typename?: 'FunctionalBlock', uid?: any | null, name?: string | null, type?: Array<FunctionalBlockType | null> | null, flags?: any | null, parents?: { __typename?: 'PAGED_FunctionalBlock', content?: Array<{ __typename?: 'FunctionalBlock', uid?: any | null } | null> | null } | null, children?: { __typename?: 'PAGED_FunctionalBlock', totalElements?: number | null } | null } | null> | null } | null, children?: { __typename?: 'PAGED_FunctionalBlock', content?: Array<{ __typename?: 'FunctionalBlock', uid?: any | null, name?: string | null, type?: Array<FunctionalBlockType | null> | null, flags?: any | null, children?: { __typename?: 'PAGED_FunctionalBlock', totalElements?: number | null } | null } | null> | null } | null } | null> | null } | null };

export type FbTotalElementsCountQueryVariables = Exact<{
  projectId?: InputMaybe<Scalars['Long']>;
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
  filter?: InputMaybe<FilterObject_FunctionalBlocks>;
}>;


export type FbTotalElementsCountQuery = { __typename?: 'Query', functionalBlocks?: { __typename?: 'PAGED_FunctionalBlock', totalElements?: number | null } | null };

export type ImsDetailsQueryVariables = Exact<{
  projectId?: InputMaybe<Scalars['Long']>;
  propertiesFilter?: InputMaybe<Scalars['JSON']>;
  taxonomyFilter?: InputMaybe<FilterObject_Modules_Content_Taxonomies_Id>;
}>;


export type ImsDetailsQuery = { __typename?: 'Query', modules?: { __typename?: 'PAGED_Module', content?: Array<{ __typename?: 'Module', id?: any | null, name?: string | null, dependencies?: Array<{ __typename?: 'innowake_mining_server_graphql_controller_ModulesGraphQlController__ModuleRelation', properties?: any | null, module?: { __typename?: 'Module', id?: any | null, name?: string | null } | null } | null> | null } | null> | null } | null };

export type ModulesQueryVariables = Exact<{
  projectId?: InputMaybe<Scalars['Long']>;
  page?: InputMaybe<Scalars['Int']>;
  size?: InputMaybe<Scalars['Int']>;
  filterObject?: InputMaybe<FilterObject_Modules>;
}>;


export type ModulesQuery = { __typename?: 'Query', modules?: { __typename?: 'PAGED_Module', totalElements?: number | null, size?: number | null, content?: Array<{ __typename?: 'Module', uid?: any | null, id?: any | null, customProperties?: any | null, project?: any | null, name?: string | null, path?: string | null, technology?: Technology | null, type?: Type | null, storage?: Storage | null, origin?: Origin | null, creator?: Creator | null, identification?: Identification | null, inCodebase?: boolean | null, description?: string | null, source?: any | null, contentHash?: string | null, linkHash?: string | null, representation?: Innowake_Mining_Shared_Entities_ModulePojo__Representation | null, requiresReview?: boolean | null, modifiedDate?: any | null, metricsDate?: any | null, complexityLevel?: Innowake_Mining_Shared_Model_ComplexityLevel | null, content?: string | null, errors?: number | null, statements?: number | null, sqlStatements?: number | null, sourceCodeAvailable?: boolean | null, parent?: any | null, parentPath?: string | null, projectId?: any | null, info?: { __typename?: 'MAP_STRING_java_lang_Object', key?: string | null } | null, location?: { __typename?: 'ModuleLocation', offset?: number | null, length?: number | null } | null, sourceMetrics?: { __typename?: 'SourceMetrics', codeLines?: number | null, physicalLines?: number | null, commentLines?: number | null, complexityMcCabe?: number | null, deadCodeLines?: number | null } | null } | null> | null } | null };

export const FaChildrenDeepDocument = gql`
    query FAChildrenDeep($projectId: Long, $uid: UUID, $page: Int, $size: Int, $filterObject: FilterObject_functionalBlocks) {
  functionalBlock(projectId: $projectId, uid: $uid) {
    childrenDeep(filterObject: $filterObject, page: $page, size: $size) {
      totalElements
      content {
        parents(filterObject: {content_type: {eq: FUNCTIONAL_GROUP}}) {
          content {
            name
            uid
          }
        }
        name
        uid
        description
        type
        generatedFrom {
          annotation {
            location {
              offset
              length
            }
            module {
              id
              name
            }
            name
            id
            sourceAttachment
            categoryName
            type
            state
            createdByUserName
            dataDictionaryEntries
          }
        }
      }
    }
  }
}
    `;

  @Injectable({
    providedIn: 'root'
  })
  export class FaChildrenDeepGQL extends Apollo.Query<FaChildrenDeepQuery, FaChildrenDeepQueryVariables> {
    document = FaChildrenDeepDocument;
    
    constructor(apollo: Apollo.Apollo) {
      super(apollo);
    }
  }
export const FaTreeDocument = gql`
    query FATree($projectId: Long, $page: Int, $size: Int, $filterObject: FilterObject_functionalBlocks, $sortObject: SortObject_functionalBlocks, $childrenFilterObject: FilterObject_functionalBlocks, $useChildrenDeep: Boolean!) {
  functionalBlocks(
    projectId: $projectId
    page: $page
    size: $size
    filterObject: $filterObject
    sortObject: [$sortObject]
  ) {
    totalElements
    content {
      uid
      name
      type
      flags
      childrenDeep(filterObject: $childrenFilterObject) @include(if: $useChildrenDeep) {
        content {
          uid
          name
          type
          flags
          parents {
            content {
              uid
            }
          }
          children(filterObject: $childrenFilterObject) {
            totalElements
          }
        }
      }
      children(filterObject: $childrenFilterObject) @skip(if: $useChildrenDeep) {
        content {
          uid
          name
          type
          flags
          children(filterObject: $childrenFilterObject) {
            totalElements
          }
        }
      }
    }
  }
}
    `;

  @Injectable({
    providedIn: 'root'
  })
  export class FaTreeGQL extends Apollo.Query<FaTreeQuery, FaTreeQueryVariables> {
    document = FaTreeDocument;
    
    constructor(apollo: Apollo.Apollo) {
      super(apollo);
    }
  }
export const FbTotalElementsCountDocument = gql`
    query FBTotalElementsCount($projectId: Long, $page: Int, $size: Int, $filter: FilterObject_functionalBlocks) {
  functionalBlocks(
    projectId: $projectId
    page: $page
    size: $size
    filterObject: $filter
  ) {
    totalElements
  }
}
    `;

  @Injectable({
    providedIn: 'root'
  })
  export class FbTotalElementsCountGQL extends Apollo.Query<FbTotalElementsCountQuery, FbTotalElementsCountQueryVariables> {
    document = FbTotalElementsCountDocument;
    
    constructor(apollo: Apollo.Apollo) {
      super(apollo);
    }
  }
export const ImsDetailsDocument = gql`
    query IMSDetails($projectId: Long, $propertiesFilter: JSON, $taxonomyFilter: FilterObject_modules_content_taxonomies_id) {
  modules(
    projectId: $projectId
    filterObject: {content_taxonomies_id: $taxonomyFilter, content_technology: {notEq: IMS}, content_storage: {eq: FILE}, content_outReferencesTechnology: {eq: IMS}, content_dependencyCount: {gte: 1}}
  ) {
    content {
      id
      name
      dependencies(
        direction: OUT
        filterObject: {content_technology: {eq: IMS}, content_type: {eq: DBD}}
        properties: $propertiesFilter
      ) {
        properties
        module {
          id
          name
        }
      }
    }
  }
}
    `;

  @Injectable({
    providedIn: 'root'
  })
  export class ImsDetailsGQL extends Apollo.Query<ImsDetailsQuery, ImsDetailsQueryVariables> {
    document = ImsDetailsDocument;
    
    constructor(apollo: Apollo.Apollo) {
      super(apollo);
    }
  }
export const ModulesDocument = gql`
    query Modules($projectId: Long, $page: Int, $size: Int, $filterObject: FilterObject_modules) {
  modules(
    projectId: $projectId
    page: $page
    size: $size
    sortObject: {content_name: ASC}
    filterObject: $filterObject
  ) {
    content {
      uid
      id
      customProperties
      project
      name
      path
      technology
      type
      storage
      origin
      creator
      identification
      info {
        key
      }
      inCodebase
      description
      source
      contentHash
      linkHash
      location {
        offset
        length
      }
      representation
      requiresReview
      modifiedDate
      metricsDate
      sourceMetrics {
        codeLines
        physicalLines
        commentLines
        complexityMcCabe
        deadCodeLines
      }
      complexityLevel
      content
      errors
      statements
      sqlStatements
      sourceCodeAvailable
      parent
      parentPath
      projectId
    }
    totalElements
    size
  }
}
    `;

  @Injectable({
    providedIn: 'root'
  })
  export class ModulesGQL extends Apollo.Query<ModulesQuery, ModulesQueryVariables> {
    document = ModulesDocument;
    
    constructor(apollo: Apollo.Apollo) {
      super(apollo);
    }
  }