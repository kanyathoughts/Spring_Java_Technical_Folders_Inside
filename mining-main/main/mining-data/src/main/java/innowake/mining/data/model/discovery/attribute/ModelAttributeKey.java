package innowake.mining.data.model.discovery.attribute;

/**
 * Contains the keys that are used in the ModelAttributeMap.
 */
public enum ModelAttributeKey {
	
	/**
	 * Attribute key for database table access type.
	 * Valid values are {@link ModelAttributeValue.TableAccess}
	 */
	FILE_ACCESS_TYPE,
	FILE_ACCESS_OPERATION,
	CALL_TYPE,
	SEND_RECEIVE_ACCESS_TYPE,
	INBOUND,
	OUTBOUND,
	OUTBOUND_TARGETS,
	PROPERTIES,
	
	IMS_PSBGEN_CMPAT,
	IMS_PSBGEN_LANG,
	
	IMS_PCB_TYPE,
	IMS_PCB_PROCSEQ,
	IMS_PCB_PROCOPT,
	IMS_PCB_SENSEG,
	IMS_DBD_NAME,
	IMS_SSA,
	
	IMS_DBD_SEGMENT_COMPRTN,
	IMS_DBD_SEGMENT_PARENT,
	IMS_DBD_DATASET,
	IMS_DBD_SEGMENT,
	IMS_SEGMENTS,
	IMS_ACCESS_TYPE,
	IMS_OS_ACCESS_TYPE,
	IMS_REFERENCE_TYPE,
	
	QUEUE_ACCESS,

	/**
	 * The name of the Cobol file descriptor associated with a file dependency.
	 */
	COBOL_FD_NAME,
	
	/**
	 * Symbolic name used for a file inside of a program or script.
	 */
	FILE_ALIAS, 
	
	DB_ACCESS_TYPE,
	DB_ACCESS_OPERATION,
	STATEMENT,

	/** Attribute key for type reference types in object oriented languages, e.g. extends and implements */
	TYPE_REFERENCE_TYPE,
	/** Attribute key for method or function parameters */
	PARAMETER_TYPES,
	
	ORGANIZATION,
	ACCESS_MODE
}
