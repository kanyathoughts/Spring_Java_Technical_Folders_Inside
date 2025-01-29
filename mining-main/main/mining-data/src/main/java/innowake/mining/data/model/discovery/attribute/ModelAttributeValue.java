package innowake.mining.data.model.discovery.attribute;

/**
 * Contains any constant values for the ModelAttributeMap. Not all values placed in the map have to be constant but if they
 * are they can be placed here for easy retrieval in other parts of discovery.
 *
 */
public class ModelAttributeValue {
	
	public interface ModelAttributeValueEnum {
		
	}
	
	/**
	 * Simplified read/write values for translating {@link TableAccess}
	 */
	public enum TableAccessOperation implements ModelAttributeValueEnum {
		READ, WRITE, UNDEFINED
	}
	
	public enum FileAccess implements ModelAttributeValueEnum {
		READ, WRITE
	}
	
	public enum TableAccess implements ModelAttributeValueEnum {
		DECLARE_TABLE(TableAccessOperation.READ),
		DELETE_TABLE(TableAccessOperation.WRITE),
		INSERT_TABLE(TableAccessOperation.WRITE),
		LOCK_TABLE(TableAccessOperation.WRITE),
		UPDATE_TABLE(TableAccessOperation.WRITE),
		DECLARE_CURSOR_TABLE(TableAccessOperation.READ),
		SELECT_TABLE(TableAccessOperation.READ),
		UNDEFINED(TableAccessOperation.UNDEFINED);
		
		private TableAccessOperation operation;
		
		private TableAccess(final TableAccessOperation operation) {
			this.operation = operation;
		}
		
		public TableAccessOperation getOperation() {
			return operation;
		}
	}
	
	/**
	 * Define the binding types for Send/Receive Map
	 */
	public enum SendReceiveAccess implements ModelAttributeValueEnum {
		SEND,
		RECEIVE,
		UNDEFINED
	}
	
	/**
	 * Define the binding types for COBOL
	 */
	public enum CallType implements ModelAttributeValueEnum {
		CALL,
		LINK,
		EXECCICSXCTL,
		MAP,
		EXECCICSFILE,
		EXECCICSRETURN,
		EXECICSCREATE,
		ENTER,
		IMS_DB
	}
	
	public enum QueueAccess implements ModelAttributeValueEnum {
		READQ, 
		WRITEQ, 
		DELETEQ,
		UNDEFINED
	}

	/**
	 * SQL statement type: SELECT, INSERT, UPDATE, MERGE, DELETE,
	 * but also DDL statement and others such as FETCH, CALL etc.
	 */
	public enum SqlStatementType implements ModelAttributeValueEnum {
		SELECT,
		INSERT,
		UPDATE,
		MERGE,
		DELETE,
		CREATE_TABLE,
		CREATE_VIEW,
		CREATE_SYNONYM,
		CREATE_PRIMARY_KEY,
		CREATE_FOREIGN_KEY,
		CREATE_INDEX,
		CREATE_CONSTRAINT,
		CREATE_CHECK_CONSTRAINT,
		CREATE_FUNCTION,
		CREATE_PROCEDURE,
		ALTER_TABLE,
		ALTER_VIEW,
		ALTER_PRIMARY_KEY,
		ALTER_FOREIGN_KEY,
		ALTER_INDEX,
		ALTER_CONSTRAINT,
		ALTER_CHECK_CONSTRAINT,
		ALTER_FUNCTION,
		ALTER_PROCEDURE,
		DROP_TABLE,
		DROP_VIEW,
		DROP_PRIMARY_KEY,
		DROP_FOREIGN_KEY,
		DROP_INDEX,
		DROP_CONSTRAINT,
		DROP_CHECK_CONSTRAINT,
		DROP_FUNCTION,
		DROP_PROCEDURE,
		GRANT,
		DECLARE_TABLE,
		DECLARE_TEMP_TABLE,
		LOCK_TABLE,
		CALL,
		FETCH,
		EXECUTE,
		ALLOCATE_CURSOR,
		ASSOCIATE_LOCATOR,
		WHENEVER,
		OPEN,
		CLOSE,
		COMMIT,
		ROLLBACK,
		SET,
		PREPARE,
		DECLARE_SCHEMA,
		UNKNOWN,
		VALUES,
		DECLARE_ALIAS,
		DECLARE_TRANSACTION,
		EXECUTE_IMMEDIATE,
		GETERROR
	}

	/**
	 * Define the types of references in object oriented languages, like Java.
	 */
	public enum TypeReferenceType implements ModelAttributeValueEnum {
		/** A type that extends another one, e.g. Java class -> class, interface -> interface */
		EXTEND,
		/** A type that implements an interface, e.g. Java enum or class -> interface */
		IMPLEMENT
	}

	/**
	 * Defines the types of references between IMS segments.
	 */
	public enum ImsReferenceType implements ModelAttributeValueEnum {
		LCHILD,
		VIRTUAL
	}
}
