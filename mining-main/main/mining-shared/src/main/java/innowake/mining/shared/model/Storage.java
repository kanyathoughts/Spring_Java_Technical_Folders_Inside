/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * The storage associated with a module.
 */
@Entity(name = "StorageEnum")
@MiningDataType(name = "Storage")
public enum Storage {

	FILE,
	FILE_SECTION,
	DATABASE,
	NETWORK,
	UNDEFINED,
	SYSTEM,
	QUEUE;

	private static final String TYPE_NOT_MAPPED_ERROR_MESSAGE = "%s is not supported";

	/**
	 * Returns the storage given a name.
	 * <p>
	 * The comparison is done case-insensitive.
	 *
	 * @param name the name the storage is associated with.
	 * @return the storage
	 */
	public static Storage fromName(final String name) {
		for (final Storage value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return value;
			}
		}
		throw new IllegalArgumentException("No enum constant " + name + " available");
	}

	/**
	 * Returns the storage given the technology and type.
	 *
	 * @param technology the technology associated with the storage
	 * @param type the type associated with the storage
	 * @return the storage mapped by the given technology and type
	 */
	public static Storage from(final Technology technology, final Type type) {
		switch (type) {
			case UNKNOWN:
			case SERVICE_REQUEST_ID:
				return UNDEFINED;
			default:
				break;
		}

		switch (technology) {
			case UNKNOWN:
			case SERVICE:
				return UNDEFINED;

			case JCL:
				return fromJCL(type);

			case COBOL:
				return fromCobol(type);

			case ASSEMBLER:
				return fromAssembler(type);

			case RESOURCE:
				return fromResource(type);

			case CSD:
				return fromCSD(type);

			case NONE:
				return fromNone(type);

			case NATURAL:
				return fromNatural(type);

			case PL1:
				return fromPl1(type);

			case XML:
				return fromXml(type);

			case BINARY:
				return fromBinary(type);

			case EASYTRIEVE:
				return fromEasytrieve(type);

			case IMS:
				return fromIMS(type);

			case C:
				return fromC(type);
				
			case CPP:
				return fromCpp(type);

			case ORACLE:
				return fromOracle(type);

			case VMS:
				return fromVMS(type);

			case BASIC:
				return fromBasic(type);

			case SQL:
				return fromSql(type);

			case JAVA:
				return fromJava(type);

			case ECL:
				return fromEcl(type);

			case CICS:
				return fromCics(type);

			case VB:
				return fromVB(type);
				
			case WINDOWS:
				return fromWindows(type);
			case CSHARP:
				return fromCSharp(type);

			default:
				final String message = String.format("Combination Technology %s and Type %s not supported", technology, type);
				throw new IllegalArgumentException(message);
		}
	}

	private static Storage fromSql(final Type type) {
		switch (type) {
			case SCRIPT:
				return FILE;
			case STORED_PROCEDURE:
				return FILE_SECTION;
			case TABLE:
			case VIEW:
			case SYNONYM:
			case INDEX:
			case TRIGGER:
			case SCHEMA:
				return DATABASE;
				
			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromJava(final Type type) {
		switch (type) {
			case COMPILATION_UNIT:
			case JSF:
			case JSP:
				return FILE;
			case PACKAGE:
			case ENUM:
			case TYPE:
			case ANNOTATION:
			case INTERFACE:
			case METHOD:
				return FILE_SECTION;
			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromBasic(final Type type) {
		switch (type) {
			case OBJECT:
				return FILE;
			case PROGRAM:
			case SUBROUTINE:
			case FUNCTION:
				return FILE_SECTION;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromVMS(final Type type) {
		switch (type) {
			case DCL:
			case FMS_FORM:
			case IFDL_FORM:
			case VAX_MACRO:
				return FILE;
			case VAX_MACRO_ENTRY:
				return FILE_SECTION;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromOracle(final Type type) {
		switch (type) {
			case CDO_FILE:
			case SQLMOD:
			case RDB_DATABASE:
				return FILE;

			case CDO_RECORD:
			case SQLMOD_PROCEDURE:
				return FILE_SECTION;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromIMS(final Type type) {
		switch (type) {
			case DBD:
			case PSB:
			case HELP:
			case HDAMPARM:
			case TDFXTRCT:
			case EXPORT:
			case MFS:
				return FILE;

			case DBD_COMPRTN:
			case DBD_DATASET:
			case DBD_SEGMENT:
			case PCB:
			case ALT_PCB:
			case TRANSACTION:
			case MID:
			case MOD:
			case APPLICATION:
				return FILE_SECTION;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromC(final Type type) {
		switch (type) {
			case PROGRAM:
			case HEADER:
				return FILE;

			case FUNCTION:
				return FILE_SECTION;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}
	
	private static Storage fromCpp(final Type type) {
		switch (type) {
			case PROGRAM:
			case HEADER:
				return FILE;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromEasytrieve(final Type type) {
		switch (type) {
			case UNKNOWN:
				return UNDEFINED;
			case MACRO_FILE:
				return FILE;
			case PROGRAM:
				return FILE;

			case INSTREAM:
				return FILE_SECTION;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromBinary(final Type type) {
		switch (type) {
			case UNKNOWN:
				return FILE;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromXml(final Type type) {
		switch (type) {
			case UNKNOWN:
			case XHTML:
				return FILE;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromPl1(final Type type) {
		switch (type) {
			case UNKNOWN:
				return UNDEFINED;

			case PROGRAM:
			case MAINPROGRAM:
			case COPYBOOK:
				return FILE;

			case SUBROUTINE:
			case FUNCTION:
				return FILE_SECTION;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromNatural(final Type type) {
		switch (type) {
			case UNKNOWN:
			case CLASS:
			case CPM:
			case ADAPTVIEW:
			case ADAPTER:
			case HELP:
			case DIALOG:
			case DIALOG_PRIV_RES:
			case TEXT:
			case ERROR_MESSAGE:
			case PROGRAM:
			case SUBROUTINE:
			case SUBPROGRAM:
			case FUNCTION:
			case COPYCODE:
			case MAP:
			case GDA:
			case PDA:
			case LDA:
			case DDM:
				return FILE;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromNone(final Type type) {
		switch (type) {
			case UNKNOWN:
				return UNDEFINED;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromAssembler(final Type type) {
		switch (type) {
			case MACRO:
			case PROGRAM:
				return FILE;
			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromJCL(final Type type) {
		switch (type) {
			case UNKNOWN:
				return UNDEFINED;

			case EXEC_PGM:
			case EXEC:
			case INLINE_PROC:
			case PGM:
			case CONTROLCARD:
			case RAP_CONTROLCARD:
				return FILE_SECTION;

			case JOB:
			case PROC:
			case CFG:
			case INCLUDE:
			case INFO:
				return FILE;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromCobol(final Type type) {
		switch (type) {
			case UNKNOWN:
				return UNDEFINED;

			case COPYBOOK:
			case PROGRAM:
			case MFS:
			case COPYLIB:
				return FILE;

			case TABLE:
			case VIEW:
			case STORED_PROCEDURE:
				return FILE_SECTION;
			case COPYPROC:
				return FILE_SECTION;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromCics(final Type type) {
		switch (type) {
			case BMS_MAPSET:
				return FILE;

			case BMS_MAP:
				return FILE_SECTION;

			case TDQ:
			case TSQ:
				return QUEUE;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromResource(final Type type) {
		switch (type) {
			case LIB:
			case FILE:
			case TPFDF_DATASET:
			case LISTCAT:
			case VSAM_FILE:
			case GDG_FILE:
			case RECORD:
				return FILE;

			case TABLE:
			case VIEW:
				return FILE_SECTION;
			case STORED_PROCEDURE:
				return DATABASE;
				
			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromCSD(final Type type) {
		switch (type) {
			case UNKNOWN:
				return UNDEFINED;

			case EXTRACT:
			case TRANSACTION:
			case PROGRAM:
			case FILE:
				return FILE_SECTION;

			case LIST:
				return FILE;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromEcl(final Type type) {
		switch (type) {
			case UNKNOWN:
				return UNDEFINED;
			case ECL_JOB:
				return FILE;

			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromVB(final Type type) {
		switch (type) {
			case MODULE:
			case CLASS:
			case FORM:
			case USER_CONTROL:
			case PROJECT:
			case WORKSPACE:
			case ACTIVEX_DOCUMENT:
			case DESIGNER_FILE:
				return FILE;
			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromWindows(final Type type) {
		switch (type) {
			case DLL:
			case OCX:
				return FILE;
			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}

	private static Storage fromCSharp(final Type type) {
		switch (type) {
			case COMPILATION_UNIT:
			case PROJECT:
			case SOLUTION:
				return FILE;
			default:
				throw new IllegalArgumentException(String.format(TYPE_NOT_MAPPED_ERROR_MESSAGE, type));
		}
	}
}
