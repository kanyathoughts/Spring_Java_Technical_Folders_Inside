/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.List;

import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * The relationship between models.
 * <p>Enums for {@code module_relationship_type} enum type in DB.
 */
@MiningDataType(name = "RelationshipType")
public enum RelationshipType {

	NONE,
	/**
	 * Defines a reference between two {@code modules} where one module is included by another {@code module} like a Cobol program that includes a Cobol
	 * copybook.
	 */
	INCLUDES,
	/**
	 * Defines a reference between two {@code modules} where one module is a part of or contained in another {@code module} like a physical module of a JCL
	 * source that contains virtual modules for the steps.
	 */
	CONTAINS,
	REFERENCES,
	CALLS,
	/** Formerly known as ReadsWrites */
	ACCESSES,
	/**
	 * An Artificial link that is used to connect two {@code modules} that are not directly connected. This type of {@link RelationshipType} is only used for the
	 * visualization in the Dependency Graph when an intermediate {@code module} has been filtered out.
	 */
	ARTIFICIAL,
	/**
	 * Defines a reference between two {@code modules} where one module is a successor of another {@code module} like a JCL job that is a successor of a JCL.
	 * This relationship is generally possible to establish only through the scheduler information.
	 */
	PRECEDES;

	/**
	 * List of dependency relationships. Contains {@link RelationshipType#NONE}, {@link RelationshipType#INCLUDES}, {@link RelationshipType#NONE},
	 * {@link RelationshipType#REFERENCES}, {@link RelationshipType#CALLS}, {@link RelationshipType#ACCESSES} AND {@link RelationshipType#PRECEDES}.
	 */
	public static final List<RelationshipType> DEPENDENCY_TYPES = List.of(NONE, INCLUDES, REFERENCES, CALLS, ACCESSES, PRECEDES);
	public static final RelationshipType[] DEPENDENCY_TYPES_ARR = DEPENDENCY_TYPES.toArray(new RelationshipType[0]);

	private static final String IS_NOT_SUPPORTED = " is not supported";

	/**
	 * Returns the {@link RelationshipType} that matches with the given name.
	 * <p>
	 * The comparison is done case-insensitive.
	 *
	 * @param name the name of the module reference type
	 * @return the {@link RelationshipType}
	 */
	public static RelationshipType from(final String name) {
		for (final RelationshipType value : values()) {
			if (value.name().equalsIgnoreCase(name)) {
				return value;
			}
		}

		throw new IllegalArgumentException("No enum constant " + name + " available");
	}

	/**
	 * Returns the {@link RelationshipType} that matches with the given technology and type.
	 *
	 * @param technology the technology the {@link RelationshipType} is associated with
	 * @param type the type the {@link RelationshipType} is associated with
	 * @return the {@link RelationshipType}
	 */
	public static RelationshipType from(final Technology technology, final Type type) {
		if (Type.UNKNOWN == type) {
			return REFERENCES;
		}

		switch (technology) {
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

			case UNKNOWN:
				return fromUnknown(type);

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

			case SERVICE:
				return fromService(type);

			case VB:
				return fromVB(type);

			case WINDOWS:
				return fromWindows(type);
			case CSHARP:
				return fromCSharp(type);

			default:
				final var message = String.format("Combination Technology %s and Type %s not supported", technology, type);
				throw new IllegalArgumentException(message);
		}
	}
	private static RelationshipType fromCSharp(final Type type) {
		switch (type) {
			case COMPILATION_UNIT:
			case PROJECT:
			case SOLUTION:
				return REFERENCES;
			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}

	}

	private static RelationshipType fromJava(final Type type) {
		switch (type) {
			case COMPILATION_UNIT:
			case PACKAGE:
			case ENUM:
			case TYPE:
			case ANNOTATION:
			case INTERFACE:
			case JSP:
			case JSF:
				return REFERENCES;
			case METHOD:
				return CALLS;
			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromSql(final Type type) {
		switch (type) {
			case SCRIPT:
				return REFERENCES;
			case STORED_PROCEDURE:
				return CALLS;
			case TABLE:
			case VIEW:
				return ACCESSES;
			case INDEX:
			case TRIGGER:
			case SYNONYM:
			case SCHEMA:
				return NONE;
			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromBasic(final Type type) {
		switch (type) {
			case OBJECT:
				return REFERENCES;
			case PROGRAM:
			case FUNCTION:
			case SUBROUTINE:
				return CALLS;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromVMS(final Type type) {
		switch (type) {
			case DCL:
			case VAX_MACRO_ENTRY:
				return CALLS;

			case FMS_FORM:
			case IFDL_FORM:
			case VAX_MACRO:
				return REFERENCES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromOracle(final Type type) {
		switch (type) {
			case CDO_FILE:
			case SQLMOD:
			case CDO_RECORD:
			case SQLMOD_PROCEDURE:
			case RDB_DATABASE:
				return REFERENCES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromIMS(final Type type) {
		switch (type) {
			case DBD:
			case DBD_COMPRTN:
			case DBD_DATASET:
			case DBD_SEGMENT:
			case HELP:
			case HDAMPARM:
			case PSB:
			case PCB:
			case ALT_PCB:
			case TDFXTRCT:
			case APPLICATION:
			case EXPORT:
			case MID:
			case MOD:
			case TRANSACTION:
				return REFERENCES;
			case MFS:
				return INCLUDES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromC(final Type type) {
		switch (type) {
			case UNKNOWN:
				return REFERENCES;

			case PROGRAM:
				return CALLS;

			case HEADER:
				return INCLUDES;

			case FUNCTION:
				return CALLS;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromCpp(final Type type) {
		switch (type) {
			case PROGRAM:
				return CALLS;

			case HEADER:
				return INCLUDES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromEasytrieve(final Type type) {
		switch (type) {
			case UNKNOWN:
				return REFERENCES;
			case MACRO_FILE:
				return CALLS;
			case PROGRAM:
			case INSTREAM:
				return CALLS;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromBinary(final Type type) {
		throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
	}

	private static RelationshipType fromXml(final Type type) {
		if (type == Type.XHTML) {
			return REFERENCES;
		}
		
		throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
	}

	private static RelationshipType fromService(final Type type) {
		return type == Type.SERVICE_REQUEST_ID ? REFERENCES : fromUnknown(type);
	}

	private static RelationshipType fromUnknown(final Type type) {
		switch (type) {
			case UTILITY:
			case PROGRAM:
			case SUBROUTINE:
			case FUNCTION:
				return CALLS;

			case COPYBOOK:
				return INCLUDES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromPl1(final Type type) {
		switch (type) {
			case UNKNOWN:
				return REFERENCES;

			case PROGRAM:
			case MAINPROGRAM:
			case SUBROUTINE:
			case FUNCTION:
				return CALLS;

			case COPYBOOK:
				return INCLUDES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromNatural(final Type type) {
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
				return REFERENCES;

			case PROGRAM:
			case SUBROUTINE:
			case SUBPROGRAM:
			case FUNCTION:
				return CALLS;

			case COPYCODE:
			case MAP:
			case GDA:
			case PDA:
			case LDA:
				return INCLUDES;

			case DDM:
				return ACCESSES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromNone(final Type type) {
		throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
	}

	private static RelationshipType fromCSD(final Type type) {
		switch (type) {
			case UNKNOWN:
			case LIST:
			case TRANSACTION:
			case EXTRACT:
				return REFERENCES;

			case PROGRAM:
				return CALLS;

			case FILE:
				return ACCESSES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromJCL(final Type type) {
		switch (type) {
			case EXEC_PGM:
			case PROC:
			case EXEC:
			case INLINE_PROC:
			case PGM:
				return CALLS;

			case CONTROLCARD:
			case JOB:
			case CFG:
			case RAP_CONTROLCARD:
				return REFERENCES;

			case INCLUDE:
				return INCLUDES;

			case INFO:
				return ACCESSES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromCobol(final Type type) {
		switch (type) {
			case PROGRAM:
				return CALLS;

			case COPYLIB:
				return REFERENCES;

			case COPYBOOK:
			case COPYPROC:
				return INCLUDES;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromResource(final Type type) {
		switch (type) {
			case TABLE:
			case VIEW:
			case LIB:
			case FILE:
			case TPFDF_DATASET:
			case VSAM_FILE:
			case LISTCAT:
			case GDG_FILE:
			case RECORD:
				return ACCESSES;
			case STORED_PROCEDURE:
				return CALLS;

			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromAssembler(final Type type) {
		if (type == Type.MACRO || type == Type.PROGRAM) {
			return REFERENCES;
		}
		
		throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
	}

	private static RelationshipType fromEcl(final Type type) {
		if (type == Type.ECL_JOB) {
			return REFERENCES;
		}

		throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
	}

	private static RelationshipType fromCics(final Type type) {
		switch (type) {
			case BMS_MAPSET:
			case BMS_MAP:
				return INCLUDES;
			case TDQ:
			case TSQ:
				return ACCESSES;
			default:
				throw new IllegalArgumentException(type + IS_NOT_SUPPORTED);
		}
	}

	private static RelationshipType fromVB(@SuppressWarnings("unused") final Type type) {
		/* Relationship is unknown for all the types of VB technology. Hence setting it to NONE */
		return NONE;
	}
	
	private static RelationshipType fromWindows(@SuppressWarnings("unused") final Type type) {
		/* Relationship is unknown for all the types of Windows technology. Hence setting it to NONE */
		return NONE;
	}

}
