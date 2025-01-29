package innowake.mining.shared.model.discovery;

import static java.util.Arrays.stream;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;

/**
 * Used to define all possible types for a model artifact. Enum enforces parent/child relationship with languages
 * having type children. Some entries are here for backwards compatibility.
 */
public enum ResolveTarget {

	LANGUAGE(),

		UNKNOWN(LANGUAGE),
			UTILITY(UNKNOWN),
			PROGRAM(UNKNOWN),
			COPYBOOK(UNKNOWN),
			SUBROUTINE(UNKNOWN),
			FUNCTION(UNKNOWN),
			UNDISCOVERED(UNKNOWN),

		COBOL(LANGUAGE),
			COBOL_PROGRAM(COBOL),
			COBOL_COPYBOOK(COBOL),
			COBOL_COPYLIB(COBOL),
			COBOL_COPYPROC(COBOL),

		NATURAL(LANGUAGE),
			/* Defining reporting mode Natural types as children of default Natural types is owed to the constraint
			 * that an extension must be unique for all items in ResolveTarget. */
			NATURAL_PROGRAM(NATURAL),
				NATURAL_PROGRAM_REPORTING(NATURAL_PROGRAM),
			NATURAL_SUBROUTINE(NATURAL),
				NATURAL_SUBROUTINE_REPORTING(NATURAL_SUBROUTINE),
			NATURAL_SUBPROGRAM(NATURAL),
				NATURAL_SUBPROGRAM_REPORTING(NATURAL_SUBPROGRAM),
			NATURAL_COPYCODE(NATURAL),
				NATURAL_COPYCODE_REPORTING(NATURAL_COPYCODE),
			NATURAL_CLASS(NATURAL),
			NATURAL_CPM(NATURAL),
			NATURAL_ADAPTVIEW(NATURAL),
			NATURAL_FUNCTION(NATURAL),
				NATURAL_FUNCTION_REPORTING(NATURAL_FUNCTION),
			NATURAL_HELP(NATURAL),
				NATURAL_HELP_REPORTING(NATURAL_HELP),
			NATURAL_ADAPTER(NATURAL),
			NATURAL_MAP(NATURAL),
				NATURAL_MAP_REPORTING(NATURAL_MAP),
			NATURAL_DIALOG(NATURAL),
			NATURAL_DIALOG_PRIV_RES(NATURAL),

			/* support original (GDA, PDA, LDA) and natclipse normalized (IW_GDA, IW_PDA, IW_LDA) data areas
			 *
			 * original:
			 * - while discovering code, data areas may not be normalized if natclipse project was not configured yet
			 * - while discovering metrics, original data areas are collected and will be normalized after automatic project configuration
			 *
			 * normalized:
			 * - while discovering code and metrics, data areas may already be normalized if natclipse project was configured already */
			NATURAL_GDA(NATURAL),
			NATURAL_IW_GDA(NATURAL),
			NATURAL_PDA(NATURAL),
			NATURAL_IW_PDA(NATURAL),
			NATURAL_LDA(NATURAL),
			NATURAL_IW_LDA(NATURAL),

			NATURAL_DDM(NATURAL),
			NATURAL_TEXT(NATURAL),
			NATURAL_ERROR_MESSAGE(NATURAL),

		JCL(LANGUAGE),
			JCL_JOB(JCL),
			JCL_PROC(JCL),
				JCL_INLINE_PROC(JCL_PROC),
			JCL_EXEC(JCL),
			JCL_EXEC_PGM(JCL),
			JCL_PGM(JCL),
			JCL_CFG(JCL),
			JCL_INFO(JCL),
			JCL_CONTROLCARD(JCL),
			JCL_INCLUDE(JCL),
			/* Custom Type, for client specific categorization. Currently, ignored by Discovery & Mining */
			RAP_CONTROLCARD(JCL),

		VMS(LANGUAGE),
		    DCL(VMS),
		    FMS_FORM(VMS),
		    IFDL_FORM(VMS),
		    VAX_MACRO(VMS),
		    VAX_MACRO_ENTRY(VMS),

		ORACLE(LANGUAGE),
	        CDO_FILE(ORACLE),
	        CDO_RECORD(ORACLE),
	        SQLMOD(ORACLE),
	        SQLMOD_PROC(ORACLE),
	        RDB_DATABASE(ORACLE),

		IMS(LANGUAGE),
			IMS_DBD(IMS),
				IMS_DBD_COMPRTN(IMS_DBD),
				IMS_DBD_DATASET(IMS_DBD),
				IMS_DBD_SEGMENT(IMS_DBD),
			IMS_HELPTXT(IMS),
			IMS_HDAMPARM(IMS),
			IMS_PSB(IMS),
				IMS_PCB(IMS_PSB),
				IMS_ALT_PCB(IMS_PSB ),
			IMS_SYSGEN_EXPORT(IMS),
				IMS_SYSGEN_TRANSACTION(IMS),
				IMS_SYSGEN_APPLICATION(IMS),
			IMS_MFS(IMS),
				IMS_MFS_MID(IMS),
				IMS_MFS_MOD(IMS),

		SQL(LANGUAGE),
		  SQL_STORED_PROCEDURE(SQL),
		  SQL_SCRIPT(SQL),
		  SQL_TABLE(SQL),
		  SQL_INDEX(SQL),
		  SQL_TRIGGER(SQL),
		  SQL_VIEW(SQL),
		  SQL_SCHEMA(SQL),
		  SQL_TEMPORARY_TABLE(SQL),
		  SQL_SYNONYM(SQL),

		ASSEMBLER(LANGUAGE),
			ASSEMBLER_PROGRAM(ASSEMBLER),
			ASSEMBLER_MACRO(ASSEMBLER),

		RESOURCE(LANGUAGE),
			RESOURCE_FILE(RESOURCE),
			RESOURCE_TPFDF_DATASET(RESOURCE),
			RESOURCE_LIB(RESOURCE),
			RESOURCE_VSAM_FILE(RESOURCE),
			RESOURCE_RECORD(RESOURCE),
			RESOURCE_GDG_FILE(RESOURCE),

		XML(LANGUAGE),
			XML_XHTML(XML),

		CSD(LANGUAGE),
			CSD_LIST(CSD),
				CSD_TRANSACTION(CSD_LIST),
				CSD_PROGRAM(CSD_LIST),
				CSD_FILE(CSD_LIST),
			CSD_EXTRACT(CSD),


		PL1(LANGUAGE),
			PL1_PROGRAM(PL1),
			PL1_MAINPROGRAM(PL1),
			PL1_COPYBOOK(PL1),
			PL1_SUBROUTINE(PL1),
			PL1_FUNCTION(PL1),

		BINARY(LANGUAGE),

		EASYTRIEVE(LANGUAGE),
			EASYTRIEVE_PROGRAM(EASYTRIEVE),
			EASYTRIEVE_INSTREAM(EASYTRIEVE),
			EASYTRIEVE_MACRO_FILE(EASYTRIEVE),

		C(LANGUAGE),
			C_PROGRAM(C),
			C_HEADER(C),
			C_FUNCTION(C),

		CPP(LANGUAGE),
			CPP_PROGRAM(CPP),
			CPP_HEADER(CPP),
			
		LISTCAT(LANGUAGE),

		BASIC(LANGUAGE),
			BASIC_PROGRAM(BASIC),
			BASIC_SUBROUTINE(BASIC),
			BASIC_FUNCTION(BASIC),
			BASIC_OBJECT(BASIC),

		JAVA(LANGUAGE),
			JAVA_COMPILATION_UNIT(JAVA),
			JAVA_PACKAGE(JAVA),
			JAVA_INTERFACE(JAVA),
			JAVA_TYPE(JAVA),
			JAVA_ENUM(JAVA),
			JAVA_ANNOTATION(JAVA),
			JAVA_METHOD(JAVA),
			JAVA_JSF(JAVA),
			JAVA_JSP(JAVA),

		ECL(LANGUAGE),
			ECL_JOB(ECL),

		CICS(LANGUAGE),
			CICS_BMS_MAPSET(CICS),
			CICS_BMS_MAP(CICS),
			CICS_TSQ(CICS),
			CICS_TDQ(CICS),

		VB(LANGUAGE),
			VB_MODULE(VB),
			VB_CLASS(VB),
			VB_FORM(VB),
			VB_CONTROL(VB),
			VB_PROJECT(VB),
			VB_WORKSPACE(VB),
			VB_ACTIVEX_DOCUMENT(VB),
			VB_DESIGNER_FILE(VB),

		WINDOWS(LANGUAGE),
			WINDOWS_DLL(WINDOWS),
			WINDOWS_OCX(WINDOWS),

		CSHARP(LANGUAGE),
			CSHARP_COMPILATION_UNIT(CSHARP),
			CSHARP_PROJECT(CSHARP),
			CSHARP_SOLUTION(CSHARP),

		NONE(LANGUAGE),

		/** Custom Type, for client specific categorization. Currently ignored by Discovery & Mining */
		SERVICE(LANGUAGE),
			SERVICE_REQUEST_ID(SERVICE);

	@Nullable private final ResolveTarget parent;

	private ResolveTarget() {
		this.parent = null;
	}

	private ResolveTarget(@Nullable final ResolveTarget parent) {
		this.parent = parent;
	}

	/**
	 * Returns a hierarchical ordered list of {@code ResolveTarget} starting from root to this element.
	 *
	 * @return a hierarchical ordered list of {@code ResolveTarget} starting from root
	 */
	public List<ResolveTarget> getRootChain() {
		final List<ResolveTarget> result = new LinkedList<>();
		for (ResolveTarget target = this; target != LANGUAGE; target = Assert.assertNotNull(target.parent, "Target must not be NULL")) {
			result.add(0, target);
		}

		return result;
	}

	/**
	 * Returns the language of this element or {@value #NONE} if the element is the {@value #LANGUAGE}.
	 *
	 * @return the language of this element or {@value #NONE} if the element is the {@value #LANGUAGE}
	 */
	public ResolveTarget getLanguage() {
		if (this == LANGUAGE || parent == null) {
			return NONE;
		} else if (parent == LANGUAGE) {
			return this;
		} else {
			final Optional<ResolveTarget> targetParent = getParent();
			if (targetParent.isPresent()) {
				return Assert.assertNotNull(targetParent.get().getLanguage(), "Parent Language must not be NULL");
			}
			return NONE;
		}
	}

	/**
	 * Returns the children of this element.
	 *
	 * @return the children of this element
	 */
	public Set<ResolveTarget> getChildren() {
		return stream(values())
				.filter(candidate -> candidate.parent == this)
				.collect(Collectors.toSet());
	}

	/**
	 * Returns the parent of this element.
	 *
	 * @return the parent of this element
	 */
	public Optional<ResolveTarget> getParent() {
		return Optional.ofNullable(parent);
	}
}
