/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.io;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Defines sheets and columns of the Discovery Excel workbook.
 * <p>Note: Changing column headers can lead to incompatibilities with already existing discovery reports. To keep backwards compatibility please proceed as
 * following:
 * <ul>
 * <li>If the constant of the column header is a string, turn it into a string array</li>
 * <li>Add the new column header to the column header array. The first element is always the latest one which is also used for exporting reports to CSV and
 * Excel!</li>
 * </ul>
 */
public final class WorkbookDefinition {
	
	private static final String MODULE_NAME = "Module Name";
	public static final String UID = "Uid";
	public static final String MODULE_COLUMN_PARENT_UID = "Parent Uid";
	public static final String MODULE_COLUMN_NAME = "Name";
	public static final String MODULE_COLUMN_PATH = "Path";
	public static final String MODULE_COLUMN_LANGUAGE = "Language";
	public static final String MODULE_COLUMN_TYPE = "Type";  
	public static final String MODULE_COLUMN_REPRESENTATION = "Representation";
	public static final String MODULE_COLUMN_COMPLEXITY = "Complexity";
	public static final String MODULE_COLUMN_ERRORS = "Errors (Nodes)";
	public static final String[] MODULE_COLUMN_CODE_LINES = { "Source Lines of Code", "Code Line Count" };
	public static final String[] MODULE_COLUMN_COMMENT_LINES = { "Comment Lines of Code","Comment Line Count" };
	public static final String[] MODULE_COLUMN_PHYSICAL_LINES = { "Physical Lines of Code", "Physical Line Count" };
	public static final String MODULE_COLUMN_STATEMENTS = "Statements (Nodes)";
	public static final String MODULE_COLUMN_SQL_STATEMENTS = "SQL Statements";
	public static final String MODULE_COLUMN_OFFSET = "Offset";
	public static final String MODULE_COLUMN_LENGTH = "Length";

	/**
	 * The column headers for the Modules sheet.
	 */
	private static final SheetColumn[] MODULES_COLUMNS = {
			SheetColumn.mandatory(UID),
			SheetColumn.mandatory(MODULE_COLUMN_PARENT_UID),
			SheetColumn.mandatory(MODULE_COLUMN_NAME),
			SheetColumn.mandatory(MODULE_COLUMN_PATH),
			SheetColumn.mandatory(MODULE_COLUMN_LANGUAGE),
			SheetColumn.mandatory(MODULE_COLUMN_TYPE),
			SheetColumn.mandatory(MODULE_COLUMN_REPRESENTATION),
			SheetColumn.mandatory(MODULE_COLUMN_COMPLEXITY),
			SheetColumn.mandatory(MODULE_COLUMN_ERRORS),
			SheetColumn.mandatory(MODULE_COLUMN_CODE_LINES),
			SheetColumn.mandatory(MODULE_COLUMN_COMMENT_LINES),
			SheetColumn.optional(MODULE_COLUMN_PHYSICAL_LINES),
			SheetColumn.mandatory(MODULE_COLUMN_STATEMENTS),
			SheetColumn.optional(MODULE_COLUMN_SQL_STATEMENTS),
			SheetColumn.optional(MODULE_COLUMN_OFFSET),
			SheetColumn.optional(MODULE_COLUMN_LENGTH)
	};

	public static final String STATEMENTS_COLUMN_STATEMENT = "Statement";
	public static final String STATEMENTS_COLUMN_STRING = "String";
	private static final SheetColumn[] STATEMENTS_COLUMNS = {
			SheetColumn.mandatory(UID),
			SheetColumn.mandatory(MODULE_NAME),
			SheetColumn.mandatory(STATEMENTS_COLUMN_STATEMENT),
			SheetColumn.mandatory(STATEMENTS_COLUMN_STRING)
	};
	
	public static final String SQL_COLUMN_STATEMENT = "Statement";
	public static final String SQL_COLUMN_SQL_LENGTH = "SQL Length";
	public static final String SQL_COLUMN_TABLES = "Tables";
	public static final String SQL_COLUMN_DISTINCT_TABLES = "Distinct Tables";
	public static final String SQL_COLUMN_CUSTOM_COMPLEXITY = "Custom Complexity";
	public static final String SQL_COLUMN_HALSTEAD_COMPLEXITY = "Halstead Complexity";
	public static final String SQL_COLUMN_HALSTEAD_DIFFICULTY = "Halstead Difficulty";
	public static final String SQL_COLUMN_STRING = "String";

	private static final SheetColumn[] SQL_COLUMNS = {
			SheetColumn.mandatory(UID),
			SheetColumn.mandatory(MODULE_NAME),
			SheetColumn.mandatory(SQL_COLUMN_STATEMENT),
			SheetColumn.mandatory(SQL_COLUMN_SQL_LENGTH),
			SheetColumn.mandatory(SQL_COLUMN_TABLES),
			SheetColumn.mandatory(SQL_COLUMN_DISTINCT_TABLES),
			SheetColumn.mandatory(SQL_COLUMN_CUSTOM_COMPLEXITY),
			SheetColumn.mandatory(SQL_COLUMN_HALSTEAD_COMPLEXITY),
			SheetColumn.mandatory(SQL_COLUMN_HALSTEAD_DIFFICULTY),
			SheetColumn.mandatory(SQL_COLUMN_STRING)
	};
	
	public static final String DEPENDENCIES_COLUMN_TARGET_UID = "Target Uid";
	public static final String DEPENDENCIES_COLUMN_TARGET_NAME = "Target Name";
	public static final String DEPENDENCIES_COLUMN_TARGET_LANGUAGE = "Target Language";
	public static final String DEPENDENCIES_COLUMN_TARGET_TYPE = "Target Type";
	public static final String DEPENDENCIES_COLUMN_RELATIONSHIP = "Relationship";
	public static final String DEPENDENCIES_COLUMN_REACHED_FROM_UIDS = "Reached From Uids";
	public static final String DEPENDENCIES_COLUMN_BINDING = "Binding";
	public static final String DEPENDENCIES_COLUMN_ATTRIBUTES = "Attributes";
	public static final String DEPENDENCIES_COLUMN_FROM_OFFSET = "fromLocationOffset";
	public static final String DEPENDENCIES_COLUMN_FROM_LENGTH = "fromLocationLength";
	public static final String DEPENDENCIES_COLUMN_TO_OFFSET = "toLocationOffset";
	public static final String DEPENDENCIES_COLUMN_TO_LENGTH = "toLocationLength";
	
	private static final SheetColumn[] DEPENDENCIES_COLUMNS = {
			SheetColumn.mandatory(UID),
			SheetColumn.mandatory(MODULE_NAME),
			SheetColumn.mandatory(DEPENDENCIES_COLUMN_TARGET_UID),
			SheetColumn.mandatory(DEPENDENCIES_COLUMN_TARGET_NAME),
			SheetColumn.mandatory(DEPENDENCIES_COLUMN_TARGET_LANGUAGE),
			SheetColumn.mandatory(DEPENDENCIES_COLUMN_TARGET_TYPE),
			SheetColumn.optional(DEPENDENCIES_COLUMN_RELATIONSHIP),
			SheetColumn.optional(DEPENDENCIES_COLUMN_REACHED_FROM_UIDS),
			SheetColumn.mandatory(DEPENDENCIES_COLUMN_BINDING),
			SheetColumn.mandatory(DEPENDENCIES_COLUMN_ATTRIBUTES),
			SheetColumn.optional(DEPENDENCIES_COLUMN_FROM_OFFSET),
			SheetColumn.optional(DEPENDENCIES_COLUMN_FROM_LENGTH),
			SheetColumn.optional(DEPENDENCIES_COLUMN_TO_OFFSET),
			SheetColumn.optional(DEPENDENCIES_COLUMN_TO_LENGTH)
	};
	
	public static final String ERRORS_COLUMN_SEVERITY = "Severity";
	public static final String ERRORS_COLUMN_FROM_LENGTH = "Length";
	public static final String ERRORS_COLUMN_FROM_OFFSET = "Offset";
	public static final String ERRORS_COLUMN_KEY = "Key";
	public static final String ERRORS_COLUMN_CAUSE= "Cause";
	private static final SheetColumn[] ERRORS_COLUMNS = {
			SheetColumn.mandatory(UID),
			SheetColumn.mandatory(MODULE_NAME),
			SheetColumn.mandatory(ERRORS_COLUMN_SEVERITY),
			SheetColumn.mandatory(ERRORS_COLUMN_KEY),
			SheetColumn.mandatory(ERRORS_COLUMN_CAUSE),
			SheetColumn.optional(ERRORS_COLUMN_FROM_OFFSET),
			SheetColumn.optional(ERRORS_COLUMN_FROM_LENGTH),
	};
	
	public static final String UNDISCOVERED_COLUMN_NAME = "Name";
	public static final String UNDISCOVERED_COLUMN_PATH = "Path";
	private static final SheetColumn[] UNDISCOVERED_COLUMNS = {
			SheetColumn.mandatory(UID),
			SheetColumn.mandatory(UNDISCOVERED_COLUMN_NAME),
			SheetColumn.mandatory(UNDISCOVERED_COLUMN_PATH)
	};
	
	public static final String DEAD_CODE_COLUMN_DEADCODE = "DeadCode";
	public static final String DEAD_CODE_COLUMN_STARTING_LINE = "Starting Line";
	public static final String DEAD_CODE_COLUMN_NUMBER_OF_LINES = "Number of Lines";
	private static final SheetColumn[] DEAD_CODE_COLUMNS = {
			SheetColumn.mandatory(UID),
			SheetColumn.mandatory(MODULE_NAME),
			SheetColumn.mandatory(DEAD_CODE_COLUMN_DEADCODE),
			SheetColumn.mandatory(DEAD_CODE_COLUMN_STARTING_LINE),
			SheetColumn.mandatory(DEAD_CODE_COLUMN_NUMBER_OF_LINES)
	};
	
	public static final String CONDITIONAL_OUTLINES_COLUMN_TYPE= "Type";
	public static final String CONDITIONAL_OUTLINES_COLUMN_RELEVANCE = "Relevance";
	public static final String CONDITIONAL_OUTLINES_COLUMN_LINE = "Line";
	public static final String CONDITIONAL_OUTLINES_COLUMN_EXPRESSION = "Expression";
	
	/** Name of the Modules sheet. */
	public static final String SHEET_MODULES = "Modules";
	/** Name of the Statements sheet. */
	public static final String SHEET_STATEMENTS = "Statements";
	/** Name of the SQL sheet. */
	public static final String SHEET_SQL = "SQL";
	/** Name of the Dependencies sheet. */
	public static final String SHEET_DEPENDENCIES = "Dependencies";
	/** Name of the Errors sheet. */
	public static final String SHEET_ERRORS = "Errors";
	/** Name of the Undiscovered sheet. */
	public static final String SHEET_UNDISCOVERED = "Undiscovered";
	/** Name of the Dead Code sheet. */
	public static final String SHEET_DEAD_CODE = "Dead Code";

	/** Defines sheets of the Discovery Excel workbook. */
	public static final Map<String, Sheet> SHEETS;
	static {
		final Map<String, Sheet> sheets = new HashMap<>();
		sheets.put(SHEET_MODULES, new Sheet(SHEET_MODULES, MODULES_COLUMNS, true));
		sheets.put(SHEET_STATEMENTS, new Sheet(SHEET_STATEMENTS, STATEMENTS_COLUMNS, true));
		/* sheet "SQL_COLUMNS" was added lately */
		sheets.put(SHEET_SQL, new Sheet(SHEET_SQL, SQL_COLUMNS, false));
		sheets.put(SHEET_DEPENDENCIES, new Sheet(SHEET_DEPENDENCIES, DEPENDENCIES_COLUMNS, true));
		sheets.put(SHEET_ERRORS, new Sheet(SHEET_ERRORS, ERRORS_COLUMNS, true));
		sheets.put(SHEET_UNDISCOVERED, new Sheet(SHEET_UNDISCOVERED, UNDISCOVERED_COLUMNS, true));
		/* sheet "DEAD_CODE" was added lately */
		sheets.put(SHEET_DEAD_CODE, new Sheet(SHEET_DEAD_CODE, DEAD_CODE_COLUMNS, false));
		SHEETS = Collections.unmodifiableMap(sheets);
	}

	private WorkbookDefinition() {}
	
	/**
	 * A single sheet of a {@link WorkbookDefinition}.
	 */
	public static final class Sheet {
		
		private final String sheetName;
		private final SheetColumn[] sheetColumns;
		private final String[] columnNames;
		private final boolean mandatory;
		
		protected Sheet(final String sheetName, final SheetColumn[] sheetColumns, final boolean mandatory) {
			this.sheetName = sheetName;
			this.sheetColumns = sheetColumns;
			this.mandatory = mandatory;

			columnNames = new String[sheetColumns.length];
			for (int i = 0; i < sheetColumns.length; i++) {
				columnNames[i] = sheetColumns[i].name;
			}
		}

		/**
		 * Returns the sheet name.
		 *
		 * @return the sheet name
		 */
		public String getSheetName() {
			return sheetName;
		}

		/**
		 * Returns the column names.
		 *
		 * @return the columns names
		 */
		public String[] getColumnNames() {
			return columnNames;
		}

		/**
		 * Returns the {@link SheetColumn SheetColumns} of this {@link Sheet}.
		 *
		 * @return the columns names
		 */
		public SheetColumn[] getSheetColumns() {
			return sheetColumns;
		}
		
		/**
		 * Returns whether the sheet is mandatory
		 *
		 * @return whether the sheet is mandatory
		 */
		public boolean isMandatory() {
			return mandatory;
		}
	}

	/**
	 * A column in a sheet of a {@link WorkbookDefinition}.
	 */
	public static final class SheetColumn {

		/**
		 * The column name.
		 */
		public final String name;

		/**
		 * {@code true} if the column is mandatory. Otherwise {@code false}
		 */
		public final boolean mandatory;

		/**
		 * The old column names for backwards compatibility
		 */
		public final String[] oldNames;

		/**
		 * Creates a new mandatory sheet column for the given {@code columnNames}.
		 * <p>You can set multiple column names, if the column name got changed to keep backwards compatibility.
		 * The first column name is considered as to be the latest one.</p>
		 *
		 * @param columnNames the list of possible column names. Set multiple names if the header label got changed for backwards compatibility.
		 * @return new {@link SheetColumn}
		 */
		protected static SheetColumn mandatory(final String... columnNames) {
			if (columnNames.length == 0) {
				throw new IllegalArgumentException("At least one column header name must be set");
			}

			if (columnNames.length == 1) {
				return new SheetColumn(columnNames[0], true);
			} else {
				return new SheetColumn(columnNames[0], true, Arrays.copyOfRange(columnNames, 1, columnNames.length));
			}
		}

		/**
		 * Creates a new optional sheet column for the given {@code columnNames}.
		 * <p>You can set multiple column names, if the column name got changed to keep backwards compatibility.
		 * The first column name is considered as to be the latest one.</p>
		 *
		 * @param columnNames the list of possible column names. 
		 * @return new {@link SheetColumn}
		 */
		protected static SheetColumn optional(final String... columnNames) {
			if (columnNames.length == 0) {
				throw new IllegalArgumentException("At least one column header name must be set");
			}

			if (columnNames.length == 1) {
				return new SheetColumn(columnNames[0], false);
			} else {
				return new SheetColumn(columnNames[0], false, Arrays.copyOfRange(columnNames, 1, columnNames.length));
			}
		}
		
		/**
		 * Constructor.
		 * 
		 * @param name the column name
		 * @param mandatory {@code true} if the column is mandatory. Otherwise {@code false}
		 * @param oldNames old column names for backwards compatibility
		 */
		protected SheetColumn(final String name, final boolean mandatory, final String... oldNames) {
			this.name = name;
			this.mandatory = mandatory;
			this.oldNames = oldNames;
		}
	}
}
