/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.io;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import innowake.mining.shared.io.WorkbookDefinition.Sheet;
import innowake.mining.shared.io.WorkbookDefinition.SheetColumn;

/**
 * Defines sheets and columns of the Effort Summary Excel workbook.
 */
public class EffortSummaryWorkbookDefinition {
	
	private static final SheetColumn[] TYPE_SUMMARY_COLUMNS = {
            SheetColumn.mandatory("Language"),
            SheetColumn.mandatory("Type"),
            SheetColumn.mandatory("Count"),
            SheetColumn.mandatory("LoC"),
            SheetColumn.mandatory("LoC comments"),
            SheetColumn.mandatory("Errors (Nodes)"),
            SheetColumn.mandatory("easy"),
            SheetColumn.mandatory("complex"),
            SheetColumn.mandatory("very complex"),
            SheetColumn.mandatory("unmaintainable")
    };
     
    private static final SheetColumn[] PRICING_SUMMARY_COLUMNS = {
            SheetColumn.mandatory("Screens"),
            SheetColumn.mandatory("Errors"),
            SheetColumn.mandatory("Modules with errors"),
            SheetColumn.mandatory("Data files"),
            SheetColumn.mandatory("Batch exec all programs"),
            SheetColumn.mandatory("Batch exec Cobol"),
            SheetColumn.mandatory("Batch exec assembler"),
            SheetColumn.mandatory("Batch exec unknown/missing"),
            SheetColumn.mandatory("RDBMS used"),
            SheetColumn.mandatory("Missing dependencies")
    };
     
     
    /** Name of the Type Summary sheet. */
    public static final String SHEET_TYPE_SUMMARY = "Summary by Language";
    /** Name of the Pricing Summary sheet. */
    public static final String SHEET_PRICING_SUMMARY = "Summary of Pricing";
     
    /** Defines sheets of the Discovery Excel workbook. */
    public static final Map<String, Sheet> SHEETS;
    static {
        final Map<String, Sheet> sheets = new HashMap<>();
        sheets.put(SHEET_TYPE_SUMMARY, new Sheet(SHEET_TYPE_SUMMARY, TYPE_SUMMARY_COLUMNS, false));
        sheets.put(SHEET_PRICING_SUMMARY, new Sheet(SHEET_PRICING_SUMMARY, PRICING_SUMMARY_COLUMNS, false));
        SHEETS = Collections.unmodifiableMap(sheets);
    }
 
    private EffortSummaryWorkbookDefinition() {}

}
