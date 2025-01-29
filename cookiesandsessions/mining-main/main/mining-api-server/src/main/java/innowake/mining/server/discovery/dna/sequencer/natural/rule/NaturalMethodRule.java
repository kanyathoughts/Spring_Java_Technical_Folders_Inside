/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dna.sequencer.natural.rule;

import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.lib.parsing.util.visitor.TopDown;
import innowake.lib.parsing.util.visitor.Visitor;
import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider.NaturalParseResult;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.model.Technology;
import innowake.ndt.naturalparser.ast.ProgramObject;

/**
 * The {@link SequencerRule} for the language {@link Technology#NATURAL} that captures all the statements into a DNA string.
 */
public class NaturalMethodRule implements SequencerRule<NaturalParseResult> {

	@Override
	public DnaSequencer getId() {
		return DnaSequencer.NATURAL_METHOD_RULE;
	}

	@Override
	public void apply(final DNACollector<NaturalParseResult> collector) throws DiscoveryException {
		final ProgramObject programObject = collector.getParseResult().getHeavyweightModel().getProgramObject();

		final Visitor visitor = new TopDown(new NaturalMethodRuleVisitor(collector));
		visitor.visit(programObject);
	}

	private static class NaturalMethodRuleVisitor extends NaturalDNAVisitor {

		private static final Map<String, String> stringMap;

		static {
			final String[][] stringMapArray = new String[][] {

					{
							"AcceptStatement", "accept"
					}, {
							"AddStmt", "add"
					}, {
							"AssignStmt", "assign"
					}, {
							"BackoutTransactionStmt", "backout_transaction"
					}, {
							"BeforeBreakStmt", "before_break"
					}, {
							"BreakStmt", "break"
					}, {
							"CallDbProcStmt", "call_db_proc"
					}, {
							"CallnatStmt", "callnat"
					}, {
							"CallStmt", "call"
					}, {
							"ClosePcFileStmt", "close_pc_file"
					}, {
							"ClosePrinterStmt", "close_printer"
					}, {
							"CloseWorkFileStmt", "close_work_file"
					}, {
							"CommitStmt", "commit"
					}, {
							"ComposeStmt", "compose"
					}, {
							"CompressStmt", "compress"
					}, {
							"ComputeStmt", "compute"
					}, {
							"DecideForStmt", "decide_for"
					}, {
							"DecideOnStmt", "decide_on"
					}, {
							"DefinePrinterStmt", "define_printer"
					}, {
							"DefineServiceStmt", "define_service"
					}, {
							"DefineSubroutineStmt", "define_subroutine"
					}, {
							"DefineWindowStmt", "define_window"
					}, {
							"DefineWorkFileStmt", "define_work_file"
					}, {
							"DeleteSQLStmt", "delete_sql"
					}, {
							"DeleteStmt", "delete"
					}, {
							"DialogOptionsStmt", "dialog_options"
					}, {
							"DisplayStmt", "display"
					}, {
							"DivideStmt", "divide"
					}, {
							"DownloadPcFileCommandStmt", "download_pc_file_cmd"
					}, {
							"DownloadPcFileStmt", "download_pc_file"
					}, {
							"EjectOnOffStmt", "eject_on_off"
					}, {
							"EjectStmt", "eject"
					}, {
							"EndAllMarkStmt", "end_all_mark"
					}, {
							"EndOfDataStmt", "end_of_data"
					}, {
							"EndOfTransactionStmt", "end_of_transaction"
					}, {
							"EndPageStmt", "end_page"
					}, {
							"EndStmt", "end"
					}, {
							"EnterStmt", "enter"
					}, {
							"Equal", "eq"
					}, {
							"ErroneousStatement", "error"
					}, {
							"EscapeStmt", "escape"
					}, {
							"ExamineStmt", "examine"
					}, {
							"ExamineTranslateStmt", "examine_translate"
					}, {
							"ExamineTranslateUsingStmt", "examine_translate_using"
					}, {
							"ExpandArrayStmt", "expand_array"
					}, {
							"ExpandDynamicStmt", "expand_dynamic"
					}, {
							"FetchStmt", "fetch"
					}, {
							"FindStmt", "find"
					}, {
							"FormatStmt", "format"
					}, {
							"ForStmt", "for"
					}, {
							"GetSameStmt", "get_same"
					}, {
							"GetStmt", "get"
					}, {
							"GetTransactionStmt", "get_transaction"
					}, {
							"Greater", "gt"
					}, {
							"GreaterEqual", "ge"
					}, {
							"HistogramStmt", "histogram"
					}, {
							"IfSelectionStmt", "if_selection"
					}, {
							"IfStmt", "if"
					}, {
							"IgnoreStmt", "ignore"
					}, {
							"IncludeStmt", "include"
					}, {
							"InputStmt", "input"
					}, {
							"InputUsingMapStmt", "input_using_map"
					}, {
							"InsertStmt", "insert"
					}, {
							"Labeled", "label"
					}, {
							"LimitStmt", "limit"
					}, {
							"MoveEncodedStmt", "move_encoded"
					}, {
							"MoveStmt", "move"
					}, {
							"MultiplyStmt", "multiply"
					}, {
							"NewPageStmt", "new_page"
					}, {
							"NotEqual", "ne"
					}, {
							"OnErrorStmt", "on_error"
					}, {
							"OptionsStmt", "options"
					}, {
							"OrderByStmt", "order_by"
					}, {
							"ParseXmlStmt", "parse_xml"
					}, {
							"PasswStmt", "passw"
					}, {
							"PerformBreakProcessingStmt", "perform_break_processing"
					}, {
							"PerformStmt", "perform"
					}, {
							"PrintStmt", "print"
					}, {
							"ProcessCommandExecStmt", "process_command_exec"
					}, {
							"ProcessGuiStmt", "process_gui"
					}, {
							"ProcessGuiWithParametersStmt", "process_gui_param"
					}, {
							"ProcessPageModalStmt", "process_page_modal"
					}, {
							"ProcessPageStmt", "process_page"
					}, {
							"ProcessPageUpdateStmt", "process_page_update"
					}, {
							"ProcessPageUsingStmt", "process_page_using"
					}, {
							"ProcessSqlStmt", "process_sql"
					}, {
							"ProcessStmt", "process"
					}, {
							"ReadStmt", "read"
					}, {
							"ReadWithoutDescriptorStmt", "read_without_descriptor"
					}, {
							"ReadWorkFileStmt", "read_work_file"
					}, {
							"ReduceArrayStmt", "reduce_array"
					}, {
							"ReduceDynamicStmt", "reduce_dynamic"
					}, {
							"ReinputStmt", "reinput"
					}, {
							"ReleaseStmt", "release"
					}, {
							"RepeatStmt", "repeat"
					}, {
							"RequestDocumentStmt", "request_document"
					}, {
							"ResetStmt", "reset"
					}, {
							"ResizeArrayStmt", "resize_array"
					}, {
							"ResizeDynamicStmt", "resize_dynamic"
					}, {
							"RetryStmt", "retry"
					}, {
							"SelectStmt", "select"
					}, {
							"SeparateStmt", "separate"
					}, {
							"SetControlStmt", "set_control"
					}, {
							"SetGlobalStmt", "set_global"
					}, {
							"SetKeyStmt", "set_key"
					}, {
							"SetSqlStmt", "set_sql"
					}, {
							"SetTimeStmt", "set_time"
					}, {
							"SetWindowStmt", "set_window"
					}, {
							"SkipStmt", "skip"
					}, {
							"Smaller", "lt"
					}, {
							"SmallerEqual", "le"
					}, {
							"SortStmt", "sort"
					}, {
							"SqlConnectStmt", "sql_connect"
					}, {
							"StackStmt", "stack"
					}, {
							"StartOfDataStmt", "start_of_data"
					}, {
							"StopStmt", "stop"
					}, {
							"StoreStmt", "store"
					}, {
							"SubtractStmt", "subtract"
					}, {
							"SuspendIdenticalSuppressStmt", "suspend_identical_suppress"
					}, {
							"TerminateStmt", "terminate"
					}, {
							"TopPageStmt", "top_page"
					}, {
							"UpdateSQLStmt", "update_sql"
					}, {
							"UpdateStmt", "update"
					}, {
							"UploadPcFileStmt", "upload_pc_file"
					}, {
							"WithCteStmt", "with_cte"
					}, {
							"WriteStmt", "write"
					}, {
							"WriteUsingMapParmStmt", "write_using_map_parm"
					}, {
							"WriteUsingMapStmt", "write_using_map"
					}, {
							"WriteWorkFileStmt", "write_work_file"
					}
			};
			stringMap = Stream.of(stringMapArray)
					.collect(Collectors.collectingAndThen(Collectors.toMap(data -> data[0], data -> data[1]), Collections::<String, String> unmodifiableMap));
		}

		private NaturalMethodRuleVisitor(final DNACollector<NaturalParseResult> collector) {
			super(collector);
		}

		@Override
		protected Map<String, String> getStringMap() {
			return stringMap;
		}

	}

}
