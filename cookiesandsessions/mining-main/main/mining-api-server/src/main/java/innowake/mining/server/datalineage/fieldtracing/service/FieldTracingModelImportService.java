/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing.service;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.fieldtracing.SqlUtil;
import innowake.mining.server.datalineage.fieldtracing.modelimport.ModelImportContext;
import innowake.mining.server.datalineage.operations.file.ResourceFileTracer;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.mining.shared.model.datalineage.ProxyContainerPrototype;
import innowake.ndt.cobol.parser.ast.model.CobolConstantReference;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.model.CobolFigurativeConstantsReference;
import innowake.ndt.cobol.parser.ast.model.CobolNumericConstantReference;
import innowake.ndt.cobol.parser.ast.model.CobolReferenceExpression;
import innowake.ndt.cobol.parser.ast.model.ProcedureDivision;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolReadStmt;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCall;
import innowake.ndt.core.parsing.ast.model.statement.FieldReference;
import innowake.ndt.fieldtracing.legacy.model.LegacyFieldUsage;
import innowake.ndt.fieldtracing.legacy.model.LegacyProgram;
import innowake.ndt.fieldtracing.model.AccessMode;
import innowake.ndt.fieldtracing.model.FieldDefinition;
import innowake.ndt.fieldtracing.model.FieldUsage;
import innowake.ndt.fieldtracing.model.Model;
import innowake.ndt.fieldtracing.model.SourceFile;
import innowake.ndt.fieldtracing.model.TracedModule;
import innowake.ndt.fieldtracing.model.UnresolvedCallee;
import innowake.ndt.parsing.parser.sql.preprocess.db2.Db2CobolPreprocessor;

/**
 * Converts a field tracing {@link Model} into {@link DataLineageResult}.
 */
@Service
public class FieldTracingModelImportService {

	private static final String COBOL_CALL_STATEMENT_TYPE = "Call";
	private static final String COBOL_PROCEDURE_DIVISION_STATEMENT_TYPE = "Procedure Division";
	/* redefintions show up as field usages with "statement type" "Data Field" in the Model */
	private static final String REDEFINITION_STATEMENT_TYPE = "Data Field";
	private static final String FILE_DEFINITION_STATEMENT_TYPE = "File Definition";
	private static final String EXEC_SQL_CALL_STATEMENT_TYPE = "Exec Sql Call";
	private static final String ROUNDED_FLAG_STATEMENT_TYPE = "*CobolExpressionWithRoundedFlag";
	private static final String MATH_STATEMENT_TYPE = "Math";
	private static final String SET_CONTAINER_STATEMENT_TYPE = "*CobolSingleSetContainer";
	private static final String EXEC_SQL_STATEMENT_PREFIX = "Exec Sql";
	private static final String READ_FROM_FILE = "Read";
	private static final String WRITE_TO_FILE = "Write";
	private static final String ERROR_MESSAGE = "No sibling ( %s ) found, the FieldDefinition might be missing (potentially defined in another "
			+ "module not present)";
	private static final Set<String> UNSUPPORTED_CLAUSES = Set.of(REDEFINITION_STATEMENT_TYPE, FILE_DEFINITION_STATEMENT_TYPE,
			ROUNDED_FLAG_STATEMENT_TYPE, SET_CONTAINER_STATEMENT_TYPE, MATH_STATEMENT_TYPE);

	private static final Logger LOG = LoggerFactory.getLogger(FieldTracingModelImportService.class);

	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private AstService astService;

	@Autowired
	private ModuleService moduleService;

	/**
	 * Imports the given {@code model} into the project with the given {@code projectId}.
	 * The project must contain corresponding modules for all source files in the model, or else the import will fail.
	 *
	 * @param dataLineageContext The context for this model import
	 * @param model the field tracing model
	 * @return Returns the DataLineageResult created at the starting offset
	 */
	public DataLineageResult importModel(final DataLineageContext dataLineageContext, final Model<ModuleLightweightPojo> model) {
		/* Prepare data necessary for import */
		final List<SourceFile<ModuleLightweightPojo>> allSourceFiles = Stream.concat(model.getSourceFiles().stream(),
				model.getSourceFiles().stream().flatMap(sf -> sf.getChildren().stream())).collect(Collectors.toList());

		final FieldDefinition<ModuleLightweightPojo> initialSelectedField = model.getSelectedField();
		final List<SourceFile<ModuleLightweightPojo>> sourceFiles = findSourceFiles(initialSelectedField.getModule().getSourceObject().getPath(), allSourceFiles);
		if (sourceFiles.isEmpty()) {
			throw new IllegalStateException("There is no corresponding source file in the model for the selected field definition: " + initialSelectedField);
		}

		DataLineageResult result = DataLineageResult.empty();
		for (final SourceFile<ModuleLightweightPojo> sourceFile : sourceFiles) {
			final SourceFile<ModuleLightweightPojo> actualSourceFile;
			if (sourceFile.getType() == SourceFile.Type.COPY) {
				actualSourceFile = sourceFile.getParent().orElseThrow(() ->
						new IllegalStateException("There is no program including the field " + initialSelectedField +
								" defined in copybook "	+ sourceFile.getName()));
			} else {
				actualSourceFile = sourceFile;
			}
			if ( ! (actualSourceFile instanceof LegacyProgram)) {
				continue;
			}
			final FieldDefinition<ModuleLightweightPojo> selectedField = ((LegacyProgram<?, ?, ?, ModuleLightweightPojo>) actualSourceFile).getSelectedField();
			final ModuleLightweightPojo moduleToCreateNodes = actualSourceFile.getModule().getSourceObject();
			final ModelImportContext context = new ModelImportContext(dataLineageContext, selectedField, actualSourceFile);
			final AstNode node = selectedField.getAstNode();
			final String variableName = getRedefinedVariableName(node, selectedField.getFieldName());
			final DataFlowNodePrototype fieldNode = getDataFlowNodeForField(context, variableName, moduleToCreateNodes.identity(),
					selectedField.getAstNode().getStartOffset());

			/* Actually do the import */
			importFieldUsages(allSourceFiles, context, moduleToCreateNodes, fieldNode);
			final Map<AstNode, List<AstNode>> redefinesMap = resolveRedefines(context);
			makeFieldsRelated(context, redefinesMap);

			result = result.combinedWith(DataLineageResult
					.ofNodes(context.getCreatedNodes().values())
					.withProxyContainers(context.getCreatedContainers().values())
					.withStatements(context.getCreatedStatements().values()));
		}

		return result;
	}

	private List<SourceFile<ModuleLightweightPojo>> findSourceFiles(final String path, final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {
		return allSourceFiles.stream()
				.filter(sf -> ! (sf instanceof UnresolvedCallee))
				.filter(sf -> sf.getModule().getPath().equals(Paths.get(path)))
				.collect(Collectors.toList());
	}

	/**
	 * Obtains root AstNodes, traverses their children and makes fields related with each other.
	 *
	 * @param context context of the model import
	 * @param redefinesMap map mapping AstNodes to list of AstNodes they redefine or get redefined by
	 */
	private void makeFieldsRelated(final ModelImportContext context, final Map<AstNode, List<AstNode>> redefinesMap) {
		final SourceFile<ModuleLightweightPojo> sourceFile = context.getSourceFile();
		if (sourceFile.getType() != SourceFile.Type.CALLER && sourceFile.getType() != SourceFile.Type.CALLEE) {
			final CobolDataField startNode = (CobolDataField) context.getSelectedField().getAstNode();
			final DataFlowNodePrototype startDfNode = getDataFlowNodeFromAstNode(context, context.getSourceFile(), startNode, startNode.getFieldName());
			linkWithChildren(startNode, context, startDfNode, redefinesMap);
			linkWithParents(startNode, context, startDfNode, redefinesMap);
			linkWithRedefinitionsChildren(context, redefinesMap, startNode, startDfNode);
			linkWithRedefinitionsParents(context, redefinesMap, startNode, startDfNode);
		}
	}

	private void linkWithRedefinitionsChildren(final ModelImportContext context, final Map<AstNode, List<AstNode>> redefinesMap, final AstNode astNode,
			final DataFlowNodePrototype startNode) {
		if (redefinesMap.containsKey(astNode)) {
			for (final AstNode redefinition : redefinesMap.get(astNode)) {
				linkWithChildren(redefinition, context, startNode, redefinesMap);
			}
		}
	}

	private void linkWithRedefinitionsParents(final ModelImportContext context, final Map<AstNode, List<AstNode>> redefinesMap, final AstNode astNode,
			final DataFlowNodePrototype startNode) {
		if (redefinesMap.containsKey(astNode)) {
			for (final AstNode redefinition : redefinesMap.get(astNode)) {
				linkWithParents(redefinition, context, startNode, redefinesMap);
			}
		}
	}

	private void linkWithChildren(final AstNode astNode, final ModelImportContext context, final DataFlowNodePrototype startNode,
			final Map<AstNode, List<AstNode>> redefinesMap) {
		final List<AstNode> children = astNode.getChildren().stream().filter(CobolDataField.class::isInstance).collect(Collectors.toList());
		for (final AstNode child : children) {
			makeRelated(startNode, (CobolDataField) child, context);
			linkWithChildren(child, context, startNode, redefinesMap);
			linkWithRedefinitionsChildren(context, redefinesMap, child, startNode);
		}
	}

	private void linkWithParents(final AstNode astNode, final ModelImportContext context, final DataFlowNodePrototype startNode,
			final Map<AstNode, List<AstNode>> redefinesMap) {
		final var parent = astNode.getParent();
		if ( ! (parent instanceof CobolDataField)) {
			return;
		}
		makeRelated(startNode, (CobolDataField) parent, context);
		linkWithParents(parent, context, startNode, redefinesMap);
		linkWithRedefinitionsParents(context, redefinesMap, parent, startNode);
	}

	private void makeRelated(final DataFlowNodePrototype node, final CobolDataField otherField, final ModelImportContext context) {
		final DataFlowNodePrototype otherDfNode = getDataFlowNodeFromAstNode(context, context.getSourceFile(), otherField, otherField.getFieldName());
		node.addRelatedField(otherDfNode.getDataFlowId());
		otherDfNode.addRelatedField(node.getDataFlowId());
	}

	/**
	 *
	 * Extracts and imports the usages from the field definition and converts them to DataFlowNodes. Once converted each node is added to the context.
	 *
	 * @param allSourceFiles all source files that are part of the import
	 * @param context context of the model import
	 * @param module the module the field definition is part of
	 * @param fieldNode node representing the field we are importing
	 */
	private void importFieldUsages(final List<SourceFile<ModuleLightweightPojo>> allSourceFiles, final ModelImportContext context,
			final ModuleLightweightPojo module, final DataFlowNodePrototype fieldNode) {
		for (final var usage : context.getSelectedField().getUsages()) {
			if (UNSUPPORTED_CLAUSES.contains(usage.getStatementName())) {
				/* HACK: when the usage is a REDEFINES clause, then we don't create a STATEMENT node for it.
				   This is not a good way to detect that, though. */
				continue;
			}

			final SourceFile<ModuleLightweightPojo> correctSF = checkForCorrectSourceFile(usage, context.getSourceFile(), allSourceFiles);
			final DataFlowNodePrototype statementNode = getDataFlowNodeForStatement(context, usage.getStatementName(), module.getId(),
					usage.getStatementAstNode().getStartOffset());
			importUsage(module.identity(), context, correctSF, usage, fieldNode, statementNode, allSourceFiles);
		}
	}

	private Map<AstNode, List<AstNode>> resolveRedefines(final ModelImportContext context) {
		final Map<AstNode, List<AstNode>> redefinesMap = new HashMap<>();
		final SourceFile<ModuleLightweightPojo> sourceFile = context.getSourceFile();
		for (final var fieldDef : sourceFile.getDefinitions()) {
			final var node = fieldDef.getAstNode();
			if (node instanceof CobolDataField && ((CobolDataField) node).getRedefineReference() != null) {
				final CobolFieldReference fieldRef = ((CobolDataField) node).getRedefineReference().getGenericType(CobolFieldReference.class);
				if (fieldRef != null) {
					final CobolDataField field = fieldRef.getField();
					handleRedefinesPair(node, field, redefinesMap, context, sourceFile);
				}
			}
		}
		return redefinesMap;
	}

	private void handleRedefinesPair(final AstNode a, final AstNode b, final Map<AstNode, List<AstNode>> redefinesMap, final ModelImportContext context,
			final SourceFile<ModuleLightweightPojo> sourceFile) {
		if (a.equals(b) || ! (a instanceof CobolDataField && b instanceof CobolDataField)) {
			return;
		}
		if (redefinesMap.containsKey(a)) {
			redefinesMap.get(a).add(b);
		} else {
			redefinesMap.put(a, new ArrayList<>(Arrays.asList(b)));
		}
		if (redefinesMap.containsKey(b)) {
			redefinesMap.get(b).add(a);
		} else {
			redefinesMap.put(b, new ArrayList<>(Arrays.asList(a)));
		}
		final var aFlowNode = getDataFlowNodeFromAstNode(context, sourceFile, a, ((CobolDataField) a).getFieldName());
		final var bFlowNode = getDataFlowNodeFromAstNode(context, sourceFile, b, ((CobolDataField) b).getFieldName());
		aFlowNode.addRelatedField(bFlowNode.getDataFlowId());
		bFlowNode.addRelatedField(aFlowNode.getDataFlowId());
	}

	private DataFlowNodePrototype getDataFlowNodeFromAstNode(final ModelImportContext context, SourceFile<ModuleLightweightPojo> sourceFile,
			final AstNode astNode, final String fieldName) {
		if (sourceFile.getType() == SourceFile.Type.COPY) {
			final Optional<SourceFile<ModuleLightweightPojo>> oParent = sourceFile.getParent();
			if (oParent.isPresent()) {
				sourceFile = oParent.get();
			} else {
				LOG.error("copy sourcefile has no parent!");
			}
		}

		final EntityId moduleId = sourceFile.getModule().getSourceObject().identity();
		final String variableName = getRedefinedVariableName(astNode, fieldName);

		return getDataFlowNodeForField(context, variableName, moduleId, astNode.getStartOffset());
	}

	/**
	 * Method for matching a usages modulePath to all the sourceFiles visited before, in order attach the correct sourceFile
	 *
	 * @param usage The usage to get the correct sourceFile for
	 * @param initialSF In case the usage is in a UnresolvedCallee we need to return the inital sourceFile
	 * @param allSourceFiles All source files which occurred in the model
	 * @return Returns the correct sourceFile corresponding to the usage
	 */
	private SourceFile<ModuleLightweightPojo> checkForCorrectSourceFile(final FieldUsage<ModuleLightweightPojo> usage, final SourceFile<ModuleLightweightPojo> initialSF,
			final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {
		if (usage.getModule() instanceof UnresolvedCallee) {
			return initialSF;
		}

		final Path modulePath = usage.getModule().getPath();

		final Optional<SourceFile<ModuleLightweightPojo>> oSF = findSourceFile(allSourceFiles, modulePath);
		if (oSF.isPresent()) {
			return oSF.get();
		}

		throw new IllegalArgumentException("No appropriate Sourcefile found for usage: " + usage);
	}

	/**
	 * Method for matching a usages field node to all the sourceFiles visited before, in order attach the correct sourceFile
	 *
	 * @param usage The usage containing the field definition to get the correct sourceFile for
	 * @param initialSF In case the usage is in a UnresolvedCallee we need to return the initial sourceFile
	 * @param allSourceFiles All source files which occurred in the model
	 * @return Returns the correct sourceFile corresponding to the field referenced by the usage
	 */
	private SourceFile<ModuleLightweightPojo> checkForCorrectFieldSourceFile(final FieldUsage<ModuleLightweightPojo> usage, final SourceFile<ModuleLightweightPojo> initialSF,
			final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {
		if (usage.getModule() instanceof UnresolvedCallee) {
			return initialSF;
		}
		final AstNode fieldDefinitionAstNode = usage.getFieldDefinitionAstNode();

		final TracedModule<?> tracedModule = initialSF.getModule().getParsedProgram().getOffsetModel().getModule(fieldDefinitionAstNode);
		final Optional<SourceFile<ModuleLightweightPojo>> potentialSF = findSourceFile(allSourceFiles, tracedModule.getPath());
		if (potentialSF.isPresent()) {
			return potentialSF.get();
		}

		throw new IllegalArgumentException("No appropriate Sourcefile found for usage: " + usage);
	}

	/**
	 * Finds a SourceFile matching to a Path.
	 *
	 * @param allSourceFiles All source files which occurred in the model
	 * @param sourceFilePath the path of the SourceFile we are looking for
	 */
	private Optional<SourceFile<ModuleLightweightPojo>> findSourceFile(final List<SourceFile<ModuleLightweightPojo>> allSourceFiles, final Path sourceFilePath) {
		if (sourceFilePath == null) {
			return Optional.empty();
		}
		for (final SourceFile<ModuleLightweightPojo> sf : allSourceFiles) {
			if ( ! (sf instanceof UnresolvedCallee<?>) && sf.getModule().getPath().equals(sourceFilePath)) {
				/* If the usage is in a copybook the module needs to be the parent module */
				if (sf.getType() == SourceFile.Type.COPY && sf.getParent().isPresent()) {
					return sf.getParent();
				}
				return Optional.of(sf);
			}
		}
		return Optional.empty();
	}

	/**
	 * Internal method to handle special type of usages depending on the {@code Technology}
	 * @param moduleId Id of the project into which the model is imported
	 * @param context The context holding relevant information during the import
	 * @param sourceFile Source code of the traced application
	 * @param usage The usages of the field
	 * @param fieldNode The {@code DataFlowNodePojo} representing the field
	 * @param statementNode The {@code DataFlowNodePojo} representing the statement
	 * @param allSourceFiles All source files which occurred in the model
	 */
	private void importUsage(final EntityId moduleId, final ModelImportContext context, final SourceFile<ModuleLightweightPojo> sourceFile,
			final FieldUsage<ModuleLightweightPojo> usage, final DataFlowNodePrototype fieldNode, final DataFlowNodePrototype statementNode,
			final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {

		checkForConstantFields(moduleId, context, usage, statementNode);

		/* handle special cases */
		/* Handling a call statement
		 * E.g. CALL PROGRAM-NAME USING A-FIELD, ANOTHER-FIELD*/
		if (COBOL_CALL_STATEMENT_TYPE.equals(usage.getStatementName())) {
			importCobolCallUsage(moduleId, context, usage, statementNode, sourceFile, allSourceFiles);
			/* Handling a USING statement in the Procedure Division
			 * E.g. PROCEDURE DIVISION USING PARAMETER-A-FIELD, PARAMETER-ANOTHER-FIELD */
		} else if (COBOL_PROCEDURE_DIVISION_STATEMENT_TYPE.equals(usage.getStatementName())) {
			importCobolProcedureDivisionUsage(moduleId, context, usage, statementNode);
		} else if (usage.getStatementName().equals(EXEC_SQL_CALL_STATEMENT_TYPE)) {
			importExecSqlCallUsage(moduleId, context, usage, fieldNode, statementNode, sourceFile, allSourceFiles);
		} else if (usage.getStatementName().startsWith(EXEC_SQL_STATEMENT_PREFIX)) {
			importExecSqlUsage(moduleId, context, usage, statementNode, sourceFile, allSourceFiles);
		} else if (usage.getStatementName().startsWith(READ_FROM_FILE)) {
			importCobolReadUsage(moduleId, context, usage, fieldNode, statementNode, sourceFile, allSourceFiles);
		} else if (usage.getStatementName().startsWith(WRITE_TO_FILE)) {
			importCobolWriteUsage(moduleId, context, usage, fieldNode, statementNode, sourceFile, allSourceFiles);
		} else {
			importDefaultUsage(moduleId, context, usage, fieldNode, statementNode, sourceFile, allSourceFiles);
		}
	}

	private void importCobolReadUsage(final EntityId moduleId, final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage,
									  final DataFlowNodePrototype fieldNode, final DataFlowNodePrototype statementNode, final SourceFile<ModuleLightweightPojo> sourceFile,
									  final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {
		importDefaultUsage(moduleId, context, usage, fieldNode, statementNode, sourceFile, allSourceFiles);
		final AstNode statementAstNode = usage.getStatementAstNode();

		if ( ! (statementAstNode instanceof CobolReadStmt)) {
			statementNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("Read usage isn't instanceof of CobolReadStmt"));
			return;
		}

		final CobolReadStmt readStmt = (CobolReadStmt) statementAstNode;

		final CobolDataField fileDescriptor = ((CobolFieldReference) ((CobolReferenceExpression) readStmt.getFileName()).getOp1()).getField();

		final DataFlowNodePrototype proxyField = createFileAccessProxyContainerPojo(moduleId, context, fileDescriptor);
		proxyField.addDataFlow(DataFlow.toStatement(statementNode));
	}

	private void importCobolWriteUsage(final EntityId moduleId, final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage,
									   final DataFlowNodePrototype fieldNode, final DataFlowNodePrototype statementNode, final SourceFile<ModuleLightweightPojo> sourceFile,
									   final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {
		importDefaultUsage(moduleId, context, usage, fieldNode, statementNode, sourceFile, allSourceFiles);

		final AstNode astNode = usage.getFieldDefinitionAstNode();

		if ( ! (astNode instanceof CobolDataField)) {
			fieldNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("Argument of WRITE statement is not CobolDataField"));
			return;
		}

		final CobolDataField fd = ((CobolDataField) astNode).getFd();

		if (fd == null) {
			fieldNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("WRITE statement is not associated with FD"));
			return;
		}

		final DataFlowNodePrototype proxyField = createFileAccessProxyContainerPojo(moduleId, context, fd);
		statementNode.addDataFlow(DataFlow.toField(proxyField));
	}

	private DataFlowNodePrototype createFileAccessProxyContainerPojo(final EntityId moduleId, final ModelImportContext context, final CobolDataField fd) {
		final ProxyContainerPrototype container = getProxyContainerPojo(context, ProxyContainerPojo.Type.FILE_ACCESS, moduleId, fd.getStartOffset());
		final DataFlowNodePrototype proxyField = new DataFlowNodePrototype(DataFlowId.forProxyField(container.getDataFlowId(), 0))
				.setName(ResourceFileTracer.PLACEHOLDER_FIELD_NAME);
		container.addField(proxyField);
		return proxyField;
	}

	/**
	 * The default method for importing DataFlowNodes from usages. This method looks at a node and then recursively calls itself on the nodes siblings
	 * E.g A-FIELD, B-FIELD, C-FIELD
	 * @param moduleId Id of the project into which the model is imported
	 * @param context The context holding relevant information during the import
	 * @param usage The usages of the field
	 * @param fieldNode The {@code DataFlowNodePojo} representing the field
	 * @param statementNode The {@code DataFlowNodePojo} representing the statement
	 * @param sourceFile The sourceFile the usage is from
	 * @param allSourceFiles All sourcefiles which occured in the model
	 */
	private void importDefaultUsage(final EntityId moduleId, final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage,
									final DataFlowNodePrototype fieldNode, final DataFlowNodePrototype statementNode, final SourceFile<ModuleLightweightPojo> sourceFile,
									final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {

		if (usage.getAccessType() == AccessMode.READ) {
			fieldNode.addDataFlow(DataFlow.toStatement(statementNode));
		} else if (usage.getAccessType() == AccessMode.WRITE) {
			statementNode.addDataFlow(DataFlow.toField(fieldNode));
		} else if (usage.getAccessType() == AccessMode.UNSUPPORTED) {
			fieldNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.WARNING)
					.setText("The access for the field '" + usage.getFieldName() + "' with the statement '"
							+ usage.getStatementName() + "' is unsupported and could not be traced further."));
		}

		for (final FieldUsage<ModuleLightweightPojo> sibling : usage.getSiblings()) {
			if (sibling.getOffset() == usage.getOffset() || sibling.getFieldName().equals("null")) {
				/* for some reason the "main usage" is duplicated in the siblings list, but it's actually not the same object either
				 * we therefore check if the offset of the sibling is the same as the offset of the "main usage"
				 * Also seems like the fieldTracer sometimes attaches a invalid field with name "null" to the trace. In order to save time we can skip it*/
				continue;
			}

			/* Gets the field node related to the usage */
			final Optional<DataFlowNodePrototype> siblingNode = getFieldNodeFromUsageNode(context, sibling, sourceFile, allSourceFiles);
			if (siblingNode.isPresent()) {
				/* Recursively call this method with the sibling node  */
				importDefaultUsage(moduleId, context, sibling, siblingNode.get(), statementNode, sourceFile, allSourceFiles);
			} else {
				statementNode.addError(new DataFlowErrorPojoPrototype()
						.setSeverity(DataFlowErrorPojo.Severity.ERROR)
						.setText(String.format(ERROR_MESSAGE, sibling.getFieldName())));
			}
		}
	}

	private void checkForConstantFields(final EntityId moduleId, final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage,
										final DataFlowNodePrototype statementNode) {
		final List<AstNode> constants = usage.getStatementAstNode().getChildren().stream()
				.filter(node -> node.getAstNodeName().equals(CobolReferenceExpression.class.getSimpleName()) && node.getChildren().stream()
						.anyMatch(child -> child instanceof CobolFigurativeConstantsReference || child instanceof CobolConstantReference))
				.map(AstNode::getChildren)
				.flatMap(Collection::stream)
				.collect(Collectors.toList());

		final var moduleNid = moduleService.getModuleNid(moduleId);
		for (final AstNode constant : constants) {
			final String fieldName;
			if (constant instanceof CobolFigurativeConstantsReference) {
				final CobolFigurativeConstantsReference startNode = (CobolFigurativeConstantsReference) constant;
				fieldName = startNode.getValue();
			} else if (constant instanceof CobolNumericConstantReference) {
				final CobolNumericConstantReference startNode = (CobolNumericConstantReference) constant;
				if (startNode.getValue() == null) {
					throw new IllegalStateException("CobolNumericConstantReference value not found.");
				}
				fieldName = startNode.getValue().toString();
			} else {
				final CobolConstantReference startNode = (CobolConstantReference) constant;
				fieldName = StringUtils.upperCase(startNode.getValue().toString());
				if ( ! fieldName.equals("TRUE") && ! fieldName.equals("FALSE")) {
					continue;
				}
			}

			final DataFlowNodePrototype constantNode = getDataFlowNodeForConstant(context, fieldName, moduleNid);
			constantNode.addDataFlow(DataFlow.toStatement(statementNode));
		}
	}

	/**
	 * Returns a DataFlowNodePojo corresponding to a FieldUsage
	 * @param context The context holding relevant information during the import
	 * @param usage The usage to get the node for
	 * @param sourceFile The sourceFile the usage was in
	 * @param allSourceFiles All sourcefiles which occured in the model
	 * @return Returns the corresponding Node to a usage
	 */
	private Optional<DataFlowNodePrototype> getFieldNodeFromUsageNode(final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage,
			final SourceFile<ModuleLightweightPojo> sourceFile, final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {

		try {
			final SourceFile<ModuleLightweightPojo> actualModule = checkForCorrectFieldSourceFile(usage, sourceFile,
					allSourceFiles); /* The usage doesn't need to have the same sourceFile as the field! */
			final EntityId actualModuleId = actualModule.getModule().getSourceObject().identity();

			/* In case of an array access which gets parsed before the fieldDefinition is resolved we need to create the node without the (*) otherwise
			 * there are 2 identical nodes at the same offset with different names*/
			final String adjustedString = usage.getFieldName().replaceAll("\\(.*\\)", "");
			return Optional.of(getDataFlowNodeForField(context, adjustedString, actualModuleId, usage.getFieldDefinitionAstNode().getStartOffset()));
		} catch (final IllegalStateException e) {
			/* getFieldDefinitionAstNode() throws IllegalStateException when the field definition can not be found - e.g. due to unresolved copybook */
			LOG.error("While tracing " + sourceFile.getName() + ": Unable to resolve field definition for " + usage, e);
			return Optional.empty();
		}
	}

	/**
	 * Checks the DB for a Node and then checks if the node is present in the current context
	 *
	 * @param context The context holding relevant information during the import
	 * @param fieldName The name of the field the node belongs to
	 * @param actualModuleId The correct ModuleId
	 * @param offset The offset of the node
	 * @return Returns the correct node
	 */
	private DataFlowNodePrototype getDataFlowNodeForField(final ModelImportContext context, final String fieldName, final EntityId actualModuleId,
			final Integer offset) {

		final DataFlowId dataFlowId = DataFlowId.forField(moduleService.getModuleNid(actualModuleId), new ModuleLocation(offset.intValue(), 0));
		return context.getCreatedNodes().computeIfAbsent(dataFlowId, id -> new DataFlowNodePrototype(dataFlowId)
				.setName(fieldName)
				.setLocation(new ModuleLocation(offset.intValue(), 0)));
	}

	private DataFlowNodePrototype getDataFlowNodeForConstant(final ModelImportContext context, final String constantValue, final Long actualModuleId) {

		final DataFlowId dataFlowId = DataFlowId.forConstant(actualModuleId, constantValue);
		return context.getCreatedNodes().computeIfAbsent(dataFlowId, id -> new DataFlowNodePrototype(dataFlowId)
				.setName(constantValue));
	}

	private DataFlowNodePrototype getDataFlowNodeForStatement(final ModelImportContext context, final String fieldName, final Long actualModuleId,
														  final Integer offset) {

		final DataFlowId dataFlowId = DataFlowId.forStatement(actualModuleId, new ModuleLocation(offset.intValue(), 0));
		return context.getCreatedStatements().computeIfAbsent(dataFlowId, id -> new DataFlowNodePrototype(dataFlowId)
				.setName(fieldName)
				.setLocation(new ModuleLocation(offset.intValue(), 0)));
	}

	@SuppressWarnings("null")
	private String getRedefinedVariableName(final AstNode node, final String fieldName) {
		if ( ! (node instanceof CobolDataField) || ((CobolDataField) node).getRedefineReference() == null) {
			return fieldName;
		}
		final CobolFieldReference redefinedReference = ((CobolDataField) node).getRedefineReference().getGenericType(CobolFieldReference.class);
		if (redefinedReference == null) {
			return fieldName;
		}
		final String redefinedVariable = redefinedReference.getField().getFieldName();
		if (redefinedVariable != null && (StringUtils.isBlank(fieldName) || fieldName.equals("REDEFINES"))) {
			return "FILLER REDEFINES" + " " + redefinedVariable;
		}
		return fieldName;
	}

	/**
	 * Checks the database for a ProxyContainerPojo and then checks if this container is already present in the current context
	 *
	 * @param context The context holding relevant information during the import
	 * @param type The Type of ProxyContainerPojo
	 * @param moduleId Id of the module into which the container is imported
	 * @param offset At which offset the container is at
	 * @return The correct ProxyContainerPojo
	 */
	private ProxyContainerPrototype getProxyContainerPojo(final ModelImportContext context, final ProxyContainerPojo.Type type, final EntityId moduleId, final Integer offset) {

		final DataFlowId dataFlowId = DataFlowId.forProxyContainer(moduleService.getModuleNid(moduleId), type, new ModuleLocation(offset.intValue(), 0));
		return context.getCreatedContainers().computeIfAbsent(dataFlowId, k -> new ProxyContainerPrototype(dataFlowId)
				.setType(type)
				.setStatementLocation(new ModuleLocation(offset.intValue(), 0)));
	}

	/**
	 * Handles the logic of a Cobol Call. Creates the Proxy Container and all corresponding Nodes including sibling nodes
	 * @param moduleId id of the (root, i.e. not copybook) Module that makes the CALL
	 * @param context The context holding relevant information during the import
	 * @param usage The usage to get the node for
	 * @param statementNode The {@code DataFlowNodePojo} representing the statement
	 * @param sourceFile type of the {@link SourceFile} that contains the Call
	 */
	private void importCobolCallUsage(final EntityId moduleId, final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage,
									  final DataFlowNodePrototype statementNode, final SourceFile<ModuleLightweightPojo> sourceFile, final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {

		final ProxyContainerPrototype container = getProxyContainerPojo(context, ProxyContainerPojo.Type.CALL, moduleId, statementNode.location.getNonNull().getOffset());

		final CobolCallStmt callStmt = ((LegacyFieldUsage<?, ?>) usage).getStatementAstNode().getGenericType(CobolCallStmt.class);
		if (callStmt == null) {
			LOG.error("While importing Cobol CALL usage in module " + moduleId + ": CALL usage is not CobolCallStmt");
			statementNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("Unable to analyze call: CALL statement is not CobolCallStmt"));
			return;
		}
		/* Sets the Target of Call statement to Call Proxy container properties */
		if (callStmt.getTarget() != null) {
			final String name = MetricsUtility.trimQuotesSpaces(callStmt.getTarget().toString());
			container.setProperties(Map.of(ProxyContainerPojo.Property.TARGET_NAME, name));
		}

		/* if the target of a CALL is a field reference (i.e. call is made using a variable containing the program name), then it is a dynamic call.
		 * The variable holding the field reference will show up in the list of siblings, but we must not create a PROXY_FIELD for it,
		 * since it is not passed to the called program. Therefore, we drop the first sibling for a dynamic call. */
		final boolean dynamicCall = callStmt.getTarget().getGenericType(FieldReference.class) != null;
		/* populate the CALL container using "sibling" information from the CALL usage
		 * - the position of the arguments is important, so sort them by offset */
		final List<FieldUsage<ModuleLightweightPojo>> siblingsSorted = usage.getSiblings().stream()
				.sorted(Comparator.comparing(FieldUsage::getOffset))
				.skip(dynamicCall ? 1 : 0)
				.collect(Collectors.toList());
		int index = 0;
		for (final FieldUsage<ModuleLightweightPojo> sibling: siblingsSorted) {
			String fieldName;
			final Optional<DataFlowNodePrototype> siblingNode = getFieldNodeFromUsageNode(context, sibling, sourceFile, allSourceFiles);
			if (siblingNode.isPresent()) {
				fieldName = siblingNode.get().name.getNonNull();
				/* a CALL always READs from each given variable - if it is a CALL BY REFERENCE, it also writes */
				siblingNode.get().addDataFlow(DataFlow.toStatement(statementNode));
				if (sibling.getAccessType() == AccessMode.WRITE) {
					statementNode.addDataFlow(DataFlow.toField(siblingNode.get()));
				}
			} else {
				fieldName = "(missing)";
				statementNode.addError(new DataFlowErrorPojoPrototype()
						.setSeverity(DataFlowErrorPojo.Severity.ERROR)
						.setText(String.format(ERROR_MESSAGE, sibling.getFieldName())));
			}

			final DataFlowNodePrototype proxyField = new DataFlowNodePrototype(DataFlowId.forProxyField(container.getDataFlowId(), index++))
					.setName(fieldName);
			container.addField(proxyField);
			statementNode.addDataFlow(DataFlow.toField(proxyField));
		}
	}

	/**
	 * Handles the logic for Procedure divisions in cobol.
	 * E.g. PROCEDURE DIVISION USING WS-A, WS-B.
	 * @param moduleId Id of the project into which the model is imported
	 * @param context The context holding relevant information during the import
	 * @param usage the FieldUsage in the PROCEDURE DIVISION USING clause
	 * @param statementNode the DataFlowNodePojo representing the PROCEDURE DIVISION USING clause
	 */
	private void importCobolProcedureDivisionUsage(final EntityId moduleId, final ModelImportContext context,
												   final FieldUsage<ModuleLightweightPojo> usage, final DataFlowNodePrototype statementNode) {

		final ProxyContainerPrototype container = getProxyContainerPojo(context, ProxyContainerPojo.Type.ENTRY_POINT, moduleId, statementNode.location.getNonNull().getOffset());

		/* sibling information is not provided for the PROCEDURE DIVISION USING "usage", so we must reach through to the AST */
		final ProcedureDivision procedureDivision = ((LegacyFieldUsage<?, ?>) usage).getStatementAstNode().getGenericType(ProcedureDivision.class);
		if (procedureDivision == null) {
			LOG.error("While importing PROCEDURE DIVISION of module " + moduleId + ": ProcedureDivision AstNode not present");
			statementNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("Unable to create ENTRY_POINT: ProcedureDivision AstNode not present."));
			return;
		}
		final List<DataFlowNodePrototype> fieldDefinitions = procedureDivision.getUsings()
				.stream()
				.sorted(Comparator.comparing(ref -> ref.getAstNode().getStartOffset()))
				.map(ref -> ref.getGenericType(FieldReference.class))
				.filter(Objects::nonNull)
				.map(FieldReference::getFieldDefinition)
				.filter(Optional::isPresent)
				.map(Optional<innowake.ndt.core.parsing.ast.model.statement.FieldDefinition<?>>::get)
				.map(fieldDef -> getDataFlowNodeForField(context, fieldDef.getFieldName(), moduleId, fieldDef.getAstNode().getStartOffset()))
				.collect(Collectors.toList());

		int index = 0;
		for (final DataFlowNodePrototype fieldDefinition : fieldDefinitions) {
			final DataFlowNodePrototype proxyField = new DataFlowNodePrototype(DataFlowId.forProxyField(container.getDataFlowId(), index++))
					.setName(fieldDefinition.name.getNonNull());
			proxyField.addRelatedField(fieldDefinition.getDataFlowId());
			container.addField(proxyField);
		}
	}

	/**
	 * Handles the logic for importing EXECSQL Statements
	 * @param moduleId Id of the module
	 * @param context The context holding relevant information during the import
	 * @param usage The usage to get the node for
	 * @param statementNode The {@code DataFlowNodePojo} representing the statement
	 * @param sourceFile The {@link SourceFile} that contains the statement
	 * @param allSourceFiles All source files which occurred in the model
	 */
	private void importExecSqlUsage(final EntityId moduleId, final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage,
									final DataFlowNodePrototype statementNode, final SourceFile<ModuleLightweightPojo> sourceFile, final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {

		final SqlUtil.SqlInfo sqlInfo = getSqlInfo(statementNode, moduleId);
		if (sqlInfo == null) {
			return;
		}
		connectSqlStatementToFields(context, usage, statementNode, sourceFile, allSourceFiles, sqlInfo);

		if (sqlInfo.getColumnMap().isEmpty()) {
			/* If there is no column map there should be no data flow between the program and the table */
			return;
		}

		final List<ModuleRelationshipPojo> readsWrites = moduleService.findRelationship(q -> q.ofProject(context.getDataLineageContext().getProjectId())
																							.ofSource(moduleId)
																							.withType(RelationshipType.ACCESSES));

		final AstNodeLocation fieldLocation = astService.find(q -> q.ofModule(moduleId).withAssembledOffset(Comperator.EQUAL, statementNode.location.getNonNull().getOffset()))
				.stream().map(AstNodePojo::getLocation)
				.collect(Collectors.toList()).get(0);
		final Optional<ModuleRelationshipPojo> foundRW = findRWByLocation(readsWrites, fieldLocation.convertToSharedModuleLocation());

		if (foundRW.isEmpty()) {
			statementNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("Unable to find the corresponding ReadsWrites to the location of this StatementNode"));
			return;
		}

		final ProxyContainerPrototype container = createDbAccessContainer(moduleId, context, statementNode, sqlInfo);

		connectStatementNodeToMapProxyFields(statementNode, container, sqlInfo.getColumnMap(), foundRW.get());
	}

	@Nullable
	private SqlUtil.SqlInfo getSqlInfo(final DataFlowNodePrototype statementNode, final EntityId moduleId) {
		final ModuleLocation location = statementNode.location.get();
		final List<AstNodePojo> astNodes = location == null ?
			Collections.emptyList() :
				astService.find(q -> q.ofModule(moduleId).withAssembledOffset(Comperator.EQUAL, location.getOffset()));
		if (astNodes.isEmpty()) {
			statementNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("Couldn't find AstNode corresponding to this DataFlowNodePojo"));
			return null;
		}
		final String label = astNodes.get(0).getLabel();
		if (label == null) {
			statementNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("The label of the EXEC SQL statement AstNode is null: " + astNodes.get(0)));
			return null;
		}
		final String sql = new Db2CobolPreprocessor().getPreprocessedSqlString(label);
		try {
			/* Replacing the numbers at the beginning and at the end of line in the sql for not preprocessed ones */
			return SqlUtil.getSqlInfo(sql.replaceAll("\\b\\d+\\s*", ""));
		} catch (final Exception e) {
			/* In case our SqlUtil fails to create the ColumnMap for the statement */
			statementNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("SqlUtil#getSqlInfo failed for the following sql statement: \"" + sql + "\" with exception: " + e));
			return null;
		}
	}

	/**
	 * Iterate through each sibling (sibling == fields of sql statement) and add the read/write access to the fieldNode.
	 *
	 * @param context context of model import
	 * @param usage {@linkplain FieldUsage representing the EXEC SQL statement}
	 * @param statementNode EXEC SQL statement
	 * @param sourceFile The {@link SourceFile} that contains the statement
	 * @param allSourceFiles All source files which occurred in the model
	 * @param sqlInfo the {@linkplain SqlUtil.SqlInfo} that contains information on how host variables are used in the EXEC SQL statement
	 */
	private void connectSqlStatementToFields(final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage, final DataFlowNodePrototype statementNode,
			final SourceFile<ModuleLightweightPojo> sourceFile, final List<SourceFile<ModuleLightweightPojo>> allSourceFiles, final SqlUtil.SqlInfo sqlInfo) {
		for (final FieldUsage<ModuleLightweightPojo> sibling : usage.getSiblings()) {
			if (sibling.getFieldName().equals("null")) {
				/* Seems like the fieldTracer sometimes attaches an invalid field with name "null" to the trace. In order to save time we can skip it*/
				continue;
			}

			/* Gets the field node related to the usage */
			final Optional<DataFlowNodePrototype> siblingNode = getFieldNodeFromUsageNode(context, sibling, sourceFile, allSourceFiles);

			if (siblingNode.isPresent()) {
				if (sqlInfo.getWrittenTo().contains(siblingNode.get().name.getNonNull())) {
					statementNode.addDataFlow(DataFlow.toField(siblingNode.get()));
				}
				if (sqlInfo.getReadFrom().contains(siblingNode.get().name.getNonNull())) {
					siblingNode.get().addDataFlow(DataFlow.toStatement(statementNode));
				}
			} else {
				statementNode.addError(new DataFlowErrorPojoPrototype()
						.setSeverity(DataFlowErrorPojo.Severity.ERROR)
						.setText(String.format(ERROR_MESSAGE, sibling.getFieldName())));
			}
		}
	}

	private ProxyContainerPrototype createDbAccessContainer(final EntityId moduleId, final ModelImportContext context, final DataFlowNodePrototype statementNode,
															final SqlUtil.SqlInfo sqlInfo) {
		/* create proxy container representing the DB access */
		final ProxyContainerPrototype container = getProxyContainerPojo(context, ProxyContainerPojo.Type.DATABASE_ACCESS, moduleId, statementNode.location.getNonNull().getOffset());
		setColumnMap(container, sqlInfo.getColumnMap());

		return container;
	}

	private Map<String, List<String>> setColumnMap(final ProxyContainerPrototype container, final Map<String, List<String>> columnMap) {

		try {
			container.setProperties(Map.of(ProxyContainerPojo.Property.DATABASE_ACCESS_SQL_COLUMN_MAP, objectMapper.writeValueAsString(columnMap)));
		} catch (final JsonProcessingException e) {
			throw new IllegalStateException("Failed to write to ColumnMap", e);
		}
		return columnMap;
	}

	private void connectStatementNodeToMapProxyFields(final DataFlowNodePrototype statementNode,
			final ProxyContainerPrototype container, final Map<String, List<String>> columnMap, final ModuleRelationshipPojo readWriteReference) {
		final Optional<Map<String, Object>> properties = readWriteReference.getProperties();
		final var rwDbAccess = properties.isEmpty() || properties.get().isEmpty() ? null :
				properties.get().get(ModelAttributeKey.DB_ACCESS_TYPE.toString());
		final boolean isReadAccess = rwDbAccess instanceof List && ((List<?>) rwDbAccess).contains(DatabaseAccessType.READ.toString());
		final boolean isWriteAccess = rwDbAccess instanceof List && ((List<?>) rwDbAccess).stream()
				.map(Object::toString)
				.anyMatch(accessType -> DatabaseAccessType.UPDATE.toString().equals(accessType)
						|| DatabaseAccessType.DELETE.toString().equals(accessType)
						|| DatabaseAccessType.STORE.toString().equals(accessType)
						|| DatabaseAccessType.OTHER.toString().equals(accessType));

		int index = 0;
		for (final Entry<String, List<String>> entry : columnMap.entrySet()) {
			final DataFlowNodePrototype proxyField = new DataFlowNodePrototype(DataFlowId.forProxyField(container.getDataFlowId(), index++))
					.setName(entry.getKey());
			container.addField(proxyField);

			if (isReadAccess) {
				proxyField.addDataFlow(DataFlow.toStatement(statementNode));
			}
			if (isWriteAccess) {
				statementNode.addDataFlow(DataFlow.toField(proxyField));
			}
		}
	}

	private void importExecSqlCallUsage(final EntityId moduleId, final ModelImportContext context, final FieldUsage<ModuleLightweightPojo> usage,
										final DataFlowNodePrototype fieldNode, final DataFlowNodePrototype statementNode, final SourceFile<ModuleLightweightPojo> sourceFile, final List<SourceFile<ModuleLightweightPojo>> allSourceFiles) {

		final AstNode statementAstNode = ((LegacyFieldUsage<?, ?>) usage).getStatementAstNode();

		if ( ! (statementAstNode instanceof ExecSqlCall)) {
			LOG.error("While analyzing EXEC SQL CALL usage in module " + usage.getModule().getSourceObject().getId()
					+ ": unexpected statement of type " + statementAstNode.getClass().getSimpleName() + ". Expected ExecSqlCall");
			statementNode.addError(new DataFlowErrorPojoPrototype()
					.setSeverity(DataFlowErrorPojo.Severity.ERROR)
					.setText("Unable to parse EXEC SQL CALL statement in order to inspect the parameters"));
			return;
		}

		/* create proxy container for the call */
		final ProxyContainerPrototype container = getProxyContainerPojo(context, ProxyContainerPojo.Type.CALL, moduleId, statementNode.location.getNonNull().getOffset());

		/* create fields inside the proxy container for each parameter, but only if the parameter is a field reference - ignore constants/literals */
		final ExecSqlCall<?> callNode = (ExecSqlCall<?>) statementAstNode;
		final List<Integer> parameterIndexes = new ArrayList<>(callNode.getParameters().size());
		int parameterIndex = 0;
		int fieldIndex = 0;
		for (final AstNode parameter : callNode.getParameters()) {
			if (parameter instanceof CobolReferenceExpression && ((CobolReferenceExpression) parameter).getOp1() instanceof CobolFieldReference) {
				final CobolFieldReference fieldReference = (CobolFieldReference) ((CobolReferenceExpression) parameter).getOp1();
				final DataFlowNodePrototype proxyField = new DataFlowNodePrototype(
						DataFlowId.forProxyField(container.getDataFlowId(), fieldIndex++))
						.setName(fieldReference.getField().getName());
				/* this is not correct since the access mode depends on whether the parameter IN, OUT, or INOUT - we don't have the parameter
				 * definition available here right now though. For now, we leave it to the stored procedure module itself to handle the further
				 * passing of data. */
				proxyField.addDataFlow(DataFlow.toStatement(statementNode));
				statementNode.addDataFlow(DataFlow.toField(proxyField));
				container.addField(proxyField);
				parameterIndexes.add(parameterIndex);
			}
			parameterIndex++;
		}

		final String procedureName = Optional.ofNullable(((CobolConstantReference) callNode.getProcedureName())).map(name -> name.getValue().toString())
				.orElse("");
		final String parameterIndexesAsString;
		try {
			parameterIndexesAsString = objectMapper.writeValueAsString(parameterIndexes);
		} catch (final JsonProcessingException e) {
			LOG.error("While creating proxy container for EXEC SQL CALL in module " + moduleId + ": Error while serializing parameter indexes" +
					" to JSON", e);
			throw new IllegalStateException(e);
		}
		final Map<ProxyContainerPojo.Property, Object> properties = Map.of(
				ProxyContainerPojo.Property.TARGET_NAME, procedureName,
				ProxyContainerPojo.Property.CALL_PARAMETER_INDEX, parameterIndexesAsString
		);
		container.setProperties(properties);

		/* this is not correct since the access mode depends on whether the parameter IN, OUT, or INOUT - see above */
		fieldNode.addDataFlow(DataFlow.toStatement(statementNode));
		statementNode.addDataFlow(DataFlow.toField(fieldNode));
		for (final FieldUsage<ModuleLightweightPojo> sibling : usage.getSiblings()) {
			final Optional<DataFlowNodePrototype> siblingNode = getFieldNodeFromUsageNode(context, sibling, sourceFile, allSourceFiles);
			if (siblingNode.isPresent()) {
				siblingNode.get().addDataFlow(DataFlow.toStatement(statementNode));
				statementNode.addDataFlow(DataFlow.toField(siblingNode.get()));
			} else {
				statementNode.addError(new DataFlowErrorPojoPrototype()
						.setSeverity(DataFlowErrorPojo.Severity.ERROR)
						.setText(String.format(ERROR_MESSAGE, sibling.getFieldName())));
			}
		}
	}

	/**
	 * Finds the correct ReadsWrites at a given location
	 *
	 * @param readsWrites All the readsWrites available
	 * @param nodeLocation The location the ReadsWrites should be located at
	 * @return Returns the ReadsWrites or null if none were found at that location
	 */
	private Optional<ModuleRelationshipPojo> findRWByLocation(final List<ModuleRelationshipPojo> readsWrites, final ModuleLocation nodeLocation) {
		for (final ModuleRelationshipPojo rw : readsWrites) {
			final var rwLocation = rw.getSrcLocation();
			if (rwLocation.isEmpty()) {
				continue;
			}

			final Integer rwOffset = rwLocation.get().getOffset();
			if (rwOffset.equals(nodeLocation.getOffset())) {
				return Optional.of(rw);
			}
		}
		return Optional.empty();
	}
}

