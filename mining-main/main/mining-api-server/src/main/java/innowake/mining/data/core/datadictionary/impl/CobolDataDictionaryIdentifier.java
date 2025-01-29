/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.datadictionary.impl;

import static innowake.mining.data.core.api.AstNodeUtils.CONSTANT_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_REFERENCE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.ndt.cobol.parser.ast.model.CobolConstantReference;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.collection.Pair;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.DataDictionaryVariableAttribute;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Type;
import innowake.ndt.cobol.parser.ast.model.CobolDataField.Usage;
import innowake.ndt.cobol.parser.ast.model.CobolFileDefinition;
import innowake.ndt.cobol.parser.ast.model.FileSection;
import innowake.ndt.cobol.parser.ast.model.LinkageSection;
import innowake.ndt.cobol.parser.ast.model.WorkingStorageSection;
import innowake.ndt.cobol.parser.ast.statement.CobolMoveStmt;
import innowake.ndt.core.parsing.ast.model.statement.ArithmeticStatement;
import innowake.ndt.core.parsing.ast.model.statement.AssigningStatement;
import innowake.ndt.core.parsing.ast.model.statement.DatabaseAccessStatement;

/**
 * Identifies {@linkplain DataDictionaryPojoPrototype} for COBOL Modules.
 */
public class CobolDataDictionaryIdentifier extends DataDictionaryIdentifier {

	private static final String FIELD_PROPERTY_COMP = "comp";
	private static final String DATABASE_ACCESS_STATEMENT = DatabaseAccessStatement.class.getSimpleName();
	private static final String WORKING_STORAGE_SECTION = WorkingStorageSection.class.getSimpleName();
	private static final String LINKAGE_SECTION = LinkageSection.class.getSimpleName();
	private static final String FILE_SECTION = FileSection.class.getSimpleName();
	private static final String READ = "READ";
	private static final String DELETE  = "DELETE";
	private static final String WRITE  = "WRITE";
	private static final String UPDATE  = "UPDATE";
	private static final String CREATE  = "CREATE";
	private static final String INPUT  = "INPUT";
	private static final String ACCESS_TYPE = "accessType";
	private static final String FILE_ACCESS_TYPE = "FILE_ACCESS_TYPE";
	private static final String UNNAMED_MEMBER = "<Unnamed-Member>";
	private static final String AST_PROPERTY_LEVEL = "level";
	private static final String AST_PROPERTY_NAME = "name";
	private static final String LEFT_OPERANDS = "leftOperands";
	private static final String DATASET_NAME_PROPERTY = "datasetName";
	private static final String COBOL_FD_NAME = "COBOL_FD_NAME";
	private static final AstNodeCollector FIELD_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(FIELD_REFERENCE));
	private static final AstNodeCollector CONSTANT_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(CONSTANT_REFERENCE));
	private static final List<String> FIELD_TRANSFORMATION_SUPER_TYPES =
			Arrays.asList(ArithmeticStatement.class.getSimpleName(), AssigningStatement.class.getSimpleName());
	private static final Set<Type> COPYBOOK_TYPES = Sets.newHashSet(Type.COPYBOOK, Type.CONTROLCARD, Type.COPYCODE, Type.COPYLIB, Type.COPYPROC, Type.GDA,
			Type.INCLUDE, Type.INLINE_PROC, Type.LDA, Type.PDA, Type.PROC);
	
	private final EntityId moduleId;
	private final AstNodePojo rootAstNode;
	private final Optional<DependencyModule> dependencyModule;

	final Map<String, Map<String, Object>> fileDependencyProperties = new HashMap<>();
	final Map<ModuleLocation, Pair<String>> dbDependencyLocations = new HashMap<>();

	/**
	 * The constructor.
	 * 
	 * @param core the {@link MiningDataCoreService}
	 * @param moduleId the Module ID
	 * @param rootAstNode the root AST node
	 * @param dependencyModule the dependencies of the {@linkplain Module}
	 */
	public CobolDataDictionaryIdentifier(final MiningDataCoreService core, final EntityId moduleId, final AstNodePojo rootAstNode, final Optional<DependencyModule> dependencyModule) {
		super(core);
		this.moduleId = moduleId;
		this.rootAstNode = rootAstNode;
		this.dependencyModule = dependencyModule;
	}
	
	@Override
	protected List<AstNodePojo> getDataFields() {
		return super.getDataFields().stream().filter(f -> ! CobolFileDefinition.class.getSimpleName().equals(f.getType())).collect(Collectors.toList());
	}
	
	@Override
	protected void collectLanguageSpecificProperties(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		final Map<String, Object> fieldProperties = field.getProperties();
		dde.setUsage(fieldProperties.containsKey(FIELD_PROPERTY_COMP) ? (String) fieldProperties.get(FIELD_PROPERTY_COMP) : Usage.DISPLAY.toString());
		collectLevelAndParentData(field, dde);
		if ( ! "Group".equalsIgnoreCase(dde.format.getNonNull())) {
			collectIntialValues(field, dde);
			calculateTargetOutputAndFieldTransformation(field, dde);
		} else {
			dde.setLength(null);
			if (dde.scopes.getNonNull().containsKey(DataDictionaryVariableScope.FILE)) {
				collectFileDataSets(field, dde);
			}
		}
	}

	@Override
	protected void assignScopesAccessTypes(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		if (fileDependencyProperties.isEmpty()) {
			getFileDependencies();
		}

		final Map<DataDictionaryVariableScope, Map<String, String>> scopes = dde.scopes.getNonNull();
		if (scopes.containsKey(DataDictionaryVariableScope.PARAMETER)) {
			/* add scope attributes, merge if parameter attributes, if already present */
			scopes.computeIfAbsent(DataDictionaryVariableScope.PARAMETER, key -> new HashMap<>()).put(ACCESS_TYPE, INPUT);
		}
		final AstNodePojo previousSibling = field.getPreviousSibling().orElse(null);
		if ( ! fileDependencyProperties.isEmpty() && previousSibling != null && CobolFileDefinition.class.getSimpleName().equals(previousSibling.getType())) {
			final String fdName = (String) previousSibling.getProperties().get(AST_PROPERTY_NAME);
			if (fileDependencyProperties.containsKey(fdName)) {
				final Map<String, Object> fdProperties = fileDependencyProperties.get(fdName);
				@SuppressWarnings("unchecked")
				final List<String> accessList = (List<String>) fdProperties.get(FILE_ACCESS_TYPE);
				if( ! accessList.isEmpty()) {
					assignScopeAccessForFileAndSQL(scopes, accessList);
				}
			}
		}
	}

	private void assignScopeAccessForFileAndSQL (final Map<DataDictionaryVariableScope, Map<String, String>> scopes, final List<String> accessList) {
		if (scopes.containsKey(DataDictionaryVariableScope.FILE)) {
			final Map<String, String> accessMap = scopes.computeIfAbsent(DataDictionaryVariableScope.FILE, key -> new HashMap<>());
			for (final String accessTypes : accessList) {
				switch (accessTypes) {
					case READ:
					case WRITE:
					case DELETE:
						accessMap.put(ACCESS_TYPE, accessTypes);
						break;
					default:
						/* nothing to do */
						break;
				}
			}
		}
		if (scopes.containsKey(DataDictionaryVariableScope.SQL_DATABASE))  {
			final Map<String, String> accessMap = scopes.computeIfAbsent(DataDictionaryVariableScope.SQL_DATABASE, key -> new HashMap<>());
			for (String accessTypes : accessList) {
				switch (accessTypes) {
					case CREATE:
					case UPDATE:
					case READ:
					case DELETE:
						accessMap.put(ACCESS_TYPE, accessTypes);
						break;
					default:
						/* nothing to do */
						break;
				}
			}
		}
	}

	@Override
	protected void assignScopes(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		final Map<DataDictionaryVariableScope, Map<String, String>> scopes = dde.scopes.getNonNull();
		
		if (hasDatabaseScope(field, dde)) {
			scopes.putIfAbsent(DataDictionaryVariableScope.SQL_DATABASE, new HashMap<>());
		}
		
		final String section = getFieldSection(field);
		if (section.equals(FILE_SECTION)) {
			scopes.putIfAbsent(DataDictionaryVariableScope.FILE, new HashMap<>());
		} else if (section.equals(LINKAGE_SECTION)) {
			scopes.putIfAbsent(DataDictionaryVariableScope.PARAMETER, new HashMap<>());
		} else if (section.equals(WORKING_STORAGE_SECTION) && scopes.isEmpty()) {
			scopes.putIfAbsent(DataDictionaryVariableScope.OTHER, new HashMap<>());
		}
		
		dde.setScopes(scopes);
	}

	@Override
	protected AstNodePojo getRootNode() {
		return rootAstNode;
	}

	@Override
	protected EntityId getModuleId() {
		return moduleId;
	}

	@Override
	public void setDefinedLocation(final @Nullable EntityId calleeModuleId, final DataDictionaryPojoPrototype dde, final AstNodePojo field) {
		if (calleeModuleId != null) {
			final var type = core.moduleService.getModuleType(calleeModuleId);
			dde.setDefinedLocation(COPYBOOK_TYPES.contains(type) ? DefinedLocation.COPYBOOK : DefinedLocation.PROGRAM);
		} else {
			dde.setDefinedLocation(DefinedLocation.PROGRAM);
		}
	}
	
	private void collectIntialValues(final AstNodePojo ast, final DataDictionaryPojoPrototype dde) {
		if ( ! ast.getChildren().isEmpty()) {
			final List<AstNodePojo> constantReferenceNodes = CONSTANT_COLLECTOR.allDeep(ast.getChildren().get(0));
			if ( ! constantReferenceNodes.isEmpty()) {
				for (final AstNodePojo node : constantReferenceNodes) {
					appendToExistingValue(() -> dde.initialValue.orElse(null), dde::setInitialValue, node.getLabel());
				}
			}
		}
	}

	private boolean hasDatabaseScope(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		final List<AstNodePojo> nodes = field.getIncomingRelations().stream()
				.filter(r -> r.getType() == AstRelationshipType.BINDING)
				.map(AstRelationshipPojo::getSrcNode)
				.filter(n -> AstNodeUtils.isEnclosedInSuperType(Collections.singleton(DATABASE_ACCESS_STATEMENT), n))
				.collect(Collectors.toList());
		
		if ( ! nodes.isEmpty()) {
			collectScopeAttributes(dde, nodes);
			determineDatabaseDependencies(dde, nodes.stream().map(n -> AstNodeUtils.getParentOfSuperType(n, DATABASE_ACCESS_STATEMENT))
					.filter(Optional::isPresent).map(Optional::get).collect(Collectors.toList()));
			return true;
		}
		return false;
	}

	private void collectScopeAttributes(final DataDictionaryPojoPrototype dde, final List<AstNodePojo> nodes) {
		final Set<String> tables = new HashSet<>();
		nodes.forEach(node -> {
			AstNodePojo parent = node.getParent().orElse(null);
			while ( ! (parent == null || parent.getSuperTypes().contains(DATABASE_ACCESS_STATEMENT))) {
				parent = parent.getParent().orElse(null);
			}
			
			if (parent != null) {
				final String[] queryToken = parent.getLabel().split(" ");
				for (int i = 0; i < queryToken.length; i++) {
					final String token = StringUtils.trimToEmpty(queryToken[i]);
					if (i < queryToken.length && (token.equalsIgnoreCase("FROM") || token.equalsIgnoreCase("FETCH"))) {
						tables.add(StringUtils.trimToEmpty(queryToken[i + 1]));
						break;
					}
				}
			}
		});
		dde.scopes.getNonNull().put(DataDictionaryVariableScope.SQL_DATABASE,
				Map.of(DataDictionaryVariableAttribute.SQL_DATABASE_TABLES.getKey(), tables.stream().collect(Collectors.joining(","))));
	}

	private String getFieldSection(final AstNodePojo field) {
		final Set<String> sections = Sets.newHashSet(FILE_SECTION, LINKAGE_SECTION, WORKING_STORAGE_SECTION);
		AstNodePojo parent = field.getParent().orElse(null);
		while (parent != null) {
			if (sections.contains(parent.getType())) {
				return parent.getType();
			}
			parent = parent.getParent().orElse(null);
		}
		return StringUtils.EMPTY;
	}
	
	private void collectLevelAndParentData(final AstNodePojo astNode, final DataDictionaryPojoPrototype dde) {
		final Long fieldLevel = Long.valueOf(astNode.getProperties().get(AST_PROPERTY_LEVEL).toString());
		if (fieldLevel == 88L) {
			dde.setLength(null);
			dde.setFormat("L88");
		}
		
		String parentGroup = null;
		long indentation = 0;
		final StringBuilder groupPath = new StringBuilder().append(dde.name.getNonNull());
		final AstNodePojo parentAstNode = astNode.getParent().orElse(null);
		if (parentAstNode != null && parentAstNode.getSuperTypes().contains(AstNodeUtils.FIELD_DEFINITION)) {
			parentGroup = (String) parentAstNode.getProperties().get(AST_PROPERTY_NAME);
			
			if (parentGroup == null || "null".equalsIgnoreCase(parentGroup)) {
				parentGroup = UNNAMED_MEMBER;
			}
			dde.setParentGroup(parentGroup);
			
			AstNodePojo currentNode = parentAstNode;
			do {
				final String name = (String) currentNode.getProperties().get(AST_PROPERTY_NAME);
				groupPath.insert(0, ((name == null || "null".equalsIgnoreCase(name)) ? UNNAMED_MEMBER : name) + '/');
				indentation++;
				currentNode = currentNode.getParent().orElse(null);
			} while (currentNode != null && currentNode.getSuperTypes().contains(AstNodeUtils.FIELD_DEFINITION));
		}
		
		dde.setFieldLevel(fieldLevel);
		dde.setIndentation(indentation);
		dde.setGroupPath(groupPath.toString());
	}
	
	private void calculateTargetOutputAndFieldTransformation(final AstNodePojo ast, final DataDictionaryPojoPrototype dde) {
		final List<AstNodePojo> nodes = ast.getIncomingRelations().stream()
											.filter(r -> r.getType() == AstRelationshipType.BINDING)
											.map(AstRelationshipPojo::getSrcNode)
											.collect(Collectors.toList());
		long childrenCount = ast.getChildren().stream()
				.filter(child -> child != null && child.getType().equals(CobolConstantReference.class.getSimpleName()))
				.count();
		if (childrenCount == 1) {
			handleEmptyAsts(ast, dde);
		} else {
			final List<AstNodePojo> selectedAsts = new ArrayList<>();
			nodes.forEach(node -> checkForFinalTransformations(node, selectedAsts, dde.name.getNonNull()));
			if (selectedAsts.isEmpty() && ast.getChildren().size() == 1) {
				handleEmptyAsts(ast, dde);
			} else if (selectedAsts.size() == 1) {
				final List<AstNodePojo> constantReferenceNodes = CONSTANT_COLLECTOR.allDeep(selectedAsts.get(0));
				if ( ! constantReferenceNodes.isEmpty() && selectedAsts.get(0).getType().equals(CobolMoveStmt.class.getSimpleName())
						&& FIELD_COLLECTOR.allDeep(selectedAsts.get(0)).size() == 1) {
					appendToExistingValue(() -> dde.targetOutput.orElse(null), dde::setTargetOutput, constantReferenceNodes.get(0).getLabel());
					dde.setFieldTransformation(selectedAsts.get(0).getLabel());
				} else if ( ! FIELD_COLLECTOR.allDeep(selectedAsts.get(0)).isEmpty()) {
					dde.setFieldTransformation(selectedAsts.get(0).getLabel());
				}
			}
		}
	}

	private void handleEmptyAsts(final AstNodePojo ast, final DataDictionaryPojoPrototype dde) {
		dde.setFieldTransformation(ast.getLabel());
		final List<AstNodePojo> constantReferenceNodes = CONSTANT_COLLECTOR.allDeep(ast.getChildren().get(0));
		if ( ! constantReferenceNodes.isEmpty()) {
			appendToExistingValue(() -> dde.targetOutput.orElse(null), dde::setTargetOutput, constantReferenceNodes.get(0).getLabel());
		}
	}

	private void checkForFinalTransformations(final AstNodePojo node, final List<AstNodePojo> selectedAsts, final String dataElementName) {
		AstNodePojo parent = node.getParent().orElse(null);
		AstNodePojo currentNode = node;
		while (parent != null) {
			if (CollectionUtils.containsAny(FIELD_TRANSFORMATION_SUPER_TYPES, parent.getSuperTypes())) {
				if (currentNode.getProperties().get(LEFT_OPERANDS) == null ||
						Arrays.asList(((String) currentNode.getProperties().get(LEFT_OPERANDS)).split(",")).contains(dataElementName)){
					selectedAsts.add(parent);
				}
				break;
			}
			currentNode = parent;
			parent = parent.getParent().orElse(null);
		}
	}

	private void determineDatabaseDependencies(final DataDictionaryPojoPrototype dde, final List<AstNodePojo> nodes) {
		final Set<ModuleLocation> locations = nodes.stream().map(AstNodePojo::getLocation)
				.map(AstNodeLocation::convertToSharedModuleLocation).collect(Collectors.toSet());
		if (dbDependencyLocations.isEmpty()) {
			getDatabaseReferenceLocations();
		}
		for (final ModuleLocation location : locations) {
			for (final Entry<ModuleLocation, Pair<String>> dbLocation : dbDependencyLocations.entrySet()) {
				if (location.isWithin(dbLocation.getKey())) {
					final Pair<String> nameAndAccess = dbLocation.getValue();
					if (READ.equals(nameAndAccess.getFirst())) {
						appendToExistingValue(() -> dde.sourceInput.orElse(null), dde::setSourceInput, nameAndAccess.getSecond());
					} else {
						appendToExistingValue(() -> dde.targetOutput.orElse(null), dde::setTargetOutput, nameAndAccess.getSecond());
					}
				}
			}
		}
	}
	
	private void collectFileDataSets(final AstNodePojo astNode, final DataDictionaryPojoPrototype dde) {
		if (fileDependencyProperties.isEmpty()) {
			getFileDependencies();
		}
		
		final AstNodePojo previousSibling = astNode.getPreviousSibling().orElse(null);
		if ( ! fileDependencyProperties.isEmpty() && previousSibling != null && CobolFileDefinition.class.getSimpleName().equals(previousSibling.getType())) {
			final String fdName = (String) previousSibling.getProperties().get(AST_PROPERTY_NAME);
			if (fileDependencyProperties.containsKey(fdName)) {
				final Map<String, Object> fdProperties = fileDependencyProperties.get(fdName);
				@SuppressWarnings("unchecked")
				final List<String> access = (List<String>) fdProperties.get(FILE_ACCESS_TYPE);
				
				if (access.contains(WRITE)) {
					appendToExistingValue(() -> dde.targetOutput.orElse(null), dde::setTargetOutput, (String) fdProperties.get(DATASET_NAME_PROPERTY));
				}
				
				if (access.contains(READ)) {
					appendToExistingValue(() -> dde.sourceInput.orElse(null), dde::setSourceInput, (String) fdProperties.get(DATASET_NAME_PROPERTY));
				}
			}
		}
	}

	private void getDatabaseReferenceLocations() {
		dependencyModule.ifPresent(depsModule ->
			depsModule.getOutgoings().forEach(edge -> {
				final Map<String, Object> properties = convertStringMapToObjectMap(edge.getProperties());
				if (properties.containsKey("DB_ACCESS_TYPE")) {
					final DefaultDependecyModule outgoing = (DefaultDependecyModule) edge.getOutgoingModule();
					Optional.ofNullable(outgoing.getFromLocation()).ifPresent(
							l -> dbDependencyLocations.put(l, new Pair<>(properties.get("DB_ACCESS_TYPE").toString(), outgoing.getName())));
				}
			}));
	}

	private void getFileDependencies() {
		dependencyModule.ifPresent(depsModule ->
			depsModule.getOutgoings().forEach(edge -> {
				final Map<String, Object> properties = convertStringMapToObjectMap(edge.getProperties());
				final String fdName = (String) properties.get(COBOL_FD_NAME);
				if (fdName != null) {
					properties.put(DATASET_NAME_PROPERTY, edge.getOutgoingModule().getName());
					fileDependencyProperties.put(fdName, properties);
				}
			}));
	}
}
