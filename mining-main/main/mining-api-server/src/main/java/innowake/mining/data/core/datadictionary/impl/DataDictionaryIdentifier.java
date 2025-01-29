/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.datadictionary.impl;

import static innowake.mining.data.core.api.AstNodeUtils.CALL_EXTERNAL_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.CALL_INTERNAL_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.CALL_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_DEFINITION;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.hasAnySuperType;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import com.google.common.collect.Sets;
import com.google.gson.Gson;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AstNodeCollectingTraverser;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.core.parsing.ast.model.statement.CallStatement;

/**
 * Template class for {@linkplain DataDictionaryPojoPrototype} identification.
 */
public abstract class DataDictionaryIdentifier {
	
	private static final Set<String> CALL_STATEMENT_TYPES = Sets.newHashSet(CallStatement.class.getSimpleName(), CobolCallStmt.class.getSimpleName(), 
			"CallStmt");


	private static final String ACCESS_TYPE = "accessType";
	private static final String OUTPUT = "OUTPUT";
	private static final String USING = "USING";
	private final Gson gson = new Gson();
	protected final MiningDataCoreService core;

	/**
	 * Constructor
	 * 
	 * @param core the {@linkplain MiningDataCoreService}
	 */
	protected DataDictionaryIdentifier(final MiningDataCoreService core) {
		this.core = core;
	}
	
	/**
	 * Identify all {@linkplain DataDictionaryPojoPrototype} present in a {@linkplain Module}
	 *
	 * @return the {@linkplain List} of {@linkplain DataDictionaryPojoPrototype}
	 */
	public List<DataDictionaryPojoPrototype> identify() {
		return createDataDictionaries(getDataFields());
	}

	/**
	 * Returns all the Data Fields present in a {@linkplain Module}
	 *
	 * @return the {@linkplain List} of Data Fields
	 */
	protected List<AstNodePojo> getDataFields() {
		final var collector =  new AstNodeCollectingTraverser(node -> node.getSuperTypes().contains(FIELD_DEFINITION));
		collector.traverse(getRootNode());
		return collector.collect();
	}

	/**
	 * Create {@linkplain DataDictionaryPojoPrototype} from a {@linkplain List} of Data Fields
	 *
	 * @param dataFields the {@linkplain List} of Data Fields
	 * @return the {@linkplain List} of {@linkplain DataDictionaryPojoPrototype}
	 */
	protected List<DataDictionaryPojoPrototype> createDataDictionaries(final List<AstNodePojo> dataFields) {
		final List<DataDictionaryPojoPrototype> ddes = new ArrayList<>();
		final Map<String, Map<DataDictionaryVariableScope, Map<String, String>>> ddeAndAccessType = new HashMap<>();

		dataFields.forEach(field -> {
			final String name = AstNodeUtils.getFieldDefinitionNameFromReference(field);
			if ( ! name.isEmpty()) {
				final DataDictionaryPojoPrototype dde = new DataDictionaryPojoPrototype()
						.setName(name)
						.setDescription(name)
						.setLocation(getModuleLocation(field, name))
						.setModule(field.getIncludedModule().orElseGet(this::getModuleId))
						.setIsCandidate(true)
						.setIsReferenced(field.getIncomingRelations().stream().filter(r -> r.getType() == AstRelationshipType.BINDING).count() != 0)
						.setState(WorkingState.CANDIDATE)
						.setScopes(new EnumMap<>(DataDictionaryVariableScope.class));
				
				assignScopes(field, dde);
				assignScopesAccessTypes(field, dde);
				Map<DataDictionaryVariableScope, Map<String, String>> scopes = dde.scopes.getNonNull();
				if ( ! scopes.isEmpty()) {
					final Map<DataDictionaryVariableScope, Map<String, String>> updatedScopes = updateScopeForCallStmt(name, scopes, dde);
					if ( ! updatedScopes.isEmpty()) {
						scopes = updatedScopes;
					}
					for (final var scopeAttributeValue: scopes.values()) {
						if (scopeAttributeValue != null && scopeAttributeValue.containsKey(ACCESS_TYPE)) {
							ddeAndAccessType.put(name, scopes);
						}
					}
				}
				collectFormat(field, dde);
				collectLanguageSpecificProperties(field, dde);
				setDefinedLocation(field.getIncludedModule().orElse(null), dde, field);

				ddes.add(dde);
			}
		});
		return addAccessToChildScopes(ddes, dataFields, ddeAndAccessType);
	}

	@SuppressWarnings("null")
	private Map<DataDictionaryVariableScope, Map<String, String>> updateScopeForCallStmt(final String name,
			final Map<DataDictionaryVariableScope, Map<String, String>> scopes, final DataDictionaryPojoPrototype dde) {
		final Map<DataDictionaryVariableScope, Map<String, String>> updatedScope = new HashMap<>();
		fieldRefsFromCallStatements().forEach(fieldRef -> {
			final String fieldName = AstNodeUtils.getFieldDefinitionNameFromReference(fieldRef);
			if (fieldName != null && fieldName.equals(name) && scopes.containsKey(DataDictionaryVariableScope.OTHER)) {
				final Map<String, String> accessTypeMap = new HashMap<>();
				accessTypeMap.put(ACCESS_TYPE, OUTPUT);
				updatedScope.put(DataDictionaryVariableScope.PARAMETER, accessTypeMap);
				dde.setScopes(updatedScope);
			}
		});
		return updatedScope;
	}
	
	@SuppressWarnings("null")
	private List<AstNodePojo> fieldRefsFromCallStatements() {
		final List<AstNodePojo> fieldRefs = new ArrayList<>();
		getCallFields().forEach(callNode -> {
			String label = callNode.getLabel();
			if (label != null) {
				final int index = label.toUpperCase().indexOf(USING);
				if (index != -1) {
					label = label.substring(index + USING.length()).trim();
				}
			}
			
			final var collector =  new AstNodeCollectingTraverser(node -> node.getSuperTypes().contains(FIELD_REFERENCE));
			collector.traverse(getRootNode());
			final List<AstNodePojo> callChildren = collector.collect();
			if (callChildren != null && ! callChildren.isEmpty() && label != null && ! label.isEmpty()) {
				final String finalLabel = label;
				callChildren.stream().filter(child -> finalLabel.contains(child.getLabel())).forEach(fieldRefs::add);
			}
		});
		return fieldRefs;
	}
	
	/**
	 * Returns all the {@linkplain AstNodePojo Call Fields} present in a {@linkplain Module}
	 *
	 * @return the {@linkplain List} of {@linkplain AstNodePojo Call Fields}
	 */
	private List<AstNodePojo> getCallFields() {
		final var collector =  new AstNodeCollectingTraverser(node -> hasAnySuperType(node, STATEMENT, CALL_STATEMENT, CALL_EXTERNAL_STATEMENT, CALL_INTERNAL_STATEMENT));
		collector.traverse(getRootNode());

		return collector.collect().stream().filter(node -> CALL_STATEMENT_TYPES.contains(node.getType()))
				.collect(Collectors.toList());
	}

	private List<DataDictionaryPojoPrototype> addAccessToChildScopes(final List<DataDictionaryPojoPrototype> ddes, final List<AstNodePojo> dataFields, final Map<String, Map<DataDictionaryVariableScope, Map<String, String>>> ddeAndAccessType) {
		final Set<String> ddesWithAccessTypes = ddeAndAccessType.keySet();
		final Map<String, Map<DataDictionaryVariableScope, Map<String, String>>> childrenAndTheirScopeAttributes = new HashMap<>();
		dataFields.forEach(field -> {
			final String fieldName = AstNodeUtils.getFieldDefinitionNameFromReference(field);
			if(ddesWithAccessTypes.contains(fieldName)) {
				final List<AstNodePojo> childrenNodes = field.getChildren();
				childrenNodes.forEach(node -> {
					final String childsName = (String) node.getProperties().get("name");
					if(childsName != null) {
						childrenAndTheirScopeAttributes.put(childsName, ddeAndAccessType.get(fieldName));
					}
				});
			}
		});
		ddes.forEach(dde -> {
			if (childrenAndTheirScopeAttributes.containsKey(dde.name.get())) {
				dde.setScopes(childrenAndTheirScopeAttributes.get(dde.name.get()));
			}
		});
		return ddes;
	}

	/**
	 * Collect Format and Size data for the {@linkplain DataDictionaryPojoPrototype}
	 *
	 * @param field the data field AST node
	 * @param dde the {@linkplain DataDictionaryPojoPrototype} object
	 */
	private void collectFormat(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		final Map<String, Object> properties = field.getProperties();
		dde.setFormat((String) properties.get("languageType"));
		Optional.ofNullable(properties.get("byteLength"))
					.map(i -> i instanceof Number ii ? ii : Long.parseLong((String) i))
					.ifPresent(i -> dde.setLength(i.longValue()));
	}

	/**
	 * Collect advanced data for the {@linkplain DataDictionaryPojoPrototype}
	 *
	 * @param field the data field AST node
	 * @param dde the {@linkplain DataDictionaryPojoPrototype} object
	 */
	protected abstract void collectLanguageSpecificProperties(AstNodePojo field, DataDictionaryPojoPrototype dde);

	/**
	 * Assign {@linkplain DataDictionaryVariableScope Scopes} to the {@linkplain DataDictionaryPojoPrototype}
	 *
	 * @param field the data field AST node
	 * @param dde the {@linkplain DataDictionaryPojoPrototype} object
	 */
	protected abstract void assignScopes(AstNodePojo field, DataDictionaryPojoPrototype dde);

	/**
	 * Assign {@linkplain DataDictionaryVariableScope Scopes} with access type attributes to the {@linkplain DataDictionaryPojoPrototype}
	 *
	 * @param field the data field AST node
	 * @param dde the {@linkplain DataDictionaryPojoPrototype} object
	 */
	protected abstract void assignScopesAccessTypes(final AstNodePojo field, final DataDictionaryPojoPrototype dde);

	/**
	 * Sets the {@linkplain DefinedLocation} for the {@linkplain DataDictionaryPojoPrototype}
	 *
	 * @param calleeModuleId the id of the Callee module if any
	 * @param dde the {@linkplain DataDictionaryPojoPrototype}
	 * @param field the data field AST node
	 */
	public abstract void setDefinedLocation(@Nullable EntityId calleeModuleId, DataDictionaryPojoPrototype dde, AstNodePojo field);

	/**
	 * @return the root AST node for the Module
	 */
	protected abstract AstNodePojo getRootNode();

	/**
	 * @return the {@linkplain Module} ID
	 */
	protected abstract EntityId getModuleId();

	/**
	 * This method helps convert a {@code Map<String, String>} to a {@code Map<String, Object>}.
	 *
	 * @param stringMap the {@code Map<String, String>}
	 * @return the {@code Map<String, Object>}
	 */
	@SuppressWarnings("unchecked")
	protected Map<String, Object> convertStringMapToObjectMap(final Map<String, Object> stringMap) {
		final String formattedJson = gson.toJson(stringMap).replace("\\\"", "").replace("\"[", "[").replace("]\"", "]");
		return gson.fromJson(formattedJson, HashMap.class);
	}

	/**
	 * Appends the new value to the existing value, or keeps the old value if both are same.
	 *
	 * @param getter reference to the getter method
	 * @param setter reference to the setter method
	 * @param newValue the new value
	 */
	protected void appendToExistingValue(final Supplier<String> getter, final Consumer<String> setter, final String newValue) {
		final String oldValue = StringUtils.trimToEmpty(getter.get());
		if (oldValue.isEmpty()) {
			setter.accept(newValue);
		} else {
			if (oldValue.equals(newValue)) {
				return;
			}
			setter.accept(oldValue + ";" + newValue);
		}
	}

	private ModuleLocation getModuleLocation(final AstNodePojo field, final String name) {
		final int offset = field.getLocation().getRetracedOffset().orElse(0);
		final Optional<Object> nameOffset = Optional.ofNullable(field.getProperties().get("nameOffset"));
		int index = nameOffset.map(i -> i instanceof Integer ii ? ii : Integer.parseInt((String) i))
								.orElseGet(() -> field.getLabel().indexOf(name));

		if (index < 0) {
			index = 0;
		}
		final Optional<Object> nameLength = Optional.ofNullable(field.getProperties().get("nameLength"));
		final int length = nameLength.map(i -> i instanceof Integer ii ? ii : Integer.parseInt((String) i))
							.orElseGet(name::length);
		return new ModuleLocation(offset + index, length);
	}
}
