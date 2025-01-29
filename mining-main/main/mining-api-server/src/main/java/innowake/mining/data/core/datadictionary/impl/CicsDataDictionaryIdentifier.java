/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.datadictionary.impl;

import java.util.HashMap;
import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.DataDictionaryVariableAttribute;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;

/**
 * Identifies {@linkplain DataDictionaryPojoPrototype} for CICS Modules.
 */
public class CicsDataDictionaryIdentifier extends DataDictionaryIdentifier {

	private static final String BMS_FIELD_FORMAT = "3270";
	private final AstNodePojo rootNode;
	private final EntityId moduleId;
	
	/**
	 * The constructor.
	 * 
	 * @param core the {@link MiningDataCoreService}
	 * @param moduleId the module ID
	 * @param rootNode the root ast node of the module
	 */
	public CicsDataDictionaryIdentifier(final MiningDataCoreService core, final EntityId moduleId, final AstNodePojo rootNode) {
		super(core);
		this.rootNode = rootNode;
		this.moduleId = moduleId;
	}

	@Override
	protected void assignScopesAccessTypes(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		final Map<String, Object> nodeProperties = field.getProperties();
		final Map<String, String> scopeAttributes = new HashMap<>();
		if (nodeProperties.containsKey("ATTRB")) {
			final String attrValue = (String) nodeProperties.get("ATTRB");
			if (attrValue.contains("ASKIP") || attrValue.contains("PROT")) {
				scopeAttributes.put("accessType", "OUTPUT");
			}
			else {
				scopeAttributes.put("accessType", "INPUT");
			}
			
			addScopeAttributes(dde, scopeAttributes);
		}
	}
	
	@Override
	protected void collectLanguageSpecificProperties(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		dde.setFormat(BMS_FIELD_FORMAT);
	}

	@Override
	protected void assignScopes(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		collectScopeAttributes(field, dde);
		dde.scopes.getNonNull().putIfAbsent(DataDictionaryVariableScope.CICS_UI, Map.of());
	}

	@Override
	public void setDefinedLocation(final @Nullable EntityId calleeModuleId, final DataDictionaryPojoPrototype dde, final AstNodePojo field) {
		dde.setDefinedLocation(DefinedLocation.PROGRAM);
	}

	@Override
	protected AstNodePojo getRootNode() {
		return rootNode;
	}

	@Override
	protected EntityId getModuleId() {
		return moduleId;
	}

	private void collectScopeAttributes(final AstNodePojo field, final DataDictionaryPojoPrototype dde) {
		final Map<String, String> scopeAttributes = new HashMap<>();
		final AstNodePojo map = field.getParent().orElseThrow(
				() -> new IllegalStateException("Could not find MapDefinition for " + dde.name.getNonNull()));
		final AstNodePojo mapSet = map.getParent().orElseThrow(
				() -> new IllegalStateException("Could not find MapSetDefinition for " + dde.name.getNonNull()));
		final String mapSetLabel = mapSet.getLabel();
		final String mapLabel = map.getLabel();
		scopeAttributes.put(DataDictionaryVariableAttribute.CICS_UI_MAPSET.getKey(), mapSetLabel.substring(0, mapSetLabel.indexOf(' ')));
		scopeAttributes.put(DataDictionaryVariableAttribute.CICS_UI_MAPNAME.getKey(), mapLabel.substring(0, mapLabel.indexOf(' ')));
		addScopeAttributes(dde, scopeAttributes);
	}

	private static void addScopeAttributes(final DataDictionaryPojoPrototype dde, final Map<String, String> scopeAttributes) {
		final Map<String, String> map2 = dde.scopes.getNonNull().get(DataDictionaryVariableScope.CICS_UI);
		if (map2 == null) {
			dde.scopes.getNonNull().put(DataDictionaryVariableScope.CICS_UI, scopeAttributes);
		} else {
			map2.putAll(scopeAttributes);
		}
	}

}
