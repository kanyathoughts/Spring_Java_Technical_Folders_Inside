/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.call;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import innowake.mining.server.datalineage.operations.unknown.UnknownTracer;
import innowake.mining.shared.access.*;
import innowake.mining.shared.model.RelationshipDirection;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import static innowake.lib.core.lang.Assert.*;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.server.datalineage.operations.DataLineageResolver;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;

/**
 * Data Lineage Resolver component for linking {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#CALL}
 * with {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#ENTRY_POINT}.
 */
@Component
public class CallResolver implements DataLineageResolver {

	private static final Logger LOG = LoggerFactory.getLogger(CallResolver.class);

	private final ObjectMapper objectMapper;
	private final ModuleService moduleService;
	private final AstService astService;
	private final DataFlowService dataFlowService;

	@Nullable
	private DataLineageCoreService dataLineageCoreService;

	/**
	 * COnstructor
	 * @param moduleService Module data access service.
	 * @param objectMapper the {@link ObjectMapper} to deserialize JSON content from given JSON content String.
	 * @param astService ast data access service
	 */
	public CallResolver(final ModuleService moduleService, final ObjectMapper objectMapper, final AstService astService,
			final DataFlowService dataFlowService) {
		this.objectMapper = objectMapper;
		this.moduleService = moduleService;
		this.astService = astService;
		this.dataFlowService = dataFlowService;
	}


	@Override
	public boolean isSupported(final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		return proxyContainer.getType() == ProxyContainerPojo.Type.CALL || proxyContainer.getType() == ProxyContainerPojo.Type.ENTRY_POINT;
	}

	@Override
	public DataLineageResult resolve(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		if (proxyContainer.getType() == ProxyContainerPojo.Type.CALL) {
			return resolveOutgoingCall(context, module, proxyContainer);
		} else {
			return resolveIncomingCalls(context, module, proxyContainer);
		}
	}
	
	@Override
	public void setDataLineageCoreService(final DataLineageCoreService dataLineageCoreService) {
		this.dataLineageCoreService = dataLineageCoreService;
	}

	private DataLineageResult resolveOutgoingCall(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		final List<ModuleRelationshipPojo> copybooks = moduleService.findRelationship(q -> q.ofProject(context.getProjectId())
				.ofSource(module.identity())
				.withType(RelationshipType.INCLUDES));

		/* Checks if there are any Call statements present in the Copybook included in the Cobol Program */
		final List<UUID> moduleIds = new ArrayList<>();
		moduleIds.add(module.getUid());
		if ( ! copybooks.isEmpty()) {
			copybooks.stream().map(ModuleRelationshipPojo::getDstModule).forEach(moduleIds::add);
		}
		final List<ModuleRelationshipPojo> outgoingCalls = moduleService.findRelationship(q -> q.ofProject(context.getProjectId())
				.ofModulesInDirection(moduleIds, RelationshipDirection.OUT)
				.withType(RelationshipType.CALLS));

		if (outgoingCalls.isEmpty()) {
			return DataLineageResult.empty();
		}

		final String targetName = (String) proxyContainer.getProperties().get(ProxyContainerPojo.Property.TARGET_NAME.name());

		final Optional<ModuleRelationshipPojo> matchingCall = findReferenceByStatementLocation(outgoingCalls, proxyContainer.getStatementLocation().orElse(null))
				.or(() -> findReferenceByTargetName(outgoingCalls, targetName));

		if (matchingCall.isEmpty()) {
			final List<DataFlowNodePrototype> fields = new ArrayList<>();
			for (final DataFlowNodePojo node : proxyContainer.getFieldNodes()) {
				final DataFlowNodePrototype field = new DataFlowNodePrototype(node.getDataFlowId());
				field.addError(new DataFlowErrorPojoPrototype()
						.setSeverity(DataFlowErrorPojo.Severity.WARNING)
						.setText("Unable to trace further as the called Module could not be found."));
				fields.add(field);
			}
			return DataLineageResult.ofNodes(fields);
		}

		final Long toModuleId = moduleService.getModuleNid(EntityId.of(matchingCall.get().getDstModule()));

		/* doesn't support modules with multiple entry points at the moment */
		final Optional<ProxyContainerPojo> entryPoint = assertNotNull(dataLineageCoreService).getProxyContainersOfType(context, EntityId.of(toModuleId), 
				ProxyContainerPojo.Type.ENTRY_POINT)
				.stream()
				.findAny();

		if (entryPoint.isEmpty()) {
			return DataLineageResult.empty();
		}

		final List<Integer> parameterIndexes = parseParameterIndexes(proxyContainer);

		return linkProxyContainersUsingParameterIndexes(parameterIndexes, proxyContainer, entryPoint.get());
	}

	private DataLineageResult resolveIncomingCalls(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		final List<ModuleRelationshipPojo> incomingCalls = moduleService.findRelationship(q -> q.ofProject(context.getProjectId())
				.ofDestination(module.identity())
				.withType(RelationshipType.CALLS));

		if (incomingCalls.isEmpty()) {
			return DataLineageResult.empty();
		}

		final Map<Long, Collection<ProxyContainerPojo>> proxyContainersOnCallModules = new HashMap<>();

		DataLineageResult result = DataLineageResult.empty();
		for (final ModuleRelationshipPojo incomingCall : incomingCalls) {
			
			final Collection<ProxyContainerPojo> callContainers = proxyContainersOnCallModules.computeIfAbsent(moduleService.getModuleNid(
					EntityId.of(incomingCall.getSrcModule())), k ->
			assertNotNull(dataLineageCoreService).getProxyContainersOfType(context, EntityId.of(k), ProxyContainerPojo.Type.CALL));

			final Optional<ProxyContainerPojo> matchingContainer = findProxyContainerByStatementLocation(callContainers, incomingCall.getSrcLocation().orElse(null));
			if (matchingContainer.isPresent()) {
				final List<Integer> parameterIndexes = parseParameterIndexes(matchingContainer.get());
				result = result.combinedWith(linkProxyContainersUsingParameterIndexes(parameterIndexes, matchingContainer.get(), proxyContainer));
			} else {
				for (final ProxyContainerPojo container : findProxyContainersByTargetName(callContainers, module.getName())) {
					final List<Integer> parameterIndexes = parseParameterIndexes(container);
					result = result.combinedWith(linkProxyContainersUsingParameterIndexes(parameterIndexes, container, proxyContainer));
				}
			}
		}

		return result;
	}


	private Optional<ModuleRelationshipPojo> findReferenceByStatementLocation(final List<ModuleRelationshipPojo> references, @Nullable final ModuleLocation statementLocation) {
		if (statementLocation == null) {
			return Optional.empty();
		}
		return references.stream()
				.filter(ref -> validateCallOffset(statementLocation, ref))
				.findAny();
	}

	private boolean validateCallOffset(final ModuleLocation statementLocation, final ModuleRelationshipPojo ref) {
		final Optional<ModuleLocation> referenceLocation = ref.getSrcLocation();
		if (referenceLocation.isPresent()) {
			final ModuleLocation moduleLocation = referenceLocation.get();
			final Integer moduleOffset = moduleLocation.getOffset();
			final List<AstNodePojo> astNode = astService.find(q -> q.ofModule(EntityId.of(ref.getSrcModule()))
																	.withRetracedOffset(Comperator.LESSER_OR_EQUAL, moduleOffset)
																	.withRetracedEndOffset(Comperator.GREATER, moduleOffset)
																	.withSuperTypes(AstNodeUtils.STATEMENT));
			if ( ! astNode.isEmpty()) {
				final var advancedModuleLocation = assertNotNull(astNode.get(0).getLocation());
				final var offset = advancedModuleLocation.getAssembledOffset();
				final var length = advancedModuleLocation.getAssembledLength();
				return offset.isPresent() && length.isPresent() && new ModuleLocation(offset.get(), length.get()).overlapsWith(statementLocation);
			}
		}
		return false;
	}

	private Optional<ModuleRelationshipPojo> findReferenceByTargetName(final List<ModuleRelationshipPojo> references, @Nullable final String targetName) {
		if (targetName == null) {
			return Optional.empty();
		}
		return references.stream()
				.filter(ref -> targetNameComparison(targetName, ref.getDstModule()))
				.findAny();
	}
	
	private boolean targetNameComparison(final String targetName, final UUID dstModuleId) {
		final Optional<ModuleLightweightPojo> optModule = moduleService.findAnyModuleLightweight(b -> b.byUid(dstModuleId));
		if (optModule.isPresent()) {
			final ModuleLightweightPojo module = optModule.get();
			if (targetName.equals(module.getName())) {
				return true;
			}
		}
		return false;
	}

	private Optional<ProxyContainerPojo> findProxyContainerByStatementLocation(final Collection<ProxyContainerPojo> containers, @Nullable final ModuleLocation statementLocation) {
		if (statementLocation == null) {
			return Optional.empty();
		}
		return containers.stream()
				.filter(c -> {
					final var containerLocation = c.getStatementLocation();
					return containerLocation.isPresent() && containerLocation.get().overlapsWith(statementLocation);
				})
				.findAny();
	}

	private List<ProxyContainerPojo> findProxyContainersByTargetName(final Collection<ProxyContainerPojo> containers, @Nullable final String targetName) {
		if (targetName == null) {
			return Collections.emptyList();
		}

		return containers.stream()
				.filter(c -> {
					final String callTargetName = (String) c.getProperties().get(ProxyContainerPojo.Property.TARGET_NAME.name());
					return callTargetName != null && callTargetName.equals(targetName);
				})
				.collect(Collectors.toList());
	}

	/**
	 * Parses the {@link ProxyContainerPojo.Property#CALL_PARAMETER_INDEX} property of the given proxy container, if present. Otherwise, just returns
	 * a list of ascending indexes, i.e. {@code [0, 1, 2, ... n]} where n is the number of fields in the proxy container.
	 * @param callContainer the container containing the property to parse
	 * @return the parsed parameter index list or a dummy list if the parameter was not present or {@code null} only if an error occurred during parsing
	 */
	private List<Integer> parseParameterIndexes(final ProxyContainerPojo callContainer) {
		List<Integer> parameterIndexes;
		try {
			final String parameterIndexProp = (String) callContainer.getProperties().get(ProxyContainerPojo.Property.CALL_PARAMETER_INDEX.name());
			if (parameterIndexProp == null) {
				/* assume default parameter indexes */
				parameterIndexes = IntStream.range(0, callContainer.getFieldNodesUids().size()).boxed().collect(Collectors.toList());
			} else {
				parameterIndexes = objectMapper.readValue(parameterIndexProp, new TypeReference<>() {});
			}
		} catch (final JsonProcessingException e) {
			LOG.error("While resolving CALL " + callContainer + ": unable to parse parameter indexes. Using defaults", e);
			parameterIndexes = IntStream.range(0, callContainer.getFieldNodesUids().size()).boxed().collect(Collectors.toList());
		}
		return parameterIndexes;
	}

	private DataLineageResult linkProxyContainersUsingParameterIndexes(final List<Integer> parameterIndexes, final ProxyContainerPojo callContainer,
			final ProxyContainerPojo entryPointContainer) {
		if (entryPointContainer.getFieldNodesUids().size() == 1
				&& entryPointContainer.getFieldNodes().get(0).getName().equals(UnknownTracer.PLACEHOLDER_FIELD_NAME)) {
			return connectEverythingToPlaceholder(callContainer, entryPointContainer);
		}

		final List<DataFlowNodePrototype> changes = new ArrayList<>();
		for (int i = 0; i < Math.min(parameterIndexes.size(), callContainer.getFieldNodesUids().size()); i++) {
			final int parameterIndex = parameterIndexes.get(i);
			if (parameterIndex >= entryPointContainer.getFieldNodesUids().size()) {
				LOG.warn(() -> "While linking proxy containers of module " + callContainer.getModuleId() + " to " + entryPointContainer.getModuleId() + ": "
						+ " parameter index " + parameterIndex + " is out of range. Entry point only has " + entryPointContainer.getFieldNodesUids().size() + " fields.");
				/* the invalid / excess parameter is ignored */
				continue;
			}
			final var callContainerNode = callContainer.getFieldNodes().get(i);
			final var entryPointContainerNode = entryPointContainer.getFieldNodes().get(parameterIndex);
			changes.add(new DataFlowNodePrototype(callContainerNode.getDataFlowId()).addDataFlow(
					//TODO: would have to determine direction of data flow here - it's not always just from caller to callee
					DataFlow.toField(entryPointContainerNode.getDataFlowId())
			));
		}

		return DataLineageResult.ofNodes(changes);
	}

	private DataLineageResult connectEverythingToPlaceholder(final ProxyContainerPojo callContainer, final ProxyContainerPojo entryPointContainer) {
		final List<DataFlowNodePrototype> changes = new ArrayList<>();
		final DataFlowNodePojo placeholderField = dataFlowService.get(entryPointContainer.getFieldNodesUids().get(0));
		/* connect everything to placeholder field */
		final var callFields = dataFlowService.find(q -> q.byIds(callContainer.getFieldNodesUids()));
		for (final DataFlowNodePojo field : callFields) {
			/* for now, data flow is added in both direction - should determine actual direction here */
			changes.add(new DataFlowNodePrototype(field.getDataFlowId()).addDataFlow(DataFlow.toField(placeholderField)));
			changes.add(new DataFlowNodePrototype(placeholderField.getDataFlowId()).addDataFlow(DataFlow.toField(field)));
		}
		return DataLineageResult.ofNodes(changes);
	}
}
