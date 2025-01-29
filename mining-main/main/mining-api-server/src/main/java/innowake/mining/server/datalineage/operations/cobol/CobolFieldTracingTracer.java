/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.cobol;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.profile.DefaultProfiler;
import innowake.lib.job.api.OperationCanceledException;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.fieldtracing.service.FieldTracingModelImportService;
import innowake.mining.server.datalineage.fieldtracing.service.FieldTracingService;
import innowake.mining.server.datalineage.operations.DataLineageTracer;
import innowake.mining.server.datalineage.operations.file.ResourceFileTracer;
import innowake.mining.server.datalineage.persistence.DataLineagePersistenceService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.mining.shared.model.datalineage.ProxyContainerPrototype;
import innowake.ndt.fieldtracing.model.Model;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * Data Lineage Tracer component tracing the internal data flow of a Cobol Module and creating Proxy Containers for input and output statements.
 * <p>
 * This implementation uses the field tracer from {@code ndt-fieldtracing-core}.
 * @see FieldTracingService
 * @see FieldTracingModelImportService
 */
@Component
@ConditionalOnProperty(name = "configuration.enable-new-cobol-data-lineage", havingValue = "false", matchIfMissing = true)
public class CobolFieldTracingTracer implements DataLineageTracer {

	private static final Logger LOG = LoggerFactory.getLogger(CobolFieldTracingTracer.class);

	private static final Set<String> UNSUPPORTED_PARENT_TYPES = Set.of("CobolPerformStmt", "CobolFunctionReference");

	private final FieldTracingService fieldTracingService;
	private final FieldTracingModelImportService modelImportService;
	private final DataLineagePersistenceService persistenceService;
	private final AstService astService;
	
	/* current used for discovery file accesses - would be better to do this from AST */
	private final ModuleService moduleService;

	private final ExecutorService executor = Executors.newCachedThreadPool();

	/**
	 * Constructor
	 * @param fieldTracingService service to collect the correct Tracer and execute tracing
	 * @param modelImportService to convert a field tracing {@link Model} into {@link DataLineageResult}
	 * @param persistenceService service allowing to persist {@link DataLineageResult} into the db
	 * @param astService Repository for {@link innowake.mining.shared.entities.ast.AstNodePojo} entity
	 * @param moduleService Module data access service.
	 */
	public CobolFieldTracingTracer(final FieldTracingService fieldTracingService,
								   final FieldTracingModelImportService modelImportService,
								   final DataLineagePersistenceService persistenceService,
								   final AstService astService,
								   final ModuleService moduleService) {
		this.fieldTracingService = fieldTracingService;
		this.modelImportService = modelImportService;
		this.persistenceService = persistenceService;
		this.astService = astService;
		this.moduleService = moduleService;
	}

	@Override
	public boolean isSupported(final ModuleLightweightPojo module) {
		return module.getTechnology() == Technology.COBOL && module.getType() == Type.PROGRAM;
	}

	@Override
	public DataLineageResult trace(final DataLineageContext context, final ModuleLightweightPojo module) {
		persistenceService.ensureAsts(context, module.identity());
		/* Find all FieldDefinitions in a given module via the SuperType */
		final List<AstNodePojo> fieldReferences = astService.find(q -> q.ofModule(module.identity()).withSuperTypes(AstNodeUtils.FIELD_REFERENCE).sortRetracedLocation());

		final Map<Integer, AstNodePojo> referencedFieldsByOffset = new HashMap<>();
		final List<AstNodePojo> distinctFieldReferences = new ArrayList<>();

		for (final AstNodePojo fieldReference : fieldReferences) {
			final List<AstNodePojo> referencedFields = fieldReference.getOutgoingRelations().stream()
					.filter(r -> r.getType() == AstRelationshipType.BINDING && "definedBy".equals(r.getLabel().orElse(null)))
					.map(AstRelationshipPojo::getDstNode)
					.toList();
			boolean distinct = false;
			for (final AstNodePojo referencedField : referencedFields) {
				final AstNodeLocation referencedFieldLocation = referencedField.getLocation();
				final Integer offset = referencedFieldLocation.getAssembledOffset().orElse(null);
				distinct |= ! referencedFieldsByOffset.containsKey(offset);
				referencedFieldsByOffset.put(offset, referencedField);
			}
			if (distinct) {
				distinctFieldReferences.add(fieldReference);
			}
		}

		/* Trace each field reference */
		final DataLineageResult dataLineageResult = traceReferencedFields(context, module, distinctFieldReferences);
		/* tracing the fields does not create the FILE_ACCESS proxy containers properly, because in order to set the correct properties
		 * like FILE_ACCESS_FILE_NAME on the containers, we need the dependency information. So we need to invoke the trace for the file
		 * access containers separately:
		 */
		final DataLineageResult fileAccesses = discoverFileAccesses(module.identity());
		return dataLineageResult.combinedWith(fileAccesses);
	}
	
	@Override
	public DataLineageResult discoverProxyContainersOfType(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo.Type type) {
		LOG.debug("discoverProxyContainersOfType " + type + " in module " + module.getId());
		if (type == ProxyContainerPojo.Type.DATABASE_ACCESS) {
			return traceDbAccesses(context, module.identity());
		} else if (type == ProxyContainerPojo.Type.ENTRY_POINT) {
			return traceEntryPoint(context, module.identity());
		} else if (type == ProxyContainerPojo.Type.CALL) {
			return traceCalls(context, module.identity());
		} else if (type == ProxyContainerPojo.Type.FILE_ACCESS) {
			return discoverFileAccesses(module.identity());
		} else {
			LOG.debug("discoverProxyContainersOfType " + type + " is not supported, so falling back to traceAll()");
			return trace(context, module);
		}
	}
	
	public DataLineageResult traceField(final DataLineageContext context, final EntityId moduleId, final ModuleLocation location) {
		LOG.debug("traceField in module " + moduleId + " offset=" + location.getOffset());
		try {
			context.getProgressMonitor().checkCanceled();
			final Model<ModuleLightweightPojo> model = fieldTracingService.traceField(context, moduleId, location);
			return modelImportService.importModel(context, model);
		} catch (final Exception e) {
			LOG.error("DataFlowNodePojo at location = " + location + " and moduleId = " + moduleId + " does not exist", e);
		} finally {
			if (DefaultProfiler.isProfilingEnabled()) {
				ProfilingFactory.getProfilingSession().flushCurrentThread();
			}
		}
		return DataLineageResult.empty();
	}

	private DataLineageResult traceReferencedFields(final DataLineageContext context, final ModuleLightweightPojo module, final List<AstNodePojo> distinctFieldReferences) {
		final List<Future<DataLineageResult>> futures = new ArrayList<>(distinctFieldReferences.size());
		try {
			submitTraceTasks(context, module, distinctFieldReferences, futures);
		} catch (final OperationCanceledException e) {
			/* await the currently running tasks - prevents the Data Flow Graph Query job to be marked as "stopped" while the field tracer is
			 * still running in the background */
			awaitResults(module, futures);
			throw e;
		}
		final List<DataLineageResult> results = awaitResults(module, futures);

		return DataLineageResult.combine(results);
	}

	private void submitTraceTasks(final DataLineageContext context, final ModuleLightweightPojo module, final List<AstNodePojo> distinctFieldReferences,
			final List<Future<DataLineageResult>> futures) {

		int fieldCount = 1;
		boolean first = true;
		for (final AstNodePojo fr : distinctFieldReferences) {
			final int currentFieldCount = fieldCount;
			final Optional<AstNodePojo> parent = fr.getParent();
			if (parent.isPresent() && ! UNSUPPORTED_PARENT_TYPES.contains(parent.get().getType())) {
				final int offset = getOffset(fr);

				LOG.debug(() -> "traceAll in module " + module.getId() + ": tracing field reference " + currentFieldCount + " of " + distinctFieldReferences.size() +
						": " + fr.getLabel() + " @ " + offset);

				context.getProgressMonitor().setStepDescription(
						String.format("Tracing fields in %s (%d/%d)", module.getName(), fieldCount, distinctFieldReferences.size()));
				if (first) {
					/* trace the first field "synchronously" and only use parallelization afterward, so that we avoid parsing the module multiple times
					 * (after tracing the first field, the parse result is cached in the DataLineageContext) */
					futures.add(CompletableFuture.completedFuture(
							traceField(context, fr.getModule(), new ModuleLocation(offset, 0))));
					first = false;
				} else {
					futures.add(executor.submit(() -> traceField(context, fr.getModule(), new ModuleLocation(offset, 0))));
				}

			}
			fieldCount++;
		}
	}

	private int getOffset(final AstNodePojo fr) {
		final AstNodeLocation locV2 = fr.getLocation();
		if (locV2 == null) {
			throw new IllegalStateException(
					"While trying to trace " + fr + ", a field was found with an AstNodePojo which does not contain an advancedModuleLocation");
		}

		final Optional<Integer> offset = fr.getIncludedModule().isEmpty() ? locV2.getRetracedOffset() : locV2.getAssembledOffset();
		if (offset.isEmpty()) {
			throw new IllegalStateException("While trying to trace " + fr
					+ ", a field was found with an AstNodePojo which contains an advancedModuleLocation " + "with a null offset");
		}
		return offset.get();
	}

	private List<DataLineageResult> awaitResults(final ModuleLightweightPojo module, final List<Future<DataLineageResult>> futures) {
		final List<DataLineageResult> results = new ArrayList<>(futures.size());
		boolean canceled = false;
		for (final Future<DataLineageResult> future : futures) {
			try {
				results.add(future.get());
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Trace interrupted while tracing module " + module.getId(), e);
			} catch (final ExecutionException e) {
				if (e.getCause() instanceof  OperationCanceledException) {
					canceled = true;
					/* we continue to await the remaining futures and throw OperationCanceledException at the end */
				} else {
					throw new IllegalStateException("Trace interrupted while tracing module " + module.getId(), e);
				}
			}
		}
		if (canceled) {
			throw new OperationCanceledException();
		}
		return results;
	}

	private DataLineageResult traceDbAccesses(final DataLineageContext context, final EntityId moduleId) {
		return traceFieldsOfType(context, moduleId, true, AstNodeUtils.DATABASE_ACCESS_STATEMENT);
	}

	private DataLineageResult traceEntryPoint(final DataLineageContext context, final EntityId moduleId) {
		return traceFieldsOfType(context, moduleId, false, "ProcedureDivision");
	}

	private DataLineageResult traceCalls(final DataLineageContext context, final EntityId moduleId) {
		return DataLineageResult.combine(
				traceFieldsOfType(context, moduleId, false, "CobolCallStmt"),
				traceFieldsOfType(context, moduleId, false, "ExecSqlCall")
		);
	}

	private DataLineageResult traceFieldsOfType(final DataLineageContext context, final EntityId moduleId, final boolean superType, final String... types) {
		DataLineageResult result = DataLineageResult.empty();
		persistenceService.ensureAsts(context, moduleId);
		/* First get the root node of the module */
		final AstNodePojo rootNode = astService.findOne(q -> q.ofModule(moduleId).ofParent(null))
				.orElseThrow(() -> new IllegalStateException("Unable to resolve the AST of module " + moduleId + " while trying to discover Proxy Container"));

		/* Then get all AstNodes with given super types */
		final List<AstNodePojo> statements;
		if (superType) {
			statements = AstNodeUtils.getChildrenDeep(rootNode, types);
		} else {
			statements = AstNodeUtils.getChildrenDeepWithType(rootNode, types);
		}

		final List<AstNodePojo> fieldReferences = new ArrayList<>();

		/* Then get all children that are referencing a field */
		for (final AstNodePojo accessStmtAst : statements) {
			fieldReferences.addAll(AstNodeUtils.getChildrenDeep(accessStmtAst, AstNodeUtils.FIELD_REFERENCE));
		}

		/* Lastly start a trace for all the references found */
		for (final AstNodePojo fieldRefAst : fieldReferences) {
			final EntityId fieldModuleId = fieldRefAst.getModule();
			final Optional<Integer> retracedOffset = fieldRefAst.getLocation().getRetracedOffset();
			if (retracedOffset.isEmpty()) {
				continue;
			}
			final var tempResult = traceField(context, fieldModuleId, new ModuleLocation(retracedOffset.get(),
					Integer.valueOf(0)));
			result = result.combinedWith(tempResult);
		}

		return result;
	}

	private DataLineageResult discoverFileAccesses(final EntityId moduleId) {
		final List<ModuleRelationshipPojo> readsWrites = moduleService.findRelationship(q -> q.ofSource(moduleId)
				.withType(RelationshipType.ACCESSES));
		
		if (readsWrites.isEmpty()) {
			return DataLineageResult.empty();
		}

		DataLineageResult result = DataLineageResult.empty();
		for (final ModuleRelationshipPojo rw : readsWrites) {
			final var targetModuleOpt = moduleService.findAnyModuleLightweight(q -> q.byUid(rw.getDstModule()));
			if (targetModuleOpt.isPresent()) {
				final var targetModule = targetModuleOpt.get();
				final Technology technology = targetModule.getTechnology();
				final Type type = targetModule.getType();
				if (technology.equals(Technology.RESOURCE) && (type.equals(Type.FILE) || type.equals(Type.VSAM_FILE) || type.equals(Type.GDG_FILE))) {
					result = result.combinedWith(createProxyContainerForFileDescriptors(rw, targetModule, moduleId));
				}
			}
		}

		return result;
	}

	private DataLineageResult createProxyContainerForFileDescriptors(final ModuleRelationshipPojo rw, final ModuleBasePojo targetModule, final EntityId moduleId) {
		final Optional<Map<String, Object>> properties = rw.getProperties();
		if (properties.isEmpty()) {
			return DataLineageResult.empty();
		}
		String fdName = (String) properties.get().get(ModelAttributeKey.COBOL_FD_NAME.name());
		String aliasName = (String) properties.get().get(ModelAttributeKey.FILE_ALIAS.name());
		if (fdName != null) {
			fdName = fdName.replace("\"","");
		}
		if (aliasName != null) {
			aliasName = aliasName.replace("\"","");
		}
		final ProxyContainerPrototype fileDescriptorContainer = createFileDescriptorContainer(fdName, aliasName, moduleId);
		if (fileDescriptorContainer == null) {
			return DataLineageResult.empty();
		}
		fileDescriptorContainer.setFields(List.of(new DataFlowNodePrototype(DataFlowId.forProxyField(fileDescriptorContainer.getDataFlowId(), 0))
				.setName(ResourceFileTracer.PLACEHOLDER_FIELD_NAME)));
		final Map<ProxyContainerPojo.Property, Object> proxyContainerProperties = new EnumMap<>(ProxyContainerPojo.Property.class);
		proxyContainerProperties.put(ProxyContainerPojo.Property.FILE_ACCESS_COBOL_FD_NAME, fdName);
		proxyContainerProperties.put(ProxyContainerPojo.Property.FILE_ACCESS_FILE_ALIAS, aliasName);
		proxyContainerProperties.put(ProxyContainerPojo.Property.FILE_ACCESS_FILE_NAME, targetModule.getName());
		fileDescriptorContainer.setProperties(proxyContainerProperties);

		return DataLineageResult.ofProxyContainers(List.of(fileDescriptorContainer));
	}

	@Nullable
	private ProxyContainerPrototype createFileDescriptorContainer(final String fdName, final String aliasName, final EntityId moduleId) {
		final ProxyContainerPrototype container;
		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(moduleId).withLabel("FD %"));
		container = astNodes.stream()
				.filter(astNode -> {
					final String label = astNode.getLabel();
					return label != null && (label.contains(aliasName) || label.contains(fdName));})
				.map(AstNodePojo::getLocation).filter(Objects::nonNull)
				.map(AstNodeLocation::getAssembledOffset)
				.filter(Optional::isPresent).findFirst()
				.map(offset -> new ProxyContainerPrototype(DataFlowId.forProxyContainer(
						moduleService.getModuleNid(moduleId), ProxyContainerPojo.Type.FILE_ACCESS, new ModuleLocation(offset.get().intValue(), 0)))
						.setType(ProxyContainerPojo.Type.FILE_ACCESS)
						.setStatementLocation(new ModuleLocation(offset.get().intValue(), 0)))
				.orElse(null);
		return container;
	}

}
