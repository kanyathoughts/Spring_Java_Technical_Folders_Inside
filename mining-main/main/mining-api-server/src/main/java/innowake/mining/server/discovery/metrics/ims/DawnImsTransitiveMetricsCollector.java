/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.ims;

import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.discovery.metrics.MetricsContributor.Phase;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeComparator;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JclJobContributor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.entities.*;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstNode;
import org.apache.commons.codec.binary.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;


import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.CALL_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_OPERATION;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_SEGMENTS;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.IMS_SSA;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.STATEMENT;

/**
 * Language independent collector that collects transitive IMS dependencies between jobs, programs and involved IMS resources. This can only be executed
 * as part of the {@link Phase#TRANSITIVE_METRICS} phase, after the whole dependency chains have been resolved.
 * Language specific implementations that want to make use of this must implement {@link ImsLanguageSpecificContributor} to provide their 
 * language specific contributions.
 */
public class DawnImsTransitiveMetricsCollector {
	private static final Set<ModuleType> IMS_PROGRAM_TYPES = Set.of(ModuleType.COBOL_PROGRAM, ModuleType.PL1_MAINPROGRAM,
			ModuleType.C_PROGRAM, ModuleType.ASSEMBLER_PROGRAM, ModuleType.ASSEMBLER_MACRO, ModuleType.UNKNOWN);
	private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
	private static final String ORDER = "ORDER";
	
	private final ModuleBuilder rootModuleBuilder;
	private final ModuleService moduleService;
	private final ModuleLightweightPojo rootModule;
	private final ImsLanguageSpecificContributor languageSpecificContributor;
	private final EntityId projectId;
	private final CallChainService callChainService;
	
	public DawnImsTransitiveMetricsCollector(final ModuleBuilder rootModuleBuilder, final ModuleService moduleService,
			final ModuleLightweightPojo rootModule, final ImsLanguageSpecificContributor languageSpecificContributor, final EntityId projectId,
			final CallChainService callChainService) {
		this.rootModuleBuilder = rootModuleBuilder;
		this.moduleService = moduleService;
		this.rootModule = rootModule;
		this.languageSpecificContributor = languageSpecificContributor;
		this.projectId = projectId;
		this.callChainService = callChainService;
	}
		
	/**
	 * Calculates the transitive metrics between jobs, programs and involved IMS resources.
	 */
	public void calculateTransitiveMetrics() {
		final List<ModuleRelationshipPojo> depsCallingIms = languageSpecificContributor.getDependenciesCallingIms();
		if (depsCallingIms.isEmpty()) {
			return;
		}
		/*
		 * Root ModuleId need to pass
		 */
		final var moduleId = EntityId.of(rootModule.getUid(), rootModule.getId());
		/* collect all JCL steps that are at the top of the call chain of the current module. */
		final Map<Long, ModuleLightweightPojo> callingSteps = collectRootModulesThatCallThisProgramRecursively(moduleId, Type.EXEC_PGM);
		if ( ! callingSteps.isEmpty()) {
			calculateTransitiveMetricsForBatch(callingSteps.values(), depsCallingIms);
		}
		
		/* collect all SYSGEN applications that are at the top of the call chain of the current module. */
		final Map<Long, ModuleLightweightPojo> callingApplications = collectRootModulesThatCallThisProgramRecursively(moduleId, Type.APPLICATION);
		if ( ! callingApplications.isEmpty()) {
			final Map<Long, ModuleLightweightPojo> callingMids = collectRootModulesThatCallThisProgramRecursively(moduleId, Type.MID);
			calculateTransitiveMetricsForOnline(callingApplications.values(), callingMids.values(), depsCallingIms);
		}
	}
	
	/**
	 * Collects all JCL_STEPs that either execute this program directly via EXEC PGM
	 * or indirectly, by calling another Cobol program which in turn calls this program via CALL.
	 *
	 * @param moduleId the id of the current module (Cobol Program) to process
	 * @return all steps that call the target program, either directly or indirectly
	 */
	private Map<Long, ModuleLightweightPojo> collectRootModulesThatCallThisProgramRecursively(final EntityId moduleId, final Type rootType) {
		final Set<EntityId> callingSteps = new HashSet<>();
		final Optional<List<CallChainGraph>> callChains = callChainService.createCallChainGraphs(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(projectId)
				.setStartModuleIds(Collections.singletonList(moduleId))
				.setCallTypes(Collections.singleton(RelationshipType.CALLS))
				.setDirections(Collections.singletonList(CallChain.CallChainDirection.IN))
				.setEndModuleTypes(Collections.singleton(rootType))
				.setParallel(1)
				.build());

		callChains.ifPresent(callChainGraphs ->
				callChainGraphs.stream()
				.flatMap(graph -> graph.getEndModules(endModule -> rootType == endModule.getType()).stream())
				.forEach(endModule -> callingSteps.add(EntityId.of(endModule.getId()))));
		final List<EntityId> moduleIds = new ArrayList<>(callingSteps);
		return moduleIds.isEmpty() ? Collections.emptyMap()
				: moduleService.findModulesLightweight(q -> q.byIds(moduleIds)).stream().collect(Collectors.toMap(ModuleLightweightPojo::getId, module -> module));
	}
	
	private void calculateTransitiveMetricsForBatch(final Collection<ModuleLightweightPojo> callingSteps, final List<ModuleRelationshipPojo> depsCallingIms) {
		for (final ModuleLightweightPojo step : callingSteps) {
			final List<ModuleRelationshipPojo> dependencies = getDependencies(step.identity());
			final List<ModuleBasePojo> targetModules = getDestinationModules(dependencies);
			if (targetModules.stream().noneMatch(targetModule -> JclJobContributor.DFSRRC00.equals(targetModule.getName()))) {
				return;
			}
			
			final Optional<ModuleBasePojo> optPsb = getPsb(targetModules);
			if (optPsb.isEmpty()) {
				return;
			}
			
			final ModuleBasePojo psb = optPsb.get();
			var hasIoPcb = false;
			/* for batch programs CMPAT controls if the implicit IO-PCB will be present or not */
			final Map<String, Object> info = psb.getInfo();
			if (info != null) {
				final Object cmpat = info.get("CMPAT");
				hasIoPcb = cmpat != null && StringUtils.equals("YES", cmpat.toString());
			}
			final Optional<ModuleBasePojo> rootProgram = getRootProgram(targetModules);
			if (rootProgram.isPresent()) {
				calculateTransitiveMetricsInternal(psb, hasIoPcb, depsCallingIms, rootProgram.get(), true, Collections.emptyList());
			}
		}
	}
	
	private Optional<ModuleBasePojo> getPsb(final List<ModuleBasePojo> modules) {
		/* returns the PSB dependency of the job or SYSGEN application */
		return modules.stream().filter(module -> module.getTechnology() == Technology.IMS && module.getType() == Type.PSB)
		.findFirst();
	}
	
	private Optional<ModuleBasePojo> getRootProgram(final List<ModuleBasePojo> targetModules) {
		/* returns the root program directly referenced by the job of SYSGEN application */
		return targetModules.stream()
				.filter(target -> target.getTechnology() == languageSpecificContributor.getTargetTechnology()
				&& IMS_PROGRAM_TYPES.contains(ModuleType.fromTechnologyAndType(target.getTechnology(), target.getType())))
				.findFirst();
	}
	
	private List<ModuleRelationshipPojo> getDependencies(final EntityId moduleId) {
		return moduleService.findRelationship(q -> q.ofProject(projectId)
				  .ofSource(moduleId)
				  .withTypes(RelationshipType.DEPENDENCY_TYPES)
				  .includeModuleDetails(false, true)
				  .distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION, RelationshipField.TYPE));

	}
	
	private void calculateTransitiveMetricsInternal(final ModuleBasePojo psb, final boolean hasImplicitIoPcb, final List<ModuleRelationshipPojo> depsCallingIms,
			final ModuleBasePojo rootProgram, final boolean isBatchMode, final Collection<ModuleLightweightPojo> callingMids) {
		final List<ModuleRelationshipPojo> dependencies = getDependencies(psb.identity());
		final List<ModuleBasePojo> targetModules = getDestinationModules(dependencies);
		final List<ModuleBasePojo> orderedPcbs = targetModules
			.stream()
			.filter(module -> module.getTechnology() == Technology.IMS && module.getType() == Type.PCB)
			.filter(pcb -> pcb.getInfo() != null)
			.filter(pcb -> Objects.requireNonNull(pcb.getInfo()).get(ORDER) != null)
			/* the order of PCBs declared in the PSB must always be preserved, as programs reference these PCBs in the exact same order.
			 * therefore we manually ensure here that the PCBs always stick to the same order as they were initially parsed from the PSB. */
			.sorted(Comparator.comparing(pcb -> Objects.requireNonNull(pcb.getInfo()).get(ORDER).toString()))
			.collect(Collectors.toList());
		
		final Optional<ModuleBasePojo> rootArtifact = ! rootProgram.equals(rootModule) ? Optional.of(rootProgram) : Optional.empty();
		for (final ModuleRelationshipPojo callDep : depsCallingIms) {
			final Optional<ModuleLocation> callLocation = callDep.getSrcLocation();
			if (callLocation.isEmpty()) {
				continue;
			}
			
			var validPcb = false;
			final Tuple2<AstNode, Integer> callNodeAndPcbNumber = languageSpecificContributor.resolveCallNodeAndPcbNumber(callLocation.get(), rootArtifact);
			if (callNodeAndPcbNumber == null) {
				continue;
			}
			final Map<String, Object> properties = callDep.getProperties().orElse(Collections.emptyMap());
			if (callNodeAndPcbNumber.b >= 0) {
				/* online programs always have an implicit IO-PCB, which is not listed in the PSB. for batch programs it's only present when CMPAT=YES
				 * is set and the IO-PCB must always be the first referenced PCB in the program when it's active. */
				final int finalPcbNumber = hasImplicitIoPcb ? callNodeAndPcbNumber.b - 1 : callNodeAndPcbNumber.b;
				
				if (finalPcbNumber < 0) {
					/* the call used an IO-PCB. for online programs this will then access the message queue, instead of the DB.
					 * as the IO-PCB is not present in the PSB, there's also no dependency to add. */
					if ( ! isBatchMode) {
						validPcb = true;
						final var accessTypes = properties.get(DB_ACCESS_TYPE.name());
						/* in case of message queue access:
						 * the "DB_ACCESS_TYPE" is used to determine the type of access, i.e. whether it is a read access ("receive map")
						 * or write access ("send map") */
						final Optional<DatabaseAccessType> accessType = getAccessType(accessTypes);
						/* and the "SSA" parameter can optionally contain the name of the MID or MOD that is used */
						final Optional<String> modName = getModName(properties.get(IMS_SSA.name()));
						clearDbAccessAttributes(callDep);
						addMfsDependency(callNodeAndPcbNumber.a, callDep, callingMids, accessType, modName);
					}
				} else if (finalPcbNumber < orderedPcbs.size()) {
					validPcb = true;
					addDependency(callNodeAndPcbNumber.a, orderedPcbs.get(finalPcbNumber), callDep);
				}
			}
			
			if ( ! validPcb) {
				/* PCB could not be resolved. therefore we add all PCBs defined in the PSB as a fallback. */
				final Object statement = properties.get(STATEMENT.name());
				if (statement != null) {
					rootModuleBuilder.addError(Severity.WARNING, ErrorKey.DEPENDENCY_RESOLUTION_ERROR,
							"Unable to resolve PCB for IMS call: " + statement);
				}
				orderedPcbs.forEach(pcb -> addDependency(callNodeAndPcbNumber.a, pcb, callDep));
			}
		}
	}
	
	private void addDependency(final AstNode callNode, final ModuleBasePojo pcb, final ModuleRelationshipPojo toImsCallDep) {
		final ModuleLocation callLocation = toImsCallDep.getSrcLocation().orElse(null);

		final var pcbDep = new ModuleRelationshipPojo(callLocation, RelationshipType.REFERENCES, Binding.LATE, pcb);
		languageSpecificContributor.addTransitiveImsDependency(callNode, pcbDep, Optional.empty());
		
		final List<ModuleRelationshipPojo> dependencies = getDependencies(pcb.identity());
		final List<ModuleBasePojo> targetModules = getDestinationModules(dependencies);
		final List<ModuleBasePojo> sensitiveSegments = targetModules.stream()
				.filter(module -> module.getTechnology() == Technology.IMS && module.getType() == Type.DBD_SEGMENT)
				.collect(Collectors.toList());
		sensitiveSegments.forEach(segment -> {
			final var segmentDep = new ModuleRelationshipPojo(callLocation, RelationshipType.REFERENCES, Binding.LATE, segment);
			languageSpecificContributor.addTransitiveImsDependency(callNode, segmentDep, Optional.empty());
		});

		final Optional<ModuleBasePojo> dbd = targetModules.stream()
				.filter(module -> module.getTechnology() == Technology.IMS && module.getType() == Type.DBD)
				.findFirst();
		if (dbd.isPresent()) {
			final String segments = sensitiveSegments.stream().map(ModuleBasePojo::getName).sorted().collect(Collectors.joining(", ", "[", "]"));
			final ModelAttributeMap<Object> attributeMap = getModelAttributeMap(toImsCallDep, segments);
			final var dbdDep = new ModuleRelationshipPojo(callLocation, RelationshipType.REFERENCES, Binding.LATE, dbd.get());
			languageSpecificContributor.addTransitiveImsDependency(callNode, dbdDep, Optional.of(attributeMap));
		}

		addDependencyForMissingModules(callNode, pcb, toImsCallDep, callLocation);
	}

	private void addDependencyForMissingModules(final AstNode callNode, final ModuleBasePojo pcb, final ModuleRelationshipPojo toImsCallDep, @Nullable final ModuleLocation callLocation) {
		/*
		 * The resolution of transitive dependencies is added during deferred action. At the time we declare the dependency here, one cycle of dependency 
		 * resolution has already been completed. If the target modules for IMS_DBD_SEGMENTS and IMS_DBD, which the dependency relies upon, are not available,
		 * the dependency resolution will not take place. 
		 * However, the dependency definitions will remain in the database for resolution through DiscoveryCoreImpl#handleUnresolvedDependencies() at the end.
		 * 
		 * In this process, we will scan all the dependency definitions for the IMS_PCB module. We will fetch all the module filters for IMS_DBD_SEGMENTS
		 * and IMS_DBD, and then declare the dependency.
		 */
		final var dependencyDefinitions = moduleService.findDependencyDefinitions(q -> q.ofModule(pcb.identity()).withResolved(false));
		if (dependencyDefinitions.isEmpty()) {
			return;
		}
		final var dbdSegmentModuleFilters = dependencyDefinitions.stream().flatMap(dependencyDefinition -> dependencyDefinition.getModuleFilters().stream())
				.filter(moduleFilter -> moduleFilter.getTypes().contains(ModuleType.IMS_DBD_SEGMENT)).collect(Collectors.toList());
		dbdSegmentModuleFilters.forEach(segmentFilter -> {
			final var segmentDep = new ModuleRelationshipPojo(callLocation, RelationshipType.REFERENCES, Binding.LATE, null);
			languageSpecificContributor.addTransitiveImsDependency(callNode, segmentDep, segmentFilter, Optional.empty(), ResolutionFlag.MERGE_DUPLICATES);
		});
		final Optional<ModuleFilter> dbdModuleFilter = dependencyDefinitions.stream()
				.flatMap(dependencyDefinition -> dependencyDefinition.getModuleFilters().stream())
				.filter(moduleFilter -> moduleFilter.getTypes().contains(ModuleType.IMS_DBD)).findFirst();
		if (dbdModuleFilter.isPresent()) {
			final String segments = dbdSegmentModuleFilters.stream().flatMap(moduleFilter -> moduleFilter.getNames().stream()).sorted()
					.collect(Collectors.joining(", ", "[", "]"));
			final ModelAttributeMap<Object> attributeMap = getModelAttributeMap(toImsCallDep, segments);
			final var segmentDep = new ModuleRelationshipPojo(callLocation, RelationshipType.REFERENCES, Binding.LATE, null);
			languageSpecificContributor.addTransitiveImsDependency(callNode, segmentDep, dbdModuleFilter.get(), Optional.of(attributeMap),
					ResolutionFlag.MERGE_DUPLICATES);
		}
	}

	private static ModelAttributeMap<Object> getModelAttributeMap(final ModuleRelationshipPojo toImsCallDep, final String sensitiveSegments) {
		final ModelAttributeMap<Object> attributeMap = new ModelAttributeMap<>(ModelAttributeComparator.getKeyComparator());
		final Map<String, Object> properties = toImsCallDep.getProperties().orElse(Collections.emptyMap());
		final Object accessType = properties.get(DB_ACCESS_TYPE.name());
		if (accessType != null) {
			attributeMap.put(DB_ACCESS_TYPE, accessType);
		}
		final Object accessOperation = properties.get(DB_ACCESS_OPERATION.name());
		if (accessOperation != null) {
			attributeMap.put(DB_ACCESS_OPERATION, accessOperation);
		}
		final Object statement = properties.get(STATEMENT.name());
		if (statement != null) {
			attributeMap.put(STATEMENT, statement);
		}
		final Object ssa = properties.get(IMS_SSA.name());
		if (ssa != null) {
			attributeMap.put(IMS_SSA, ssa);
		}

		/* we only want the sensitive segments defined by the PCB and not all segments of the DBD as part of the dependency */
		attributeMap.put(IMS_SEGMENTS, sensitiveSegments);
		return attributeMap;
	}

	private void clearDbAccessAttributes(final ModuleRelationshipPojo callDependency) {
		clearDbAccessAttribute(callDependency);
		languageSpecificContributor.getAdditionalDependencies(callDependency).forEach(this::clearDbAccessAttribute);
	}
	
	private void clearDbAccessAttribute(final ModuleRelationshipPojo dep) {
		final Map<String, Object> properties = dep.getProperties().orElse(Collections.emptyMap());
		final Map<String, Object> newProperties = new HashMap<>();
		final List<String> accessTypes = Arrays.asList(DB_ACCESS_TYPE.name(), DB_ACCESS_OPERATION.name(), CALL_TYPE.name());
		for (final Map.Entry<String, Object> mapEntry : properties.entrySet()) {
			final String key = mapEntry.getKey();
			if ( ! accessTypes.contains(key)) {
				final var value = mapEntry.getValue();
				newProperties.put(key, value);
			}
		}
		
		try {
			final var dependencyAttributes = OBJECT_MAPPER.writeValueAsString(newProperties);
			moduleService.updateRelationshipProperties(newProperties, dependencyAttributes, dep.getId());
		} catch (final Exception e) {
			throw new IllegalArgumentException("While persisting dependency attributes to the database: the attributes cannot be represented as JSON", e);
		}
	}
	
	private void calculateTransitiveMetricsForOnline(final Collection<ModuleLightweightPojo> callingApplications, final Collection<ModuleLightweightPojo> callingMids,
			final List<ModuleRelationshipPojo> depsCallingIms) {
		for (final ModuleLightweightPojo application : callingApplications) {
			final List<ModuleRelationshipPojo> dependencies = getDependencies(application.identity());
			final List<ModuleBasePojo> targetModules = getDestinationModules(dependencies);
			final Optional<ModuleBasePojo> optPsb = getPsb(targetModules);
			if ( ! optPsb.isPresent()) {
				return;
			}
			
			final Optional<ModuleBasePojo> rootProgram = getRootProgram(targetModules);
			if (rootProgram.isPresent()) {
				calculateTransitiveMetricsInternal(optPsb.get(), true, depsCallingIms, rootProgram.get(), false, callingMids);
			}
		}
	}

	private List<ModuleBasePojo> getDestinationModules(final List<ModuleRelationshipPojo> dependencies) {
		return dependencies.stream().map(ModuleRelationshipPojo::getDstModuleDetails).filter(Optional::isPresent)
				.map(Optional::get)
				.collect(Collectors.toList());
	}
	
	@SuppressWarnings("unchecked")
	private Optional<DatabaseAccessType> getAccessType(@Nullable final Object attribute) {

		if (attribute == null) {
			return Optional.empty();
		}

		final Optional<Object> value;
		if (attribute instanceof Collection) {
			value = ((Collection<Object>) attribute).stream().findFirst();
		} else {
			value = Optional.of(attribute);
		}

		return value.map(v -> DatabaseAccessType.valueOf(v.toString()));
	}

	@SuppressWarnings("unchecked")
	private Optional<String> getModName(@Nullable final Object attribute) {
		if (attribute == null) {
			return Optional.empty();
		}

		final Optional<Object> value;
		if (attribute instanceof Collection) {
			value = ((Collection<Object>) attribute).stream().findFirst();
		} else {
			value = Optional.of(attribute);
		}

		return value.map(Objects::toString);
	}
	
	/**
	 * Add dependency from a program to IMS_MFS.
	 *
	 * @param callNode the statement making the call to the IMS utility (e.g. "CALL CBLTDLI")
	 * @param toImsCallDep the dependency to the IMS utility created for the call statement
	 * @param callingMids optional, list of MIDs that invoked the transaction that invoked the current program
	 * @param accessType "access type" used in the utility call - I believe this indicates whether it is a send or receive operation
	 * @param modName optionally, name of the target MID or MOD if it is given explicitly in the utility call
	 */
	private void addMfsDependency(final AstNode callNode, final ModuleRelationshipPojo toImsCallDep, final Collection<ModuleLightweightPojo> callingMids,
			final Optional<DatabaseAccessType> accessType, final Optional<String> modName) {

		final var callLocation = toImsCallDep.getSrcLocation().orElse(new ModuleLocation());

		if (accessType.isEmpty()) {
			rootModuleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY, "Unable to determine type of IMS message queue access",
					AstNodeLocation.fromModuleLocation(callLocation));
			return;
		}

		final List<ModuleFilter> targetModuleFilters = new ArrayList<>();
		if (accessType.get() == DatabaseAccessType.READ) {
			if (modName.isPresent()) {
				final ModuleFilter midFilter = new ModuleFilter().setNames(modName.get()).setTypes(ModuleType.IMS_MFS_MID);
				targetModuleFilters.add(midFilter);
			} else if (callingMids.size() == 1) {
				final ModuleFilter callingMidFilter = new ModuleFilter().setModuleIds(callingMids.iterator().next().identity());
				targetModuleFilters.add(callingMidFilter);
			} else {
				rootModuleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
						"Unable to determine name of MID used in IMS message queue access", AstNodeLocation.fromModuleLocation(callLocation));
				return;
			}
		} else if (accessType.get() == DatabaseAccessType.STORE) {
			if (modName.isPresent()) {
				final ModuleFilter midFilter = new ModuleFilter().setNames(modName.get()).setTypes(ModuleType.IMS_MFS_MOD);
				targetModuleFilters.add(midFilter);
			} else if (callingMids.size() == 1) {
				final ModuleLightweightPojo callingMid = callingMids.stream().findFirst().orElseThrow();
				final String midName = callingMid.getName();
				final String potentialModName = midName.substring(0, midName.length() - 1) + "O";
				final ModuleFilter midFilter = new ModuleFilter().setNames(potentialModName).setTypes(ModuleType.IMS_MFS_MOD);
				targetModuleFilters.add(midFilter);
			} else {
				rootModuleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
						"Unable to determine name of MOD used in IMS message queue access", AstNodeLocation.fromModuleLocation(callLocation));
				return;
			}
		} else {
			rootModuleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
					"Unhandled access type " + accessType.get() + " for IMS message queue access", AstNodeLocation.fromModuleLocation(callLocation));
			return;
		}

		for (final ModuleFilter target : targetModuleFilters) {
			final var midDep = new ModuleRelationshipPojo(callLocation, RelationshipType.REFERENCES, Binding.LATE, null);
			/* Dependency target should be the MFS module, not the MID or MOD -> therefore getParentModule()
			 * this is kind of an arbitrary choice */
			languageSpecificContributor.addTransitiveImsDependency(callNode, midDep, target, Optional.empty(), ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL,
					ResolutionFlag.RESOLVE_TO_PARENT);
		}
	}
	
	/**
	 * This will be used by the {@link DawnImsTransitiveMetricsCollector} to delegate language specific operations.
	 */
	public interface ImsLanguageSpecificContributor {
		
		/**
		 * @return the target {@link Technology} for the language specific operations
		 */
		Technology getTargetTechnology();

		/**
		 * Resolves the {@link AstNode} of the statement calling the IMS utility and the order number of the PCB used in the call. The {@link AstNode} will
		 * be used to decide which resource should receive the transitive dependencies, based on the unassembled content. If the PCB could not be
		 * resolved {@code -1} is being returned.
		 *
		 * @param callLocation the location where the call to the IMS utility is located at. These are unassembled source locations!
		 * @param rootProgram if the program containing the call and the main program called by the job or transaction are different ones,
		 * 						then this provides the root program of the call chain, that can be used to extract the PCB order information
		 * @return tuple with the {@link AstNode} of the call statement with or without resolved PCB number (zero based) or {@code null} if 
		 * no valid IMS utility call
		 */
		@Nullable
		Tuple2<AstNode, Integer> resolveCallNodeAndPcbNumber(final ModuleLocation callLocation, final Optional<ModuleBasePojo> rootProgram);

		/**
		 * Called whenever a transitive IMS dependency should be added to the currently processed module.
		 * 
		 * @param callNode the {@link AstNode} of the statement calling the IMS utility. This can be used to decide which resource should receive the 
		 * dependency, based on the unassembled content
		 * @param dependency the {@link ModuleRelationshipPojo} to be added
		 * @param attributeMap attribute map for dependency
		 */
		void addTransitiveImsDependency(final AstNode callNode, final ModuleRelationshipPojo dependency, final Optional<ModelAttributeMap<Object>> attributeMap);
		
		/**
		 * Called whenever a transitive IMS dependency should be added to the currently processed module.
		 * 
		 * @param callNode the {@link AstNode} of the statement calling the IMS utility. This can be used to decide which resource should receive the 
		 * dependency, based on the unassembled content
		 * @param dependency the {@link ModuleRelationshipPojo} to be added
		 * @param targetModuleFilter target {@link ModuleFilter}
		 * @param attributeMap attribute map for dependency
		 * @param resolutionFlags set of {@link ResolutionFlag}s
		 */
		default void addTransitiveImsDependency(final AstNode callNode, final ModuleRelationshipPojo dependency, final ModuleFilter targetModuleFilter,
				final Optional<ModelAttributeMap<Object>> attributeMap, final ResolutionFlag... resolutionFlags) {

		}

		/**
		 * @return all {@linkplain ModuleRelationshipPojo}s directly referenced by the currently processed module and also by its included copies
		 * 			that execute calls to IMS utilities
		 */
		List<ModuleRelationshipPojo> getDependenciesCallingIms();

		/**
		 * Returns any additional language specific dependencies based on the given {@code callDependency}.
		 * 
		 * @param callDependency the dependency from the module to the IMS utility
		 * @return additonal language specific dependencies
		 */
		default List<ModuleRelationshipPojo> getAdditionalDependencies(final ModuleRelationshipPojo callDependency) {
			return Collections.emptyList();
		}
	}

}
