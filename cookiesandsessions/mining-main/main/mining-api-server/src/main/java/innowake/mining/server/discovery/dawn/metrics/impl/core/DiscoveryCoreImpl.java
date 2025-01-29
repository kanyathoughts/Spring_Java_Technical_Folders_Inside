/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.core;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult.Type;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.server.discovery.dawn.metrics.api.persistence.ImportResult;
import innowake.mining.server.discovery.dawn.metrics.api.temporarystorage.DiscoveryTemporaryStorage;
import innowake.mining.server.discovery.dawn.metrics.impl.contributor.DiscoveryBuilderImpl;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.util.ModuleFilterUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.commons.compress.utils.Sets;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.shaded.minlog.Log;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ClassUtils;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;

@Service
public class DiscoveryCoreImpl implements DiscoveryCore {

	@Autowired
	private DiscoveryPersistenceImpl orientDiscoveryPersistence;
	
	@Autowired
	private DiscoveryTemporaryStorage discoveryTemporaryStorage;
	
	@Autowired
	private SourceCachingService sourceService;
	
	@Autowired
	private ModuleFilterUtil moduleFilterUtil;
	
	@Autowired
	private DiscoveryCache discoveryCache;
	
	private final List<DiscoveryContributor> contributors;
	
	private final List<DiscoveryContributorFromSource> sourceContributors;

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryCoreImpl.class);

	public static final String DISCOVERY_CORE_IMPL_DEPENDENCY = "discoveryCoreImplDependency";

	public static final String DISCOVERY_CORE_IMPL_UNRESOLVED_DEPENDENCY = "discoveryCoreImplUnresolvedDependency";
	
	/**
	 * Creates a new Discovery core. This constructor is to be used when the
	 * {@link DiscoveryContributor} and {@link DiscoveryContributorFromSource} is used for the {@link DiscoveryCore} interface.
	 *
	 * @param contributors all discovery contributors that are not executed on a particular source file
	 * @param sourceContributors all discovery contributors that operate on a source file
	 */
	@Autowired
	public DiscoveryCoreImpl(final List<DiscoveryContributor> contributors, 
			final List<DiscoveryContributorFromSource> sourceContributors) {
		this.contributors = contributors;
		this.sourceContributors = sourceContributors;
	}

	@Override
	public List<ContributorResult> executeContributor(final DiscoveryContributor contributor, final DiscoveryContext context) {
		final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(contributor.getClass().getName());
		contributor.contribute(builder, context);
		return builder.buildResults();
	}

	@Override
	public List<ContributorResult> executeContributorOnSourceObject(final DiscoveryContributorFromSource contributor, final DiscoveryContext context,
			final SourcePojo sourceObject) {
		final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(contributor.getClass().getName(), sourceObject.getPath());
		contributor.contribute(builder, context, sourceObject);
		return builder.buildResults();
	}

	@Override
	public void importContributorResults(final DiscoveryContext context, final List<ContributorResult> contributorResults) {
		final FailureCounts failureCounts = new FailureCounts();
		final List<ContributorResult> rootModules = contributorResults.stream().filter(result -> 
					result.getType() == ContributorResult.Type.ROOT_MODULE).collect(Collectors.toList());
		
		if (rootModules.size() > 1) {
			throw new IllegalArgumentException("More than one root module is not allowed");
		}
		final List<ContributorResult> subModules = contributorResults.stream().filter(result -> 
				result.getType() == ContributorResult.Type.SUB_MODULE)
				.collect(Collectors.toList());
		final List<ContributorResult> externalModules = contributorResults.stream()
				.filter(result -> result.getType() == ContributorResult.Type.EXTERNAL_MODULE)
				.collect(Collectors.toList());
		final List<ContributorResult> anchoredModules = contributorResults.stream().
				filter(result -> result.getType() == ContributorResult.Type.ANCHOR_TO).collect(Collectors.toList());
		/* Persist the root module first and update the containedIn filter for all other results. */
		if ( ! rootModules.isEmpty()) {
			final ContributorResult rootModule = rootModules.iterator().next();
			final ImportResult<EntityId> result = orientDiscoveryPersistence.persistModule(context, rootModule.getModuleFilter(), 
					rootModule.getModuleDefinition());
			final EntityId rootModuleId = result.getKey().orElseThrow(() ->
					new IllegalStateException("While importing the contributor result with moduleFilter: " + rootModule.getModuleFilter() +
							" Failed to get or create rootModule." +
							" Encountered " + result.getStatus() + ": " + result.getMessage().orElse(null),
							result.getCause().orElse(null)));

			storeContributorResultProperties(context, failureCounts, rootModule, rootModuleId);
			final ModuleFilter containedInModuleFilter = new ModuleFilter().setModuleIds(rootModuleId);
			subModules.forEach(contributorResult -> contributorResult.getModuleFilter().setContainedIn(containedInModuleFilter));
		}

		anchoredModules.forEach(result -> {
			/* if the ModuleFilter of the contributor result contains moduleId, then the target Module must already exist, and we can import the result
			 * directly. Otherwise, the result is stored temporarily and we try to locate the target Module when executing the AnchorTasks */
			if (result.getModuleFilter().getModuleIds().isEmpty()) {
				discoveryTemporaryStorage.storeUnanchoredContributorResult(context.getJobId(), result);
			} else {
				anchorAndImportResult(context, result);
			}
		});

		subModules.forEach(contributorResult -> importSubOrExternalModules(context, failureCounts, contributorResult));

		externalModules.forEach(contributorResult -> {
			final ModuleFilter moduleFilter = contributorResult.getModuleFilter();
			String lookupKey = null;
			/* lock only if no id is present but a name and module type, so we can create a lock key */
			if (moduleFilter.getModuleIds().isEmpty()) {
				final var moduleDefinition = contributorResult.getModuleDefinition();
				final var technology = moduleDefinition.technology.orElse(null);
				final var type = moduleDefinition.type.orElse(null);
				final String name = moduleDefinition.name.orElse(null);
				lookupKey = name != null && type != null && technology != null ? DiscoveryCache.createLockKeyForParameters(name, technology, type) : null;
			}

			final String jobIdLocal = context.getJobId();
			try {
				if (lookupKey != null) {
					discoveryCache.createLocks(jobIdLocal, lookupKey);
				}

				importSubOrExternalModules(context, failureCounts, contributorResult);
			} catch (final Exception e) {
				throw new IllegalStateException(e);
			} finally {
				if (lookupKey != null) {
					discoveryCache.releaseLocks(jobIdLocal, lookupKey);
				}
			}
		});
		if (failureCounts.containsFailures()) {
			throw new IllegalStateException(failureCounts.getErrorMessage());
		}
	}
	
	private void importSubOrExternalModules(final DiscoveryContext context, final FailureCounts failureCounts, final ContributorResult contributorResult) {
		final boolean caseInsensitiveFlag = contributorResult.getResolutionFlags().contains(ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		final ImportResult<EntityId> persistResult = orientDiscoveryPersistence.persistModule(context, contributorResult.getModuleFilter(),
				contributorResult.getModuleDefinition(),
				caseInsensitiveFlag && contributorResult.getType() == Type.EXTERNAL_MODULE ? ResolutionFlag.RESOLVE_CASE_INSENSITIVE : null);
		/* we will not proceed to store the properties if we are unable to store the module */
		if ( ! persistResult.isSuccess()) {
			LOG.error("Error occurred while trying to persist the sub/external module with moduleFilter :  " + contributorResult.getModuleFilter()
			+ " Encountered " + persistResult.getStatus() + ": " + persistResult.getMessage(), persistResult.getCause().orElse(null));
			failureCounts.moduleFailureCount += 1;
		} else {
			final EntityId key = persistResult.getKey().orElseThrow(IllegalStateException::new);
			storeContributorResultProperties(context, failureCounts, contributorResult, key);
		}
	}

	@Override
	public void anchorAndImportResult(final DiscoveryContext context, final ContributorResult anchoredResult) {
		final Set<ResolutionFlag> resolutionFlags = anchoredResult.getResolutionFlags();		
		if (CollectionUtils.intersection(resolutionFlags, Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY,
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR)).size() > 1) {
			throw new IllegalArgumentException("Mutually exclusive flags are not allowed " + resolutionFlags);
		}
	
		final ModuleFilter moduleFilter = anchoredResult.getModuleFilter();
		final boolean caseInsensitiveFlag = anchoredResult.getResolutionFlags().contains(ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		final List<EntityId> modules = orientDiscoveryPersistence.findModules(context, moduleFilter,
				caseInsensitiveFlag ? ResolutionFlag.RESOLVE_CASE_INSENSITIVE : null);
		if (modules.isEmpty()) {
			/* we are not able to find any matching targets, which could mean that the required targets may not yet created.*/
			/* therefore, we should put the result back into the temporary storage to be retried later. */
			discoveryTemporaryStorage.storeUnanchoredContributorResult(context.getJobId(), anchoredResult);
			return;
		}
		final List<ContributorResult> contributorResults = new ArrayList<>();
		if (modules.size() > 1 && ( ! CollectionUtils.containsAny(resolutionFlags, Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL,
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY)))) {
			throw new IllegalArgumentException("More than one target anchor found for " + moduleFilter);
		}
		
		if (resolutionFlags.contains(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY)) {
			final ModuleLightweightPojo module = orientDiscoveryPersistence.fetchModulesLightWeight(context, Collections.singletonList(modules.get(0))).get(0);
			contributorResults.add(getAnchoredContributorResult(module, anchoredResult));
		} else {
			/* execute for all the found modules. */
			orientDiscoveryPersistence.fetchModulesLightWeight(context, modules).stream()
			/* In an ideal scenario missing modules won't be coming up for anchoring or any other DAWN phases as it is the last step, but due to 
			 * partial migration of few contributors this scenario might occur hence filtering it out to avoid updating the missing modules */
			.filter(module -> module.getIdentification() == Identification.IDENTIFIED)
			.map(module -> getAnchoredContributorResult(module, anchoredResult))
					.forEachOrdered(contributorResults::add);
		}
		contributorResults.forEach(result -> importContributorResults(context, Collections.singletonList(result)));
		
		if (resolutionFlags.contains(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL)) {
			final ModuleFilter moduleFilterCopy = new ModuleFilter(anchoredResult.getModuleFilter());
			moduleFilterCopy.setNot(new ModuleFilter().setModuleIds(modules));
			final ContributorResult resultCopy = new ContributorResult(anchoredResult.getType(), moduleFilterCopy, Collections.emptySet(),
					anchoredResult.getModuleDefinition(), anchoredResult.getErrors(), anchoredResult.getDeadCodes(),
					anchoredResult.getDeferrredActions(), anchoredResult.getStatements(), anchoredResult.getDependencies());
			discoveryTemporaryStorage.storeUnanchoredContributorResult(context.getJobId(), resultCopy);
		}
	}
	
	private Type getModuleType(final ModuleLightweightPojo module) {
		if (module.getParent() != null) {
			return Type.SUB_MODULE;
		} else if (StringUtils.isNotBlank(module.getPath())) {
			/* As path exists it means it is a physical file, hence it is a root module */
			return Type.ROOT_MODULE;
		} else {
			/* As it neither has a containingModuleId and nor has path, it means it is a external module */
			return Type.EXTERNAL_MODULE;
		}
	}
	
	private ContributorResult getAnchoredContributorResult(final ModuleLightweightPojo module, final ContributorResult anchoredResult) {
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(module.identity());
		final var parent = module.getParent();
		if (parent != null) {
			moduleFilter.setContainedIn(new ModuleFilter().setModuleIds(parent));
		}
		final Type type = getModuleType(module);
		var anchoredModuleDefinition = anchoredResult.getModuleDefinition();
		
		/* Since ResolutionFlag.CREATE_IF_MISSING flag is used only to create the target module, hence while updated we skip 
		 * name and moduleType values as it was set from ANCHOR_TO and ResolutionFlag.CREATE_IF_MISSING and should not be used to update */
		if (anchoredResult.getResolutionFlags().contains(ResolutionFlag.CREATE_IF_MISSING)) {
			final ModulePojoPrototype anchoredModuleDefinition2 = new ModulePojoPrototype();
			if (anchoredModuleDefinition.location.isPresent()) {
				anchoredModuleDefinition2.setLocation(anchoredModuleDefinition.location.getNonNull());
			}
			if (anchoredModuleDefinition.path.isPresent()) {
				anchoredModuleDefinition2.setPath(anchoredModuleDefinition.path.getNonNull());
			}
			if (anchoredModuleDefinition.storage.isPresent()) {
				anchoredModuleDefinition2.setStorage(anchoredModuleDefinition.storage.getNonNull());
			}
			if (anchoredModuleDefinition.representation.isPresent()) {
				anchoredModuleDefinition2.setRepresentation(anchoredModuleDefinition.representation.getNonNull());
			}
			/* Don't change the identification of module, if it's already identified */
			if (module.getIdentification() == Identification.IDENTIFIED) {
				anchoredModuleDefinition2.setIdentification(Identification.IDENTIFIED);
			} else if (anchoredModuleDefinition.identification.isPresent()) {
				anchoredModuleDefinition2.setIdentification(anchoredModuleDefinition.identification.getNonNull());
			}
			if (anchoredModuleDefinition.origin.isPresent()) {
				anchoredModuleDefinition2.setOrigin(anchoredModuleDefinition.origin.getNonNull());
			}
			anchoredModuleDefinition = anchoredModuleDefinition2;
		}

		return new ContributorResult(type, moduleFilter, Collections.emptySet(), anchoredModuleDefinition, anchoredResult.getErrors(),
				anchoredResult.getDeadCodes(), anchoredResult.getDeferrredActions(), anchoredResult.getStatements(), anchoredResult.getDependencies());
	}

	@Override
	public List<ContributorResult> executeDeferredAction(final DiscoveryContext context, final EntityId moduleId, final DeferredActionDefinition deferredAction) {
		final Predicate<Object> contributorNameFilter = contributor -> {
			try {
				return Class.forName(deferredAction.getContributorClassName()).isInstance(contributor);
			} catch (final ClassNotFoundException e) {
				return false;
			}
		};
		final Optional<DiscoveryContributor> matchingContributor = contributors.stream()
				.filter(contributorNameFilter).findFirst();
		final Optional<DiscoveryContributorFromSource> matchingSourceContributor = sourceContributors.stream()
				.filter(contributorNameFilter)
			.findFirst();
		final List<ContributorResult> contributorResults = new ArrayList<>();
		if (matchingContributor.isPresent()) {
			contributorResults.addAll(invokeDeferredMethod(context, moduleId, deferredAction, matchingContributor.get()));
		} else if (matchingSourceContributor.isPresent()) {
			contributorResults.addAll(invokeDeferredMethod(context, moduleId, deferredAction, matchingSourceContributor.get()));
		} else {
			throw new IllegalStateException(String.format("No matching contributor class with name %s exists", deferredAction.getContributorClassName()));
		}
		
		importContributorResults(context, contributorResults);
		return contributorResults;
	}

	@Override
	public void resolveDependency(final DiscoveryContext context, final EntityId moduleId, final DependencyDefinitionPojo dependency) {
		final Set<ResolutionFlag> resolutionFlags = dependency.getResolutionFlags();
		
		if (resolutionFlags.contains(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY)) {
			throw new IllegalArgumentException(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY + " is not allowed for dependency resolution");
		}
		if (resolutionFlags.containsAll(Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL,
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR))) {
			throw new IllegalArgumentException("Mutually exclusive flags are set in dependency resolution " + resolutionFlags);
		}
		final var moduleFiltersIterator = dependency.getModuleFilters().iterator();
		Set<EntityId> targetModules = new HashSet<>();
		while (moduleFiltersIterator.hasNext()) {
			 final var moduleIds = getDependencyTargetModules(context, moduleId, moduleFiltersIterator.next(), dependency.getLocation(),
					 dependency.getResolutionFlags().toArray(ResolutionFlag[]::new));
			/* As we found exactly one target, it is most likely the correct target module, Hence we will resolve the dependency with this module */
			if (moduleIds.size() == 1) {
				targetModules = new HashSet<>(moduleIds);
				break;
			}
			/* Collect all the moduleIds for MULTIPLE_MATCH_RESOLVE_ALL and MULTIPLE_MATCH_RESOLVE_ERROR*/
			targetModules.addAll(moduleIds);
		}

		if (targetModules.isEmpty()) {
			return;
		}
		
		/* Removing already resolved targets for dependencies with MULTIPLE_MATCH_RESOLVE_ALL flag */
		if (resolutionFlags.contains(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL)) {
			final Set<EntityId> alreadyResolvedTargets = orientDiscoveryPersistence.getResolvedTargetsForDependency(dependency.getId());
			targetModules = targetModules.stream()
					.filter(targetModuleId -> ! alreadyResolvedTargets.contains(targetModuleId))
					.collect(Collectors.toSet());
		}

		if (targetModules.size() > 1 && (resolutionFlags.contains(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR)
				|| ! resolutionFlags.contains(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL) || resolutionFlags.contains(ResolutionFlag.RESOLVE_TO_PARENT))) {
			addError(context, moduleId, Severity.ERROR, ErrorKey.UNDISCOVERED_DEPENDENCY, dependency.getLocation(),
					"Multiple possible candidates found: " + EntityId.allNids(targetModules));
			orientDiscoveryPersistence.markDependencyDefinitionResolved(dependency.getId());
			return;
		}

		targetModules.forEach(targetModuleId -> {
			final ImportResult<EntityId> result = orientDiscoveryPersistence.createDependency(context, moduleId, targetModuleId, dependency.getId(),
					dependency.getLocation().orElse(null), dependency.getRelationshipType(), dependency.getBindingType(), dependency.getAttributes(),
					dependency.getReachedFromModules());
			if ( ! result.isSuccess()) {
				final var errorMessage = String.format("Failed to create dependency from moduleId : %s to target moduleId: %s. Encountered %s : %s , %s",
						moduleId, targetModuleId, result.getStatus(), result.getMessage().orElse(null), result.getCause().orElse(null));
				addError(context, moduleId, Severity.ERROR, ErrorKey.DEPENDENCY_RESOLUTION_ERROR, dependency.getLocation(), errorMessage);
				LOG.error(errorMessage);
			}
		});
			
		if ( ! resolutionFlags.contains(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL)) {
			/* dependencies with MULTIPLE_MATCH_RESOLVE_ALL flag set are not marked as resolved. They are processed again in each
			* dependency resolution cycle, in case additional valid targets were added. */
			orientDiscoveryPersistence.markDependencyDefinitionResolved(dependency.getId());
		}
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void handleUnresolvedDependencies(final DiscoveryContext context, final  EntityId moduleId, final DependencyDefinitionPojo dependency) {
		final Set<ResolutionFlag> resolutionFlags = dependency.getResolutionFlags();
		/* MULTIPLE_MATCH_RESOLVE_ALL flag means 0 or more dependencies.
		 * We are not setting resolved=true for MULTIPLE_MATCH_RESOLVE_ALL flag, as the dependencies for this can created in multiple cycles, 
		 * hence we will not create a missing or utility module, if this flag is present.
		 * RESOLVE_TO_PARENT should not create a missing module as it is used to create dependency only if it exists
		 */
		if (resolutionFlags.contains(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL) || resolutionFlags.contains(ResolutionFlag.RESOLVE_TO_PARENT)) {
			return;
		}
		final Optional<ModuleFilter> moduleFilter = dependency.getModuleFilters().stream()
																.reduce((filter1, filter2) -> moduleFilterUtil.joinModuleFilter(filter1, filter2).orElse(filter1));

		if (moduleFilter.isPresent() && moduleFilter.get().getNames().size() == 1) {
			final String name = moduleFilter.get().getNames().iterator().next();
			final boolean isUtilityDependency = context.getConfig().getUtilityList().isUtility(name);
			final ModuleType moduleType = moduleFilter.get().getTypes().size() == 1 ? moduleFilter.get().getTypes().iterator().next() : ModuleType.UNKNOWN;
			/* Also update the moduleFilter with this moduleType, so it doesn't modify modules with other types */
			moduleFilter.get().setTypes(moduleType);
			final var moduleDefinition = new ModulePojoPrototype()
					.setName(name)
					.setStorage(Storage.UNDEFINED)
					.setRepresentation(Representation.VIRTUAL);
			
			dependency.getLocation().ifPresent(moduleDefinition::setLocation);
			if (isUtilityDependency) {
				/*
				 * Duplicate utility modules are creating when we declare the module filter without
				 * ModuleType.UNKNOWN_UTILITY type, So we have to add ModuleType.UNKNOWN_UTILITY type to existing types to check in DB and create.
				 */
				final Set<ModuleType> types = moduleFilter.get().getTypes();
				if ( ! types.contains(ModuleType.UNKNOWN_UTILITY)) {
					final HashSet<ModuleType> targetTypes = new HashSet<>(types);
					targetTypes.add(ModuleType.UNKNOWN_UTILITY);
					moduleFilter.get().setTypes(targetTypes);
				}
				moduleDefinition.setIdentification(Identification.IDENTIFIED);
				moduleDefinition.setOrigin(Origin.ENVIRONMENT);
				moduleDefinition.setTechnology(ModuleType.UNKNOWN_UTILITY.getTechnology());
				moduleDefinition.setType(ModuleType.UNKNOWN_UTILITY.getType());
			} else {
				moduleDefinition.setIdentification(Identification.MISSING);
				moduleDefinition.setOrigin(Origin.CUSTOM);
				moduleDefinition.setTechnology(moduleType.getTechnology());
				moduleDefinition.setType(moduleType.getType());
				
				/*
				 * We need to add the filter to fetch the missing module to avoid the update of identified modules if exist with the given module filter.
				 */
				moduleFilter.get().setIdentification(Identification.MISSING);
			}
			final String lookupKey = DiscoveryCache.createLockKeyForParameters(name.concat(DiscoveryCoreImpl.DISCOVERY_CORE_IMPL_DEPENDENCY),
					moduleType.getTechnology(), moduleType.getType());
			/*
			 * This is the edge case like if you have contained in when your creating a missing module with default type , it will fail.
			 * so removed contained in.
			 */
			moduleFilter.get().setContainedIn(null);
			final Callable<ImportResult<EntityId>> computeCallable = () -> orientDiscoveryPersistence.persistModule(context, moduleFilter.get(),
					moduleDefinition);
			final String jobIdLocal = context.getJobId();
			final ImportResult<EntityId> result;
			try {
				result = (ImportResult<EntityId>) discoveryCache.computeValueIfAbsent(jobIdLocal, lookupKey, computeCallable);
			} catch (final Exception e) {
				throw new IllegalStateException(e);
			}
			final EntityId targetModuleId = result.getKey().orElseThrow(() ->
					new IllegalStateException("While creating a reference from Module " + moduleId + " to MISSING target module:" +
							" Failed to get or create target module " + name + " (" + moduleType + ")" +
							" Encountered " + result.getStatus() + ": " + result.getMessage().orElse(null),
							result.getCause().orElse(null)));

			final ImportResult<EntityId> dependencyImportResult = orientDiscoveryPersistence.createDependency(context, moduleId, targetModuleId,
					dependency.getId(), dependency.getLocation().orElse(null), dependency.getRelationshipType(), dependency.getBindingType(),
					dependency.getAttributes(), dependency.getReachedFromModules());
			if ( ! dependencyImportResult.isSuccess()) {
				 final var errorMessage = String.format("Failed to create MISSING dependency from moduleId : %s to target moduleId: %s." +
								" Encountered %s : %s, %s", moduleId, targetModuleId, dependencyImportResult.getStatus(),
						dependencyImportResult.getMessage().orElse(null), dependencyImportResult.getCause().orElse(null));
				addError(context, moduleId, Severity.ERROR, ErrorKey.DEPENDENCY_RESOLUTION_ERROR, dependency.getLocation(), errorMessage);
				throw new IllegalStateException(errorMessage);
			}
			/* Mark the dependency definition as resolved as we created a missing dependency to it */
			orientDiscoveryPersistence.markDependencyDefinitionResolved(dependency.getId());
		} else {
				addError(context, moduleId, Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY, dependency.getLocation(),
						"No target modules found with : " + StringUtils.replace(dependency.getModuleFilters().toString(), "\n", " "));
			}
	}

	private void storeContributorResultProperties(final DiscoveryContext context, final FailureCounts failureCounts, final ContributorResult contributorResult,
			final EntityId key) {
		long failures;
		if ( ! contributorResult.getStatements().isEmpty()) {
			failures = orientDiscoveryPersistence.persistStatements(context, key, contributorResult.getModuleDefinition(), contributorResult.getStatements()).stream()
				.filter(result -> ! result.isSuccess())
				.count();
			failureCounts.statementFailureCount += failures ;
			LOG.debug(String.format("Failed to persist %d statements for moduleId: %s", Long.valueOf(failures), key));
		}
		if ( ! contributorResult.getErrors().isEmpty()) {
			failures = orientDiscoveryPersistence.persistErrors(context, key, contributorResult.getErrors()).stream()
				.filter(result -> ! result.isSuccess())
				.count();
			failureCounts.errorFailureCount += failures;
			LOG.debug(String.format("Failed to persist %d errors for moduleId: %s", Long.valueOf(failures), key ));
		}
		if ( ! contributorResult.getDeadCodes().isEmpty()) {
			failures = orientDiscoveryPersistence.persistDeadCode(context, key, contributorResult.getDeadCodes()).stream()
				.filter(result -> ! result.isSuccess())
				.count();
			failureCounts.deadCodeFailureCount += failures;
			LOG.debug(String.format("Failed to persist %d deadCodes for moduleId: %s", Long.valueOf(failures), key ));
		}
		contributorResult.getDeferrredActions().forEach(deferredAction -> discoveryTemporaryStorage
				.storeDeferredAction(context.getJobId(), key, deferredAction));
		if( ! contributorResult.getDependencies().isEmpty()) {
			failures = orientDiscoveryPersistence.persistDependencyDefinitions(context, key, contributorResult.getDependencies()).stream()
					.filter(result -> ! result.isSuccess())
					.count();
			failureCounts.dependencyDefinitionFailureCount += failures;
			LOG.debug(String.format("Failed to persist %d dependencyDefinitions for moduleId: %s", Long.valueOf(failures), key ));
		}
	}
	
	private static class FailureCounts {
		long moduleFailureCount = 0;
		long statementFailureCount = 0;
		long errorFailureCount = 0;
		long deadCodeFailureCount = 0;
		long dependencyDefinitionFailureCount = 0;
		
		boolean containsFailures() {
			return (moduleFailureCount + statementFailureCount + errorFailureCount + deadCodeFailureCount + dependencyDefinitionFailureCount > 0);
		}
		
		String getErrorMessage() {
			final StringBuilder stringBuilder = new StringBuilder(128);
			if (moduleFailureCount > 0 ) {
				stringBuilder.append( moduleFailureCount).append(" module(s) ");
			}
			if (statementFailureCount > 0) {
				stringBuilder.append( statementFailureCount).append(" statement(s) ");
			}
			if (errorFailureCount > 0) {
				stringBuilder.append( errorFailureCount).append(" error(s) ");
			}
			if (deadCodeFailureCount > 0 ) {
				stringBuilder.append(deadCodeFailureCount).append(" deadCode(s) ");
			}
			if (dependencyDefinitionFailureCount > 0 ) {
				stringBuilder.append(dependencyDefinitionFailureCount + " dependencyDefinition(s) ");
			}
			stringBuilder.append("is/are unable to be stored due to an error. Please check the logs for more info");
			return stringBuilder.toString();
		}
	}

	private void addError(final DiscoveryContext context, final EntityId moduleId, final Severity severity, final ErrorKey errorKey,
			final Optional<ModuleLocation> location , final String message) {
		final var errorLocation = location.orElse(new ModuleLocation(-1, -1));
		final var offset = errorLocation.getOffset();
		final var length = errorLocation.getLength();
		final var advancedLocation = new AstNodeLocation(offset, length, offset, length, offset, length, -1, -1);
		final ErrorMarker error = new ErrorMarker(severity, errorKey, message, advancedLocation);
		orientDiscoveryPersistence.persistErrors(context, moduleId, Collections.singletonList(error));
	}

	private List<EntityId> getDependencyTargetModules(final DiscoveryContext context, final EntityId moduleId, final ModuleFilter moduleFilter,
			final Optional<ModuleLocation> location, final ResolutionFlag[] resolutionFlags) {
		final List<EntityId> modules = orientDiscoveryPersistence.findModules(context, moduleFilter, resolutionFlags);
		/* If we don't find or find only one module, we don't need to consider the search orders. */
		if (modules.size() <= 1) {
			return modules;
		}
		if (moduleFilter.getModuleIds().isEmpty()) {
		     /*configured search orders are used by default unless moduleIds are declared */
		    final ModuleLightweightPojo sourceModule = orientDiscoveryPersistence.fetchModulesLightWeight(context, Collections.singletonList(moduleId)).get(0);
		    final String contextPath = sourceModule.getPath() == null ? sourceModule.getParentPath() : sourceModule.getPath();
		    
		    final List<SearchOrder> matchingSearchOrders = context.getSearchOrders().getSearchOrdersList().stream()
					.filter(searchOrder -> moduleFilterUtil.matches(context, moduleFilterUtil.toModuleFilter(searchOrder.getSource(), Optional.ofNullable(contextPath)),
							sourceModule))
					.collect(Collectors.toList());
		    final List<ModuleFilter> targetFilters = matchingSearchOrders.stream()
		    		.flatMap(searchOrder -> searchOrder.getTargets().stream())
		        	.map(target -> moduleFilterUtil.joinModuleFilter(moduleFilter, moduleFilterUtil.toModuleFilter(target, Optional.ofNullable(contextPath))))
		        	.filter(Optional::isPresent)
		        	.map(Optional::get)
		        	.collect(Collectors.toList());
		    if (targetFilters.isEmpty()) {
		    	final String message = String.format("No SearchOrder matches for contextPath: %s while resolving dependency"
		    			+ " from dependency.getModuleFilter(): %s, for artifact name: %s and contextPath: %s.",
		    			contextPath, moduleFilter, sourceModule.getName(), contextPath );
		    	addError(context, moduleId, Severity.ERROR, ErrorKey.DEPENDENCY_RESOLUTION_ERROR, location, message);
		    }

		    List<EntityId> targetModules = new ArrayList<>();
			for (final ModuleFilter filter : targetFilters) {
				targetModules = orientDiscoveryPersistence.findModules(context, filter, resolutionFlags);
		        if (targetModules.isEmpty()) {
					/* Workaround for modules which don't have path */
					final boolean pathMatch = filter.getPathPatterns().stream().anyMatch("**/*"::equals);
					if (pathMatch) {
						filter.setPathPatterns(Collections.emptySet());
						targetModules = orientDiscoveryPersistence.findModules(context, filter, resolutionFlags);
					}
		        }
		        if ( ! targetModules.isEmpty()) {
		        	return targetModules;
		        }
		    }
		   return targetModules;
		}
		return modules;
	}
	

	private  List<ContributorResult> invokeDeferredMethod(final DiscoveryContext context, final EntityId moduleId, final DeferredActionDefinition deferredAction,
			final Object contributor) {
		final Class<?> contributorClass = ClassUtils.getUserClass(contributor);
		/* Get the matching methods that are annotated with DeferredAction */
		final List<Method> matchingMethods = Arrays.asList(contributorClass.getMethods()).stream()
				.filter(method -> method.isAnnotationPresent(DeferredAction.class))
				.filter(method -> method.getName().equals(deferredAction.getName()) || 
						method.getDeclaredAnnotation(DeferredAction.class).value().equals(deferredAction.getName()))
				.collect(Collectors.toList());
		if (matchingMethods.isEmpty()) {
			throw new IllegalArgumentException(String.format("No method with name %s is present inside class %s",
					deferredAction.getName(), contributorClass.getName()));
		}
		if (matchingMethods.size() > 1) {
			throw new IllegalArgumentException(String.format("contributor class %s defines more than one deferred action named %s",
					contributorClass.getName(), deferredAction.getName()));
		}
		Optional<Serializable> optionalContext = deferredAction.getContext();
		final Method matchingMethod = matchingMethods.get(0);
		final List<Class<?>> parameterTypes = Arrays.asList(matchingMethod.getParameterTypes());
		DiscoveryBuilderImpl discoveryBuilderImpl = null;
		final ModuleLightweightPojo module = orientDiscoveryPersistence.fetchModulesLightWeight(context, Collections.singletonList(moduleId)).stream()
				.findFirst().orElseThrow(() -> new IllegalStateException("Module with Id:" + moduleId + " not found"));
		final List<Object> params = new ArrayList<>();
		/* Build the parameters for invoking the method */
		for (final Class<?> type : parameterTypes) {
			if (type.isAssignableFrom(DiscoveryContext.class)) {
			     params.add(context);
			}
			else if (type.isAssignableFrom(DiscoveryBuilder.class) || type.isAssignableFrom(DiscoveryBuilderFromSource.class)) {
				if ( ! (StringUtils.isNotBlank(module.getPath()) || StringUtils.isNotBlank(module.getParentPath()))
						&& DiscoveryBuilderFromSource.class.isAssignableFrom(type)) {
					throw new IllegalStateException(String.format("Module path/containsPath should be present for source based DiscoveryBuilder %s",
							contributorClass.getName()));
				}
				discoveryBuilderImpl = getDiscoveryBuilder(deferredAction.getContributorClassName(), module);
				params.add(discoveryBuilderImpl);
				
			}
			else if (type.isAssignableFrom(ModuleBuilder.class)) {
				if (discoveryBuilderImpl == null) {
					discoveryBuilderImpl = getDiscoveryBuilder(deferredAction.getContributorClassName(), module);
				}
				final ModuleBuilder moduleBuilder = discoveryBuilderImpl.anchorTo(new ModuleFilter().setModuleIds(module.identity()));
				params.add(moduleBuilder);
			}
			else if (type.isAssignableFrom(ModuleLightweightPojo.class)){
				final var lightweightModule = orientDiscoveryPersistence.fetchModulesLightWeight(context,
						Collections.singletonList(moduleId)).stream()
						.findFirst().orElseThrow(() -> new IllegalStateException("Module with Id:" + moduleId + " not found"));
				params.add(lightweightModule);
			}
			else if (type.isAssignableFrom(ModulePojo.class)) {
				final ModulePojo fullModule = orientDiscoveryPersistence.fetchModules(context, Collections.singletonList(moduleId)).stream()
						.findFirst().orElseThrow(() -> new IllegalStateException("Module with Id:" + moduleId + " not found"));
				params.add(fullModule);
			}
			else if (type.isAssignableFrom(SourcePojo.class)) {
				String path = StringUtils.EMPTY;
				if (module.getPath() != null) {
					/* As it has path, it means the module is a physical file, so we can use the module path itself to get sourceObject */
					path = module.getPath();
				}
				if (module.getParentPath() != null ) {
					/* The module is contained inside another module, hence we can use that path to get the sourceObject */
					path = module.getParentPath();
				}
				if (StringUtils.isNotBlank(path)) {
					final SourcePojo sourceObject = sourceService.cachingByProjectPath(context.getProjectId().getNid(), assertNotNull(path));
					params.add(sourceObject);
				} else {
					params.add(null);
				}
			}
			else {
				/* The given parameter has to be a context object or an unsupported parameter */
				if ( ! optionalContext.isPresent()) {
					final String errorMsg = String.format(
							"Unsupported parameter of type %s on deferred action method %s." + " Maybe a required context object was not provided?", type,
							deferredAction.getName());
					throw new IllegalArgumentException(errorMsg);
				} else {
					final Serializable deferredContext = optionalContext.get();
					if ( ! deferredContext.getClass().isAssignableFrom(type)) {
						final String errorMsg  = String.format("Unsupported parameter of type %s on deferred action method %s ."
								+ " Provided context object of type %s is not assignable to the parameter", type, deferredAction.getName(),
								deferredContext.getClass().getName());
						throw new IllegalArgumentException(errorMsg);
					}
					params.add(deferredContext);
					/* Set the context to empty, in case if the method declares multiple Serializable parameters we can throw error */
					optionalContext = Optional.empty();
				}
			}
		}

		try {
			LOG.debug(String.format("Invoking method %s of contributor %s with params %s", matchingMethod, contributor.getClass(),
					Arrays.toString(params.toArray())));
			matchingMethod.invoke(contributor, params.toArray());
		} catch (final IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new IllegalStateException(String.format("Exception occurred while invoking the method %s in class %s", matchingMethod.getName(),
					contributorClass.getName()), e);
		}
		return discoveryBuilderImpl != null ? discoveryBuilderImpl.buildResults() : Collections.emptyList();
	}

	private DiscoveryBuilderImpl getDiscoveryBuilder(final String contributorClassName, final ModuleLightweightPojo module) {
		DiscoveryBuilderImpl discoveryBuilderImpl;
		if (StringUtils.isNotBlank(module.getPath())) {
			discoveryBuilderImpl = new DiscoveryBuilderImpl(contributorClassName, assertNotNull(module.getPath()));
		} else if (StringUtils.isNotBlank(module.getParentPath())) {
			discoveryBuilderImpl = new DiscoveryBuilderImpl(contributorClassName, assertNotNull(module.getParentPath()));
		} else {
			discoveryBuilderImpl = new DiscoveryBuilderImpl(contributorClassName);
		}
		return discoveryBuilderImpl;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void createIfMissingDefaultModules(final DiscoveryContext context, final ContributorResult anchoredResult) {
		final ModuleFilter moduleFilter = anchoredResult.getModuleFilter();
		final boolean caseInsensitiveFlag = anchoredResult.getResolutionFlags().contains(ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		final List<EntityId> modules = orientDiscoveryPersistence.findModules(context, moduleFilter,
				caseInsensitiveFlag ? ResolutionFlag.RESOLVE_CASE_INSENSITIVE : null);
		
		if (modules.isEmpty()) {
			/* avoid creating the same default module twice concurrently by setting an appropriate Module filter here, that checks
			 * whether the Module we are about to create already exists.
			 * This is the same logic as in DiscoveryBuilderImpl.buildModuleFilterForExternalModule() */
			final var moduleDefinition = anchoredResult.getModuleDefinition();
			final String name = moduleDefinition.name.orElse(null);
			if (name == null) {
				throw new IllegalStateException("createIfMissing() was used without name argument");
			}
			final var technology = moduleDefinition.technology.orElse(null);
			if (technology == null) {
				throw new IllegalStateException("createIfMissing() was used without technology argument");
			}
			final var type = moduleDefinition.type.orElse(null);
			if (type == null) {
				throw new IllegalStateException("createIfMissing() was used without type argument");
			}

			final ModuleFilter moduleFilterForCreation = new ModuleFilter()
					.setNames(name)
					.setTypes(ModuleType.fromTechnologyAndType(technology, type));

			/* create a new moduleDefinition and set the representation to virtual */
			final var updatedModuleDefinition = new ModulePojoPrototype()
					.setName(name)
					.setTechnology(technology)
					.setType(type)
					.setLocation(moduleDefinition.location.orElse(null))
					.setPath(moduleDefinition.path.orElse(null))
					.setRepresentation(Representation.VIRTUAL)
					.setSourceMetrics(moduleDefinition.sourceMetrics.orElse(null));
			final Storage storage = moduleDefinition.storage.orElse(null);
			if (storage != null) {
				updatedModuleDefinition.setStorage(storage);
			}
			final Identification identification = moduleDefinition.identification.orElse(Identification.IDENTIFIED);
			if (identification != null) {
				updatedModuleDefinition.setIdentification(identification);
			}
			final Origin origin = moduleDefinition.origin.orElse(null);
			if (origin != null) {
				updatedModuleDefinition.setOrigin(origin);
			}

			final String lookupKey = DiscoveryCache.createLockKeyForParameters(DISCOVERY_CORE_IMPL_UNRESOLVED_DEPENDENCY + name, technology, type);
			final Callable<ImportResult<EntityId>> computeCallable = () -> orientDiscoveryPersistence.persistModule(context, moduleFilterForCreation,
					updatedModuleDefinition);
			final String jobIdLocal = context.getJobId();
			final ImportResult<Long> importResult;
			try {
				importResult = (ImportResult<Long>) discoveryCache.computeValueIfAbsent(jobIdLocal, lookupKey, computeCallable);
			} catch (final Exception e) {
				throw new IllegalStateException(e);
			}
			if ( ! importResult.isSuccess()) {
				Log.error("Failed to persist createIfMissing external module", importResult.getMessage().orElse(null), importResult.getCause().orElse(null));
			}
		}
	}

	@Override
	public void mergeDependencies(final DiscoveryContext context, final EntityId moduleId, final List<DependencyDefinitionPojo> dependencies) {
		final var dependencyMap = new ArrayListValuedHashMap<String, DependencyDefinitionPojo>();
		final var toBeDeletedDependencyIds = new ArrayList<UUID>();
		final var mergedDependencies = new ArrayList<DependencyDefinitionPojo>();
		dependencies
				.forEach(dependency -> dependencyMap.put(dependency.buildString(), dependency));
		dependencyMap.asMap().forEach((key, values) -> {
			if (values.size() > 1) {
				final var mergedDependency = getMergedDependency(values);
				mergedDependencies.add(mergedDependency);
				toBeDeletedDependencyIds.addAll(values.stream().map(DependencyDefinitionPojo::getId).collect(Collectors.toList()));
			}
		});
		final var prototypes = mergedDependencies.stream().map(dependencyDefinitionPojo ->
				new DependencyDefinitionPojoPrototype()
				.setBindingType(dependencyDefinitionPojo.getBindingType())
				.setAttributes(dependencyDefinitionPojo.getAttributes())
				.setRelationshipType(dependencyDefinitionPojo.getRelationshipType())
				.setModuleFilters(dependencyDefinitionPojo.getModuleFilters())
				.setResolutionFlags(dependencyDefinitionPojo.getResolutionFlags())
				.setLocation(dependencyDefinitionPojo.getLocation().orElse(null))
				.setReachedFromModules(dependencyDefinitionPojo.getReachedFromModules())).collect(Collectors.toList());
		orientDiscoveryPersistence.persistDependencyDefinitions(context, moduleId, prototypes);
		orientDiscoveryPersistence.deleteDependencyDefinitions(toBeDeletedDependencyIds);
	}

	private DependencyDefinitionPojo getMergedDependency(final Collection<DependencyDefinitionPojo> dependencies) {
		final var iterator = dependencies.iterator();
		final var mergedDependency = iterator.next();
		final var mergedReachedFromModules = new HashSet<ModuleFilter>(mergedDependency.getReachedFromModules());
		while (iterator.hasNext()) {
			final var dependency = iterator.next();
			mergedReachedFromModules.addAll(dependency.getReachedFromModules());
		}
		mergedDependency.setReachedFromModules(new ArrayList<>(mergedReachedFromModules));
		mergedDependency.getResolutionFlags().remove(ResolutionFlag.MERGE_DUPLICATES);

		return mergedDependency;
 	}
}
