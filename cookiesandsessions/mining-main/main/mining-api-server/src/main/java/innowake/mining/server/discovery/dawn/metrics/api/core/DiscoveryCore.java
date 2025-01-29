/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.core;

import java.util.List;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Discovery core operations.
 */
public interface DiscoveryCore {
	
	/**
	 * Executes a contributor and retrieves the contributor results.
	 * <p>
	 * This method is used to execute contributors implementing the {@link DiscoveryContributor} interface, which are invoked once per Discovery run.
	 *
	 * @param contributor the contributor to invoke
	 * @param context the context of the current Discovery
	 * @return the list of results contributed by the contributor
	 */
	List<ContributorResult> executeContributor(DiscoveryContributor contributor, DiscoveryContext context);
	
	/**
	 * Executes a contributor on a source file and retrieves the contributor results.
	 * <p>
	 * This method is used to execute contributors implementing the {@link DiscoveryContributorFromSource} interface. This kind of contributor is invoked
	 * for each source file. First its {@linkplain DiscoveryContributorFromSource#accept(DiscoveryContext, SourcePojo) accept()} method is called
	 * to check if the contributor can process the source file. If it can, the contributor is executed on the source file using
	 * {@link DiscoveryContributorFromSource#contribute(innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource, DiscoveryContext, SourcePojo)}.
	 *
	 * @param contributor the contributor to invoke
	 * @param context the context of the current Discovery
	 * @param sourceObject the source file on which the contributor is executed
	 * @return the list of results contributed by the contributor
	 */
	List<ContributorResult> executeContributorOnSourceObject(DiscoveryContributorFromSource contributor, DiscoveryContext context, SourcePojo sourceObject);
	
	/**
	 * Imports a list of contributor results into persistent storage.
	 * Module definitions created via {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...)}  anchorTo()},
	 * deferred actions and declared dependencies are moved to temporary storage instead. All of these require the definitions of their target module(s)
	 * to be imported first, so they are processed separately.
	 * See {@linkplain #anchorAndImportResult(DiscoveryContext, ContributorResult) anchorAndImportResult()} and
	 * {@linkplain #executeDeferredAction(DiscoveryContext, EntityId, DeferredActionDefinition) executeDeferredAction()}.
	 *
	 * @param context the current Discovery context
	 * @param contributorResults the contributor results to import
	 * @see #anchorAndImportResult(DiscoveryContext, ContributorResult)
	 * @see #executeDeferredAction(DiscoveryContext, EntityId, DeferredActionDefinition)
	 */
	void importContributorResults(DiscoveryContext context, List<ContributorResult> contributorResults);
	
	/**
	 * Anchor a module builder and apply the contributor result to the target module.
	 * <p>
	 * Anchoring means that the existing module(s) identified by the {@linkplain ContributorResult#getModuleFilter() ModuleFilter} inside the ContributorResult
	 * are located and then the definitions from the ContributorResult are imported into the existing module(s).
	 *
	 * @param context the current Discovery context
	 * @param anchoredResult the contributor result to anchor and then import
	 */
	void anchorAndImportResult(DiscoveryContext context, ContributorResult anchoredResult);
	
	/**
	 * Executes a deferred action by using a definition.
	 *
	 * @param context the current Discovery context
	 * @param moduleId the id of the module on which the deferred action is run
	 * @param deferredAction the definition of the deferred action to execute
	 * @return the result from executing the deferred action
	 */
	List<ContributorResult> executeDeferredAction(DiscoveryContext context, EntityId moduleId, DeferredActionDefinition deferredAction);
	
	/**
	 * Resolves a dependency and persists the result of the dependency resolution to persistent storage.
	 *
	 * @param context the current Discovery context
	 * @param moduleId the id of the module on which the dependency was declared
	 * @param dependency the dependency definition
	 */
	void resolveDependency(DiscoveryContext context, EntityId moduleId, DependencyDefinitionPojo dependency);
	
	/**
	 * Resolves the unresolved dependencies at the end of the discovery cycle. 
	 *
	 * @param context the current Discovery context
	 * @param moduleId the id of the module on which the dependency was declared
	 * @param dependency the dependency definition
	 */
	void handleUnresolvedDependencies(DiscoveryContext context, EntityId moduleId, DependencyDefinitionPojo dependency);
	
	/**
	 * Creates default modules if missing and updates it if found. It is executed on the collected result from the anchoring 
	 * with {@link ResolutionFlag#CREATE_IF_MISSING}
	 * 
	 * @param context the current Discovery context
	 * @param anchoredResult the contributor result to anchor and then import
	 */
	void createIfMissingDefaultModules(DiscoveryContext context, ContributorResult anchoredResult);

	/**
	 * Merges duplicate dependencies that have the {@link ResolutionFlag#MERGE_DUPLICATES} flag.
	 *
	 * @param context the current Discovery context
	 * @param moduleId the id of the module for which the dependencies should be merged
	 * @param dependencies the list of dependencies to merge
	 */
	void mergeDependencies(DiscoveryContext context, EntityId moduleId, List<DependencyDefinitionPojo> dependencies);
}
