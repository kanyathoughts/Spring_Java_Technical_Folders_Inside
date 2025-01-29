/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.persistence;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * Discovery persistence interface. Allows to import contributor results into persistent storage
 * and also allows to look up existing modules based on a {@link ModuleFilter}.
 */
public interface DiscoveryPersistence {

	/**
	 * Find all existing modules that match the given module filter in the given context.
	 *
	 * @param context the current discovery context
	 * @param filter the filter object describing the target modules
	 * @param flags the {@link ResolutionFlag}s
	 * @return the list of ids of matched modules
	 */
	List<EntityId> findModules(DiscoveryContext context, ModuleFilter filter, ResolutionFlag... flags);

	/**
	 * Fetch Modules using a list of ids. The Modules are returned in the same order in which the corresponding ids were passed.
	 *
	 * @param context the current discovery context
	 * @param moduleIds the ids of the modules
	 * @return the fetched Modules corresponding to each id
	 */
	List<ModulePojo> fetchModules(DiscoveryContext context, List<EntityId> moduleIds);

	/**
	 *  Fetches the lightweight Module containing minimal information.
	 *
	 * @param context the current discovery context
	 * @param moduleIds the ids of the modules to fetch
	 * @return the list of light weight {@link ModuleLightweightPojo}
	 */
	List<ModuleLightweightPojo> fetchModulesLightWeight(DiscoveryContext context, List<EntityId> moduleIds);

	/**
	 * Imports a module into storage. If the module already exists, the existing definition is updated, otherwise a new module is created.
	 * <p>
	 * The provided {@code ModuleFilter} must match exactly one target module (which may or may not already exist). In case there are multiple existing
	 * modules in the database that can be matched by the provided filter, this method will return unsuccessfully, returning a {@link ImportResult}
	 * with {@link ImportResult.Status#AMBIGUOUS_MATCH}.
	 *
	 * @param context the current discovery context
	 * @param moduleFilter a filter object describing the target module
	 * @param moduleDefinition the attribute to apply to the target module
	 * @param flags the {@link ResolutionFlag}s
	 * @return the result of the import operation
	 */
	ImportResult<EntityId> persistModule(DiscoveryContext context, ModuleFilter moduleFilter, ModulePojoPrototype moduleDefinition,  ResolutionFlag... flags);

	/**
	 * Imports a list of statements into storage and attaches them to an existing module.
	 *
	 * @param context the current discovery context
	 * @param moduleId the id of the existing module to which the statements are attached
	 * @param module the module to which the statements are attached
	 * @param statements the list of statements
	 * @return the result of the import operations
	 */
	List<ImportResult<EntityId>> persistStatements(DiscoveryContext context, EntityId moduleId, ModulePojoPrototype module, List<StatementPojoPrototype> statements);
	
	/**
	 * Imports a list of error markers into storage and attaches them to an existing module.
	 *
	 * @param context the current discovery context
	 * @param moduleId the id of the existing module to which the error markers are attached
	 * @param errors the list of error markers
	 * @return the result of the import operations
	 */
	List<ImportResult<EntityId>> persistErrors(DiscoveryContext context, EntityId moduleId, List<ErrorMarker> errors);
	
	
	/**
	 * Imports a list of dead code regions into storage and attaches them to an existing module.
	 *
	 * @param context the current discovery context
	 * @param moduleId the id of the existing module to which the dead code regions are attached
	 * @param deadCode the list of dead code regions
	 * @return the result of the import operations
	 */
	List<ImportResult<EntityId>> persistDeadCode(DiscoveryContext context, EntityId moduleId, List<ModuleDeadCodePojoPrototype> deadCode);
	
	/**
	 * Creates a dependency relationship from one existing module to another existing module.
	 *
	 * @param context the current discovery context
	 * @param fromModuleId the id of the module from which the dependency originates
	 * @param toModuleId the id of the module that is the target of the dependency
	 * @param dependencyDefinitionId the id of the dependency definition from which this dependency is created
	 * @param fromModuleLocation the location of the dependency within in the origin module
	 * @param relationshipType the type of relationship
	 * @param bindingType the binding type of the relationship
	 * @param attributes additional attributes for the relationship
	 * @param conditionalModules list of reaching modules (conditional dependencies). Empty if there are none.
	 * @return the result of the import operation
	 */
	ImportResult<EntityId> createDependency(DiscoveryContext context, EntityId fromModuleId, EntityId toModuleId, @Nullable final UUID dependencyDefinitionId,
			@Nullable ModuleLocation fromModuleLocation, RelationshipType relationshipType, Binding bindingType,
			Map<String, Object> attributes, final List<ModuleFilter> conditionalModules);
	
	/**
	 * Imports a list of dependency definitions for the given module. This method does not resolve the dependency targets
	 * and as such, does not establish a link between two modules. It only records the declared dependencies in the database,
	 * so they can be resolved later.
	 *
	 * @param context the current discovery context
	 * @param moduleId the id of the existing module on which the dependencies are declared
	 * @param dependencyDefinitions the list of dependency definitions
	 * @return the result of the import operations
	 */
	List<ImportResult<UUID>> persistDependencyDefinitions(DiscoveryContext context, EntityId moduleId, List<DependencyDefinitionPojoPrototype> dependencyDefinitions);

	/**
	 * Updates the dependency definition with resolved set as true.
	 *
	 * @param dependencyDefinitionId the id of the dependency definition that has to be marked as resolved
	 * @return the result of resolve operation
	 */
	ImportResult<Long> markDependencyDefinitionResolved(UUID dependencyDefinitionId);

	/**
	 * Fetches the modules with dependency definitions of a project
	 *
	 * @param projectId id of the project
	 * @return the moduleIds with dependency definitions 
	 */
	List<EntityId> getModulesWithUnresolvedDependencies(EntityId projectId);
	
	/**
	 * Fetches the unresolved dependency definitions of the module ie the dependency definitions with resolved set to false
	 *
	 * @param moduleId the id of the module
	 * @return the list of dependency definitions
	 */
	List<DependencyDefinitionPojo> fetchUnresolvedDependencyDefinitions(EntityId moduleId);

	/**
	 * Gets the collection of ids of targets to which the given dependency definition was already resolved.
	 *
	 * @param dependencyDefinitionId the primary key of the dependencyDefinition
	 * @return the collection of already resolved targets for this DependencyDefinition
	 */
	Set<EntityId> getResolvedTargetsForDependency(UUID dependencyDefinitionId);

	/**
	 * Fetches the moduleIds of unresolved dependency definitions with mergeDuplicates flag
	 *
	 * @param projectId the id of the project
	 * @return the list of moduleIds with merge duplicates dependency definitions
	 */
	List<EntityId> fetchModuleIdsWithMergeDuplicates(EntityId projectId);

	/**
	 * Deletes the dependency definitions
	 *
	 * @param dependencyDefinitionIds the ids of the dependency definitions to be deleted
	 * @return the result of the delete operation
	 */
	ImportResult<Integer> deleteDependencyDefinitions(List<UUID> dependencyDefinitionIds);

}
