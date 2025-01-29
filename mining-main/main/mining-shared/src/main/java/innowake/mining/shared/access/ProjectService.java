/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.ProjectNature;

/**
 * Specifies basic functions for accessing the Project database entity.
 */
public interface ProjectService {

	static final String PLACEHOLDER_XML_CONFIG_KEY = "placeholder_xml_config_key";

	/**
	 * Query sort builder for sorting {@code project} entities when performing queries on {@code project} entities.
	 */
	interface ProjectOrderBuilder {
		/**
		 * Sorts Projects by their numeric ID.
		 * @param sort Direction.
		 * @return Sorting builder.
		 */
		ProjectOrderBuilder sortNid(final SortDirection sort);
		
		/**
		 * Sorts Projects by their name using binary collation.
		 * @param sort Direction.
		 * @return Sorting builder.
		 */
		ProjectOrderBuilder sortName(final SortDirection sort);
	}

	/**
	 * Query builder for performing queries on {@code project} entities.
	 */
	interface ProjectInquiryBuilder extends ProjectOrderBuilder {
		/**
		 * Filters a single project by it's ID.
		 * @param id ID of a project.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder withId(EntityId id);
		
		/**
		 * Filters Projects by name.
		 * @param name Pattern matching the names to include.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder withName(String name);
		
		/**
		 * Filters out all Projects up to a certain ID.
		 * @param nid Numeric ID, that which and all below are to be filtered.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder withIdAbove(Long nid);
		
		/**
		 * Filters Projects by the given IDs.
		 * @param nids Set of IDs to include.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder withIds(Collection<Long> nids);
		
		/**
		 * Filters Projects by any of the given IDs.
		 * @param projectNids Project IDs to include.
		 * @param clientNids Client IDs Projects of which to include.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder withIds(Collection<Long> projectNids, Collection<Long> clientNids);
		
		/**
		 * Filters Projects based on their deletion status.
		 * @param toBeDeleted Whether to include only valid ({@code false}, default) or only deleted ({@code true}) or any ({@code null}) Project.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder filterMarkedForDeletion(@Nullable Boolean toBeDeleted);
		
		/**
		 * Filters Projects by a Client.
		 * @param clientId ID of the Client.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder ofClient(EntityId clientId);
		
		/**
		 * Filters Projects by Clients.
		 * @param clientIds Unique IDs of Clients.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder ofClientUIDs(Collection<UUID> clientIds);
		
		/**
		 * Filters Projects by Clients.
		 * @param clientIds Numeric IDs of Clients.
		 * @return Filter builder.
		 */
		ProjectInquiryBuilder ofClientNIDs(Collection<Long> clientIds);
	}
	
	/**
	 * Retrieves all or a filtered subset of Projects.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return All matching Project entities.
	 */
	List<ProjectPojo> find(BuildingConsumer<ProjectInquiryBuilder> builder);
	
	/**
	 * Retrieves paged subset of optionally filtered Projects.
	 * @param paging Pagination specification.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return Paged subset of matching Project entities.
	 */
	Paged<ProjectPojo> find(Pagination paging, BuildingConsumer<ProjectInquiryBuilder> builder);
	
	/**
	 * Retrieves any Project by its ID.
	 * @param uid Unique ID of the Project.
	 * @return The Project if it exists.
	 */
	Optional<ProjectPojo> find(UUID uid);
	
	/**
	 * Retrieves a Project by its ID.
	 * @param projectId ID of the Project.
	 * @return The Project if it exists.
	 */
	Optional<ProjectPojo> find(EntityId projectId);
	
	/**
	 * Checks if the specified project exists and is not marked for deletion.
	 * @param projectId Project ID.
	 * @return If the project is valid.
	 */
	boolean isValid(EntityId projectId);
	
	/**
	 * Counts all Project entries.
	 * @param builder Builder for filter criteria.
	 * @return The number of Project entries.
	 */
	Long count(final BuildingConsumer<ProjectInquiryBuilder> builder);
	
	/**
	 * Retrieves any Project by its ID.
	 * @param uid Unique ID of the Project.
	 * @return The Project if it exists.
	 */
	ProjectPojo get(UUID uid);
	
	/**
	 * Retrieves a Project by its ID.
	 * @param projectId ID of the Project.
	 * @return The Project if it exists and is not marked for deletion.
	 */
	ProjectPojo get(EntityId projectId);
	
	/**
	 * Retrieves all Projects, that are currently valid.
	 * @param projectIds Limit eligible Projects to IDs.
	 * @return List if Projects which may be empty.
	 */
	List<ProjectPojo> getAll(@Nullable Collection<Long> projectIds);
	
	/**
	 * Retrieves all Projects.
	 * @param paging Pagination specification.
	 * @param order Builder for sorting options.
	 * @param projectIds ID of the Project.
	 * @return List if Projects which may be empty.
	 */
	Paged<ProjectPojo> getAll(Pagination paging, @Nullable Consumer<ProjectOrderBuilder> order, @Nullable Collection<Long> projectIds);
	
	/**
	 * Retrieves IDs of certain Projects.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return List of ProjectIds which may be empty.
	 */
	List<UUID> getUids(BuildingConsumer<ProjectInquiryBuilder> builder);
	
	/**
	 * Retrieves IDs of certain Projects.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return List of ProjectIds which may be empty.
	 */
	List<Long> getNids(BuildingConsumer<ProjectInquiryBuilder> builder);
	
	/**
	 * Retrieves the XML configuration for a Project
	 * @param projectId ID of the Project.
	 * @param name Name of the configuration
	 * @return Configuration data.
	 */
	String getXmlConfig(EntityId projectId, String name);
	
	/**
	 * Retrieves all configurations of a Project
	 * @param projectId ID of the Project.
	 * @return Configuration data.
	 */
	Map<String, Map<String, Object>> getConfigs(EntityId projectId);

	/**
	 * Retrieves all configurations of a Project for a given name and maps the result to the provided type.
	 * @param <T> Type of the configuration.
	 * @param projectId ID of the Project.
	 * @param valueType Type of the configuration.
	 * @param name Name of the configuration.
	 * @return Configuration data.
	 */
	<T> Optional<T> getConfigByName(EntityId projectId, Class<T> valueType, String name);

	/**
	 * Retrieves XML configurations of a Project
	 * @param projectId ID of the Project.
	 * @param names Names of the configurations.
	 * @return Configuration data.
	 */
	Map<String, String> getXmlConfigs(EntityId projectId, Collection<String> names);
	
	/**
	 * Stores a configuration for a Project.
	 * @param projectId ID of the Project.
	 * @param name Name of the configuration.
	 * @param value Content of the configuration.
	 */
	void putConfig(EntityId projectId, String name, Object value);
	
	/**
	 * Increments the source code revision of a Project or sets it to 1 if it is null.
	 * @param projectId ID of the Project.
	 * @return Updated source code revision.
	 */
	Long incrementSourceCodeRevision(EntityId projectId);
	
	/**
	 * Creates a new Project.
	 * <p>After the project is created, the service will invoke the authorization service to create roles and natures for the project.</p>
	 * 
	 * @param project Initial specification of the Project. 
	 * @return The newly created Project.
	 * @see #create(ProjectPojoPrototype, boolean)
	 */
	ProjectPojo create(ProjectPojoPrototype project);

	/**
	 * Creates a new Project. If {@code createAuthorization} is {@code true} the authorization service is called to create roles and natures for the created 
	 * {@code project}.
	 * 
	 * @param project Initial specification of the Project.
	 * @param createAuthorization {@code true} to create roles and natures. {@code false} to not create roles and natures.
	 * @return The newly created project.
	 */
	ProjectPojo create(ProjectPojoPrototype project, boolean createAuthorization);

	/**
	 * Modifies an existing project.
	 * @param project Specification of the Project attributes to modify.
	 * @return ID of the modified project.
	 */
	EntityId update(ProjectPojoPrototype project);
	
	/**
	 * Modifies an existing project.
	 * @param projectBuilder Defines the key to find and attributes to update.
	 * @return ID of the modified project.
	 */
	EntityId update(BuildingConsumer<ProjectPojoPrototype> projectBuilder); 
	
	/**
	 * Resets the configuration to the default configuration files and stores them.
	 * @param projectId ID of the Project.
	 */
	void resetConfiguration(EntityId projectId);
	
	/**
	 * Marks a Project for deletion.
	 * @param projectId ID of the Project.
	 * @param andStartDeletion Whether to trigger the deletion job.
	 * @return IDs of the Project.
	 */
	EntityId markForDeletion(EntityId projectId, boolean andStartDeletion);
	
	/**
	 * Deletes a Project directly.
	 * @param projectId
	 */
	void deleteDirectly(EntityId projectId);
	
	/**
	 * Gets the {@link ProjectNature project natures} for a project.
	 * @param projectId ID of the Project.
	 * @return The List of unique {@link ProjectNature project natures} assigned to the project.
	 */
	List<ProjectNature> findProjectNatures(final EntityId projectId);
	
	/**
	 * Changes the {@link ProjectNature project natures} for a specific project.
	 * This also changes the nature roles assigned to the project users replacing the default roles with the new ones being assigned.
	 * All users' "custom" natures are left untouched, this means that for every user we only remove/change the natures that are part of the default set.
	 * @param projectId ID of the Project.
	 * @param natures Set of {@link ProjectNature project natures} to assign.
	 */
	void changeProjectNatures(final EntityId projectId, final Set<ProjectNature> natures);
	
	/**
	 * Deletes a project in a cascading manner.
	 * It also deletes all entities associated with the given Project.
	 *
	 * @param clientId The ID of the Client
	 * @param projectId The ID of the Project
	 */
	void deleteProjectCascading(Long clientId, Long projectId);
	
	/**
	 * Returns the numeric id from the given {@code projectId}. If {@code projectId} contains no numeric id, then the nid is fetched from the DB.
	 *
	 * @param projectId the project {@link EntityId}
	 * @return the numeric id of the project
	 */
	Long getNid(EntityId projectId);

}
