/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.mining.shared.entities.scheduler.SchedulerEntryPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojo;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Functions for accessing scheduler information
 */
public interface SchedulerInfoService {

	/**
	 * Create a scheduler import
	 *
	 * @param schedulerImport The scheduler import to create
	 * @return The UUID of the created scheduler import
	 */
	UUID createSchedulerImport(SchedulerImportPojoPrototype schedulerImport);

	/**
	 * Create a scheduler entry
	 *
	 * @param schedulerEntry The scheduler entry to create
	 * @return The UUID of the created scheduler entry
	 */
	UUID createSchedulerEntry(SchedulerEntryPojoPrototype schedulerEntry);

	/**
	 * Find scheduler imports
	 *
	 * @param paging  The pagination
	 * @param builder The builder for the inquiry
	 * @return The paged result
	 */
	Paged<SchedulerImportPojo> findImports(Pagination paging, BuildingConsumer<SchedulerImportInquiryBuilder> builder);

	/**
	 * Create scheduler entry relationships
	 *
	 * @param schedulerEntries The scheduler entries to create relationships for
	 * @return The UUIDs of the created relationships
	 */
	List<UUID> createSchedulerEntryRelationships(List<SchedulerEntryRelationshipPojoPrototype> schedulerEntries);

	/**
	 * Find scheduler entry relationships
	 *
	 * @param paging  The pagination
	 * @param builder The builder for the inquiry
	 * @return The paged result
	 */
	Paged<SchedulerEntryRelationshipPojo> findRelationships(Pagination paging, BuildingConsumer<SchedulerEntryRelationshipInquireBuilder> builder);

	/**
	 * Find scheduler entries
	 *
	 * @param builder The builder for the inquiry
	 * @return The result
	 */
	List<SchedulerEntryPojo> findEntries(BuildingConsumer<SchedulerEntryInquiryBuilder> builder);

	/**
	 * Find scheduler entries, paginated
	 *
	 * @param paging  The pagination
	 * @param builder The builder for the inquiry
	 * @return The paged result
	 */
	Paged<SchedulerEntryPojo> findEntries(Pagination paging, BuildingConsumer<SchedulerEntryInquiryBuilder> builder);

	/**
	 * Find any scheduler entry
	 *
	 * @param builder The builder for the inquiry
	 * @return The found scheduler entry
	 */
	Optional<SchedulerEntryPojo> findAnyEntry(BuildingConsumer<SchedulerEntryInquiryBuilder> builder);

	/**
	 * delete the scheduler import
	 *
	 * @param builder The builder for the inquiry
	 * @return The number of deleted scheduler imports
	 */
	int deleteSchedulerImport(BuildingConsumer<SchedulerImportInquiryBuilder> builder);

	/**
	 * Builders for scheduler import inquiries
	 */
	interface SchedulerImportInquiryBuilder {

		/**
		 * Filter by project ID
		 *
		 * @param projectId The project ID
		 * @return this instance for method chaining
		 */
		SchedulerImportInquiryBuilder ofProject(EntityId projectId);

		/**
		 * Filter by scheduler import ID
		 *
		 * @param schedulerImportId The scheduler import ID
		 * @return this instance for method chaining
		 */
		SchedulerImportInquiryBuilder byId(UUID schedulerImportId);

	}

	/**
	 * Builder for scheduler entry inquiries
	 */
	interface SchedulerEntryInquiryBuilder {

		/**
		 * Filter by project ID
		 *
		 * @param projectId The project ID
		 * @return this instance for method chaining
		 */
		SchedulerEntryInquiryBuilder ofProject(EntityId projectId);

		/**
		 * Filter by scheduler import ID
		 *
		 * @param schedulerImportId The scheduler import ID
		 * @return this instance for method chaining
		 */
		SchedulerEntryInquiryBuilder ofSchedulerImportId(UUID schedulerImportId);

		/**
		 * Filter by scheduler entry ID
		 *
		 * @param schedulerEntryId The scheduler entry ID
		 * @return this instance for method chaining
		 */
		SchedulerEntryInquiryBuilder byId(UUID schedulerEntryId);

		/**
		 * Filter by module IDs that are linked to an entry
		 *
		 * @param moduleIds The module IDs
		 * @return this instance for method chaining
		 */
		SchedulerEntryInquiryBuilder withModules(EntityId... moduleIds);

/**
		 * Filter by scheduler entry type
		 *
		 * @param type The scheduler entry type
		 * @return this instance for method chaining
		 */
		SchedulerEntryInquiryBuilder withType(SchedulerEntryType type);

		/**
		 * Filter by identifier
		 *
		 * @param identifier the identifier
		 * @return this instance for method chaining
		 */
		SchedulerEntryInquiryBuilder withIdentifier(String identifier);

		/**
		 * Filter by content's key and value
		 *
		 * @param key the key of the content json
		 * @param value the value of the content json
		 * @return this instance for method chaining
		 */
		SchedulerEntryInquiryBuilder withContent(String key, String value);
	}

	/**
	 * Builder for scheduler entry relationship inquiries
	 */
	interface SchedulerEntryRelationshipInquireBuilder {

		/**
		 * Filter relationships by project ID
		 *
		 * @param projectId The project ID
		 * @return this instance for method chaining
		 */
		SchedulerEntryRelationshipInquireBuilder ofProject(EntityId projectId);

		/**
		 * Filter relationships by scheduler import ID
		 *
		 * @param importId The scheduler import ID
		 * @return this instance for method chaining
		 */
		SchedulerEntryRelationshipInquireBuilder ofImportId(UUID importId);
	}
}
