/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.mining.shared.entities.FieldInfoPojo;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Functions for accessing entities such as {@code field_info} that are used by the database schema import.
 */
public interface FieldInfoService {

	interface FieldInfoInquiryBuilder {

		/**
		 * Filters {@code field_info} entities by {@code project} id.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		FieldInfoInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters {@code field_info} entities by {@code module} id.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		FieldInfoInquiryBuilder ofModule(EntityId module);

		/**
		 * Filters {@code field_info} entities by numeric {@code ordinal}.
		 * 
		 * @param ordinal the numeric ordinal of the field info
		 * @return this instance for method chaining
		 */
		FieldInfoInquiryBuilder withOrdinal(Integer ordinal);
	}

	/**
	 * Creates a new {@code field_info} entity.
	 *
	 * @param fieldInfo the {@link FieldInfoPojoPrototype} to create
	 * @return the ordinal of the created entity.
	 */
	UUID create(FieldInfoPojoPrototype fieldInfo);

	/**
	 * Creates a new {@code field_info} entity.
	 *
	 * @param fieldInfo the {@link FieldInfoPojoPrototype} to create
	 */
	void update(FieldInfoPojoPrototype fieldInfo);

	/**
	 * Deletes all {@code field_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain FieldInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted entities
	 */
	int delete(BuildingConsumer<FieldInfoInquiryBuilder> builder);

	/**
	 * Returns all {@code field_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain FieldInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain FieldInfoPojo FieldInfoPojos}
	 */
	List<FieldInfoPojo> find(BuildingConsumer<FieldInfoInquiryBuilder> builder);

	/**
	 * Returns paged subset of optionally filtered {@code field_info} entities that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain FieldInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching  {@linkplain FieldInfoPojo FieldInfoPojos}
	 */
	Paged<FieldInfoPojo> find(Pagination paging, BuildingConsumer<FieldInfoInquiryBuilder> builder);

	/**
	 * Returns the first {@code field_info} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain FieldInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return first matching {@linkplain FieldInfoPojo}
	 */
	Optional<FieldInfoPojo> findAny(BuildingConsumer<FieldInfoInquiryBuilder> builder);
}
