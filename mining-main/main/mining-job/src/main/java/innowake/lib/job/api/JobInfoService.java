/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import static innowake.mining.shared.access.FilterUtil.toInstant;
import static innowake.mining.shared.access.FilterUtil.toUuid;
import static innowake.mining.shared.access.FilterUtil.toUuids;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.UUID;

import javax.persistence.EntityNotFoundException;

import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.entities.JobInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Functions for accessing {@code job_info} database entities.
 */
public interface JobInfoService {

	interface JobInfoOrderBuilder<B extends JobInfoOrderBuilder<B>> {

		/**
		 * Sorts {@code job_info} entities by {@code name} in the given {@code direction}.
		 * @param direction the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortName(SortDirection direction);

		/**
		 * Sorts {@code job_info} entities by {@code submit_time} in the given {@code direction}.
		 * @param direction the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortSubmitTime(SortDirection direction);

		/**
		 * Generic sort method for any {@link JobInfoFieldName} for sorting in the given {@code direction}.
		 * 
		 * @param field the {@link JobInfoFieldName} to sort by.
		 * @param direction the {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		B sortBy(JobInfoFieldName field, SortDirection direction);

		/**
		 * Default sort method that applies the sorting from the given {@code sortObject}.
		 * 
		 * @param sortObject list of sort mappings. Each sort mapping contains a {@link JobInfoFieldName} and a {@link SortDirection}.
		 * @return this instance for method chaining
		 */
		@SuppressWarnings("unchecked")
		default B withSortObject(List<Map<JobInfoFieldName, SortDirection>> sortObject) {
			for (var map : sortObject) {
				Entry<JobInfoFieldName, SortDirection> entry = map.entrySet().iterator().next();
				sortBy(entry.getKey(), entry.getValue());
			}
			return (B) this;
		}
	}

	interface JobInfoInquiryBuilder extends JobInfoOrderBuilder<JobInfoInquiryBuilder> {

		/**
		 * Filters {@code job_info} entities by id.
		 * 
		 * @param id the id to filter by
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder byId(UUID id);

		/**
		 * Filters {@code job_info} entities by ids.
		 * 
		 * @param ids the list of ids to filter by
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder byIds(Collection<UUID> ids);

		/**
		 * Filters {@code job_info} entities by name.
		 * 
		 * @param name the name to filter by
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withName(String name);

		/**
		 * Filters {@code job_info} entities by the id of the user that created it.
		 * 
		 * @param user the user to filter by
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withCreatedByUserId(String user);

		/**
		 * Filters {@code job_info} entities by description.
		 * 
		 * @param description the description to filter by
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withDescription(String description);

		/**
		 * Filters {@code job_info} entities by submit time.
		 * 
		 * @param time the submit time to filter by
		 * @param comperator the filter comperator
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withSubmitTime(Comperator comperator, Instant time);

		/**
		 * Filters {@code job_info} entities by scheduled start time.
		 * 
		 * @param time the scheduled start time to filter by
		 * @param comperator the filter comperator
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withScheduledStartTime(Comperator comperator, Instant time);

		/**
		 * Filters {@code job_info} entities by start time.
		 * 
		 * @param time the start time to filter by
		 * @param comperator the filter comperator
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withStartTime(Comperator comperator, Instant time);

		/**
		 * Filters {@code job_info} entities by finish time.
		 * 
		 * @param time the finish time to filter by
		 * @param comperator the filter comperator
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withFinishTime(Comperator comperator, Instant time);

		/**
		 * Filters {@code job_info} entities by {@link JobStatus}.
		 * 
		 * @param status the {@link JobStatus} to filter by
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withStatus(JobStatus status);

		/**
		 * Filters {@code job_info} entities whose status matches with any of the {@link JobStatus} in the given {@code status} list.
		 * 
		 * @param status the list of {@link JobStatus} to filter by
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder withStatus(Collection<JobStatus> status);

		/**
		 * Filters {@code job_info} entities whose status does not matche with any of the {@link JobStatus} in the given {@code status} list.
		 * 
		 * @param status the list of {@link JobStatus} to filter by
		 * @return this instance for method chaining
		 */
		JobInfoInquiryBuilder notWithStatus(Collection<JobStatus> status);

		/**
		 * Default filter method that applies the filtering from the given {@code filterObject}.
		 * <p><b>Please note</b> that only 'EQ' ({@linkplain FilterOperators#OPERATOR_EQ}) is supported for filtering.</p>
		 * 
		 * @param filterObject map of filter mappings. Each filter mapping contains a {@link JobInfoFieldName} and a mapping of {@link FilterOperators} to
		 * {@code filter value}.
		 * @return this instance for method chaining
		 */
		default JobInfoInquiryBuilder withFilterObject(final Map<JobInfoFieldName, Map<String, Object>> filterObject) {
			filterObject.forEach((fieldName, filter) -> filter.forEach((operator, value) -> {
				switch (fieldName) {
					case ID:
						switch (operator) {
							case FilterOperators.OPERATOR_EQ:
								byId(toUuid(value, "ID"));
								break;
							case FilterOperators.OPERATOR_IN:
								byIds(toUuids(value, "ID"));
								break;
							default:
								throw new UnsupportedOperationException("Operator " + operator + " not supported for JobInfo ID filtering.");
						}
						break;
					case NAME:
						if (FilterOperators.OPERATOR_EQ.equals(operator)) {
							withName(value.toString());
						} else {
							throw new UnsupportedOperationException("Operator " + operator + " not supported for JobInfo name filtering.");
						}
						break;
					case CREATED_BY_USER_ID:
						if (FilterOperators.OPERATOR_EQ.equals(operator)) {
							withCreatedByUserId(value.toString());
						} else {
							throw new UnsupportedOperationException("Operator " + operator + " not supported for JobInfo user filtering.");
						}
						break;
					case STATUS:
						if (FilterOperators.OPERATOR_EQ.equals(operator)) {
							withStatus(value instanceof JobStatus ? (JobStatus) value : JobStatus.valueOf(value.toString()));
						}
						break;
					case SUBMIT_TIME:
						withSubmitTime(Comperator.convert(operator), toInstant(value));
						break;
					case SCHEDULED_START_TIME:
						withScheduledStartTime(Comperator.convert(operator), toInstant(value));
						break;
					case START_TIME:
						withStartTime(Comperator.convert(operator), toInstant(value));
						break;
					case FINISH_TIME:
						withFinishTime(Comperator.convert(operator), toInstant(value));
						break;
					default:
						throw new UnsupportedOperationException(String.format("JobInfo field %s not supported", fieldName));
				}
			}));
			return this;
		}
	}

	/**
	 * Creates a new {@code job_info} entity if it doesn't exist yet or updates the existing one.
	 * <p>Does not store the job result. Call {@link #createJobResult(UUID, byte[])} to store job results.</p>
	 * <p>Does not store the job messages. Use {@link #createJobMessages(UUID, List)} or {@link #addJobMessages(UUID, List)} to store messages.</p>
	 * @param jobInfo the {@link JobInfoPojoPrototype} to create
	 * @return the {@link UUID} of the new {@code job_info} entity
	 */
	UUID upsert(JobInfoPojoPrototype jobInfo);

	/**
	 * Updates an existing {@code job_info} entity.
	 * <p>Does not store the job result. Call {@link #createJobResult(UUID, byte[])} to store job results.</p>
	 * <p>Does not store the job messages. Use {@link #createJobMessages(UUID, List)} or {@link #addJobMessages(UUID, List)} to store messages.</p>
	 *
	 * @param jobInfo the {@link JobInfoPojoPrototype} to update
	 * @return the {@link UUID} of the new {@code job_info} entity
	 */
	UUID update(JobInfoPojoPrototype jobInfo);

	/**
	 * Creates new {@code job_info_results} entity for the job result for the given job {@code id}.
	 * <p>A {@code job_info} entity with the {@code id} in the given {@code jobInfo} must exist.</p>
	 *
	 * @param id the id of the job for which to create the job result entity
	 * @param result the job result to store
	 * @return the number of created job results.
	 */
	int createJobResult(UUID id, byte[] result);

	/**
	 * Performance optimized method for fast inserts. Call this method only, if no messages exist yet in the DB for the given job {@code id}. If job messages
	 * already exists then call {@link #addJobMessages(UUID, List)}.
	 * <p>Creates new {@code job_info_messages} entities for the given job {@code id}.</p>
	 * <p>A {@code job_info} entity with the {@code id} in the given {@code jobInfo} must exist.</p>
	 *
	 * @param id the id of the job for which to create the messages
	 * @param messages the messages to create
	 * @return the number of messages.
	 */
	int createJobMessages(UUID id, List<Message> messages);

	/**
	 * Creates new {@code job_info_messages} entities for the given job {@code id}.
	 * <p>A {@code job_info} entity with the {@code id} in the given {@code jobInfo} must exist.</p>
	 *
	 * @param id the id of the job for which to create the messages
	 * @param messages the messages to create
	 * @return the number of messages.
	 */
	int addJobMessages(UUID id, List<Message> messages);

	/**
	 * Returns all {@code job_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain JobInfoPojo JobInfoPojos}
	 */
	List<JobInfoPojo> find(BuildingConsumer<JobInfoInquiryBuilder> builder);

	/**
	 * Returns paged subset of optionally filtered {@code job_info} entities that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain JobInfoPojo JobInfoPojos}
	 */
	Paged<JobInfoPojo> find(Pagination paging, BuildingConsumer<JobInfoInquiryBuilder> builder);

	/**
	 * Returns the first {@code job_info} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return matching {@linkplain JobInfoPojo}
	 */
	Optional<JobInfoPojo> findAny(BuildingConsumer<JobInfoInquiryBuilder> builder);

	/**
	 * Returns the {@code job_info} entity whose id matches with the specified ID or.
	 * 
	 * @param id {@link UUID} of the job info to be found.
	 * @return the matching {@link JobInfoPojo}
	 * @throws EntityNotFoundException If no match was found.
	 */
	JobInfoPojo get(UUID id);

	/**
	 * Deletes all {@code job_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria.
	 * @return number of deleted {@code job_info} entities
	 */
	int delete(BuildingConsumer<JobInfoInquiryBuilder> builder);
}
