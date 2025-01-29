/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.JobInfoService.JobInfoInquiryBuilder;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.entities.JobInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;
/**
 * Postgres specific access methods for the {@code job_info} entity.
 */
public class JobInfoPgDao extends PgDao {

	public class JobInfoQueryBuilder implements JobInfoInquiryBuilder {

		private final FilterStreamBuilder filters = new FilterStreamBuilder();
		private final OrderStreamBuilder orders = new OrderStreamBuilder();

		protected Paged.Builder<JobInfoPojo> build(@Nullable final Pagination paging) {
			return query("SELECT id,"														/* 1 */
						+ "name,"															/* 2 */
						+ "description,"													/* 3 */
						+ "step_description,"												/* 4 */
						+ "status,"															/* 5 */
						+ "array(SELECT to_jsonb(msgs) FROM (SELECT ordinal, text, severity FROM job_info_messages m WHERE m.id = job_info.id) as msgs),"		/* 6 */
						+ "(SELECT content FROM job_info_results WHERE job_info_results.id = job_info.id) result,"												/* 7 */
						+ "pending_tasks,"													/* 8 */
						+ "total_work_units,"												/* 9 */
						+ "processed_work_units,"											/* 10 */
						+ "submit_time,"													/* 11 */
						+ "scheduled_start_time,"											/* 12 */
						+ "start_time,"														/* 13 */
						+ "finish_time,"													/* 14 */
						+ "created_by "														/* 15 */
						+ "FROM job_info")
					.with(filters::build)
					.toPageable(paging, (rs, row) -> new JobInfoPojo(
															(UUID) rs.getObject(1),										/* id */
															rs.getString(2),											/* name */
															rs.getString(3),											/* description */
															rs.getString(4),											/* step_description */
															mapNullable(rs.getString(5), JobStatus::valueOf),			/* status */
															PgUtil.<Object>streamArray(rs.getArray(6))
																		.map(m -> PgJSON.fromPGobject(m, Message.class))
																		.collect(Collectors.toUnmodifiableList()),		/* messages */
															rs.getBytes(7),												/* result */
															rs.getInt(8),												/* pending_tasks */
															rs.getInt(9),												/* total_work_units */
															rs.getDouble(10),											/* processed_work_units */
															mapNullable(rs.getTimestamp(11), Timestamp::toInstant),		/* submit_time */
															mapNullable(rs.getTimestamp(12), Timestamp::toInstant),		/* scheduled_start_time */
															mapNullable(rs.getTimestamp(13), Timestamp::toInstant),		/* start_time */
															mapNullable(rs.getTimestamp(14), Timestamp::toInstant),		/* finish_time */
															rs.getString(15)));
		}

		protected QueryBuilder buildIds() {
			return query("SELECT id FROM job_info")
					.with(filters::build);
		}
		
		protected QueryBuilder buildDelete() {
			return query("DELETE FROM job_info")
					.with(filters::build);
		}

		@Override
		public JobInfoQueryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("job_info.id = ?", id));
			return this;
		}

		@Override
		public JobInfoQueryBuilder byIds(final Collection<UUID> ids) {
			filters.accept(q -> q.append("job_info.id = any(?)", arrayFromCollection(PgType.UUID, ids)));
			return this;
		}

		@Override
		public JobInfoQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("job_info.name = ?", name));
			return this;
		}

		@Override
		public JobInfoQueryBuilder withCreatedByUserId(final String createdByUserId) {
			filters.accept(q -> q.append("job_info.created_by = ?", createdByUserId));
			return this;
		}

		@Override
		public JobInfoQueryBuilder withDescription(final String description) {
			filters.accept(q -> q.append("job_info.description = ?", description));
			return this;
		}

		@Override
		public JobInfoQueryBuilder withSubmitTime(final Comperator comperator, final Instant time) {
			switch (comperator) {
				case EQUAL:
					filters.accept(q -> q.append("job_info.submit_time = ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case GREATER:
					filters.accept(q -> q.append("job_info.submit_time > ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case GREATER_OR_EQUAL:
					filters.accept(q -> q.append("job_info.submit_time >= ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case LESSER:
					filters.accept(q -> q.append("job_info.submit_time < ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case LESSER_OR_EQUAL:
					filters.accept(q -> q.append("job_info.submit_time <= ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case UNEQUAL:
					filters.accept(q -> q.append("job_info.submit_time != ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				default:
					throw new UnsupportedOperationException("Operator " + comperator.operator() + " not supported for filtering submit time.");
			}
			return this;
		}

		@Override
		public JobInfoQueryBuilder withScheduledStartTime(final Comperator comperator, final Instant time) {
			switch (comperator) {
				case EQUAL:
					filters.accept(q -> q.append("job_info.scheduled_start_time = ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case GREATER:
					filters.accept(q -> q.append("job_info.scheduled_start_time > ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case GREATER_OR_EQUAL:
					filters.accept(q -> q.append("job_info.scheduled_start_time >= ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case LESSER:
					filters.accept(q -> q.append("job_info.scheduled_start_time < ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case LESSER_OR_EQUAL:
					filters.accept(q -> q.append("job_info.scheduled_start_time <= ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case UNEQUAL:
					filters.accept(q -> q.append("job_info.scheduled_start_time != ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				default:
					throw new UnsupportedOperationException("Operator " + comperator.operator() + " not supported for filtering scheduled start time.");
			}
			return this;
		}

		@Override
		public JobInfoQueryBuilder withStartTime(final Comperator comperator, final Instant time) {
			switch (comperator) {
				case EQUAL:
					filters.accept(q -> q.append("job_info.start_time = ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case GREATER:
					filters.accept(q -> q.append("job_info.start_time > ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case GREATER_OR_EQUAL:
					filters.accept(q -> q.append("job_info.start_time >= ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case LESSER:
					filters.accept(q -> q.append("job_info.start_time < ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case LESSER_OR_EQUAL:
					filters.accept(q -> q.append("job_info.start_time <= ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case UNEQUAL:
					filters.accept(q -> q.append("job_info.start_time != ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				default:
					throw new UnsupportedOperationException("Operator " + comperator.operator() + " not supported for filtering start time.");
			}
			return this;
		}

		@Override
		public JobInfoQueryBuilder withFinishTime(final Comperator comperator, final Instant time) {
			switch (comperator) {
				case EQUAL:
					filters.accept(q -> q.append("job_info.finish_time = ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case GREATER:
					filters.accept(q -> q.append("job_info.finish_time > ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case GREATER_OR_EQUAL:
					filters.accept(q -> q.append("job_info.finish_time >= ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case LESSER:
					filters.accept(q -> q.append("job_info.finish_time < ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case LESSER_OR_EQUAL:
					filters.accept(q -> q.append("job_info.finish_time <= ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				case UNEQUAL:
					filters.accept(q -> q.append("job_info.finish_time != ?::timestamp_zoned_milli", Timestamp.from(time)));
					break;
				default:
					throw new UnsupportedOperationException("Operator " + comperator.operator() + " not supported for filtering finish time.");
			}
			return this;
		}

		@Override
		public JobInfoQueryBuilder withStatus(final JobStatus status) {
			filters.accept(q -> q.append("job_info.status = ?", status.name()));
			return this;
		}

		@Override
		public JobInfoQueryBuilder withStatus(final Collection<JobStatus> status) {
			filters.accept(q -> q.append("job_info.status = any(?)").addArg(PgType.STRING, status.stream()
																								 .map(JobStatus::name)
																								 .collect(Collectors.toList())));
			return this;
		}

		@Override
		public JobInfoQueryBuilder notWithStatus(final Collection<JobStatus> status) {
			filters.accept(q -> q.append("NOT job_info.status = any(?)").addArg(PgType.STRING, status.stream()
																									 .map(JobStatus::name)
																									 .collect(Collectors.toList())));
			return this;
		}

		@Override
		public JobInfoQueryBuilder sortName(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("job_info.name", direction));
			return this;
		}

		@Override
		public JobInfoQueryBuilder sortSubmitTime(final SortDirection direction) {
			orders.accept(q -> q.appendOrder("job_info.submit_time", direction));
			return this;
		}

		@Override
		public JobInfoQueryBuilder sortBy(final JobInfoFieldName field, final SortDirection direction) {
			switch (field) {
				case ID:
				case NAME:
				case DESCRIPTION:
				case STEP_DESCRIPTION:
				case STATUS:
				case PENDING_TASKS:
				case TOTAL_WORK_UNITS:
				case PROCESSED_WORK_UNITS:
				case SUBMIT_TIME:
				case SCHEDULED_START_TIME:
				case START_TIME:
				case FINISH_TIME:
				case CREATED_BY_USER_ID:
					orders.accept(q -> q.append("job_info.").appendOrder(field.name().toLowerCase(), direction));
					break;
				default:
					throw new UnsupportedOperationException("JobInfo field " + field + " not supported for sorting");
			}

			return this;
		}
	}

	/**
	 * Creates a new {@code job_info} data access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public JobInfoPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Creates a new {@code job_info} entity or updates the existing one.
	 * <p>Does not store the job result. Call {@link #createJobResult(UUID, byte[])} to store job results.</p>
	 * <p>Does not store the job messages. Use {@link #createJobMessages(UUID, List)} or {@link #addJobMessages(UUID, List)} to store messages.</p>
	 *
	 * @param jobInfo the {@link JobInfoPojoPrototype} to create
	 * @param upsert {@code true} for upsert, {@code false} to update an existing entity
	 * @return the id of the created or updated {@code job_info} entities.
	 */
	public UUID put(final JobInfoPojoPrototype jobInfo, final boolean upsert) {
		final var fields = new FieldBuilder()
				.add(jobInfo.name.required(upsert), "name", "?")
				.add(jobInfo.description, "description", "?")
				.add(jobInfo.stepDescription, "step_description", "?")
				.add(jobInfo.status, "status", "?", JobStatus::name)
				.add(jobInfo.pendingTasks, "pending_tasks", "?")
				.add(jobInfo.totalWorkUnits, "total_work_units", "?")
				.add(jobInfo.processedWorkUnits, "processed_work_units", "?")
				.add(jobInfo.submitTime, "submit_time", "?", passNull(Timestamp::from))
				.add(jobInfo.scheduledStartTime, "scheduled_start_time", "?", passNull(Timestamp::from))
				.add(jobInfo.startTime, "start_time", "?", passNull(Timestamp::from))
				.add(jobInfo.finishTime, "finish_time", "?", passNull(Timestamp::from))
				.add(jobInfo.createdByUserId.required(upsert), "created_by", "?");

		final QueryBuilder query;
		if (upsert) {
			final UUID id = jobInfo.id.orElseNonNull(UUID::randomUUID);
			fields.add("id", "?", id);
			query = query("INSERT INTO job_info ");
			fields.buildUpsert(query, "id");
		} else {
			final UUID id = jobInfo.id.getNonNull();
			query = query("UPDATE job_info SET ")
					.with(fields::buildUpdate)
					.append(" WHERE id = ?", id);
		}

		return query.append(" RETURNING id")
							 .first(rs -> (UUID) rs.getObject(1))
							 .orElseThrow(() -> new MiningEntityNotFoundException(JobInfoPojo.class, jobInfo.toString()));
	}

	/**
	 * Performance optimized method for fast inserts. Call this method only, if no messages exist yet in the DB for the given job {@code id}.
	 * <p>Creates new {@code job_info_messages} entities for the given job {@code id}.</p>
	 * <p>A {@code job_info} entity with the {@code id} in the given {@code jobInfo} must exist.</p>
	 *
	 * @param id the id of the job for which to create the messages
	 * @param messages the messages to create
	 * @return the number of messages.
	 */
	public int createJobMessages(final UUID id, final List<Message> messages) {
		if (messages.isEmpty()) {
			return 0;
		}

		final AtomicInteger ordinal = new AtomicInteger(1);
		final var insertArgs = messages.stream()
									.filter(message -> message.getOrdinal() < 1)
									.map(message -> Stream.<Object>of(id, ordinal.getAndIncrement(), message.getText(), message.getSeverity().name()));

		query("INSERT INTO job_info_messages VALUES (?, ?, ?, ?)")
					.updateBatch(insertArgs, 1_000);
		return messages.size();
	}

	/**
	 * Creates new {@code job_info_messages} entities for the given job {@code id}.
	 * <p>A {@code job_info} entity with the {@code id} in the given {@code jobInfo} must exist.</p>
	 *
	 * @param id the id of the job for which to create the messages
	 * @param messages the messages to create
	 * @return the number of messages.
	 */
	public int addJobMessages(final UUID id, final List<Message> messages) {
		if (messages.isEmpty()) {
			return 0;
		}

		final var insertArgs = messages.stream()
									.filter(message -> message.getOrdinal() < 1)
									.map(message -> Stream.<Object>of(id, id, message.getText(), message.getSeverity().name()));

		query("INSERT INTO job_info_messages VALUES (?, (SELECT COALESCE(max(ordinal) + 1, 1) FROM job_info_messages WHERE id = ?), ?, ?)")
					.updateBatch(insertArgs, 1_000);

		return messages.size();
	}

	/**
	 * Creates new {@code job_info_results} entity for the job result for the given job {@code id}.
	 * <p>A {@code job_info} entity with the {@code id} in the given {@code jobInfo} must exist.</p>
	 *
	 * @param id the id of the job for which to create the job result entity
	 * @param result the job result to store
	 * @return the number of created job results.
	 */
	public int createJobResult(final UUID id, final byte[] result) {
		final var fields = new FieldBuilder()
				.add("id", "?", id)
				.add("name", "?", "$result")
				.add("type", "?", "application/x-java-object")
				.add("class", "?", "innowake.lib.job.api.Result")
				.add("content", "?", result);

		return query("INSERT INTO job_info_results ")
									.with(fields::buildInsert)
									.update();
	}

	/**
	 * Deletes all {@code job_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code job_info} entities
	 */
	public int delete(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return builder.prepare(new JobInfoQueryBuilder())
						.buildDelete()
						.update();
	}

	/**
	 * Returns all {@code job_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain JobInfoPojo JobInfoPojos}
	 */
	public List<JobInfoPojo> find(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return builder.prepare(new JobInfoQueryBuilder())
						.build(null)
						.all();
	}

	/**
	 * Returns all {@code job_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain JobInfoPojo JobInfoPojos}
	 */
	public List<UUID> findIds(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return builder.prepare(new JobInfoQueryBuilder())
				.buildIds()
				.toList((rs, n) -> (UUID) rs.getObject(1));
	}

	/**
	 * Returns paged subset of optionally filtered {@code job_info} entities that match with the filters in the given {@code builder}.
	 * 
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain JobInfoPojo JobInfoPojos}
	 */
	public Paged<JobInfoPojo> find(final Pagination paging, final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return builder.prepare(new JobInfoQueryBuilder())
						.build(paging)
						.page();
	}

	/**
	 * Returns the first {@code job_info} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain JobInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return first matching {@linkplain JobInfoPojo}
	 */
	public Optional<JobInfoPojo> findAny(final BuildingConsumer<JobInfoInquiryBuilder> builder) {
		return builder.prepare(new JobInfoQueryBuilder())
				.build(Pagination.FIRST)
				.first();
	}

}
