/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.persistence.PersistenceException;

import innowake.mining.shared.entities.scheduler.SchedulerImportPojo;
import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SchedulerInfoService.SchedulerEntryInquiryBuilder;
import innowake.mining.shared.access.SchedulerInfoService.SchedulerEntryRelationshipInquireBuilder;
import innowake.mining.shared.access.SchedulerInfoService.SchedulerImportInquiryBuilder;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerType;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Data access object for scheduler information.
 */
public class SchedulerInfoPgDao extends PgDao {

	private static final int INSERT_BATCH_SIZE = 1_000;

	/**
	 * Builder for scheduler import inquiries.
	 */
	public class SchedulerImportQueryBuilder implements SchedulerImportInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected QueryBuilder delete() {
			return query("DELETE FROM scheduler_import ").with(filters::build);
		}

		protected QueryBuilder buildSelect() {
			return query("SELECT * FROM scheduler_import" + " ").with(filters::build);
		}

		protected Optional<SchedulerImportPojo> selectFirst() {
			return buildSelect().first(rs -> new SchedulerImportPojo((UUID) rs.getObject(1),
					(UUID) rs.getObject(2),
					SchedulerType.valueOf(rs.getString(3)),
					rs.getString(4),
					rs.getString(5),
					(UUID) rs.getObject(6),
					rs.getString(7),
					rs.getTimestamp(8).toInstant(),
					rs.getString(9),
					PgJSON.fromPGobject(rs.getObject(10))));
		}

		protected Paged.Builder<SchedulerImportPojo> build(@Nullable final Pagination paging) {
			return buildSelect().toPageable(paging, (rs, n) -> new SchedulerImportPojo((UUID) rs.getObject(1),
					(UUID) rs.getObject(2),
					SchedulerType.fromName(rs.getString(3)),
					rs.getString(4),
					rs.getString(5),
					(UUID) rs.getObject(6),
					rs.getString(7),
					rs.getTimestamp(8).toInstant(),
					rs.getString(9),
					PgJSON.fromPGobject(rs.getObject(10))));
		}

		@Override
		public SchedulerImportInquiryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append(" project = ")
					.with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}

		@Override
		public SchedulerImportInquiryBuilder byId(final UUID schedulerImportId) {
			filters.accept(q -> q.append("uid = ?", schedulerImportId));
			return this;
		}
	}

	/**
	 * Builder for scheduler entry inquiries.
	 */
	public class SchedulerEntryQueryBuilder implements SchedulerEntryInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		@SuppressWarnings("unchecked")
		protected Paged.Builder<SchedulerEntryPojo> buildSelect(@Nullable final Pagination paging) {
			return query("SELECT uid, type, scheduler_import, content, identifier, module, contained_in FROM scheduler_entry ").with(
							filters::build)
					.toPageable(paging,
							(rs, n) -> new SchedulerEntryPojo((UUID) rs.getObject(1), (UUID) rs.getObject(7), (UUID) rs.getObject(6), rs.getString(5),
									(UUID) rs.getObject(3), SchedulerEntryType.fromName(rs.getString(2)),
									PgJSON.fromPGobject(rs.getObject(4), Map.class)));
		}

		@Override
		public SchedulerEntryInquiryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append(" scheduler_import IN (SELECT uid FROM scheduler_import WHERE project = ")
					.with(ProjectPgDao.referenceUidOrNid(projectId))
					.append(")"));
			return this;
		}

		@Override
		public SchedulerEntryInquiryBuilder ofSchedulerImportId(final UUID schedulerImportId) {
			filters.accept(q -> q.append("scheduler_import = ?", schedulerImportId));
			return this;
		}

		@Override
		public SchedulerEntryInquiryBuilder byId(final UUID schedulerEntryId) {
			filters.accept(q -> q.append("uid = ?", schedulerEntryId));
			return this;
		}

		@Override
		public SchedulerEntryInquiryBuilder withModules(final EntityId... moduleIds) {
			final List<EntityId> moduleIdsList = Arrays.asList(moduleIds);
			final boolean allHaveUid = moduleIdsList.stream().allMatch(EntityId::hasUid);
			boolean allHaveNid = false;
			if ( ! allHaveUid) {
				allHaveNid = moduleIdsList.stream().allMatch(EntityId::hasNid);
			}
			if ( ! (allHaveUid || allHaveNid)) {
				throw new IllegalArgumentException("You can not mix UUIDs and numerical ids in the call to withModules()");
			}
			if (allHaveUid) {
				filters.accept(q -> q.append("module = any(?)")
						.addArg(PgType.UUID, moduleIdsList.stream().map(EntityId::getUid).collect(Collectors.toList())));
			} else {
				filters.accept(q -> q.append("module IN (SELECT uid FROM module WHERE nid = any(?))")
						.addArg(PgType.LONG, moduleIdsList.stream().map(EntityId::getNid).collect(Collectors.toList())));
			}
			return this;
		}

		@Override
		public SchedulerEntryInquiryBuilder withType(final SchedulerEntryType type) {
			filters.accept(q -> q.append("type = ?::scheduler_entry_type", type.getValue()));
			return this;
		}

		@Override
		public SchedulerEntryInquiryBuilder withIdentifier(final String identifier) {
			filters.accept(q -> q.append("identifier = ?", identifier));
			return this;
		}

		@Override
		public SchedulerEntryInquiryBuilder withContent(final String key, final String value) {
			filters.accept(q -> q.append("content ->> ? = ?", key, value));
			return this;
		}
	}

	/**
	 * Builder for scheduler entry relationship inquiries.
	 */
	public class SchedulerEntryRelationshipQueryBuilder implements SchedulerEntryRelationshipInquireBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected Paged.Builder<SchedulerEntryRelationshipPojo> buildSelect(@Nullable final Pagination paging) {
			return query("SELECT r.uid, r.predecessor, r.successor, r.is_ok, r.identifier, p.module, s.module FROM scheduler_entry s JOIN scheduler_entry_relationship r "
					+ "ON s.uid = r.successor JOIN scheduler_entry p ON r.predecessor = p.uid ").with(filters::build)
					.toPageable(paging, (rs, n) -> new SchedulerEntryRelationshipPojo((UUID) rs.getObject(1), (UUID) rs.getObject(2), (UUID) rs.getObject(3),
							rs.getBoolean(4), rs.getString(5), (UUID) rs.getObject(6), (UUID) rs.getObject(7)));
		}

		@Override
		public SchedulerEntryRelationshipInquireBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append(" s.scheduler_import IN (SELECT i.uid FROM scheduler_import i WHERE i.project = ")
					.with(ProjectPgDao.referenceUidOrNid(projectId))
					.append(")"));
			return this;
		}

		@Override
		public SchedulerEntryRelationshipInquireBuilder ofImportId(final UUID importId) {
			filters.accept(q -> q.append(" s.scheduler_import = ?", importId));
			return this;
		}
	}

	public SchedulerInfoPgDao(final JdbcTemplate jdbc) {
		super(jdbc);
	}

	/**
	 * Creates a scheduler import.
	 *
	 * @param schedulerImport the scheduler import
	 * @return the UID of the created scheduler import
	 */
	public UUID createSchedulerImport(final SchedulerImportPojoPrototype schedulerImport) {
		final QueryBuilder query = query("INSERT INTO scheduler_import");
		final var uid = UUID.randomUUID();
		new FieldBuilder().add(schedulerImport.schedulerType, "scheduler_type", "?::scheduler_type", SchedulerType::getName)
				.add(schedulerImport.importedOn, "imported_on", "?", passNull(Timestamp::from))
				.add(schedulerImport.schedulerVersion, "scheduler_version", "?")
				.add(schedulerImport.identifier, "identifier", "?")
				.add(schedulerImport.source, "source", "?")
				.add("uid", "?", uid)
				.add(schedulerImport.project, "project", ProjectPgDao::referenceUidOrNid)
				.add(schedulerImport.importerUsed, "importer_used", "?")
				.add(schedulerImport.description, "description", "?")
				.add(schedulerImport.properties, "properties", props -> qb -> qb.append("?", PgJSON.toPGobject(props)))
				.buildInsert(query);

		return query.append(" RETURNING uid")
				.first(rs -> (UUID) rs.getObject(1))
				.orElseThrow(() -> new PersistenceException("Failed to create scheduler import: '" + schedulerImport));
	}

	/**
	 * Creates scheduler entry relationships.
	 *
	 * @param prototypes the scheduler entry relationship prototypes
	 * @return the UIDs of the created scheduler entry relationships
	 */
	public List<UUID> createSchedulerEntryRelationships(final List<SchedulerEntryRelationshipPojoPrototype> prototypes) {
		final List<UUID> uids = new ArrayList<>(prototypes.size());
		query("INSERT INTO scheduler_entry_relationship(uid, predecessor, successor, is_ok, identifier) VALUES (?, ?, ?, ?, ?)").updateBatch(prototypes.stream()
				.map(prototype -> {
					final UUID uid = prototype.uid.orElseNonNull(UUID::randomUUID);
					uids.add(uid);
					return Stream.<Object> of(prototype.uid.getNonNull(), prototype.predecessor.getNonNull(), prototype.successor.getNonNull(),
							prototype.isOk.orElseNonNull(Boolean.TRUE), prototype.identifier.orElse(null));
				}), INSERT_BATCH_SIZE);
		return uids;
	}

	/**
	 * Creates a scheduler entry.
	 *
	 * @param schedulerEntry the scheduler entry
	 * @return the UID of the created scheduler entry
	 */
	public UUID createSchedulerEntry(final SchedulerEntryPojoPrototype schedulerEntry) {
		final QueryBuilder query = query("INSERT INTO scheduler_entry");
		final var uid = UUID.randomUUID();
		new FieldBuilder().add("uid", "?", uid)
				.add(schedulerEntry.containedIn, "contained_in", "?")
				.add(schedulerEntry.module, "module", ModulePgDao::referenceUidOrNid)
				.add(schedulerEntry.identifier, "identifier", "?")
				.add(schedulerEntry.schedulerImport, "scheduler_import", "?")
				.add(schedulerEntry.type, "type", "?::scheduler_entry_type", SchedulerEntryType::getValue)
				.add(schedulerEntry.content, "content", content -> qb -> qb.append("?", PgJSON.toPGobject(content)))
				.buildInsert(query);

		return query.append(" RETURNING uid")
				.first(rs -> (UUID) rs.getObject(1))
				.orElseThrow(() -> new PersistenceException("Failed to create scheduler entry: '" + schedulerEntry));
	}

	/**
	 * Finds any scheduler entry for the given builder. This must be used when expecting a singleton result.
	 *
	 * @param builder the builder
	 * @return the scheduler entry
	 */
	public Optional<SchedulerEntryPojo> findAnySchedulerEntry(final BuildingConsumer<SchedulerEntryInquiryBuilder> builder) {
		return builder.prepare(new SchedulerEntryQueryBuilder())
				.buildSelect(Pagination.FIRST)
				.first();
	}

	/**
	 * Finds scheduler entry.
	 * 
	 * @param paging the page
	 * @param builder the builder
	 * @return the scheduler entries paged
	 */
	public Paged<SchedulerEntryPojo> findSchedulerEntries(final Pagination paging, final BuildingConsumer<SchedulerEntryInquiryBuilder> builder) {
		return builder.prepare(new SchedulerEntryQueryBuilder())
				.buildSelect(paging)
				.page();
	}

	/**
	 * Finds scheduler entry.
	 *
	 * @param builder the builder
	 * @return the list of scheduler entries
	 */
	public List<SchedulerEntryPojo> findSchedulerEntries(final BuildingConsumer<SchedulerEntryInquiryBuilder> builder) {
		return builder.prepare(new SchedulerEntryQueryBuilder())
				.buildSelect(null)
				.all();
	}

	/**
	 * Finds scheduler entry relationships.
	 * 
	 * @param paging the page
	 * @param builder the builder
	 * @return the scheduler entry relationships
	 */
	public Paged<SchedulerEntryRelationshipPojo> findRelationships(final Pagination paging,
			final BuildingConsumer<SchedulerEntryRelationshipInquireBuilder> builder) {
		return builder.prepare(new SchedulerEntryRelationshipQueryBuilder())
				.buildSelect(paging)
				.page();
	}

	/**
	 * Deletes scheduler import.
	 *
	 * @param builder the builder
	 * @return the number of affected rows
	 */
	public int deleteSchedulerImport(final BuildingConsumer<SchedulerImportInquiryBuilder> builder) {
		return builder.prepare(new SchedulerImportQueryBuilder())
				.delete()
				.update();
	}

	/**
	 * Finds scheduler import.
	 *
	 * @param builder the builder
	 * @return the scheduler import
	 */
	public Optional<SchedulerImportPojo> findSchedulerImport(final BuildingConsumer<SchedulerImportInquiryBuilder> builder) {
		return builder.prepare(new SchedulerImportQueryBuilder())
				.selectFirst();
	}

	/**
	 * Finds scheduler imports.
	 *
	 * @param paging the page
	 * @param builder the builder
	 * @return the scheduler imports
	 */
	public Paged<SchedulerImportPojo> findSchedulerImports(final Pagination paging, final BuildingConsumer<SchedulerImportInquiryBuilder> builder) {
		return builder.prepare(new SchedulerImportQueryBuilder()).build(paging).page();
	}

}
