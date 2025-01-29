/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Triple;
import org.springframework.jdbc.core.JdbcTemplate;

import com.google.common.collect.Lists;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.DnaDataService.DnaSimilarityInquiryBuilder;
import innowake.mining.shared.access.DnaDataService.DnaStringElementInquiryBuilder;
import innowake.mining.shared.access.DnaDataService.DnaStringInquiryBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSimilarityPojo;
import innowake.mining.shared.entities.dna.DnaSimilarityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringElementPojo;
import innowake.mining.shared.entities.dna.DnaStringElementPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringPojo;
import innowake.mining.shared.entities.dna.DnaStringPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Postgres specific access methods for DNA similarity entities.
 */
public class DnaSimilarityPgDao extends PgDao {

	private static final int INSERT_BATCH_SIZE = 1_000;

	/**
	 * Creates a new DNA similarity access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database
	 */
	public DnaSimilarityPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Query builder for performing queries on {@code dna_string} entities.
	 */
	public class DnaStringBuilder implements DnaStringInquiryBuilder {

		private final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected List<UUID> build() {
			return query("SELECT module FROM dna_string s")
					.with(filters::build)
					.toList((rs, row) -> (UUID) rs.getObject(1));
		}

		protected long buildCount() {
			return query("SELECT COUNT(*) FROM dna_string s")
					.with(filters::build)
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElse(Long.valueOf(0l))
					.longValue();
		}

		@Override
		public DnaStringBuilder bySequencer(final DnaSequencer sequencer) {
			filters.accept(q -> q.append("s.sequencer = ?", sequencer.name()));
			return this;
		}

		@Override
		public DnaStringBuilder byModule(final EntityId moduleId) {
			filters.accept(q -> q.append("s.module = ").with(ModulePgDao.referenceUidOrNid(moduleId)));
			return this;
		}

		@Override
		public DnaStringBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("s.module IN (SELECT uid FROM module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(projectId)).append(")"));
			return this;
		}

		@Override
		public DnaStringBuilder withGenerated(final Instant generated) {
			filters.accept(q -> q.append("s.generated = ?::timestamp_zoned_milli", Timestamp.from(generated)));
			return this;
		}

		@Override
		public DnaStringBuilder withMinimumAmount(final Integer minimumAmount) {
			filters.accept(q -> q.append("(SELECT COUNT(*) FROM dna_string_element e WHERE e.module = s.module AND e.sequencer = s.sequencer) > ?", minimumAmount));
			return this;
		}
	}

	/**
	 * Query builder for performing queries on {@code dna_string_element} entities.
	 */
	public class DnaStringElementBuilder implements DnaStringElementInquiryBuilder {
		
		private final FilterStreamBuilder filters = new FilterStreamBuilder();
		
		protected List<Tuple2<UUID, String>> buildTuples() {
			return query("SELECT module, value FROM dna_string_element")
					.with(filters::build)
					.toList((rs, rowNum) -> Tuple2.of((UUID) rs.getObject(1), rs.getString(2)));
		}

		protected long buildCount() {
			return query("SELECT COUNT(*) FROM dna_string_element")
					.with(filters::build)
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElse(Long.valueOf(0l))
					.longValue();
		}
		
		@Override
		public DnaStringElementBuilder bySequencer(final DnaSequencer sequencer) {
			filters.accept(q -> q.append("sequencer = ?", sequencer.name()));
			return this;
		}
		
		@Override
		public DnaStringElementBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("module IN (SELECT uid FROM module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(projectId)).append(")"));
			return this;
		}

		@Override
		public DnaStringElementInquiryBuilder byModule(final EntityId moduleId) {
			filters.accept(q -> q.append("module = ").with(ModulePgDao.referenceUidOrNid(moduleId)));
			return this;
		}

		@Override
		public DnaStringElementInquiryBuilder byModules(final Collection<UUID> moduleUids) {
			filters.accept(q -> q.append("module = any(?)", arrayFromCollection(PgType.UUID, moduleUids)));
			return this;
		}
	}

	/**
	 * Query builder for performing queries on {@code dna_similarity} entities.
	 */
	public class DnaSimilarityBuilder implements DnaSimilarityInquiryBuilder {

		private final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected List<DnaSimilarityPojo> build() {
			/* SELECT * FROM dna_similarity s WHERE 
								s.sequencer = ?
							AND s.similarity_algo = ?
							AND s.a_module IN (SELECT uid FROM module WHERE project = ?)
							AND s.similarity > ?
							AND (SELECT COUNT(*) FROM dna_string_element e WHERE e.module = s.a_module AND e.sequencer = s.sequencer) > ?
							AND (SELECT COUNT(*) FROM dna_string_element e WHERE e.module = s.b_module AND e.sequencer = s.sequencer) > ? */
			try {
				final var queryBuilder = query("SELECT a_module, b_module, sequencer, similarity_algo, similarity FROM dna_similarity s")
						.with(filters::build);
				LOG.info("Generated SQL query: {}", queryBuilder.toString());
				return queryBuilder.toList((rs, row) -> new DnaSimilarityPojo(EntityId.of((UUID) rs.getObject(1)),	/* similarity a_module uid */
								EntityId.of((UUID) rs.getObject(2)),									/* similarity b_module uid */
								DnaSequencer.valueOf(rs.getString(3)),									/* similarity sequencer */
								DnaSimilarityAlgorithm.valueOf(rs.getString(4)),						/* similarity similarity_algo */
								rs.getDouble(5)));														/* similarity similarity */
			} catch (final Exception e) {
				LOG.error("Error executing SQL query", e);
				return Collections.emptyList();
			}
		}

		protected long buildCount() {
			return query("SELECT COUNT(*) FROM dna_similarity s")
						.with(filters::build)
						.first(rs -> Long.valueOf(rs.getLong(1)))
						.orElse(Long.valueOf(0l))
						.longValue();
		}

		@Override
		public DnaSimilarityBuilder bySequencer(final DnaSequencer sequencer) {
			filters.accept(q -> q.append("s.sequencer = ?", sequencer.name()));
			return this;
		}

		@Override
		public DnaSimilarityBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("s.a_module IN (SELECT uid FROM module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(projectId)).append(")"));
			return this;
		}

		@Override
		public DnaSimilarityBuilder byModule(final EntityId moduleId) {
			filters.accept(query -> query.append("s.a_module = ").with(ModulePgDao.referenceUidOrNid(moduleId))
									  .append(" OR s.b_module = ").with(ModulePgDao.referenceUidOrNid(moduleId)));
			return this;
		}

		@Override
		public DnaSimilarityInquiryBuilder withAlgorithm(final DnaSimilarityAlgorithm algorithm) {
			filters.accept(q -> q.append("s.similarity_algo = ?", algorithm.name()));
			return this;
		}

		@Override
		public DnaSimilarityInquiryBuilder withThreshold(final double similarityThreshold) {
			filters.accept(q -> q.append("s.similarity > ?", Double.valueOf(similarityThreshold)));
			return this;
		}

		@Override
		public DnaSimilarityInquiryBuilder withMinimumAmount(final int minAmount) {
			filters.accept(q -> q.append("(SELECT COUNT(*) FROM dna_string_element e WHERE e.module = s.a_module AND e.sequencer = s.sequencer) > ?",
											Integer.valueOf(minAmount)));
			filters.accept(q -> q.append("(SELECT COUNT(*) FROM dna_string_element e WHERE e.module = s.b_module AND e.sequencer = s.sequencer) > ?",
											Integer.valueOf(minAmount)));
			return this;
		}
	}

	/**
	 * Creates a new {@code dna_string} record out of the given {@code dnaString}.
	 *
	 * @param dnaString the {@linkplain DnaStringPojoPrototype} to create the record from.
	 */
	public void createDnaString(final DnaStringPojoPrototype dnaString) {
		final FieldBuilder fields = new FieldBuilder()
				.add("module", query -> query.with(ModulePgDao.referenceUidOrNid(dnaString.module.getNonNull())))
				.add(dnaString.sequencer.required(true), "sequencer", "?", DnaSequencer::name)
				.add(dnaString.generated.required(true), "generated", "?", Timestamp::from)
				.add(dnaString.contentHash.required(true), "content_hash", "?");

		final var queryBuilder = query("INSERT INTO dna_string ");
		fields.buildInsert(queryBuilder);
		queryBuilder.update();
	}

	/**
	 * Creates new {@code dna_string_element} records out of given {@code dnaString} and {@code dnaStringElements}.
	 *
	 * @param dnaString the {@linkplain DnaStringPojoPrototype} for which the records are created.
	 * @param dnaStringElements the list of {@linkplain DnaStringElementPojoPrototype DnaStringElementPojoPrototypes}he {@code dna_string_element} 
	 * records are created from
	 */
	public void createDnaStringElements(final DnaStringPojoPrototype dnaString, final List<DnaStringElementPojoPrototype> dnaStringElements) {
		final EntityId moduleId = dnaString.module.getNonNull();
		final DnaSequencer sequencer = dnaString.sequencer.getNonNull();

		final var batchArgs = dnaStringElements.stream().map(dnaStringElement -> Stream.<Object>of(
				dnaStringElement.index.getNonNull(),
				dnaStringElement.location.getNonNull().getOffset(),
				dnaStringElement.location.getNonNull().getLength(),
				dnaStringElement.value.getNonNull()
		));

		query("INSERT INTO dna_string_element(module, sequencer, index, location, value) VALUES (")
			.with(ModulePgDao.referenceUidOrNid(moduleId))
			.append(", ?, ?, (?, ?), ?)", sequencer.name())
			.updateBatch(batchArgs, INSERT_BATCH_SIZE);
	}

	/**
	 * Creates new {@code dna_similarity} records out of the given {@code dnaSimilarities}.
	 *
	 * @param dnaSimilarities a {@link DnaSimilarityPojoPrototype} to create
	 * @param batchConsumer a callback after each batch operation completes, that is called with the number of inserted records
	 */
	public void create(final List<DnaSimilarityPojoPrototype> dnaSimilarities, final Optional<Consumer<Integer>> batchConsumer) {
		final var createdRecords = new AtomicInteger(0);

		final var queryBuilder = query("INSERT INTO dna_similarity(a_module, b_module, sequencer, similarity_algo, similarity) VALUES (?, ?, ?, ?, ?)");

		Lists.partition(dnaSimilarities, INSERT_BATCH_SIZE).forEach(similarities -> {
			final var batchArgs = similarities.stream()
						.map(dnaSimilarity -> Stream.<Object>of(dnaSimilarity.aModule.getNonNull().getUid(),
																dnaSimilarity.bModule.getNonNull().getUid(),
																dnaSimilarity.sequencer.getNonNull().name(),
																dnaSimilarity.similarityAlgorithm.getNonNull().name(),
																dnaSimilarity.similarity.getNonNull()));

			queryBuilder.updateBatch(batchArgs, INSERT_BATCH_SIZE);

			batchConsumer.ifPresent(consumer -> consumer.accept(Integer.valueOf(createdRecords.addAndGet(similarities.size()))));
		});
	}

	/**
	 * Deletes all {@code dna_string}, {@code dna_string_element} and {@code dna_similarity} records for the given {@code projectId} and {@code generated}
	 * timestamp.
	 *
	 * @param projectId the ID of the {@code Project}he records have been created with
	 * @param generated the {@linkplain Instant} when the records were created
	 * @return the number of deleted {@code dna_string} records
	 */
	public int deleteDnaStrings(final EntityId projectId, final Instant generated) {
		/* Both dna_string_element and dna_similarity are referencing dna_string with option "ON DELETE CASCADE" */
		return query("DELETE FROM dna_string WHERE module IN (SELECT uid FROM module WHERE project = ")
							.with(ProjectPgDao.referenceUidOrNid(projectId))
							.append(") AND generated = ?::timestamp_zoned_milli").addArg(Timestamp.from(generated))
							.update();
	}

	/**
	 * Deletes all {@code dna_string}, {@code dna_string_element} and {@code dna_similarity} records for the given {@code projectId}.
	 *
	 * @param projectId the ID of the {@code Project}he records have been created with
	 * @return the number of deleted {@code dna_string} records
	 */
	public int deleteDnaStrings(final EntityId projectId) {
		/* Both dna_string_element and dna_similarity are referencing dna_string with option "ON DELETE CASCADE" */
		return query("DELETE FROM dna_string WHERE module IN (SELECT uid FROM module WHERE project = ")
						.with(ProjectPgDao.referenceUidOrNid(projectId))
						.append(")")
						.update();
	}

	/**
	 * Deletes all {@code dna_string}, {@code dna_string_element} and {@code dna_similarity} records for the given {@code moduleIds}.
	 * 
	 * @param moduleUids the uids of the modules for which DNA data must be deleted
	 * @return the number of deleted {@code dna_string} records
	 */
	public int deleteDnaStrings(final Collection<UUID> moduleUids) {
		/* Both dna_string_element and dna_similarity are referencing dna_string with option "ON DELETE CASCADE" */
		return query("DELETE FROM dna_string WHERE module = any(?)")
						.addArg(PgType.UUID, moduleUids)
						.update();
	}
	
	/**
	 * Returns the {@link UUID UUIDs} of all matching {@code dna_string} records for the filters in the given {@code builder}.
	 *
	 * @param builder the {@link DnaStringInquiryBuilder} containing the filter criteria and sorting options.
	 * @return list of matching {@linkplain DnaStringPojo DnaStringPojos}
	 */
	public List<UUID> findAllDnaStringUuids(final BuildingConsumer<DnaStringInquiryBuilder> builder) {
		return builder.prepare(new DnaStringBuilder())
						.build();
	}

	/**
	 * Returns all {@code module} {@link UUID UUIDs} and {@code values} of all {@code dna_string_element} entities that match with the filters in the given
	 * {@code builder}.
	 *
	 * @param builder the {@linkplain DnaStringElementInquiryBuilder} containing the filter criteria
	 * @return list of {@link Tuple2} with module {@link UUID UUIDs} and string values
	 */
	public List<Tuple2<UUID, String>> findDnaStringElementValues(final BuildingConsumer<DnaStringElementInquiryBuilder> builder) {
		return builder.prepare(new DnaStringElementBuilder())
						.buildTuples();
	}

	/**
	 * Returns the number of {@code dna_string_element} records that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DnaStringElementInquiryBuilder} containing the filter criteria.
	 * @return number of matching {@code dna_string_element} records
	 */
	public long getDnaStringElementCount(final BuildingConsumer<DnaStringElementInquiryBuilder> builder) {
		return builder.prepare(new DnaStringElementBuilder())
					.buildCount();
	}

	/**
	 * Returns the {@linkplain DnaSequencer DnaSequencers} that were used for creating {@code dna_string} records in the given {@code projectId}.
	 *
	 * @param projectId the ID of the {@code Project} the {@linkplain DnaSequencer DnaSequencers} were created for
	 * @return list of {@linkplain DnaSequencer DnaSequencers}
	 */
	public List<DnaSequencer> getSequencerIdsFromDnaString(final EntityId projectId) {
		return query("SELECT distinct(sequencer) FROM dna_string WHERE module IN (SELECT uid FROM module WHERE project = ")
						.with(ProjectPgDao.referenceUidOrNid(projectId))
						.append(")")
						.toList((rs, rowNum) -> DnaSequencer.valueOf(rs.getString(1)));
	}

	/**
	 * Returns the number of {@code dna_string} for the specified filters in the given {@code builder}.
	 *
	 * @param builder the {@link DnaStringInquiryBuilder} containing the filter criteria.
	 * @return number of matching {@code dna_string} records
	 */
	public long getDnaStringCount(final BuildingConsumer<DnaStringInquiryBuilder> builder) {
		return builder.prepare(new DnaStringBuilder())
					.buildCount();
	}

	/**
	 * Returns all {@code module} {@linkplain UUID UUIDs} for which {@code dna_string} records with the given {@code projectId} and {@code sequencer} exists but
	 * which are not present in any {@code dna_community} for the given {@code snapshotId} and {@code sequencer}.
	 *
	 * @param projectId the ID of the {@code Project} the records have been created with
	 * @param sequencer the {@link DnaSequencer} the records have been created with
	 * @param snapshotId the ID of the DnaSnapshot
	 * @return list of {@code module} {@linkplain UUID UUIDs}
	 */
	public List<UUID> getUnassignedModules(final EntityId projectId, final DnaSequencer sequencer, final UUID snapshotId) {
		/* SELECT module FROM dna_string s WHERE s.sequencer = ?
											 AND s.module IN (SELECT uid FROM module WHERE project = ?)
											 AND s.module NOT IN
					(SELECT module FROM dna_community c, dna_community_modules m WHERE c.snapshot = ? AND m.community = c.id AND c.sequencer = ?) */
		return query("SELECT module FROM dna_string s WHERE s.sequencer = ?").addArg(sequencer.name())
			.append(" AND s.module IN (SELECT uid FROM module WHERE project = ")
			.with(ProjectPgDao.referenceUidOrNid(projectId))
			.append(") AND s.module NOT IN (SELECT module FROM dna_community c, dna_community_modules m WHERE c.snapshot = ? AND m.community = c.id AND c.sequencer = ?)",
					snapshotId, sequencer.name())
			.toList((rs, row) -> (UUID) rs.getObject(1));
	}

	/**
	 * Returns all {@code values} from {@code dna_string_element} records that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain DnaStringElementInquiryBuilder} containing the filter criteria.
	 * @return list {@linkplain DnaStringElementPojo DnaStringElementPojos}
	 */
	public List<DnaSimilarityPojo> findDnaSimilarities(final BuildingConsumer<DnaSimilarityInquiryBuilder> builder) {
		return builder.prepare(new DnaSimilarityBuilder())
						.build();
	}

	/**
	 * Returns the number of {@code dna_similarity} records that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@link DnaSimilarityInquiryBuilder} containing the filter criteria
	 * @return List of matching similarities
	 */
	public long getDnaSimilarityCount(final BuildingConsumer<DnaSimilarityInquiryBuilder> builder) {
		return builder.prepare(new DnaSimilarityBuilder())
						.buildCount();
	}

	/**
	 * Returns a list of {@linkplain Triple Triples} of the the top three most similar {@code Module} IDs for the given {@code moduleId}, {@code sequencer} and
	 * {@code algorithm}.
	 * <p>
	 * The {@linkplain Triple Triples} are created of (in this order) the "of" Module path, the "to" Module path and the similarity value of the two modules
	 * </p>
	 * 
	 * @param moduleId the {@link UUID} of the {@code Module} the records have been created with
	 * @param sequencer the {@linkplain DnaSequencer}he records have been created with
	 * @param algorithm the {@linkplain DnaSimilarityAlgorithm} records have been created with
	 *
	 * @return list the top three most similar {@code Module} IDs
	 */
	public List<Triple<String, String, Double>> findNeighboringModules(final UUID moduleId, final DnaSequencer sequencer,
																	final DnaSimilarityAlgorithm algorithm) {
		return query("SELECT (SELECT path from module where uid = a_module) as a_module_path, "
					+ "(SELECT path from module where uid = b_module) as b_module_path, similarity FROM dna_similarity WHERE "
					+ "sequencer = ? AND similarity_algo = ? AND (a_module = ? OR b_module = ?) ORDER BY similarity DESC LIMIT 3")
					.addArgs(sequencer.name(), algorithm.name(), moduleId, moduleId)
					.toList((rs, row) -> Triple.of(rs.getString(1),						/* a_module path */
												   rs.getString(2),						/* b_module path */
												   Double.valueOf(rs.getDouble(3))));	/* similarity */
	}

}
