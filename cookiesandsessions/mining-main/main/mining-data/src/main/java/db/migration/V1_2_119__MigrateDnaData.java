/* Copyright (c) 2022 Deloitte. All rights reserved. */
package db.migration;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.io.StringReader;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.flywaydb.core.api.migration.BaseJavaMigration;
import org.flywaydb.core.api.migration.Context;
import org.flywaydb.core.internal.jdbc.JdbcUtils;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.PreparedStatementCallback;
import org.springframework.jdbc.core.PreparedStatementCreator;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.opencsv.CSVParser;
import com.opencsv.CSVParserBuilder;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;

import db.migration.model.legacy.dna.ClusterAlgorithmId;
import db.migration.model.legacy.dna.DnaCommunity;
import db.migration.model.legacy.dna.DnaSimilarity;
import db.migration.model.legacy.dna.DnaSnapshot;
import db.migration.model.legacy.dna.DnaString;
import db.migration.model.legacy.dna.DnaStringElement;
import db.migration.model.legacy.dna.SequencerId;
import db.migration.model.legacy.dna.SimilarityId;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.StatementBuilder;
import innowake.mining.data.model.discovery.ModelAlgorithmOption;
import innowake.mining.data.model.discovery.dna.DnaConfig;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.shared.discovery.dna.Constants;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.model.Entity;

/**
 * Flyway migration script to migrate Dna Data from old schema to new schema.
 */
@SuppressWarnings("deprecation") /* This migration will be deleted together with all legacy model classes once migration from OrientDB to Postgres is finished */
public class V1_2_119__MigrateDnaData extends BaseJavaMigration {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MIGRATION);
	private static final String JSON_EXTENSION = ".json";
	private static final String JSON_EXTENSION_WITHOUT_DOT = "json";
	private static final String MODULES_IN_CLUSTER_FILE_PATTERN = "Modules_in_%s_Cluster.csv";

	/* this is required because we recently changed the value of sequencer id and it is possible that some files have old names */
	private static final Map<String, SequencerId> SEQUENCER_ID = new HashMap<>(4);

	private static final int CHUNK_SIZE = 5_000_000;
	private static final int BATCH_SIZE = 1000;
	private static final String LOG_TEXT = "Migrating %s of the project id: %d";

	static {
		SEQUENCER_ID.put("COBOL SKELETON", SequencerId.COBOL_SKELETON_RULE);
		SEQUENCER_ID.put("COBOL METHODS", SequencerId.COBOL_METHOD_RULE);
		SEQUENCER_ID.put("PL/1 SKELETON", SequencerId.PLI_SKELETON_RULE);
		SEQUENCER_ID.put("PL/1 METHODS", SequencerId.PLI_METHOD_RULE);
	}

	private static final Function<Entity, PreparedStatementCallback<Entity>> statementCallBack = entity -> statement -> {
		ResultSet resultSet = null;
		try {
			if (statement.execute()) {
				resultSet = statement.getResultSet();
				if (resultSet.getMetaData().getColumnCount() == 0) {
					throw new IllegalStateException("ResultSet column count must not be 0");
				}

				entity.setRecordId(assertNotNull(resultSet.getString("@rid")));
				return entity;
			} else {
				throw new IllegalStateException("There must be a ResultSet when inserting a DnaString.");
			}
		} catch (final SQLException e) {
			throw new IllegalStateException(e);
		} finally {
			JdbcUtils.closeResultSet(resultSet);
		}
	};

	@Override
	public void migrate(@Nullable final Context context) throws Exception {
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(new SingleConnectionDataSource(assertNotNull(context).getConnection(), true));
		final List<Long> projectIds = jdbcTemplate.queryForList("SELECT projectLink.id FROM DnaData WHERE projectLink.id IS NOT NULL", Long.class);
		LOG.info(() -> String.format("Found %d projects for DnaData migration", projectIds.size()));
		try {
			for (final Long projectId : projectIds) {
				LOG.info(() -> String.format("Migrating DnaData for project id: %d", projectId));
				final List<String> dnaFiles = jdbcTemplate.queryForList("SELECT files.keys() as keys FROM DnaData WHERE projectLink.id=? UNWIND keys",
						String.class, projectId);
				final Map<String, String> dnaData = new HashMap<>(dnaFiles.size());
				for (final String dnaFile : dnaFiles) {
					if (dnaFile != null) {
						final String queryDnaFileLength = String.format("SELECT files['%s'].length() FROM DnaData WHERE projectLink.id=?", dnaFile);
						final Integer length = jdbcTemplate.queryForObject(queryDnaFileLength, Integer.class, projectId);
						if (length != null) {
							int processed = 0;
							final StringBuilder fileContentSb = new StringBuilder(length.intValue());
							while (processed < length.intValue()) {
								final String queryExtractContent = String.format("SELECT files['%s'].substring(%d,%d) FROM DnaData WHERE projectLink.id=?",
										dnaFile, Integer.valueOf(processed), Integer.valueOf(Math.min(processed + CHUNK_SIZE, length.intValue())));
								final String fileContentSubString = jdbcTemplate.queryForObject(queryExtractContent, String.class, projectId);
								fileContentSb.append(fileContentSubString);
								processed += CHUNK_SIZE;
							}

							dnaData.put(dnaFile, fileContentSb.toString());
						}
					}
				}
				final Optional<String> timeStamp = dnaData.keySet().stream()
						.filter(fileName -> FilenameUtils.getExtension(fileName).equals(JSON_EXTENSION_WITHOUT_DOT) && fileName.contains("discovery_clusters_"))
						.map(fileName -> StringUtils.substringBetween(fileName, "discovery_clusters_", JSON_EXTENSION))
						.sorted(Comparator.reverseOrder())
						.findFirst();

				if ( ! dnaData.containsKey(Constants.DNA_STRING_FILENAME) || ! dnaData.containsKey(Constants.DNA_SIMILARITY_FILENAME)
						|| ! dnaData.containsKey(Constants.DNA_COMMUNITY_FILENAME) || ! timeStamp.isPresent()) {
					LOG.warn("Not all Dna files present for the project id: {}. Skipping migration for this project", projectId);
					continue;
				}

				final Date generatedOn = new Date(Long.parseLong(timeStamp.get()));

				LOG.info(() -> String.format(LOG_TEXT, Constants.DNA_STRING_FILENAME, projectId));
				final Map<String, Module> moduleInfo = migrateDnaStringAndReturnModuleInfo(jdbcTemplate, projectId,
						dnaData.get(Constants.DNA_STRING_FILENAME), generatedOn);

				LOG.info(() -> String.format(LOG_TEXT, Constants.DNA_SIMILARITY_FILENAME, projectId));
				migrateDnaSimilarity(jdbcTemplate, projectId, dnaData.get(Constants.DNA_SIMILARITY_FILENAME), moduleInfo);

				LOG.info(() -> String.format(LOG_TEXT, "DNASnapshot", projectId));
				final String dnaSnapshotRid = migrateDnaSnapshot(jdbcTemplate, projectId,
						dnaData.get("discovery_clusters_" + timeStamp.get() + JSON_EXTENSION), generatedOn);

				LOG.info(() -> String.format(LOG_TEXT, Constants.DNA_COMMUNITY_FILENAME, projectId));
				for (final SequencerId sequencerId : SequencerId.values()) {
					migrateDnaCommunity(jdbcTemplate, projectId, dnaData, dnaSnapshotRid, sequencerId, moduleInfo);
				}
			}
			LOG.info(() -> "Migrated the Dna data successfully. Dropping the old schema");
			jdbcTemplate.execute("DROP Class DnaData UNSAFE");
		} catch (final Exception e) {
			LOG.error("Error while migrating Dna Data. Reverting the changes.", e);
			jdbcTemplate.execute("delete from BelongsToCluster unsafe");
			jdbcTemplate.execute("delete from DnaCommunity unsafe");
			jdbcTemplate.execute("delete from DnaStringElement unsafe");
			jdbcTemplate.execute("delete from DnaSimilarity unsafe");
			jdbcTemplate.execute("delete from DnaString unsafe");
			jdbcTemplate.execute("delete from DnaSnapshot unsafe");
			LOG.info(() -> "Successfully reverted the changes.");
			throw e;
		}
	}

	private Map<String, Module> migrateDnaStringAndReturnModuleInfo(final JdbcTemplate jdbcTemplate, final Long projectId, final String fileContent,
			final Date generatedOn) throws IOException {
		final Map<String, Module> moduleInfo = new HashMap<>();
		final CSVParser parser = new CSVParserBuilder().withSeparator(';').build();
		final CSVReader csvReader = new CSVReaderBuilder(new StringReader(fileContent)).withCSVParser(parser).build();
		String[] line = csvReader.readNext();

		while (line != null) {
			if (line.length < 2) {
				LOG.warn("Invalid line in Dna String file: {}. Skipping this line", Arrays.toString(line));
				line = csvReader.readNext();
				continue;
			}
			final Module module = moduleInfo.computeIfAbsent(line[0], path -> {
				try {
					final Module m = jdbcTemplate.queryForObject("SELECT @rid, contentHash from Module where projectLink.id=? AND path=?",
							new ModuleRowMapper(), projectId, path);
					if (m == null) {
						LOG.info("Module with path: {} not found. Skipping the module.", path);
						return new Module("", "");
					} else if (StringUtils.isBlank(m.contentHash)) { /* content hash in the module is null or empty. hence calculation required */
						LOG.info("Module {} does not have content hash. Computing now.", path);
						final String content = jdbcTemplate.queryForObject("SELECT sourceAttachmentLink.content from Module where projectLink.id=? AND path=?",
								String.class, projectId, path);
						if (StringUtils.isBlank(content)) {
							LOG.info("Module {} does not have content. Skipping the module.", path);
							return new Module("", "");
						}
						m.contentHash = CityHash.cityHash128Hex(content);
					}
					return m;
				} catch (final EmptyResultDataAccessException exception) {
					LOG.info("Module with path: {} not found. Skipping the module.", path);
					return new Module("", "");
				}
			});
			if (module.moduleRid.equals("") || ! SEQUENCER_ID.containsKey(line[1].toUpperCase())) {
				LOG.warn("Invalid line in Dna String file: {}. Skipping this line", Arrays.toString(line));
				line = csvReader.readNext();
				continue;
			}
			final DnaString dnaString = new DnaString();
			dnaString.setContentHash(module.contentHash);
			dnaString.setGeneratedOn(generatedOn);
			dnaString.setSequencerId(SEQUENCER_ID.get(line[1].toUpperCase()));
			dnaString.setProjectId(projectId);
			dnaString.setModuleUnitRid(module.moduleRid);

			final String[] strings = StringUtils.split(line[2], ',');
			dnaString.setLength(strings.length);

			final String moduleStringRid = createDnaString(jdbcTemplate, dnaString);
			module.moduleStringRid = moduleStringRid;
			final List<DnaStringElement> stringElements = new ArrayList<>(strings.length);
			int i = 0;
			for (final String string : strings) {
				final DnaStringElement element = new DnaStringElement();
				element.setDnaStringRecordId(moduleStringRid);
				element.setIndex(i++);
				element.setValue(string);
				stringElements.add(element);
			}

			createDnaStringElement(jdbcTemplate, stringElements, moduleStringRid);
			line = csvReader.readNext();
		}

		return moduleInfo;
	}

	private void migrateDnaSimilarity(final JdbcTemplate jdbcTemplate, final Long projectId, final String fileContent, final Map<String, Module> moduleInfo)
			throws IOException {
		final CSVParser parser = new CSVParserBuilder().withSeparator(';').build();
		final CSVReader csvReader = new CSVReaderBuilder(new StringReader(fileContent)).withCSVParser(parser).build();
		String[] line = csvReader.readNext();
		while (line != null) {
			if (line.length < 3 || moduleInfo.get(line[0]).moduleStringRid.equals("") || moduleInfo.get(line[3]).moduleStringRid.equals("")
					|| ! SEQUENCER_ID.containsKey(line[1].toUpperCase())) {
				LOG.warn("Invalid line in Dna Similarity file: {}. Skipping this line", Arrays.toString(line));
				line = csvReader.readNext();
				continue;
			}
			final DnaSimilarity dnaSimilarity = new DnaSimilarity();
			dnaSimilarity.setFromDnaStringRecordId(moduleInfo.get(line[0]).moduleStringRid);
			dnaSimilarity.setToDnaStringRecordId(moduleInfo.get(line[3]).moduleStringRid);
			dnaSimilarity.setProjectId(projectId);
			dnaSimilarity.setSequencerId(SEQUENCER_ID.get(line[1].toUpperCase()));
			dnaSimilarity.setSimilarityId(DnaSimilarityAlgorithm.WEIGHTED_LEVENSHTEIN);
			dnaSimilarity.setSimilarity(Double.parseDouble(line[4]));

			createDnaSimilarity(jdbcTemplate, dnaSimilarity);
			line = csvReader.readNext();
		}
	}

	private String migrateDnaSnapshot(final JdbcTemplate jdbcTemplate, final Long projectId, final String fileContent, final Date generatedOn) {
		final ModelDna d = new Gson().fromJson(fileContent, ModelDna.class);
		final DnaSnapshot sn = new DnaSnapshot();
		sn.setTotalModuleCount(d.getModuleCount());
		final DnaConfig dnaConfig = new DnaConfig();
		sn.setDnaConfig(dnaConfig);
		dnaConfig.setDefaultTolerance(d.getClusterings().get(0).getOptions().stream()
				.filter(o -> o.getName().equals("defaultTolerance"))
				.map(ModelAlgorithmOption::getValue)
				.map(Double::parseDouble)
				.findFirst()
				.orElse(0.0001));
		dnaConfig.setSimilarityThreshold(d.getClusterings().get(0).getOptions().stream()
				.filter(o -> o.getName().equals("similarity threshold"))
				.map(ModelAlgorithmOption::getValue)
				.map(Double::parseDouble)
				.findFirst()
				.orElse(0.85));
		dnaConfig.setMaxLevels(d.getClusterings().get(0).getOptions().stream()
				.filter(o -> o.getName().equals("maxLevels"))
				.map(ModelAlgorithmOption::getValue)
				.map(Integer::parseInt)
				.findFirst()
				.orElse(5));
		dnaConfig.setMinDnaLength(d.getClusterings().get(0).getOptions().stream()
				.filter(o -> o.getName().equals("minDNALength"))
				.map(ModelAlgorithmOption::getValue)
				.map(Integer::parseInt)
				.findFirst()
				.orElse(20));
		sn.setUpdatedOn(generatedOn);
		sn.setProjectId(projectId);
		return createSnapshot(jdbcTemplate, sn);
	}

	private void migrateDnaCommunity(final JdbcTemplate jdbcTemplate, final Long projectId, final Map<String, String> dnaDataFiles,
			final String snapshotRid, final SequencerId sequencerId, final Map<String, Module> moduleInfo) throws IOException {
		String fileName = String.format(MODULES_IN_CLUSTER_FILE_PATTERN, sequencerId);
		String fileContent = dnaDataFiles.get(fileName);
		if (fileContent == null) {
			/* With WMIN-4003, sequencer rules: METHOD_RULE and SKELETON_RULE were changed to COBOL_METHOD_RULE and COBOL_SKELETON_RULE respectively.
			 * However, the existing DnaData was not modified. So, we would need to assume these rules to be COBOL_*_RULE
			 */
			if (sequencerId == SequencerId.COBOL_METHOD_RULE || sequencerId == SequencerId.COBOL_SKELETON_RULE) {
				fileName = fileName.replace("COBOL_", "");
				fileContent = dnaDataFiles.get(fileName);
			}

			if (fileContent == null) {
				return;
			}
		}
		final CSVReader r = new CSVReader(new StringReader(fileContent));
		r.skip(1);
		String[] line = r.readNext();
		final Map<String, List<String>> clusterInfo = new HashMap<>();
		while (line != null) {
			if (line.length < 9) {
				LOG.warn("Invalid line in {} file: {}. Skipping this line", fileName, Arrays.toString(line));
			} else {
				clusterInfo.computeIfAbsent(line[0], x -> new ArrayList<String>()).add(line[1]);
			}
			line = r.readNext();
		}
		clusterInfo.entrySet().forEach(x -> {
			final DnaCommunity dnaCommunity = new DnaCommunity();
			dnaCommunity.setClusterAlgorithmId(ClusterAlgorithmId.LOUVAIN);
			dnaCommunity.setClusterIndex(Integer.valueOf(x.getKey()));
			final List<String> moduleUnitRids = x.getValue().stream()
													.map(moduleInfo::get)
													.map(module -> module.moduleRid)
													.filter(StringUtils::isNotBlank)
													.collect(Collectors.toList());
			dnaCommunity.setModuleUnitRids(moduleUnitRids);
			dnaCommunity.setCount(moduleUnitRids.size());
			dnaCommunity.setProjectId(projectId);
			dnaCommunity.setSequencerId(sequencerId);
			dnaCommunity.setSimilarityId(SimilarityId.WEIGHTED_LEVENSHTEIN);
			dnaCommunity.setSnapshotRecordId(snapshotRid);

			createDnaCommunity(jdbcTemplate, dnaCommunity);
		});
	}

	private static class Module {

		private final String moduleRid;
		private String contentHash;
		private String moduleStringRid = "";

		public Module(final String moduleRid, final String contentHash) {
			this.moduleRid = moduleRid;
			this.contentHash = contentHash;
		}
	}

	private static class ModuleRowMapper implements RowMapper<Module> {

		@Override
		public @Nullable Module mapRow(final ResultSet rs, final int rowNum) throws SQLException {
			return new Module(rs.getString(1), rs.getString(2));
		}
	}

	private String createSnapshot(final JdbcTemplate jdbcTemplate, final DnaSnapshot dnaSnapshot) {
		final DnaConfig dnaConfig = dnaSnapshot.getDnaConfig();
		final StatementBuilder builder = new StatementBuilder()
				.addAttribute("projectLink=(SELECT FROM Project WHERE id=?)", dnaSnapshot.getProjectId())
				.addAttribute("updatedOn=?", dnaSnapshot.getUpdatedOn())
				.addAttribute("similarityThreshold=?", dnaConfig.getSimilarityThreshold())
				.addAttribute("maxLevels=?", dnaConfig.getMaxLevels())
				.addAttribute("maxIterations=?", dnaConfig.getMaxIterations())
				.addAttribute("defaultTolerance=?", dnaConfig.getDefaultTolerance())
				.addAttribute("minDNALength=?", dnaConfig.getMinDnaLength())
				.addAttribute("totalModuleCount=?", dnaSnapshot.getTotalModuleCount());

		final String query = String.format("INSERT INTO DnaSnapshot SET %s RETURN @rid", builder.attributes());
		final PreparedStatementCreator creator = connection -> {
			final PreparedStatement prepareStatement = connection.prepareStatement(query);
			builder.init(prepareStatement);
			return prepareStatement;
		};

		final PreparedStatementCallback<Entity> callback = statementCallBack.apply(dnaSnapshot);

		return assertNotNull(assertNotNull(jdbcTemplate.execute(creator, callback)).getRecordId());
	}

	private void createDnaCommunity(final JdbcTemplate jdbc, final DnaCommunity dnaCommunity) {
		final StatementBuilder builder = new StatementBuilder()
				.addAttribute("projectLink=(SELECT FROM Project WHERE id=?)", dnaCommunity.getProjectId())
				.addAttribute("snapshotLink=?", dnaCommunity.getSnapshotRecordId())
				.addAttribute("sequencerId=?", dnaCommunity.getSequencerId().name())
				.addAttribute("similarityId=?", dnaCommunity.getSimilarityId().name())
				.addAttribute("clusterAlgorithmId=?", dnaCommunity.getClusterAlgorithmId().name())
				.addAttribute("title=?", dnaCommunity.getTitle())
				.addAttribute("clusterId=?", dnaCommunity.getClusterIndex());

		final String query = String.format("INSERT INTO DnaCommunity SET %s RETURN @rid", builder.attributes());
		final PreparedStatementCreator creator = connection -> {
			final PreparedStatement prepareStatement = connection.prepareStatement(query);
			builder.init(prepareStatement);
			return prepareStatement;
		};

		final PreparedStatementCallback<Entity> callback = statementCallBack.apply(dnaCommunity);

		jdbc.execute(creator, callback);

		final List<String> moduleRids = dnaCommunity.getModuleUnitRids();
		jdbc.update(String.format("CREATE EDGE BelongsToCluster FROM (SELECT FROM ModuleUnit WHERE @rid in %s) TO %s", moduleRids, dnaCommunity.getRecordId()));
	}

	private String createDnaString(final JdbcTemplate jdbc, final DnaString dnaString) {
		final StatementBuilder builder = new StatementBuilder()
				.addAttribute("moduleUnitLink=?", dnaString.getModuleUnitRid())
				.addAttribute("contentHash=?", dnaString.getContentHash())
				.addAttribute("projectLink=(SELECT FROM Project WHERE id=?)", dnaString.getProjectId())
				.addAttribute("sequencerId=?", dnaString.getSequencerId().name())
				.addAttribute("generatedOn=?", dnaString.getGeneratedOn())
				.addAttribute("length=?", dnaString.getLength());

		final String query = String.format("INSERT INTO DnaString SET %s RETURN @rid", builder.attributes());
		final PreparedStatementCreator creator = connection -> {
			final PreparedStatement prepareStatement = connection.prepareStatement(query);
			builder.init(prepareStatement);
			return prepareStatement;
		};

		final PreparedStatementCallback<Entity> callback = statementCallBack.apply(dnaString);
		jdbc.execute(creator, callback);
		return assertNotNull(dnaString.getRecordId());
	}

	private void createDnaStringElement(final JdbcTemplate jdbc, final List<DnaStringElement> dnaStringElements, final String dnaStringRecordId) {
		final List<Object[]> queries = dnaStringElements.stream().map(dnaString -> {
			final Object[] args = new Object[3];
			args[0] = dnaStringRecordId;
			args[1] = dnaString.getIndex();
			args[2] = dnaString.getValue();
			return args;

		}).collect(Collectors.toList());

		final List<List<Object[]>> queriesSplitList = Lists.partition(queries, BATCH_SIZE);
		queriesSplitList.forEach(x -> jdbc.batchUpdate("INSERT INTO DnaStringElement SET stringLink=?, index=?, value=?", x));
	}

	private void createDnaSimilarity(final JdbcTemplate jdbc, final DnaSimilarity dnaSimilarity) {
		jdbc.update("INSERT INTO DnaSimilarity SET projectLink=(SELECT FROM Project WHERE id=?), sequencerId=?, similarityId=?, fromDnaString=?, toDnaString=?, similarity=?",
				dnaSimilarity.getProjectId(), dnaSimilarity.getSequencerId(), dnaSimilarity.getSimilarityId(), dnaSimilarity.getFromDnaStringRecordId(),
				dnaSimilarity.getToDnaStringRecordId(), dnaSimilarity.getSimilarity());
	}

}
