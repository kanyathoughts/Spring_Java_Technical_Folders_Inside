/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration.discovery.dna.sequencer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.server.discovery.dna.sequencer.jcl.JclSequencer;
import org.apache.commons.lang.StringUtils;
import org.apache.logging.log4j.ThreadContext;
import org.ff4j.FF4j;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import com.opencsv.CSVReader;
import com.opencsv.CSVWriter;

import brave.Tracer;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.dawn.metrics.test.TestTimedWorker;
import innowake.mining.server.discovery.dna.sequencer.Sequencer;
import innowake.mining.server.discovery.dna.sequencer.SequencerProvider;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * The DNA sequencer test that generates DNA string and tests against the expected file.
 */
@WithMockUser
abstract class AbstractSequencerTest extends DatabaseResettingTest {

	private static final boolean WRITE_EXPECTED = Boolean.getBoolean("innowake.base.discovery.write.expected");

	@Autowired
	private FF4j ff4j;
	@Autowired
	private ParseResultCacheService parseResultCacheService;
	@Autowired
	private DiscoveryCache discoveryCache;
	@Autowired
	protected SourceCachingService sourceService;
	@Autowired
	protected JobManager jobManager;
	@Autowired
	protected Tracer tracer;
	@Autowired
	private JclSequencer jclSequencer;
	
	private static final Path BASE_FOLDER = Paths.get("./test-resources/innowake/mining/server/discovery");
	private static final Path SOURCE_BASE_FOLDER = BASE_FOLDER.resolve("source/DNA/sequencer");
	private static final Path EXPECTED_BASE_FOLDER = BASE_FOLDER.resolve("expected/DNA/sequencer");
	private final Path sourceFolder;
	private final Path expectedFile;
	private static final String EXPECTED_BASE_FILE = "expected-file.csv.dump";
	protected static final EntityId PROJECT_ID = EntityId.of(5l);
	private static final Long ONE = Long.valueOf(1);

	@BeforeEach
	void createTestData() {
		createProject();
	}
	
	/**
	 * constructor.
	 */
	public AbstractSequencerTest() {
		sourceFolder = SOURCE_BASE_FOLDER.resolve(getFolder());
		expectedFile = EXPECTED_BASE_FOLDER.resolve(getFolder()).resolve(EXPECTED_BASE_FILE);
		ThreadContext.put("job-id", "test");
	}

	@Test
	void testDnaStrings() {
		final List<DnaStringDump> actualContent = generateDnaString();
		if ( ! WRITE_EXPECTED) {
			final List<DnaStringDump> expectedContent = readDump();
			assertEquals(expectedContent, actualContent);
		} else {
			writeDump(actualContent);
		}
	}

	/**
	 * Gets the {@link Technology} and {@link Type} pair for which DNA string needs to be run. This is only used for providing the sequencer and not for
	 * filtering the source objects.
	 *
	 * @return the {@link Technology} and {@link Type} pair
	 */
	protected abstract Tuple2<Technology, Type> getTechnologyType();

	/**
	 * Gets the folder on which DNA needs to be run.
	 *
	 * @return the folder string
	 */
	protected abstract String getFolder();

	/**
	 * Process any pre-requisite before generating DNA string such as upload source object, discover code, discover metrics.
	 * By default it runs only source object upload.
	 */
	protected void preProcess() {
		uploadResources();
	}
	
	private Sequencer provideSequencer() {
		final Map<FeatureId, Boolean> featureMap = new EnumMap<>(FeatureId.class);
		final ProjectPojo project = projectService.get(PROJECT_ID);
		final String jobId = UUID.randomUUID().toString(); /* dummy job id */
		Stream.of(FeatureId.values()).forEach(feature -> featureMap.put(feature, Boolean.valueOf(ff4j.getFeature(feature.getId()).isEnable())));
		final TimedWorker timedWorker = new TestTimedWorker();
		final SearchOrders searchOrder = new SearchOrders(project.getSearchOrders());
		final PersistingSourceObjectResolver sourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrder);
		Config discoveryConfig;
		try {
			discoveryConfig = Config.loadConfig(projectService, PROJECT_ID);
			final SequencerProvider sequencerProvider = new SequencerProvider(jobId, cfg -> projectService.getXmlConfig(PROJECT_ID, cfg),
					timedWorker, sourceObjectResolver, parseResultCacheService, sourceService, searchOrder, discoveryCache, featureMap, discoveryConfig, jclSequencer);

			return sequencerProvider.provideSequencer(getTechnologyType().a, getTechnologyType().b);
		} catch (DiscoveryException e) {
			e.printStackTrace();
			fail("Failed to read the discovery config", e);
			throw new IllegalStateException(); /* this won't occur */
		}
	}

	private List<DnaStringDump> generateDnaString() {
		preProcess();

		final Sequencer sequencer = provideSequencer();
		return sourceService.find(q -> q.ofProject(PROJECT_ID)).stream().map(sourceObject -> {
			try {
				return sequencer.apply(sourceObject).stream().map(dnaString -> {
					return new DnaStringDump(sourceObject.getName(), sourceObject.getPath(), sourceObject.getTechnology().name(), sourceObject.getType().name(),
							dnaString.a.sequencer.getNonNull().getId(),
							dnaString.b.stream().map(pojo -> pojo.value.get()).sorted().collect(Collectors.toList()));
				}).collect(Collectors.toList());
			} catch (DiscoveryException e) {
				e.printStackTrace();
				fail("Error while generating Dna String for " + sourceObject.getPath(), e);
				throw new IllegalStateException(); /* this won't occur */
			}
		}).flatMap(Collection::stream).sorted().collect(Collectors.toList());
	}

	private void uploadResources() {
		try (final Stream<Path> walk = Files.walk(sourceFolder)) {
			walk.filter(Files::isRegularFile).map(path -> {
				final String content;
				try {
					content = getFileContent(path);
				} catch (final IOException e) {
					throw new IllegalStateException(e);
				}
				final ModuleType moduleType = FileExtension.resolve(path.toString());
				return createSourceObject(path, content, moduleType);
			}).forEach(this::uploadSourceObject);
		} catch (final Exception e) {
			e.printStackTrace();
			fail("Error while uploading resource: " + e.getMessage(), e);
		}
	}

	private SourcePojoPrototype createSourceObject(final Path path, final String content, final ModuleType moduleType) {
		return new SourcePojoPrototype()
				.setProject(PROJECT_ID)
				.setName(path.getFileName().toString())
				.setPath(SOURCE_BASE_FOLDER.relativize(path).toString())
				.setTechnology(moduleType.getTechnology())
				.setType(moduleType.getType())
				.setMetaDataRevision(Long.valueOf(1))
				.setContentRevision(Long.valueOf(1))
				.setContent(new BinaryString(content));
	}

	private EntityId uploadSourceObject(final SourcePojoPrototype sourceObject) {
		return sourceService.create(sourceObject);
	}

	private String getFileContent(final Path path) throws IOException {
		return Files.readAllLines(path, StandardCharsets.UTF_8).stream().collect(Collectors.joining(System.getProperty("line.separator")));
	}

	private void writeDump(final List<DnaStringDump> content) {
		try (final CSVWriter writer = new CSVWriter(new OutputStreamWriter(Files.newOutputStream(expectedFile), StandardCharsets.UTF_8))) {
			writer.writeNext(new String[] {
					"name", "path", "technology", "type", "sequencer id", "value"
			}); // dump headers
			content.stream().map(DnaStringDump::asCsvString).forEach(writer::writeNext);
		} catch (IOException e) {
			e.printStackTrace();
			fail("Failed to write the expected file", e);
			throw new IllegalStateException(); /* this won't occur */
		}
	}

	private ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("DNA Test Project")
				.setClient(EntityId.of(ONE))
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY))));
	}
	
	private List<DnaStringDump> readDump() {
		try (CSVReader reader = new CSVReader(new InputStreamReader(Files.newInputStream(expectedFile)))) {
			reader.readNext(); /* read the header and discard */
			final List<DnaStringDump> result = new ArrayList<>();
			String[] s;
			while ((s = reader.readNext()) != null) {
				result.add(new DnaStringDump(s));
			}
			return result.stream().sorted().collect(Collectors.toList());
		} catch (IOException e) {
			e.printStackTrace();
			fail("Failed to read the expected file", e);
			throw new IllegalStateException(); /* this won't occur */
		}
	}

	static class DnaStringDump implements Comparable<DnaStringDump> {

		final String moduleName;
		final String modulePath;
		final String technologyName;
		final String typeName;
		final String sequencerId;
		final List<String> values;

		DnaStringDump(final String moduleName, final String modulePath, final String technologyName, final String typeName, final String sequencerId,
				final List<String> values) {
			this.moduleName = moduleName;
			this.modulePath = modulePath;
			this.technologyName = technologyName;
			this.typeName = typeName;
			this.sequencerId = sequencerId;
			this.values = values;
		}

		DnaStringDump(String[] array) {
			if (array.length < 5) {
				fail("Input file contains invalid length. Check the expected file");
			}
			this.moduleName = array[0];
			this.modulePath = array[1];
			this.technologyName = array[2];
			this.typeName = array[3];
			this.sequencerId = array[4];
			if (array[5].length() == 0) { /* to prevent creating an ArrayList of size 1 */
				this.values = Collections.emptyList();
			} else {
				this.values = Arrays.asList(array[5].split(","));
			}
		}

		private String[] asCsvString() {
			return new String[] {
					moduleName, modulePath, technologyName, typeName, sequencerId, StringUtils.join(values, ",")
			};
		}

		@Override
		public boolean equals(Object obj) {
			if (obj == this) {
				return true;
			}
			if (obj == null || ! (obj instanceof DnaStringDump)) {
				return false;
			}
			final DnaStringDump other = (DnaStringDump) obj;
			return other.moduleName.equals(moduleName) && other.sequencerId.equals(sequencerId) && other.technologyName.equals(technologyName)
					&& other.typeName.equals(typeName) && other.values.equals(values); /* does not checks path as the csv reader strips of the slashes */
		}

		@Override
		public int compareTo(final DnaStringDump other) {
			final StringBuilder sb = new StringBuilder();
			sb.append(moduleName).append(modulePath).append(technologyName).append(typeName).append(sequencerId);
			final StringBuilder oSb = new StringBuilder();
			oSb.append(other.moduleName).append(other.modulePath).append(other.technologyName).append(other.typeName).append(other.sequencerId);
			return sb.toString().compareTo(oSb.toString());
		}

		@Override
		public int hashCode() {
			return super.hashCode();
		}

		@Override
		public String toString() {
			return StringUtils.join(asCsvString(), ",") + "\n";
		}
	}
}
