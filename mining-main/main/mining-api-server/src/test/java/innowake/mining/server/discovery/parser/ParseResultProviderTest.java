/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser;

import static innowake.mining.shared.model.Technology.BASIC;
import static innowake.mining.shared.model.Technology.NATURAL;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.exceptions.base.MockitoException;
import org.springframework.test.util.ReflectionTestUtils;

import com.google.common.cache.CacheBuilder;

import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.parser.basic.BasicParseResultProvider;
import innowake.mining.server.discovery.parser.batch.DiscoveryJclContentProvider;
import innowake.mining.server.discovery.parser.c.CAntlrAstModelParseResultProvider;
import innowake.mining.server.discovery.parser.c.CAntlrParseResult;
import innowake.mining.server.discovery.parser.c.CAntlrParseResultProvider;
import innowake.mining.server.discovery.parser.csd.CsdParseResultProvider;
import innowake.mining.server.discovery.parser.ecl.EclParseResultProvider;
import innowake.mining.server.discovery.parser.ims.ImsParseResultProvider;
import innowake.mining.server.discovery.parser.ims.ImsParseResultProvider.ImsParseResult;
import innowake.mining.server.discovery.parser.java.JavaParseResultProvider;
import innowake.mining.server.discovery.parser.natural.DataArea;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider.NaturalParseResult;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider.Pl1ParseResult;
import innowake.mining.server.discovery.parser.plsql.PlSqlParseResultProvider;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.natural.INaturalParseResult;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.csd.CsdParserFactory.CsdParserType;
import innowake.ndt.parsing.parser.csd.model.Csd;
import innowake.ndt.parsing.parser.ecl.model.EclModel;
import innowake.ndt.parsing.parser.java.model.JavaModel;
import innowake.ndt.parsing.parser.plsql.model.PlSqlModel;

/**
 * Test case to validate the caching for different language result providers.
 */
@NonNullByDefault(false)
class ParseResultProviderTest {

	@InjectMocks
	private ParseResultCacheService parseResultCacheServiceMock;
	@Mock
	private JobManager jobManager;
	@Mock
	private GenericConfigProperties configPropertiesMock;
	@InjectMocks
	private DiscoveryJclContentProvider discoveryJclContentProvider;

	private static final BinaryString CONTENT_BASIC = new BinaryString("PROGRAM BAS_PRG1\n" + "    CALL \"BAS_PRG2\" \n" + "    END PROGRAM\n");
	private static final BinaryString CONTENT_C = new BinaryString("int main () {}");
	private static final EntityId PROJECT_ID = EntityId.of(5L);
	private static final String jobId = "job1";

	private static TimedWorker timedWorkerMock;

	/**
	 * Initializing the mocks and the fields maximumCacheSize, cache and timedWorker.
	 *
	 */
	@BeforeEach
	void initMocks() {
		try {
			MockitoAnnotations.openMocks(this).close();
			ReflectionTestUtils.setField(parseResultCacheServiceMock, "maximumCacheSize", 100000);
			ReflectionTestUtils.setField(parseResultCacheServiceMock, "cache", CacheBuilder.newBuilder().maximumSize(100000).build());
			timedWorkerMock = mock(TimedWorker.class);
			ReflectionTestUtils.setField(discoveryJclContentProvider, "projectId", PROJECT_ID);
		} catch (final Exception e) {
			throw new MockitoException("Failed to release mocks");
		}
	}
	
	private SourcePojo createSource(String name, String path, Technology tech, BinaryString content) {
		return SourcePojoDummy.build(o -> o
				.setProject(PROJECT_ID)
				.setName(name)
				.setPath(path)
				.setTechnology(tech)
				.setType(Type.JOB)
				.setContent(content));
	}
	
	/**
	 * Tests that the {@link BasicParseResultProvider} is caching parse results for the paths of {@link SourcePojo}.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingByPath() throws DiscoveryException, WorkerException {
		final TimedWorker timedWorkerMock = mock(TimedWorker.class);
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(BasicModel.class));

		final SourcePojo sourceObject1 = createSource("BAS_PRG1", "path1", BASIC, CONTENT_BASIC);
		final BasicParseResultProvider provider =
				new BasicParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, "job2", parseResultCacheServiceMock);
		/* check that nothing is cached so far */
		assertNull(parseResultCacheServiceMock.getCached("job2", getKey(sourceObject1, provider)), "Cached result for sourceObject1 must be null");

		/* Call provider for sourceObject1 and check that the result of sourceObject1 got cached */
		provider.getParseResult(sourceObject1);
		verify(timedWorkerMock, times(1)).execute(any(), anyLong(), any(), any());
		assertNotNull(parseResultCacheServiceMock.getCached("job2", getKey(sourceObject1, provider)), "Cached result for sourceObject1 must NOT be null");

		/* Call provider for sourceObject2 with same path and check that the cached result of sourceObject1 is returned */
		SourcePojo sourceObject2 = createSource("BAS_PRG4", "path1", BASIC, CONTENT_BASIC);
		assertNotNull(parseResultCacheServiceMock.getCached("job2", getKey(sourceObject2, provider)), "Cached result for sourceObject2 must NOT be null");
		provider.getParseResult(sourceObject2);
		verify(timedWorkerMock, times(1)).execute(any(), anyLong(), any(), any());

		/* test that when provider is called multiple times for the same SourcePojo with different path, then NOT the cached result is returned */
		sourceObject2 = createSource("BAS_PRG4", "path3", BASIC, CONTENT_BASIC);
		assertNull(parseResultCacheServiceMock.getCached("job2", getKey(sourceObject2, provider)), "Cached result for sourceObject2 must be null");
		provider.getParseResult(sourceObject2);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());
		assertNotNull(parseResultCacheServiceMock.getCached("job2", getKey(sourceObject2, provider)), "Cached result for sourceObject2 must NOT be null");
	}

	/**
	 * Tests that different instances of the {@link BasicParseResultProvider} serve the same cached parse results for {@link SourcePojo}s if they were
	 * created for the same job id.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForSameJob() throws DiscoveryException, WorkerException {
		final TimedWorker timedWorkerMock = mock(TimedWorker.class);
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(BasicModel.class));

		final SourcePojo sourceObject1 = createSource("BAS_PRG1", "path1", BASIC, CONTENT_BASIC);
		final BasicParseResultProvider provider1 = new BasicParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, "job3",
				parseResultCacheServiceMock);
		final BasicParseResultProvider provider2 = new BasicParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, "job3",
				parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		assertNull(parseResultCacheServiceMock.getCached("job3", getKey(sourceObject1, provider1)),
				"Cached result for sourceObject1 in provider1 must be null");
		assertNull(parseResultCacheServiceMock.getCached("job3", getKey(sourceObject1, provider1)),
				"Cached result for sourceObject1 in provider2 must be null");

		/* Call provider1 for sourceObject1 and check it got cached */
		provider1.getParseResult(sourceObject1);
		verify(timedWorkerMock, times(1)).execute(any(), anyLong(), any(), any());
		assertNotNull(parseResultCacheServiceMock.getCached("job3", getKey(sourceObject1, provider1)),
				"Cached result for sourceObject1 in provider1 must NOT be null");

		/* Check that provider2 also serves the cached result for sourceObject1 since it has the same job id */
		assertNotNull(parseResultCacheServiceMock.getCached("job3", getKey(sourceObject1, provider2)),
				"Cached result for sourceObject1 in provider2 must NOT be null");
		provider2.getParseResult(sourceObject1);
		verify(timedWorkerMock, times(1)).execute(any(), anyLong(), any(), any());
	}

	/**
	 * Tests that different instances of the {@link BasicParseResultProvider} don't serve the same cached parse results for {@link SourcePojo}s if they have
	 * been created with different job id.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForDifferentJob() throws DiscoveryException, WorkerException {
		final TimedWorker timedWorkerMock = mock(TimedWorker.class);
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(BasicModel.class));

		final SourcePojo sourceObject1 = createSource("BAS_PRG1", "path1", BASIC, CONTENT_BASIC);
		final BasicParseResultProvider provider1 = new BasicParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, "job4",
				parseResultCacheServiceMock);
		final BasicParseResultProvider provider2 = new BasicParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, "job5",
				parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		assertNull(parseResultCacheServiceMock.getCached("job4", getKey(sourceObject1, provider1)),
				"Cached result for sourceObject1 in provider1 must be null");
		assertNull(parseResultCacheServiceMock.getCached("job5", getKey(sourceObject1, provider2)),
				"Cached result for sourceObject1 in provider2 must be null");

		/* Call provider1 for sourceObject1 and check it got cached */
		provider1.getParseResult(sourceObject1);
		verify(timedWorkerMock, times(1)).execute(any(), anyLong(), any(), any());
		assertNotNull(parseResultCacheServiceMock.getCached("job4", getKey(sourceObject1, provider1)),
				"Cached result for sourceObject1 in provider1 must NOT be null");

		/* Check that provider2 does not serve the cached result for sourceObject1 since it has a different job ID */
		assertNull(parseResultCacheServiceMock.getCached("job5", getKey(sourceObject1, provider2)),
				"Cached result for sourceObject1 in provider2 must still be null");
		provider2.getParseResult(sourceObject1);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());
		assertNotNull(parseResultCacheServiceMock.getCached("job5", getKey(sourceObject1, provider2)),
				"Cached result for sourceObject1 in provider2 must NOT be null");
	}

	/**
	 * Tests that the different parser provider for the same {@link SourcePojo} does not returns the same cached results.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForSameSourceObjectButDifferentProvider() throws DiscoveryException, WorkerException {
		final TimedWorker timedWorkerMock1 = mock(TimedWorker.class);
		final TimedWorker timedWorkerMock2 = mock(TimedWorker.class);

		when(timedWorkerMock1.execute(any(), anyLong(), any(), any())).thenReturn(mock(CAntlrParseResult.class));
		when(timedWorkerMock2.execute(any(), anyLong(), any(), any())).thenReturn(mock(AstModel.class));

		final SourcePojo cSourceObject = createSource("C_PRG1", "path3", Technology.C, CONTENT_C);
		final CAntlrParseResultProvider cResultProvider1 =
				new CAntlrParseResultProvider(Config.getDefaultConfig(), timedWorkerMock1, jobId, parseResultCacheServiceMock);
		final CAntlrAstModelParseResultProvider cResultProvider2 =
				new CAntlrAstModelParseResultProvider(Config.getDefaultConfig(), timedWorkerMock2, jobId, parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		assertNull(parseResultCacheServiceMock.getCached(jobId, getKey(cSourceObject, cResultProvider1)),
				"Cached result for sourceObject in provider1 must be null");
		assertNull(parseResultCacheServiceMock.getCached(jobId, getKey(cSourceObject, cResultProvider2)),
				"Cached result for sourceObject in provider2 must be null");

		/* Call cResultProvider1 for sourceObject and check it got cached */
		cResultProvider1.getParseResult(cSourceObject);
		verify(timedWorkerMock1, times(1)).execute(any(), anyLong(), any(), any());
		assertNotNull(parseResultCacheServiceMock.getCached(jobId, getKey(cSourceObject, cResultProvider1)),
				"Cached result for sourceObject in cResultProvider1 must NOT be null");

		/* Check that provider2 does not serve the cached result for sourceObject1 since it has a different job ID */
		assertNull(parseResultCacheServiceMock.getCached(jobId, getKey(cSourceObject, cResultProvider2)),
				"Cached result for sourceObject in cResultProvider2 must still be null");
		cResultProvider2.getParseResult(cSourceObject);
		verify(timedWorkerMock2, times(1)).execute(any(), anyLong(), any(), any());
		assertNotNull(parseResultCacheServiceMock.getCached(jobId, getKey(cSourceObject, cResultProvider2)),
				"Cached result for sourceObject in cResultProvider2 must NOT be null");
	}

	/**
	 * Tests that the {@link BasicParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForBasicParser() throws DiscoveryException, WorkerException {
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(BasicModel.class));

		final SourcePojo basicSourceObject1 = createSource("BAS_PRG1", "path1", BASIC, CONTENT_BASIC);
		final SourcePojo basicSourceObject2 = createSource("BAS_PRG3", "path2", BASIC, CONTENT_BASIC);
		final BasicParseResultProvider basicResultProvider =
				new BasicParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, jobId, parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		testForEmptyCache(jobId, basicSourceObject1, basicSourceObject2, basicResultProvider);

		/* Call basicProvider for sourceObject1 and check that the result of sourceObject1 got cached */
		final BasicModel basicParseResult = basicResultProvider.getParseResult(basicSourceObject1);
		testForCaching(jobId, getKey(basicSourceObject1, basicResultProvider), 1);

		/* Call basicProvider for sourceObject2 and check that the result of sourceObject2 got cached */
		basicResultProvider.getParseResult(basicSourceObject2);
		testForCaching(jobId, getKey(basicSourceObject2, basicResultProvider), 2);

		/* test that when provider is called multiple times for the same SourcePojo with same path, then the cached result is returned */
		basicResultProvider.getParseResult(basicSourceObject1);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());
		basicResultProvider.getParseResult(basicSourceObject2);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());

		/* test cached value is same as the parse result value for the specific valid key */
		testCachingForValidAndInvalidKey(jobId, getKey(basicSourceObject1, basicResultProvider), basicParseResult);
	}

	/**
	 * Tests that the {@link EclParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForEclParser() throws DiscoveryException, WorkerException {
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(EclModel.class));

		final SourcePojo eclSourceObject1 = createSource("ECL_PRG1", "path3", Technology.ECL, CONTENT_BASIC);
		final SourcePojo eclSourceObject2 = createSource("ECL_PRG2", "path4", Technology.ECL, CONTENT_BASIC);
		final EclParseResultProvider eclResultProvider =
				new EclParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, jobId, parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		testForEmptyCache(jobId, eclSourceObject1, eclSourceObject2, eclResultProvider);

		/* Call eclProvider for eclSourceObject1 and check that the result of eclSourceObject1 got cached */
		final EclModel eclParseResult = eclResultProvider.getParseResult(eclSourceObject1);
		testForCaching(jobId, getKey(eclSourceObject1, eclResultProvider), 1);

		/* Call eclProvider for eclSourceObject2 and check that the result of eclSourceObject2 got cached */
		eclResultProvider.getParseResult(eclSourceObject2);
		testForCaching(jobId, getKey(eclSourceObject2, eclResultProvider), 2);

		/* test that when provider is called multiple times for the same SourcePojo with same path, then the cached result is returned */
		eclResultProvider.getParseResult(eclSourceObject1);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());
		eclResultProvider.getParseResult(eclSourceObject2);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());

		testCachingForValidAndInvalidKey(jobId, getKey(eclSourceObject1, eclResultProvider), eclParseResult);
	}

	/**
	 * Tests that the {@link CAntlrParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForCParser() throws DiscoveryException, WorkerException {
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(CAntlrParseResult.class));

		final SourcePojo cSourceObject1 = createSource("C_PRG1", "path3", Technology.C, CONTENT_BASIC);
		final SourcePojo cSourceObject2 = createSource("C_PRG2", "path4", Technology.C, CONTENT_BASIC);
		final CAntlrParseResultProvider cResultProvider =
				new CAntlrParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, jobId, parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		testForEmptyCache(jobId, cSourceObject1, cSourceObject2, cResultProvider);

		/* Call cProvider for cSourceObject1 and check that the result of cSourceObject1 got cached */
		final CAntlrParseResult cParseResult = cResultProvider.getParseResult(cSourceObject1);
		testForCaching(jobId, getKey(cSourceObject1, cResultProvider), 1);

		/* Call cProvider for cSourceObject2 and check that the result of cSourceObject2 got cached */
		cResultProvider.getParseResult(cSourceObject2);
		testForCaching(jobId, getKey(cSourceObject2, cResultProvider), 2);

		/* test that when provider is called multiple times for the same SourcePojo with same path, then the cached result is returned */
		cResultProvider.getParseResult(cSourceObject1);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());
		cResultProvider.getParseResult(cSourceObject2);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());

		testCachingForValidAndInvalidKey(jobId, getKey(cSourceObject1, cResultProvider), cParseResult);
	}

	/**
	 * Tests that the {@link CsdParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForCsdParser() throws DiscoveryException, WorkerException {
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(Csd.class));

		final SourcePojo csdSourceObject1 = createSource("CSD_PRG1", "path3", Technology.CSD, CONTENT_BASIC);
		final SourcePojo csdSourceObject2 = createSource("CSD_PRG2", "path4", Technology.CSD, CONTENT_BASIC);
		final CsdParseResultProvider csdResultProvider =
				new CsdParseResultProvider(Config.getDefaultConfig(), CsdParserType.LIST, timedWorkerMock, jobId, parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		testForEmptyCache(jobId, csdSourceObject1, csdSourceObject2, csdResultProvider);

		/* Call csdProvider for csdSourceObject1 and check that the result of csdSourceObject1 got cached */
		final Csd csdParseResult = csdResultProvider.getParseResult(csdSourceObject1);
		testForCaching(jobId, getKeyForCsd(csdSourceObject1, csdResultProvider), 1);

		/* Call csdProvider for csdSourceObject2 and check that the result of csdSourceObject2 got cached */
		csdResultProvider.getParseResult(csdSourceObject2);
		testForCaching(jobId, getKeyForCsd(csdSourceObject2, csdResultProvider), 2);

		/* test that when provider is called multiple times for the same SourcePojo with same path, then the cached result is returned */
		csdResultProvider.getParseResult(csdSourceObject1);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());
		csdResultProvider.getParseResult(csdSourceObject2);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());

		testCachingForValidAndInvalidKey(jobId, getKeyForCsd(csdSourceObject1, csdResultProvider), csdParseResult);
	}

	/**
	 * Tests that the {@link JavaParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForJavaParser() throws DiscoveryException, WorkerException {
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(JavaModel.class));

		final SourcePojo javaSourceObject1 = createSource("JAVA_PRG1", "path3", Technology.JAVA, CONTENT_BASIC);
		final SourcePojo javaSourceObject2 = createSource("JAVA_PRG2", "path4", Technology.JAVA, CONTENT_BASIC);
		final JavaParseResultProvider javaResultProvider = new JavaParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, "path", jobId,
				parseResultCacheServiceMock, new SearchOrders(Collections.singletonList(new SearchOrder())));

		/* check that nothing is cached so far */
		testForEmptyCache(jobId, javaSourceObject1, javaSourceObject2, javaResultProvider);

		/* Call javaProvider for javaSourceObject1 and check that the result of javaSourceObject1 got cached */
		final JavaModel javaParseResult = javaResultProvider.getParseResult(javaSourceObject1);
		testForCaching(jobId, getKey(javaSourceObject1, javaResultProvider), 1);

		/* Call javaProvider for javaSourceObject2 and check that the result of javaSourceObject2 got cached */
		javaResultProvider.getParseResult(javaSourceObject2);
		testForCaching(jobId, getKey(javaSourceObject2, javaResultProvider), 2);

		/* test that when provider is called multiple times for the same SourcePojo with same path, then the cached result is returned */
		javaResultProvider.getParseResult(javaSourceObject1);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());
		javaResultProvider.getParseResult(javaSourceObject2);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());

		testCachingForValidAndInvalidKey(jobId, getKey(javaSourceObject1, javaResultProvider), javaParseResult);
	}

	/**
	 * Tests that the {@link PlSqlParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 */
	@Test
	void testCachingForPlSqlParser() throws DiscoveryException, WorkerException {
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(PlSqlModel.class));

		final SourcePojo plSqlSourceObject1 = createSource("plSql_PRG1", "path3", Technology.SQL, CONTENT_BASIC);
		final SourcePojo plSqlSourceObject2 = createSource("plSql_PRG2", "path4", Technology.SQL, CONTENT_BASIC);
		final PlSqlParseResultProvider plSqlResultProvider =
				new PlSqlParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, jobId, parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		testForEmptyCache(jobId, plSqlSourceObject1, plSqlSourceObject2, plSqlResultProvider);

		/* Call plSqlProvider for plSqlSourceObject1 and check that the result of plSqlSourceObject1 got cached */
		final PlSqlModel plSqlParseResult = plSqlResultProvider.getParseResult(plSqlSourceObject1);
		testForCaching(jobId, getKey(plSqlSourceObject1, plSqlResultProvider), 1);

		/* Call plSqlProvider for plSqlSourceObject2 and check that the result of plSqlSourceObject2 got cached */
		plSqlResultProvider.getParseResult(plSqlSourceObject2);
		testForCaching(jobId, getKey(plSqlSourceObject2, plSqlResultProvider), 2);

		/* test that when provider is called multiple times for the same SourcePojo with same path, then the cached result is returned */
		plSqlResultProvider.getParseResult(plSqlSourceObject1);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());
		plSqlResultProvider.getParseResult(plSqlSourceObject2);
		verify(timedWorkerMock, times(2)).execute(any(), anyLong(), any(), any());

		testCachingForValidAndInvalidKey(jobId, getKey(plSqlSourceObject1, plSqlResultProvider), plSqlParseResult);
	}

	/**
	 * Tests that the {@link Pl1ParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 * @throws WorkerException if parsing failed
	 */
	@Test
	void testCachingForPl1Parser() throws DiscoveryException, WorkerException {
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(Pl1ParseResultProvider.Pl1ParseResult.class));

		final SourcePojo pl1SourceObject1 = createSource("pl1_PRG1", "path3", Technology.PL1, CONTENT_BASIC);
		final SourcePojo pl1SourceObject2 = createSource("pl1_PRG2", "path4", Technology.PL1, CONTENT_BASIC);
		final Pl1ParseResultProvider pl1ResultProvider =
				new Pl1ParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, jobId, parseResultCacheServiceMock, new SourceObjectResolver() {

					@Nullable
					@Override
					public SourcePojo resolveObject(SourcePojo context, String targetName, SourceObjectMatcher targetMatcher) {
						return null;
					}

					@Nullable
					@Override
					public SourcePojo resolveObject(SourcePojo context, String targetName) {
						return null;
					}
				}, Collections.emptyMap());


		/* check that nothing is cached so far */
		testForEmptyCache(jobId, pl1SourceObject1, pl1SourceObject2, pl1ResultProvider);

		/* Call pl1Provider for pl1SourceObject1 and check that the result of pl1SourceObject1 got cached */
		final Pl1ParseResult pl1ParseResult = pl1ResultProvider.getParseResult(pl1SourceObject1);
		testForCaching(jobId, getKey(pl1SourceObject1, pl1ResultProvider), 1);

		/* Call pl1Provider for pl1SourceObject2 and check that the result of pl1SourceObject2 got cached */
		pl1ResultProvider.getParseResult(pl1SourceObject2);
		testForCaching(jobId, getKey(pl1SourceObject2, pl1ResultProvider), 2);

		testCachingForValidAndInvalidKey(jobId, getKey(pl1SourceObject1, pl1ResultProvider), pl1ParseResult);
	}

	/**
	 * Tests that the {@link ImsParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 * @throws WorkerException if parsing failed
	 */
	@Test
	void testCachingForImsParser() throws DiscoveryException, WorkerException{

		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(ImsParseResultProvider.ImsParseResult.class));

		final SourcePojo imsSourceObject1 = createSource("ims_PRG1", "path3", Technology.IMS, CONTENT_BASIC);
		final SourcePojo imsSourceObject2 = createSource("ims_PRG2", "path4", Technology.IMS, CONTENT_BASIC);
		final ImsParseResultProvider imsResultProvider =
				new ImsParseResultProvider(Config.getDefaultConfig(), timedWorkerMock, jobId, parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		testForEmptyCache(jobId, imsSourceObject1, imsSourceObject2, imsResultProvider);

		/* Call imsProvider for imsSourceObject1 and check that the result of imsSourceObject1 got cached */
		final ImsParseResult imsParseResult = imsResultProvider.getParseResult(imsSourceObject1);
		testForCaching(jobId, getKey(imsSourceObject1, imsResultProvider), 1);

		/* Call imsProvider for imsSourceObject2 and check that the result of imsSourceObject2 got cached */
		imsResultProvider.getParseResult(imsSourceObject2);
		testForCaching(jobId, getKey(imsSourceObject2, imsResultProvider), 2);

		testCachingForValidAndInvalidKey(jobId, getKey(imsSourceObject1, imsResultProvider), imsParseResult);
	}

	/**
	 * Tests that the {@link NaturalParseResultProvider} is caching parse results and serves them from the cache once they are present.
	 *
	 * @throws DiscoveryException if parsing failed
	 * @throws WorkerException if parsing failed
	 */
	@Test
	void testCachingForNaturalParser() throws DiscoveryException, WorkerException{
		when(timedWorkerMock.execute(any(), anyLong(), any(), any())).thenReturn(mock(NaturalParseResultProvider.NaturalParseResult.class));

		final SourcePojo naturalSourceObject1 = createSource("nat_PRG1", "path3", NATURAL, CONTENT_BASIC);
		final SourcePojo naturalSourceObject2 = createSource("nat_PRG2", "path4", NATURAL, CONTENT_BASIC);

		final SourceService sourceObjectDao = Mockito.mock(SourceService.class);
		final SearchOrders searchOrder = new SearchOrders(Arrays.asList(SearchOrder.fromPatterns("**/*", "./*", "**/*")));
		final SourceObjectResolver sourceObjectResolver = new PersistingSourceObjectResolver(sourceObjectDao, searchOrder);
		final NaturalParseResultProvider naturalParseResultProvider =
				new NaturalParseResultProvider(sourceObjectResolver, Config.getDefaultConfig(), timedWorkerMock, jobId, parseResultCacheServiceMock);

		/* check that nothing is cached so far */
		testForEmptyCache(jobId, naturalSourceObject1, naturalSourceObject2, naturalParseResultProvider);

		/* Call naturalProvider for naturalSourceObject1 and check that the result of naturalSourceObject1 got cached */
		final NaturalParseResult naturalParseResult = naturalParseResultProvider.getParseResult(naturalSourceObject1);
		testForCaching(jobId, getKey(naturalSourceObject1, naturalParseResultProvider), 1);

		final INaturalModel heavyWeightModelMock = mock(INaturalModel.class);
		final INaturalParseResult lightWeightModelMock = mock(INaturalParseResult.class);
		final Tuple2<INaturalModel, INaturalParseResult> parseResult = Tuple2.of(heavyWeightModelMock, lightWeightModelMock);

		ReflectionTestUtils.setField(naturalParseResult, "parseResult", parseResult);

		final Optional<DataArea> dataAreaMock = Optional.of(mock(DataArea.class));
		ReflectionTestUtils.setField(naturalParseResult, "dataArea", dataAreaMock);

		final NaturalParseResult naturalParseResult1 = naturalParseResultProvider.getParseResult(naturalSourceObject1);
		testForCaching(jobId, getKey(naturalSourceObject1, naturalParseResultProvider), 1);
		assertSame(naturalParseResult.getHeavyweightModel(), naturalParseResult1.getHeavyweightModel(), "NaturalModel must be cached");
		assertSame(naturalParseResult.getDataArea(), naturalParseResult1.getDataArea(), "DataArea must be cached");

		/* Call naturalProvider for naturalSourceObject2 and check that the result of naturalSourceObject2 got cached */
		naturalParseResultProvider.getParseResult(naturalSourceObject2);
		testForCaching(jobId, getKey(naturalSourceObject2, naturalParseResultProvider), 2);

		testCachingForValidAndInvalidKey(jobId, getKey(naturalSourceObject1, naturalParseResultProvider), naturalParseResult);
	}

	/**
	 * To create key for the cache.
	 *
	 * @param sourceObject The {@link SourcePojo} for calculating the key for caching
	 * @param parseResultProvider The {@link AbstractCachingParseResultProvider} for calculating the key for caching
	 * @return the key for cache
	 */
	private String getKey(final SourcePojo sourceObject, final AbstractCachingParseResultProvider<?> parseResultProvider) {
		return sourceObject.getPath() + "$" + parseResultProvider.getClass().getSimpleName();
	}

	/**
	 * To create key for the csd parser cache.
	 *
	 * @param sourceObject The {@link SourcePojo} for calculating the key for caching
	 * @param parseResultProvider The {@link AbstractCachingParseResultProvider} for calculating the key for caching
	 * @return the key for csd parser cache
	 */
	private String getKeyForCsd(final SourcePojo sourceObject, final AbstractCachingParseResultProvider<?> parseResultProvider) {
		return getKey(sourceObject, parseResultProvider) + "$" + CsdParserType.LIST;
	}

	/**
	 * Tests whether the parse result of sourceObject got cached.
	 */
	private void testForCaching(final String jobId, final String key, final int invocationTimes) throws WorkerException {
		verify(timedWorkerMock, times(invocationTimes)).execute(any(), anyLong(), any(), any());
		assertNotNull(parseResultCacheServiceMock.getCached(jobId, key), "Cached result for sourceObject must NOT be null");
	}

	/**
	 * Tests for caching result for valid and invalid keys.
	 */
	private <T> void testCachingForValidAndInvalidKey(final String jobId, final String key, final T parseResult) {
		/* test cached value is same as the parse result value for the specific key */
		assertEquals(parseResultCacheServiceMock.getCached(jobId, key), parseResult);

		/* test for invalid key */
		assertNotEquals(parseResultCacheServiceMock.getCached("blah", key), parseResult);
	}

	/**
	 * Test to check nothing has been cached.
	 */
	private void testForEmptyCache(final String jobId, final SourcePojo sourceObject1, final SourcePojo sourceObject2,
			final AbstractCachingParseResultProvider<?> parseResultProvider) {
		assertNull(parseResultCacheServiceMock.getCached(jobId, getKey(sourceObject1, parseResultProvider)), "Cached result for sourceObject1 must be null");
		assertNull(parseResultCacheServiceMock.getCached(jobId, getKey(sourceObject2, parseResultProvider)), "Cached result for sourceObject2 must be null");
	}

}
