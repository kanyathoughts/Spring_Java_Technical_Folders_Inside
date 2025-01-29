/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.natural;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorkerImpl;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.access.SourceService.SourceInquiryBuilder;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.NaturalObjectType;

/**
 * Tests for the {@link NaturalParseResultProvider}.
 */
public class NaturalParserTest {

	/**
	 * For asserting that the changes made with WMIN-1547 to NaturalParserFactory.createObjectResolver are working correctly
	 *
	 * @throws DiscoveryException in case of a DiscoveryException
	 * @throws IOException in case of an IOException
	 */
	@Test
	public void testObjectResolver() throws DiscoveryException, IOException {
		final String resourcePath = "./test-resources/innowake/mining/server/discovery/parser/natural/WMIN1547/";
		final String expectedPath = resourcePath + "expected/WMIN1547.assembling";
		final NaturalParseResultProvider naturalParser = createNaturalParserResultProvider(resourcePath);
		final SourcePojo contextNatObject = getSourceObject(resourcePath, "MAIN.nsp");
		final INaturalModel model = naturalParser.getParseResult(contextNatObject).getHeavyweightModel();

		assertEquals(new String(Files.readAllBytes(Paths.get(expectedPath))), model.toString().replaceAll("\r?\n", "\r\n"));
	}

	private static NaturalParseResultProvider createNaturalParserResultProvider(final String rootDirectory) {
		final SourceService sourceService = getSourceServiceMock(rootDirectory);
		final SearchOrder order = SearchOrder.fromPatterns("**/*", "./*", "**/*");
		final List<SearchOrder> searchOrders = Arrays.asList(order);
		final SearchOrders searchOrder = new SearchOrders(searchOrders);
		final SourceObjectResolver sourceObjectResolver = new PersistingSourceObjectResolver(sourceService, searchOrder);
		final TimedWorker timedWorker = new TimedWorkerImpl(new NullProgressMonitor(), null, null);
		final String jobId = CityHash.cityHash128Hex(rootDirectory);
		final GenericConfigProperties configProperties = new GenericConfigProperties(0, 0, 0, 0, null, 1000000, 0, 0, 0, 0L, true, false, 8, 5_000_000, -1L,
				10D, 100, null, 1000, false, 0, 0, 0, 0, 0, null, 0, 0, 0, 0);
		final ParseResultCacheService parseResultCacheService = new ParseResultCacheService(configProperties);
		return new NaturalParseResultProvider(sourceObjectResolver, Config.getDefaultConfig(), timedWorker, jobId, parseResultCacheService);
	}

	@SuppressWarnings("unchecked")
	private static SourceService getSourceServiceMock(final String root) {
		final var sourceObjectDao = Mockito.mock(SourceService.class);
		/* When SourceService.find(BuildingConsumer<SourceInquiryBuilder>) is called, then return the SourceObject
		 * that matches with the name and types which are set by the PersistingSourceObjectResolver */
		when(sourceObjectDao.find(any())).then(i -> {
			/* The BuildingConsumer created by the PersistingSourceObjectResolver */
			final BuildingConsumer<SourceInquiryBuilder> builderConsumer = (BuildingConsumer<SourceInquiryBuilder>) i.getArgument(0);
			final SourceInquiryBuilder builder = Mockito.mock(SourceInquiryBuilder.class);
			final ArgumentCaptor<String> nameCaptor = ArgumentCaptor.forClass(String.class);
			final ArgumentCaptor<Set<Type>> typesCaptor = ArgumentCaptor.forClass(Set.class);

			/* Avoid NPEs when chaining is used when calling filter methods */
			when(builder.ofProject(any())).thenReturn(builder);
			when(builder.withPathRegex(any())).thenReturn(builder);
			when(builder.withName(any())).thenReturn(builder);
			when(builder.withTechnology((Technology) any())).thenReturn(builder);
			when(builder.withType((Collection<Type>) any())).thenReturn(builder);

			builderConsumer.prepare(builder);

			Mockito.verify(builder).withName(nameCaptor.capture());
			Mockito.verify(builder).withType(typesCaptor.capture());

			final String name = nameCaptor.getValue();
			final Set<Type> types = typesCaptor.getValue();
			final NaturalObjectType objectType = NaturalObjectType.getInnofied(NaturalParseResultProvider.mapType(types.iterator().next()));
			final String suffix = objectType.getFileSuffix();
			final String fileName = name + '.' + suffix;
			return List.of(getSourceObject(root, fileName));
		});

		return sourceObjectDao;
	}

	/**
	 * @param fileName the file name with or without extension
	 * @param root the root file path, project relative
	 * @return the source object
	 */
	private static SourcePojo getSourceObject(final String root, final String fileName) {
		final String path = getFilePath(root, fileName);
		try {
			final BinaryString content = new BinaryString(Files.readAllBytes(Paths.get(path)));
			return SourcePojoDummy.build(o -> o
					.setProject(EntityId.of(5L))
					.setName(fileName)
					.setPath(path)
					.setTechnology(Technology.NATURAL)
					.setType(Type.PROGRAM)
					.setContent(content));
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	private static String getFilePath(final String root, final String fileName) {
		try {
			return Files.walk(Paths.get(root))
					.filter(Files::isRegularFile)
					.filter(f -> f.getFileName().toString().startsWith(fileName))
					.findAny()
					.get()
					.toString();
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
}
