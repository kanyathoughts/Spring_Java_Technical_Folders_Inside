/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.parser.jcl;

import static innowake.lib.core.lang.Assert.assertNull;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import innowake.mining.server.discovery.parser.batch.DiscoveryJclContentProvider;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceCachingServiceDummy;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests whether the {@code DiscoveryJclContentProvider#getProc()} and {@code DiscoveryJclContentProvider#getIncludeMember()}
 * returns the content of the file by checking with the JCL Lib Orders.
 */
class DiscoveryJclContentProviderTest extends DatabaseRelatedTest {

	private DiscoveryJclContentProvider discoveryJclContentProvider;
	private List<SourcePojo> testData;

	@BeforeAll
	void initialize() {
		testData = new ArrayList<>(16);
		discoveryJclContentProvider = new DiscoveryJclContentProvider(EntityId.of(-1L), new SourceCachingServiceDummy(testData));
	}
	
	@BeforeEach
	void resetTestSources() {
		testData.clear();
		testData.add(createSourcePojo("testProc", "/proc/testProc.proc", Type.PROC, "testProcContent1"));
		testData.add(createSourcePojo("TESTPROC", "/proc/TESTPROC.proc", Type.PROC, "testProcContent2"));
		testData.add(createSourcePojo("testProc", "/proc/A.B/testProc.proc", Type.PROC, "testProcContent3"));
		testData.add(createSourcePojo("testProc", "/proc/B/C/testProc.proc", Type.PROC, "testProcContent4"));
		testData.add(createSourcePojo("testInclude", "/include/testInclude.proc", Type.INCLUDE, "testIncludeContent1"));
		testData.add(createSourcePojo("TESTINCLUDE", "/include/TESTINCLUDE.proc", Type.PROC, "testIncludeContent2"));
		testData.add(createSourcePojo("testInclude", "/include/A.B/testInclude.job", Type.JOB, "testIncludeContent3"));
		testData.add(createSourcePojo("testInclude", "/include/B/C/testInclude.proc", Type.PROC, "testIncludeContent4"));
	}
	
	@Test
	void testContentWithoutSourcePojos() {
		testData.clear();
		final IllegalStateException exception = assertThrows(IllegalStateException.class,
				() -> discoveryJclContentProvider.getProc("testProc", Collections.emptyList()));
		assertTrue(exception.getMessage().contains("Procedure not found with the provided name"));
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.emptyList());
		assertNull(includeContent, "Content of testInclude should be null");
	}

	@Test
	void testGetProcWithoutJclLibOrders() {
		final String procContent = discoveryJclContentProvider.getProc("testProc", Collections.emptyList());
		assertEquals("testProcContent1", procContent);
	}
	
	@Test
	void testGetProcWithOneMatchingJclLibOrder1() {
		final String procContent = discoveryJclContentProvider.getProc("testProc", Collections.singletonList("A.B"));
		assertEquals("testProcContent3", procContent);
	}
	
	@Test
	void testGetProcWithOneMatchingJclLibOrder2() {
		final String procContent = discoveryJclContentProvider.getProc("testProc", Collections.singletonList("B.C"));
		assertEquals("testProcContent4", procContent);
	}
	
	@Test
	void testGetProcWithTwoMatchingJclLibOrders() {
		final String procContent = discoveryJclContentProvider.getProc("testProc", Arrays.asList("B.C", "A.B"));
		assertEquals("testProcContent4", procContent);
	}
	
	@Test
	void testGetProcWithoutAnyMatchingJclLibOrders() {
		final String procContent = discoveryJclContentProvider.getProc("testProc", Arrays.asList("X.Y.Z", "X.Y"));
		assertEquals("testProcContent1", procContent);
	}
	
	@Test
	void testGetIncludeMemberWithoutJclLibOrders() {
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.emptyList());
		assertEquals("testIncludeContent1", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithOneMatchingJclLibOrder1() {
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.singletonList("A.B"));
		assertEquals("testIncludeContent3", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithOneMatchingJclLibOrder2() {
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.singletonList("B.C"));
		assertEquals("testIncludeContent4", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithTwoMatchingJclLibOrders() {
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Arrays.asList("B.C", "A.B"));
		assertEquals("testIncludeContent4", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithoutAnyMatchingJclLibOrders() {
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Arrays.asList("X.Y.Z", "X.Y"));
		assertEquals("testIncludeContent1", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithMatchingJclLibOrdersWithPrecedence1() {
		/* SourcePojo with Type as JCL_INCLUDE will be the first in precedence order when multiple matches */
		testData.removeIf(s -> s.getName().startsWith("testInclude"));
		testData.add(createSourcePojo("testInclude", "/include/A.B/testInclude.job", Type.JOB, "testIncludeContentJob"));
		testData.add(createSourcePojo("testInclude", "/include/A/B/testInclude.proc", Type.PROC, "testIncludeContentProc"));
		testData.add(createSourcePojo("testInclude", "/include/A.B/testInclude.job", Type.INCLUDE, "testIncludeContentInclude"));
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.singletonList("A.B"));
		assertEquals("testIncludeContentInclude", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithMatchingJclLibOrdersWithPrecedence2() {
		testData.removeIf(s -> s.getName().startsWith("testInclude"));
		/* SourcePojo with Type as JCL_PROC will be the second in precedence order when multiple matches */
		testData.add(createSourcePojo("testInclude", "/include/A.B/testInclude.job", Type.JOB, "testIncludeContentJob"));
		testData.add(createSourcePojo("testInclude", "/include/A/B/testInclude.proc", Type.PROC, "testIncludeContentProc"));
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.singletonList("A.B"));
		assertEquals("testIncludeContentProc", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithMatchingJclLibOrdersWithPrecedence3() {
		testData.removeIf(s -> s.getName().startsWith("testInclude"));
		/* SourcePojo with Type as JCL_JOB will be the third in precedence order when multiple matches */
		testData.add(createSourcePojo("testInclude", "/include/A.B/testInclude.job", Type.PGM, "testIncludeContentPgm"));
		testData.add(createSourcePojo("testInclude", "/include/A/B/testInclude.job", Type.JOB, "testIncludeContentJob"));
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.singletonList("A.B"));
		assertEquals("testIncludeContentJob", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithMatchingJclLibOrdersWithPrecedence4() {
		testData.removeIf(s -> s.getName().startsWith("testInclude"));
		/* If there is no matching type among (JCL_INCLUDE, JCL_PROC, JCL_JOB) present with the given JCL lib orders then first matching SourcePojo content 
		 * will be returned. */
		testData.add(createSourcePojo("testInclude", "/include/A.B/testInclude.job", Type.PGM, "testIncludeContentOfPgm"));
		testData.add(createSourcePojo("testInclude", "/include/A/B/testInclude.job", Type.CONTROLCARD, "testIncludeContentControlCard"));
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.singletonList("A.B"));
		assertEquals("testIncludeContentOfPgm", includeContent);
	}
	
	@Test
	void testGetIncludeMemberWithMatchingJclLibOrdersWithSameType() {
		testData.removeIf(s -> s.getName().startsWith("testInclude"));
		testData.add(createSourcePojo("testInclude", "/include/A.B/testInclude.job", Type.PGM, "testIncludeContentOfPgm1"));
		testData.add(createSourcePojo("testInclude", "/include/A/B/testInclude.job", Type.PGM, "testIncludeContentOfPgm2"));
		final String includeContent = discoveryJclContentProvider.getIncludeMember("testInclude", Collections.singletonList("A.B"));
		assertEquals("testIncludeContentOfPgm1", includeContent);
	}

	@Test
	void testGetContentWithSearchOrder() {
		importSearchOrder("/proc/B/C/*");
		final String procContent = discoveryJclContentProvider.getProc("testProc", Collections.emptyList());
		assertEquals("testProcContent4", procContent);
	}

	@Test
	void testGetProcWithOneMatchingJclLibOrderAndSearchOrder() {
		importSearchOrder("/proc/B/C/*");
		final String procContent = discoveryJclContentProvider.getProc("testProc", Collections.singletonList("B"));
		assertEquals("testProcContent4", procContent);
	}

	@Test
	void testGetWithGenericSearchOrder() {
		importSearchOrder("./*");
		final String procContent = discoveryJclContentProvider.getProc("testProc", Collections.emptyList());
		assertEquals("testProcContent1", procContent);
	}

	@Test
	void testGetProcWithOneMatchingJclLibOrderAndGenericSearchOrder() {
		importSearchOrder("./*");
		final String procContent = discoveryJclContentProvider.getProc("testProc", Collections.singletonList("B"));
		assertEquals("testProcContent3", procContent);
	}

	private void importSearchOrder(final String targetPatternRegex) {
		final List<String> targetPattern = Arrays.asList(targetPatternRegex);
		final SearchOrder searchOrder = SearchOrder.fromPatterns("src/**/A/**/*", targetPattern);
		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder);
		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);

		discoveryJclContentProvider = new DiscoveryJclContentProvider(EntityId.of(-1L), new SourceCachingServiceDummy(testData), searchOrders, "src/**/A/**/*");
	}

	private SourcePojo createSourcePojo(final String name, final String path, final Type type, final String content) {
		return SourcePojoDummy.build(o -> o.setName(name).setPath(path).setTechnology(Technology.JCL).setType(type).setContent(new BinaryString(content)));
		//return new SourcePojo(-1L, "Test", path, Technology.JCL, type, 0L, 0L, content, CityHash.cityHash128Hex(content));
	}
}
