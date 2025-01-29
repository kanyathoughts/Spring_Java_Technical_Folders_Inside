/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;

/**
 * Tests for {@link SearchOrders #resolvePattern(String)}.
 */
public class ResolvePatternTest {

	@Test
	public void testResolvePattern() {
		final String path = "application/programs/a/A.cbl";
		final List<String> targetPattern1 = Arrays.asList("application/copies/a/A.cpy", "application/copies/b/B.cpy");
		final List<String> targetPattern2 = Arrays.asList("application/copies/d/D.cpy", "application/copies/e/E.cpy");

		final SearchOrder searchOrder1 = SearchOrder.fromPatterns("application/programs/a/C.cbl", targetPattern1);
		final SearchOrder searchOrder2 = SearchOrder.fromPatterns("application/programs/a/A.cbl", targetPattern2);

		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder1);
		searchOrdersList.add(searchOrder2);

		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);
		final List<String> targetPatterns = Arrays.asList(searchOrders.resolvePattern(path));

		assertEquals(2, targetPatterns.size());
		assertEquals("application/copies/d/D.cpy", targetPatterns.get(0));
		assertEquals("application/copies/e/E.cpy", targetPatterns.get(1));
	}

	@Test
	public void testResolvePatternToMatchFile() {
		final String path = "A.cbl";
		final List<String> targetPattern1 = Arrays.asList("application/copies/a/A.cpy", "application/copies/b/B.cpy");
		final List<String> targetPattern2 = Arrays.asList("application/copies/d/D.cpy", "application/copies/e/E.cpy");

		final SearchOrder searchOrder1 = SearchOrder.fromPatterns("application/programs/a/A.cbl", targetPattern1);
		final SearchOrder searchOrder2 = SearchOrder.fromPatterns("A.cbl", targetPattern2);

		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder1);
		searchOrdersList.add(searchOrder2);

		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);
		final List<String> targetPatterns = Arrays.asList(searchOrders.resolvePattern(path));

		assertEquals(2, targetPatterns.size());
		assertEquals("application/copies/d/D.cpy", targetPatterns.get(0));
		assertEquals("application/copies/e/E.cpy", targetPatterns.get(1));
	}

	@Test
	public void testResolvePatternToMatchCurrentDirectory() {
		final String path = "application/Test.cbl";
		final List<String> targetPattern1 = Arrays.asList("application/copies/a/A.cpy", "application/copies/b/B.cpy");
		final List<String> targetPattern2 = Arrays.asList("application/copies/d/D.cpy", "application/copies/e/E.cpy");
		final List<String> targetPattern3 = Arrays.asList("application/copies/f/F.cpy", "application/copies/g/G.cpy");

		final SearchOrder searchOrder1 = SearchOrder.fromPatterns("test/Test.cbl", targetPattern1);
		final SearchOrder searchOrder2 = SearchOrder.fromPatterns("test/application/Test.cbl", targetPattern2);
		final SearchOrder searchOrder3 = SearchOrder.fromPatterns("application/Test.cbl", targetPattern3);

		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder1);
		searchOrdersList.add(searchOrder2);
		searchOrdersList.add(searchOrder3);

		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);
		final List<String> targetPatterns = Arrays.asList(searchOrders.resolvePattern(path));

		assertEquals(2, targetPatterns.size());
		assertEquals("application/copies/f/F.cpy", targetPatterns.get(0));
		assertEquals("application/copies/g/G.cpy", targetPatterns.get(1));
	}

	@Test
	public void testResolvePatternToMatchParentDirectory() {
		final String path = "application/copies/A.cbl";
		final List<String> targetPattern1 = Arrays.asList("application/copies/a/A.cpy", "application/copies/b/B.cpy");
		final List<String> targetPattern2 = Arrays.asList("application/copies/d/D.cpy", "application/copies/e/E.cpy");
		final List<String> targetPattern3 = Arrays.asList("application/copies/f/F.cpy", "application/copies/g/G.cpy");
		final List<String> targetPattern4 = Arrays.asList("application/copies/h/H.cpy", "application/copies/i/I.cpy");

		final SearchOrder searchOrder1 = SearchOrder.fromPatterns("copies/A.cbl", targetPattern1);
		final SearchOrder searchOrder2 = SearchOrder.fromPatterns("application/copies/copy/A.cbl", targetPattern2);
		final SearchOrder searchOrder3 = SearchOrder.fromPatterns("application/copies/X.cbl", targetPattern3);
		final SearchOrder searchOrder4 = SearchOrder.fromPatterns("application/copies/A.cbl", targetPattern4);

		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder1);
		searchOrdersList.add(searchOrder2);
		searchOrdersList.add(searchOrder3);
		searchOrdersList.add(searchOrder4);

		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);
		final List<String> targetPatterns = Arrays.asList(searchOrders.resolvePattern(path));

		assertEquals(2, targetPatterns.size());
		assertEquals("application/copies/h/H.cpy", targetPatterns.get(0));
		assertEquals("application/copies/i/I.cpy", targetPatterns.get(1));
	}

	@Test
	public void testResolvePatternToMatchZeroOrMoreDirectories() {
		final String path = "src/cobol/copies/A.cbl";
		final List<String> targetPattern1 = Arrays.asList("application/copies/a/A.cpy", "application/copies/b/B.cpy");
		final List<String> targetPattern2 = Arrays.asList("application/copies/d/D.cpy", "application/copies/e/E.cpy");
		final List<String> targetPattern3 = Arrays.asList("application/copies/f/F.cpy", "application/copies/g/G.cpy");
		final List<String> targetPattern4 = Arrays.asList("application/copies/h/H.cpy", "application/copies/i/I.cpy");

		final SearchOrder searchOrder1 = SearchOrder.fromPatterns("A.cbl", targetPattern1);
		final SearchOrder searchOrder2 = SearchOrder.fromPatterns("copies/A.cbl", targetPattern2);
		final SearchOrder searchOrder3 = SearchOrder.fromPatterns("src/cobol/copies/A.cbl", targetPattern3);
		final SearchOrder searchOrder4 = SearchOrder.fromPatterns("src/cobol/copies/A.cbl", targetPattern4);

		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder1);
		searchOrdersList.add(searchOrder2);
		searchOrdersList.add(searchOrder3);
		searchOrdersList.add(searchOrder4);

		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);
		final List<String> targetPatterns = Arrays.asList(searchOrders.resolvePattern(path));

		assertEquals(4, targetPatterns.size());
		assertEquals("application/copies/f/F.cpy", targetPatterns.get(0));
		assertEquals("application/copies/g/G.cpy", targetPatterns.get(1));
	}
	
	@Test
	public void testMultipleResolvePattern() {
		final List<String> targetPattern = Arrays.asList("application/copies/a/A.cpy", "application/copies/b/B.cpy");
		
		final SearchOrder searchOrder = SearchOrder.fromPatterns("application/programs/**/*.cbl", targetPattern);
		
		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder);

		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);
		final List<String> targetPatterns = Arrays.asList(searchOrders.resolvePattern("application/programs/a/A.cbl"));

		assertEquals(2, targetPatterns.size());
		assertEquals("application/copies/a/A.cpy", targetPatterns.get(0));
		assertEquals("application/copies/b/B.cpy", targetPatterns.get(1));
		
		final List<String> targetPatterns1 = Arrays.asList(searchOrders.resolvePattern("application/programs/B.cbl"));
		assertEquals(2, targetPatterns1.size());
		assertEquals("application/copies/a/A.cpy", targetPatterns1.get(0));
		assertEquals("application/copies/b/B.cpy", targetPatterns1.get(1));
		
		final List<String> targetPatterns2 = Arrays.asList(searchOrders.resolvePattern("application/programs/a/A.cpy"));
		assertTrue(targetPatterns2.isEmpty());
		
		final List<String> targetPatterns3 = Arrays.asList(searchOrders.resolvePattern("application/B.cbl"));
		assertTrue(targetPatterns3.isEmpty());
	}
	
	@Test
	public void testResolvePatternNoMatch() {
		final String path = "application/programs/a/A.cpy";
		final List<String> targetPattern1 = Arrays.asList("application/copies/a/A.cpy", "application/copies/b/B.cpy");
		final List<String> targetPattern2 = Arrays.asList("application/copies/d/D.cpy", "application/copies/e/E.cpy");

		final SearchOrder searchOrder1 = SearchOrder.fromPatterns("application/programs/a/A.cbl", targetPattern1);
		final SearchOrder searchOrder2 = SearchOrder.fromPatterns("application/programs/b/A.cbl", targetPattern2);

		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder1);
		searchOrdersList.add(searchOrder2);

		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);
		final List<String> targetPatterns = Arrays.asList(searchOrders.resolvePattern(path));

		assertTrue(targetPatterns.isEmpty());
	}
	
	@Test
	public void testResolvePatternDuplicateSourcePattern() {
		final String path = "application/programs/a/A.cbl";
		final List<String> targetPattern = Arrays.asList("application/copies/a/A.cpy", "application/copies/b/B.cpy", "application/copies/d/D.cpy",
				"application/copies/e/E.cpy");
		final List<String> targetPattern1 = Arrays.asList(targetPattern.get(0), targetPattern.get(1));
		final List<String> targetPattern2 = Arrays.asList(targetPattern.get(2), targetPattern.get(3));

		final SearchOrder searchOrder1 = SearchOrder.fromPatterns(path, targetPattern1);
		final SearchOrder searchOrder2 = SearchOrder.fromPatterns(path, targetPattern2);

		final List<SearchOrder> searchOrdersList = new ArrayList<>();
		searchOrdersList.add(searchOrder1);
		searchOrdersList.add(searchOrder2);

		final SearchOrders searchOrders = new SearchOrders(searchOrdersList);
		final List<String> resolvedTargetPatterns = Arrays.asList(searchOrders.resolvePattern(path));

		assertEquals(targetPattern, resolvedTargetPatterns);
	}

}
