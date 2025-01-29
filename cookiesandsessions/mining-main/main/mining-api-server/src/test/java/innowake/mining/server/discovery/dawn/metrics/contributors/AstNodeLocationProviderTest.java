/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.dawn.metrics.test.DiscoveryTestContext;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.ndt.cobol.parser.ast.statement.CobolGoBackStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolMoveStmt;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

/**
 * Tests for {@link AstNodeLocationProvider}.
 */
@WithMockUser
class AstNodeLocationProviderTest extends BaseDiscoveryTest {

	@Autowired
	private ParserProviderService parserProviderService;

	@Test
	void testGetAstNodeLocation() throws DiscoveryException {
		final var projectId = setupTestData();
		final var sources = sourceService.find(q -> q.ofProject(projectId));
		assertEquals(2, sources.size());
		final var program = sources.stream().filter(source -> source.getName().equals("PGM1")).findFirst();
		assertTrue(program.isPresent());

		final var testContext = new DiscoveryTestContext(sources, projectId);
		final var cobolParser = parserProviderService.createCobolParser(testContext);
		final var parseResult = cobolParser.getParseResult(program.get());
		final var locationProvider = new AstNodeLocationProvider<>(parseResult.e2.getAssembling(), program.get().getContent().toString());
		final var procedureDivision = parseResult.e1.getCobolProgram().getProcedureDivision();
		assertNotNull(procedureDivision);

		/* Move statement which is inside copybook */
		final var nodeInsideCopyBook = procedureDivision.getChildrenDeep(CobolMoveStmt.class);
		assertEquals(1, nodeInsideCopyBook.size());
		final var location = locationProvider.getAstNodeLocation(nodeInsideCopyBook.get(0));
		final var expectedLocation = "AstNodeLocation [retracedOffset=117, retracedLength=15, assembledOffset=352, assembledLength=15," +
				" rootRelativeOffset=205, rootRelativeLength=8, rootRelativeStartLineNumber=9, rootRelativeEndLineNumber=9]";
		assertEquals(expectedLocation, location.toString());

		final var nodeOutsideCopyBook = procedureDivision.getChildrenDeep(CobolGoBackStmt.class);
		assertEquals(1, nodeOutsideCopyBook.size());
		final var location2 = locationProvider.getAstNodeLocation(nodeOutsideCopyBook.get(0));
		final var expectedLocation2 = "AstNodeLocation [retracedOffset=248, retracedLength=6, assembledOffset=426, assembledLength=6, " +
				"rootRelativeOffset=248, rootRelativeLength=6, rootRelativeStartLineNumber=11, rootRelativeEndLineNumber=11]";
		assertEquals(expectedLocation2, location2.toString());
	}

	EntityId setupTestData() {
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		return projectId;
	}

	@Override
	public String getTestFolder() {
		return "WMIN13621";
	}
}
