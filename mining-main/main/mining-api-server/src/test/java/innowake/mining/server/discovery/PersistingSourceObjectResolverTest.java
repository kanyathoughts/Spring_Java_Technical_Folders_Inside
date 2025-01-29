/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery;

import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Technology.NATURAL;
import static innowake.mining.shared.model.Type.COPYBOOK;
import static innowake.mining.shared.model.Type.PROGRAM;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static innowake.lib.core.lang.Assert.assertNotNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for {@link PersistingSourceObjectResolver}.
 */
class PersistingSourceObjectResolverTest extends DatabaseResettingTest {

	private final Long ONE = Long.valueOf(1);
	
	@Autowired
	private SourceService sourceService;
	
	@Nullable
	private PersistingSourceObjectResolver objectResolver;

	@BeforeEach
	public void createObjectResolver() {
		final ProjectPojo project = projectService.get(EntityId.of(ONE));
		objectResolver = new PersistingSourceObjectResolver(sourceService, new SearchOrders(project.getSearchOrders()));
		assertNotNull(objectResolver);
	}
	
	@Test
	void findInSameFolderTest() {
		final SourcePojo program = createProgram(ONE, "a", "PRG1");
		final SourcePojo copy = createCopy(ONE, "a", "CPY1");
		final SourcePojo resolvedObject = resolveObject(program, "CPY1");
		assertEquals(copy.getUid(), assertNotNull(resolvedObject).getUid());
	}
	
	@Test
	void findInSameFolderWithMatcherTest() {
		final SourcePojo program = createProgram(ONE, "a", "PRG1");
		final SourcePojo copy = createCopy(ONE, "a", "CPY1");
		
		final SourcePojo resolvedObject = resolveObject(program, "CPY1", new SourceObjectMatcher(COBOL, COPYBOOK));
		assertEquals(copy.getUid(), assertNotNull(resolvedObject).getUid());
	}
	
	
	@Test
	void notFindInSameFolderWithWrongNameTest() {
		final SourcePojo program = createProgram(ONE, "a", "PRG1");
		final SourcePojo resolvedObject = resolveObject(program, "CPY2", new SourceObjectMatcher(COBOL, COPYBOOK));
		assertNull(resolvedObject);
	}
	
	@Test
	void notFindInWrongFolderTest() {
		final SourcePojo program = createProgram(ONE, "a", "PRG1");
		createCopy(ONE, "b", "CPY1");
		final SourcePojo resolvedObject = resolveObject(program, "CPY1", new SourceObjectMatcher(COBOL, COPYBOOK));
		assertNull(resolvedObject);
	}
	
	@Test
	void notFindInSameFolderWithWrongMatcherTest() {
		final SourcePojo program = createProgram(ONE, "a", "PRG1");
		createCopy(ONE, "a", "CPY1");
		final SourcePojo resolvedObject = resolveObject(program, "CPY1", new SourceObjectMatcher(COBOL, PROGRAM));
		assertNull(resolvedObject);
	}
	
	
	@Test
	
	void findWithSearchOrderTest() {
		final SourcePojo program = createProgram(ONE, "application/programs/A", "PRG1");
		final SourcePojo copy = createCopy(ONE, "application/copies/B", "CPY1");
		
		final SourcePojo resolvedObject = resolveObject(program, "CPY1");
		assertEquals(copy.getUid(), assertNotNull(resolvedObject).getUid());
	}
	
	
	@Test
	void findWithSearchOrder2Test() {
		final SourcePojo program = createProgram(ONE, "application/programs/D", "PRG1");
		final SourcePojo copy = createCopy(ONE, "application/copies/F", "CPY1");
		
		final SourcePojo resolvedObject = resolveObject(program, "CPY1");
		assertEquals(copy.getUid(), assertNotNull(resolvedObject).getUid());
	}
	
	
	@Test
	void notFindWithSearchOrderTest() {
		final SourcePojo program = createProgram(ONE, "application/programs/A", "PRG1");
		createCopy(ONE, "application/copies/C", "CPY1");
		final SourcePojo resolvedObject = resolveObject(program, "CPY1");
		assertNull(resolvedObject);
	}
	
	
	@Test
	void notFindWithSearchOrder2Test() {
		final SourcePojo program = createProgram(ONE, "application/programs/A", "PRG1");
		createCopy(ONE, "application/copies/E", "CPY1");
		final SourcePojo resolvedObject = resolveObject(program, "CPY1");
		assertNull(resolvedObject);
	}
	
	
	@Test
	void matcherTest() {
		final SourcePojo program = createProgram(ONE, "application/programs/C", "PRG1");
		final SourcePojo target1 = createProgram(ONE, "application/copies/C", "TARGET");
		final SourcePojo target2 = createCopy(ONE, "application/copies/C", "TARGET");
		
		final SourcePojo resolvedTarget1 = resolveObject(program, "TARGET", new SourceObjectMatcher(COBOL, PROGRAM));
		assertEquals(target1.getUid(), assertNotNull(resolvedTarget1).getUid());
		
		final SourcePojo resolvedTarget2 = resolveObject(program, "TARGET", new SourceObjectMatcher(COBOL, COPYBOOK));
		assertEquals(target2.getUid(), assertNotNull(resolvedTarget2).getUid());
		
		final SourcePojo resolvedTarget3 = resolveObject(program, "TARGET", new SourceObjectMatcher(NATURAL, COPYBOOK));
		assertNull(resolvedTarget3);
	}
	
	
	@Test
	void findWithQuestionMarkTest() {
		final SourcePojo program = createProgram(ONE, "application/programs/B", "PRG1");
		final SourcePojo copy = createCopy(ONE, "application/copies/A", "TARGET");
		
		final SourcePojo resolvedObject = resolveObject(program, "TARGET");
		assertEquals(copy.getUid(), assertNotNull(resolvedObject).getUid());

		createCopy(ONE, "application/copies/A", "TARGE");
		assertNull(resolveObject(program, "TARGE"));
		createCopy(ONE, "application/copies/A", "TARGET2");
		assertNull(resolveObject(program, "TARGET2"));
	}
	
	
	@Test
	void findWithAsteriskAndQuestionMarkTest() {
		final SourcePojo program = createProgram(ONE, "application/programs/B", "PRG1");
		final SourcePojo copy = createCopy(ONE, "application/copies/B", "TARGET");
		
		final SourcePojo resolvedObject = resolveObject(program, "TARGET");
		assertEquals(copy.getUid(), assertNotNull(resolvedObject).getUid());
		
		final SourcePojo copy2 = createCopy(ONE, "application/copies/B", "TARGE");
		assertEquals(copy2.getUid(), assertNotNull(resolveObject(program, "TARGE")).getUid());
		
		final SourcePojo copy3 = createCopy(ONE, "application/copies/B", "TARGET2");
		assertEquals(copy3.getUid(), assertNotNull(resolveObject(program, "TARGET2")).getUid());

		final SourcePojo copy4 = createCopy(ONE, "application/copies/B", "XARGET");
		assertEquals(copy4.getUid(), assertNotNull(resolveObject(program, "XARGET")).getUid());
		
		createCopy(ONE, "application/copies/B", "XTARGET");
		assertNull(resolveObject(program, "XTARGET"));
	}
	
	
	@Test
	void findWithDoubleAsteriskTest() {
		final SourcePojo program = createProgram(ONE, "application/programs/E", "PRG1");
		final SourcePojo copy = createCopy(ONE, "application/copies/A/a", "TARGET");
		
		final SourcePojo resolvedObject = resolveObject(program, "TARGET");
		assertEquals(copy.getUid(), assertNotNull(resolvedObject).getUid());
	}
	
	
	@Test
	void findWithMultipleAsteriskTest() {
		final SourcePojo program = createProgram(ONE, "application/programs/E", "PRG1");
		final SourcePojo copy = createCopy(ONE, "application/copies/B/a", "TARGET");
		
		final SourcePojo resolvedObject = resolveObject(program, "TARGET");
		assertEquals(copy.getUid(), assertNotNull(resolvedObject).getUid());
	}

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}

	@Nullable
	private SourcePojo resolveObject(final SourcePojo context, final String targetName) {
		return Assert.assertNotNull(objectResolver).resolveObject(context, targetName);
	}

	@Nullable
	private SourcePojo resolveObject(final SourcePojo context, final String targetName, final SourceObjectMatcher targetMatcher) {
		return Assert.assertNotNull(objectResolver).resolveObject(context, targetName, targetMatcher);
	}
	
	private SourcePojo createProgram(final Long projectId, final String folder, final String name) {
		final String path = folder + "/" + name + ".cbl";
		sourceService.create(new SourcePojoPrototype()
				.setProject(EntityId.of(projectId))
				.setName(name)
				.setPath(path)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setContent(new BinaryString("some code")));
		return sourceService.get(q -> q.ofProject(EntityId.of(projectId)).withPath(path));
	}
	
	private SourcePojo createCopy(final Long projectId, final String folder, final String name) {
		final String path = folder + "/" + name + ".cpy";
		sourceService.create(new SourcePojoPrototype()
				.setProject(EntityId.of(projectId))
				.setName(name)
				.setPath(path)
				.setTechnology(Technology.COBOL)
				.setType(Type.COPYBOOK)
				.setContent(new BinaryString("some code")));
		return sourceService.get(q -> q.ofProject(EntityId.of(projectId)).withPath(path));
	}
}
