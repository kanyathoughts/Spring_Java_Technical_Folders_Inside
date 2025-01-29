/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests if {@link DatabaseResettingTest} resets DB after each test method.
 */
class ResetDbTest extends DatabaseResettingTest {

	private final Long ONE = Long.valueOf(1);
	
	@Autowired
	private SourceService sourceService;

	@Test
	void autowiredNotNullTest() {
		assertNotNull(sourceService);
	}
	
	@Test
	void createSourceObjectWithIdOneTest() {
		createSourceObjectWithIdOne();
	}

	@Test
	void createAnotherSourceObjectWithIdOneTest() {
		createSourceObjectWithIdOne();
	}

	private void createSourceObjectWithIdOne() {
		final SourcePojo sourceObjectResult = sourceService.get(sourceService.create(new SourcePojoPrototype()
				.setProject(EntityId.of(ONE))
				.setName("A")
				.setPath("a/A.cbl")
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setContent(new BinaryString("some code"))));
		assertEquals(ONE, sourceObjectResult.getId());
	}
}
