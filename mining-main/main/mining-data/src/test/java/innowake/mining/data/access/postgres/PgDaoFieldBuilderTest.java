/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import innowake.mining.data.access.postgres.PgDao.FieldBuilder;
import innowake.mining.data.access.postgres.PgDao.QueryBuilder;
import innowake.mining.shared.access.EntityId;


class PgDaoFieldBuilderTest {

	@Test
	void testBuildInsert() {
		PgDao pgDao = Mockito.mock(PgDao.class);
		StringBuilder sb = new StringBuilder();
		QueryBuilder qb = pgDao.new QueryBuilder(sb);
		FieldBuilder fb = new PgDao.FieldBuilder();
		fb.add("module", "?", EntityId.of(1l))
			.add("taxonomy", "?", EntityId.of(2l));
		fb.buildUpsert(qb, "module", "taxonomy");
		Assert.assertEquals("(module, taxonomy) VALUES (?, ?) ON CONFLICT (module, taxonomy) DO NOTHING", sb.toString());
		Assert.assertEquals(2, qb.getArgs().length);
	}
	
	@Test
	void testBuildInsertOneNotInUpsert() {
		PgDao pgDao = Mockito.mock(PgDao.class);
		StringBuilder sb = new StringBuilder();
		QueryBuilder qb = pgDao.new QueryBuilder(sb);
		FieldBuilder fb = new PgDao.FieldBuilder();
		fb.add("uid", "?", EntityId.of(1l))
			.add("project", "?", "Project 1")
			.add("type", "?", "type")
			.add("name", "?", "test")
			;
		fb.buildUpsert(qb, "uid");
		Assert.assertEquals("(uid, project, type, name) VALUES (?, ?, ?, ?) ON CONFLICT (uid) DO UPDATE SET project = EXCLUDED.project, type = EXCLUDED.type, name = EXCLUDED.name", sb.toString());
		Assert.assertEquals(4, qb.getArgs().length);
	}
	
	@Test
	void testBuildMultiInsert() {
		PgDao pgDao = Mockito.mock(PgDao.class);
		StringBuilder sb = new StringBuilder();
		QueryBuilder qb = pgDao.new QueryBuilder(sb);
		FieldBuilder fb = new PgDao.FieldBuilder();
		fb.add("module", "?", EntityId.of(1l))
			.add("taxonomy", "?", EntityId.of(2l));
		fb.buildInsert(qb);
		Assert.assertEquals("(module, taxonomy) VALUES (?, ?)", sb.toString());
		Assert.assertEquals(2, qb.getArgs().length);
	}

}
