/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojo;

@Disabled
class SamplePgDaoTest extends AbstractPgDaoTest {
	
	@Test
	void testSum() {
		final var dummyDao = new PgDao(jdbc) {
			public int getSum(int a, int b) {
				return query("SELECT ? + ?")
						.addArgs(a, b)
						.first(rs -> rs.getInt(1))
						.orElseThrow();
			}
		};
		assertEquals(3, dummyDao.getSum(1, 2));
	}
	
	@Test
	void testClient() {
		final ClientPgDao clientDao = new ClientPgDao(jdbc);
		final ClientPojo client = clientDao.find(EntityId.of(0L), true).orElseThrow();
		assertEquals("SYSTEM", client.getName());
	}
	
}
