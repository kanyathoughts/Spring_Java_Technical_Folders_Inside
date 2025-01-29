/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import innowake.mining.shared.UUIDv5;

/**
 * Test UUIDv5 implementation
 */
class UUIDv5Test {

	/* compare uuidtools.com/v5 */
	
	@Test
	void testDNS() {
		final UUIDv5 uuid5 = new UUIDv5(UUIDv5.Namespace.DNS.uuid());
		assertEquals(UUID.fromString("6bc16401-89aa-5dac-affe-96250a62f40b"), uuid5.generate("innowake.hq"));
	}
	
	@Test
	void testURL() {
		final UUIDv5 uuid5 = new UUIDv5(UUIDv5.Namespace.URL.uuid());
		assertEquals(UUID.fromString("978a6ee6-5da6-5cd6-aeed-f173439dfb94"), uuid5.generate("http://innowake.hq/test"));
	}
	
	@Test
	void testOID() {
		final UUIDv5 uuid5 = new UUIDv5(UUIDv5.Namespace.OID.uuid());
		assertEquals(UUID.fromString("106dd502-8b3e-50db-80ed-1134f5c18eae"), uuid5.generate("1.3.6.1.4.1"));
	}
	
	@Test
	void testX500() {
		final UUIDv5 uuid5 = new UUIDv5(UUIDv5.Namespace.X500.uuid());
		assertEquals(UUID.fromString("05e4c899-339e-5f5a-a8cc-8b33dbb06314"), uuid5.generate("CN=UUID Test,OU=mining,O=innowake"));
	}

}
