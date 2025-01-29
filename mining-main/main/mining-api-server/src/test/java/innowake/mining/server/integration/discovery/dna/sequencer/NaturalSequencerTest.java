/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery.dna.sequencer;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.tags.DiscoveryTest;

/**
 * Tests the DNA strings of {@link Technology#NATURAL}.
 */
@DiscoveryTest
public class NaturalSequencerTest extends AbstractSequencerTest {

	@Override
	protected Tuple2<Technology, Type> getTechnologyType() {
		return Tuple2.of(Technology.NATURAL, Type.PROGRAM);
	}

	@Override
	protected String getFolder() {
		return "NATURAL";
	}

}
