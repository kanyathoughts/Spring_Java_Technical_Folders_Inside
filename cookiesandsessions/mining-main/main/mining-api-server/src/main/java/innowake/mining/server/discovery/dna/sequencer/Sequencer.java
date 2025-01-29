/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer;

import java.util.List;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.dna.DnaStringElementPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Interface for DNA sequencers that extract a DNA string from a SourcePojo.
 */
public interface Sequencer {
	/**
	 * Applies the sequencer to a source object. The sequencer applies one or more rules to the
	 * given source object. Each applied rule generates a {@link DnaStringPojoPrototype} along with the 
	 * list of {@link DnaStringElementPojoPrototype DnaStringElementPojoPrototypes} which contains the extracted DNA string and 
	 * an identifier of the Rule that generated it.
	 *
	 * @param sourceObject the source object to which the sequencer is applied
	 * @return a collection of pairs of the extracted DNA strings and their list of DNA string elements
	 * @throws DiscoveryException if the collection fails
	 */
	List<Tuple2<DnaStringPojoPrototype, List<DnaStringElementPojoPrototype>>> apply(SourcePojo sourceObject) throws DiscoveryException;
}
