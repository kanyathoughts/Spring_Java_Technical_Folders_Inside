/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.discovery.dna;

import java.util.List;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.Tuple2;

/**
 * Interface for generating various discovery dna files
 */
@FunctionalInterface
interface DiscoverDnaGenerator {
	
	/**
	 * Generates Discover Dna files.
	 * 
	 * @param projectId the project Id
	 * @return List of {@link Tuple2} consisting of file name and its content
	 */
	List<Tuple2<String, byte[]>> build(final EntityId projectId);

}
