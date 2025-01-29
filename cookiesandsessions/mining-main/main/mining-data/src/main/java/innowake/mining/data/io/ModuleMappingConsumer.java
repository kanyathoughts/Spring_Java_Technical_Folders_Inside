/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.util.Map;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;

/**
 * Processes all instances of an entity for the given project using the mapping between Module ID and a {@link Tuple2} of UID and Discovery type.
 */
interface ModuleMappingConsumer extends RowCreator {

	/**
	 * Processes all instances of an entity for the given project using the mapping between Module ID and a {@link Tuple2} of UID and Discovery type.
	 *
	 * @param projectId the project ID
	 * @param moduleMapping between numeric module id and module excel type
	 * @param options additional options for customizing the export
	 * @param sorted if {@code true}, sorts the result
	 * @throws IOException if an error occurs
	 */
	void process(final EntityId projectId, final Map<Long, String> moduleMapping, final DiscoveryExportOptions options, 
			final boolean sorted) throws IOException;
}
