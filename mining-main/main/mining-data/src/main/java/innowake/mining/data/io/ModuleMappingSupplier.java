/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.io.IOException;
import java.util.Map;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;

/**
 * Creates a mapping between Module ID and a {@link Tuple2} of UID and Discovery type.
 */
interface ModuleMappingSupplier extends RowCreator {

	/**
	 * Processes all Modules for the given project and creates a mapping between Module ID and a {@link Tuple2} of UID and Discovery type.
	 *
	 * @param projectId the project ID
	 * @param options additional options for customizing the export
	 * @param sorted if {@code true}, sorts the result
	 * @return mapping between numeric module id and module excel type
	 * @throws IOException if an error occurs
	 */
	Map<Long, String> process(final EntityId projectId, final DiscoveryExportOptions options, final boolean sorted) throws IOException;
}
