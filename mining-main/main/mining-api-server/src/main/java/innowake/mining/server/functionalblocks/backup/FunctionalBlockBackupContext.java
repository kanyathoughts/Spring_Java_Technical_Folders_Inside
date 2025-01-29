/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.backup;

import innowake.mining.shared.access.EntityId;

import java.util.Map;

/**
 * Context object providing context information to {@link FunctionalBlockBackupHandler}.
 */
public class FunctionalBlockBackupContext {

	private final Map<Long, EntityId> restoredAnnotationIdMap;

	/**
	 * Constructor.
	 *
	 * @param restoredAnnotationIdMap map of Annotation ids:
	 * key is the id of the Annotations as stored in the backup, value is the id of the Annotation after restore
	 */
	public FunctionalBlockBackupContext(final Map<Long, EntityId> restoredAnnotationIdMap) {
		this.restoredAnnotationIdMap = restoredAnnotationIdMap;
	}

	/**
	 * Returns a map of Annotation ids: key is the id of the Annotations as stored in the backup, value is the id of the Annotation after restore.
	 * <p>
	 * Currently, this is required to restore the "GeneratedFrom" information of functional blocks. This can be removed when Annotations have stable UUIDs.
	 * @return annotation id map
	 */
	public Map<Long, EntityId> getRestoredAnnotationIdMap() {
		return restoredAnnotationIdMap;
	}
}
