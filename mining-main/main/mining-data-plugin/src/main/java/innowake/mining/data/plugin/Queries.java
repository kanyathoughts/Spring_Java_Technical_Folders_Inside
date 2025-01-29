/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.plugin;

import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Common OrientDB queries.
 */
public final class Queries {

	/**
	 * Query for resolving a Module based on multiple attributes.
	 * <p>
	 * This can be used for resolving Modules for assembling purposes.
	 * <p>
	 * Parameters:
	 * <ol>
	 *  <li>String: The name of the module to be resolved
	 *  <li>String: The name of the relationship, see {@link RelationshipType}
	 *  <li>Long: The ID of the module from which the other module should be resolved from
	 *  <li>Long: The ID of the project
	 *  <li>String: The name of the {@link Technology}
	 *  <li>String: The name of the {@link Type}
	 */
	public static final String RESOLVE_MODULE_QUERY = 
			"SELECT id, name, sourceAttachmentLink.content, projectId" +
			" FROM (" +
			"  TRAVERSE out(?)" +
			"  FROM (" +
			"   SELECT FROM Module WHERE id=?" +
			"  )" +
			" )" +
			" WHERE" +
			"  name=?" +
			"  AND (projectId=? OR projectId=0)" +
			"  AND objectTypeLink.technologyLink.name=?" +
			"  AND objectTypeLink.typeLink.name=?";

	private Queries() {}
}
