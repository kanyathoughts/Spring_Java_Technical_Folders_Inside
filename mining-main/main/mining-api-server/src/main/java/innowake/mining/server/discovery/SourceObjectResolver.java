/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.SourcePojo;

/**
 * An Interface for resolving the dependencies between source objects.
 */
public interface SourceObjectResolver {

	/**
	* Finds a source for the given context and target name.
	*
	* @param context the context; usually the outgoing SourcePojo
	* @param targetName the name of the target SourcePojo to resolve
	* @return the found SourcePojo or {@code null} if no object could be found
	*/
	@Nullable
	public SourcePojo resolveObject(SourcePojo context, String targetName);
	
	/**
	 * Finds a source for the given context, target name and matcher.
	 *
	 * @param context the context; usually the outgoing SourcePojo
	 * @param targetName the name of the target SourcePojo to resolve
	 * @param targetMatcher the target type matcher
	 * @return the found SourcePojo or {@code null} if no object could be found
	 */
	@Nullable
	public SourcePojo resolveObject(SourcePojo context, String targetName, SourceObjectMatcher targetMatcher);
}
