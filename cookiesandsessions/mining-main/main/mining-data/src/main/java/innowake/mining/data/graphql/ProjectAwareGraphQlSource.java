/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.graphql;

import org.springframework.graphql.execution.GraphQlSource;

/**
 * GraphQL source with mining project-specific schema.
 */
public interface ProjectAwareGraphQlSource extends GraphQlSource {
	/**
	 * Sets the projectId to assume for GraphQL requests for the current Thread.
	 * Always call {@link #removeThreadLocalProjectId()} to unset this value!
	 * @param projectId the project id
	 */
	void setThreadLocalProjectId(final Long projectId);

	/**
	 * Clears the projectId previously set with {@link #setThreadLocalProjectId(Long)}.
	 */
	void removeThreadLocalProjectId();
}
