/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.springframework.graphql.data.method.annotation.SchemaMapping;

import graphql.execution.DataFetcherResult;
import graphql.schema.DataFetchingEnvironment;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.MiningDataPointBuilder.DataPointBuilder;

/**
 * An object that can be used as "local context" by GraphQL controllers.
 * <p>
 * A local context is essentially a piece of information (represented by this object and additional ones that can registered via
 * {@link #withLocalContext(Object)}) that is "passed down" from outer request handling methods to inner ones.
 * <p>
 * For example, assume we are executing this GraphQL query:
 * <pre>
 * query {
 *   foo {
 *     bar {
 *       baz
 *     }
 *   }
 * }
 * </pre>
 * The code that handles retrieval of the "foo" field can establish a local context object (by returning a {@link DataFetcherResult} which contains
 * the local context in addition to the result data). Then any handling method that is "downstream" from there can retrieve the established local context (
 * by injecting the {@link DataFetchingEnvironment} and calling {@link DataFetchingEnvironment#getLocalContext()}). In our example, the handling method
 * for "bar" can retrieve the local context established by "foo". The handling method for "baz" can also retrieve the local context established by "foo",
 * unless it was replaced with a new local context object by "bar".
 * <p>
 * Note: such handling methods are methods annotated with {@link SchemaMapping} or methods registered
 * via {@link DataPointBuilder#withCustomFetch(graphql.schema.DataFetcher)}.
 */
public class ControllerLocalContext {
	
	private final Long queriedProjectId;
	private final Collection<Long> userProjectIds;
	private final Collection<Long> clientAdminIds;
	private final boolean admin;
	
	private final Map<Class<?>, Object> localContexts;
	
	/**
	 * Creates a new local context object.
	 * 
	 * @param queriedProjectId The project id for which the query was created for
	 * @param projectIds The list of project ids associated with the current user
	 * @param clientAdminIds the list of client ids for which the user has client admin rights
	 * @param isAdmin whether the user is a global (super) admin
	 */
	public ControllerLocalContext(final Long queriedProjectId, final Collection<Long> projectIds, final Collection<Long> clientAdminIds, final boolean isAdmin) {
		this(queriedProjectId, projectIds, clientAdminIds, isAdmin, new HashMap<>());
	}
	
	private ControllerLocalContext(final Long queriedProjectId, final Collection<Long> projectIds, final Collection<Long> clientAdminIds, final boolean isAdmin, final Map<Class<?>, Object> localContexts) {
		this.queriedProjectId = queriedProjectId;
		this.userProjectIds = projectIds;
		this.clientAdminIds = clientAdminIds;
		this.admin = isAdmin;
		this.localContexts = localContexts;
	}
	
	private ControllerLocalContext copy() {
		return new ControllerLocalContext(queriedProjectId, new ArrayList<>(userProjectIds), new ArrayList<>(clientAdminIds), admin, new HashMap<>(localContexts));
	}

	/**
	 * @return The project id for which the query was created
	 */
	public Long getQueriedProjectId() {
		return queriedProjectId;
	}

	/**
	 * @return The list of project ids associated with the current user
	 */
	public Collection<Long> getUserProjectIds() {
		return Collections.unmodifiableCollection(userProjectIds);
	}
	
	/**
	 * Retrieves a context represented by the given class from this {@code ControllerLocalContext} object.
	 *
	 * @param <T> the implementation class of the context
	 * @param clazz the implementation class of the context
	 * @return the context object that was registered for the given class, or {@code null} if no such context was registered
	 * @see #withLocalContext(Object)
	 */
	@Nullable
	@SuppressWarnings("unchecked")
	public <T> T getLocalContext(final Class<T> clazz) {
		return (T) localContexts.get(clazz);
	}
	
	/**
	 * Returns a (shallow) copy of the {@code ControllerLocalContext} object with the additional {@code context} object registered.
	 * The registered {@code context} can be retrieved via {@link #getLocalContext(Class)}.
	 *
	 * @param context the sub-context to register
	 * @return a new {@code ControllerLocalContext} with the additional context registered (in addition to any existing ones)
	 */
	public ControllerLocalContext withLocalContext(final Object context) {
		final ControllerLocalContext copied = copy();
		copied.localContexts.put(context.getClass(), context);
		return copied;
	}
	
	/**
	 * @return the client IDs for which the user has client-admin rights
	 */
	public Collection<Long> getClientAdminIds() {
		return Collections.unmodifiableCollection(clientAdminIds);
	}
	
	/**
	 * @return whether the user is a global (super) admin
	 */
	public boolean isAdmin() {
		return admin;
	}
}
