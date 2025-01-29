/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.lang.reflect.Method;

import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.util.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Query;

/**
 * Orient specific extension of {@link org.springframework.data.repository.query.QueryMethod}.
 */
public class OrientQueryMethod extends QueryMethod {

	private final Method method;
	private final Class<?> repositoryInterface;

	/**
	 * Instantiates a new {@link OrientQueryMethod}.
	 *
	 * @param method for named queries
	 * @param metadata information about the repository
	 * @param factory instance of {@link ProjectionFactory}
	 */
	public OrientQueryMethod(final Method method, final RepositoryMetadata metadata, final ProjectionFactory factory) {
		super(method, metadata, factory);
		this.method = method;
		repositoryInterface = metadata.getRepositoryInterface();
	}

	/**
	 * Gets the target method.
	 *
	 * @return the method in repository
	 */
	public Method getQueryMethod() {
		return method;
	}

	/**
	 * Gets the repository interface.
	 *
	 * @return the repository interface
	 */
	public Class<?> getRepositoryInterface() {
		return repositoryInterface;
	}

	/**
	 * Returns whether the method has an annotated query.
	 * 
	 * @return whether the method has an annotated query
	 */
	public boolean hasAnnotatedQuery() {
		return getAnnotatedQuery() != null;
	}

	/**
	 * Returns the query string declared in a {@link Query} annotation or {@literal null} if neither the annotation found
	 * nor the attribute was specified.
	 *
	 * @return the native query with in @Query annotation
	 */
	@Nullable
	String getAnnotatedQuery() {
		if (getQueryAnnotation() != null) {
			final String query = (String) AnnotationUtils.getValue(assertNotNull(getQueryAnnotation()));
			return StringUtils.hasText(query) ? query : null;
		}
		return null;
	}

	/**
	 * Returns the {@link Query} annotation that is applied to the method or {@code null} if none available.
	 *
	 * @return annotation on the method
	 */
	@Nullable
	Query getQueryAnnotation() {
		return assertNotNull(method, "Method is null").getAnnotation(Query.class);
	}

}
