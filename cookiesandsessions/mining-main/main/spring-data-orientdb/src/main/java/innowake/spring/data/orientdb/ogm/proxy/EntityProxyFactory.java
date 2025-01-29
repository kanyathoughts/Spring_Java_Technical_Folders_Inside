/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.proxy;

import com.orientechnologies.orient.core.record.OElement;

import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.core.SessionManager;

/**
 * Creates the proxy instance of the domain object, used for lazy-loading.
 */
public class EntityProxyFactory {

	private EntityProxyFactory() {}

	/**
	 * Returns a proxy object for the given entity object.
	 *
	 * @param entity real entity object
	 * @param oElement {@link OElement} instance
	 * @param orientOperations instance to use basic orientDb operations
	 * @param sessionManager instance used to create a orientDb session
	 * @return a new proxy object for given entity
	 */
	public static <S> IEntityProxy create(final S entity, final OElement oElement, final OrientOperations<?> orientOperations,
			final SessionManager sessionManager) {
		final Class<?> entityClass = entity instanceof Class<?> ? (Class<?>) entity : entity.getClass();
		final EntityProxy proxyInterceptor = new EntityProxy(entityClass, oElement, orientOperations, sessionManager);
		return (IEntityProxy) proxyInterceptor.__getProxiedObject();
	}
	
}
