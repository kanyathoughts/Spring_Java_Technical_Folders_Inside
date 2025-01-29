/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.proxy;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.orientechnologies.orient.core.sql.executor.OResultInternal;
import org.springframework.cglib.proxy.Enhancer;
import org.springframework.cglib.proxy.MethodInterceptor;
import org.springframework.cglib.proxy.MethodProxy;
import com.orientechnologies.orient.core.id.ORecordId;
import com.orientechnologies.orient.core.record.ODirection;
import com.orientechnologies.orient.core.record.OEdge;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.ORecord;
import com.orientechnologies.orient.core.record.OVertex;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.springdata.annotations.RelationshipProperties;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.core.SessionManager;
import innowake.spring.data.orientdb.commons.exception.EntityProxyMappingException;
import innowake.spring.data.orientdb.commons.exception.MetadataException;
import innowake.spring.data.orientdb.ogm.mapping.EntityClassMapper;
import innowake.spring.data.orientdb.ogm.mapping.Primitives;

/**
 * Proxy intercepter for supported collection classes used to load data lazily.
 */
public class CollectionProxyInterceptor implements MethodInterceptor {

	private static final Logger LOGGER = LoggerFactory.getLogger(CollectionProxyInterceptor.class);

	private final OElement oElement;
	private final String fieldName;
	@Nullable
	private final Class<?> keyClass; /* Only for Map type */
	private final Class<?> valueClass;
	private final ODirection direction;
	private final OrientOperations<?> orientOperations;
	private final Class<?> targetClass;
	private final Object proxiedObject;
	private volatile boolean lazyLoad;
	private final SessionManager sessionManager;

	/**
	 * Instantiates a lazy collection object.
	 * 
	 * @param targetClass : instance class type
	 * @param oElement : element to which the collection is related
	 * @param fieldName : relationship attribute name
	 * @param keyClass : type of key in the map type
	 * @param valueClass : type of value in the {@link Map} or {@link Collection} type
	 * @param direction : direction of relationship
	 * @param orientOperations : instance to use basic orientDb operations
	 * @param sessionManager : instance used to create a orientDb session
	 */
	public CollectionProxyInterceptor(final Class<?> targetClass, final OElement oElement, final String fieldName, @Nullable final Class<?> keyClass,
			final Class<?> valueClass, final ODirection direction, final OrientOperations<?> orientOperations, final SessionManager sessionManager) {
		this.oElement = oElement;
		this.fieldName = fieldName;
		this.keyClass = keyClass;
		this.valueClass = valueClass;
		this.direction = direction;
		this.targetClass = targetClass;
		this.orientOperations = orientOperations;
		final Enhancer enhancer = new Enhancer();
		enhancer.setSuperclass(targetClass);
		enhancer.setCallback(this);
		enhancer.setInterfaces(new Class[] {
				ICollectionProxy.class
		});
		proxiedObject = enhancer.create();
		lazyLoad = true;
		this.sessionManager = sessionManager;
	}

	/**
	 * An instance of target class type.
	 *
	 * @return an instance of target class type
	 */
	public Object getProxiedObject() {
		return this.proxiedObject;
	}

	@Override
	public Object intercept(@Nullable final Object object, @Nullable final Method method, @Nullable final Object[] args, @Nullable final MethodProxy proxy)
			throws Throwable {
		final String methodName = assertNotNull(method).getName();
		if (methodName.equals("__isLazyLoaded")) {
			return Boolean.valueOf(__isLazyLoaded());
		}
		if (lazyLoad) {
			try {
				lazyLoad();
			} finally {
				sessionManager.closeNonTransactionalThreadDatabase();
			}
		}
		if (proxy != null) {
			return proxy.invokeSuper(object, args);
		}
		throw new EntityProxyMappingException("Could not invoke the" + targetClass.getSimpleName() + " proxy class method.");
	}

	/**
	 * Returns whether the collection has been loaded lazily or not.
	 *
	 * @return true if collection has been lazy loaded, false otherwise
	 */
	public boolean __isLazyLoaded() {
		return ! lazyLoad;
	}
	
	private synchronized void lazyLoad() {
		LOGGER.debug(() -> "lazy load :: " + oElement);
		if (lazyLoad) {
			lazyLoad = false;
			lazyLoadColection();
		}
	}

	private void lazyLoadColection() {
		if (oElement instanceof OVertex) {
			final Iterator<?> element;
			/* retrieve all items from the edges and add them to the collection */
			if (valueClass.isAnnotationPresent(RelationshipProperties.class)) {
				element = ((OVertex) oElement).getEdges(direction, fieldName).iterator();
				loadData(element, true);
			} else {
				element = ((OVertex) oElement).getVertices(direction, fieldName).iterator();
				loadData(element, false);
			}
		} else {
			loadLinkLists();
		}
	}

	@SuppressWarnings("unchecked")
	private void loadData(final Iterator<?> element, final boolean isEdge) {
		if (element.hasNext()) {
			element.forEachRemaining(elementData -> {
				/* the Lazy always loads the data from the database dodging the objects that are in the cache. */
				final Object object = orientOperations.findByIdInternal(valueClass, ((OElement) elementData).getIdentity());
				if (Collection.class.isAssignableFrom(targetClass)) {
					((Collection<Object>) proxiedObject).add(object);
				} else if (Map.class.isAssignableFrom(targetClass)) {
					if (isEdge) {
						mapEdge((OEdge) elementData, object);
					} else {
						mapEdgesForVetices(object);
					}
				}
			});
		} else {
			loadLinkLists();
		}
	}

	@SuppressWarnings("unchecked")
	private void loadLinkLists() {
		final List<Object> records = oElement.getProperty(fieldName);
		records.stream()
				.map(record2 -> record2 instanceof ORecord ? ((ORecord) record2): ((ORecordId) record2))
				.map(record2 -> orientOperations.mapDataToEntity(new OResultInternal(record2.getRecord()), new EntityClassMapper(valueClass)))
				.forEach(((Collection<Object>)proxiedObject)::add);
		
	}
	
	private void mapEdgesForVetices(final Object mapValue) {
		for (final OEdge edge : ((OVertex) oElement).getEdges(direction, fieldName)) {
			mapEdge(edge, mapValue);
		}
	}

	@SuppressWarnings("unchecked")
	private void mapEdge(final OEdge edge, final Object mapValue) {
		LOGGER.debug(() -> "edge keyclass: " + keyClass + "RID: " + edge.getIdentity().toString());
		/* If the keyClass is not native, map to an object. */
		if (Primitives.isSupportedDataType(assertNotNull(keyClass))) {
			final Object key = getEdgeProperty(edge);
			((Map<Object, Object>) proxiedObject).put(key, mapValue);
		} else {
			throw new UnsupportedOperationException("WMIN-708: Entity as key in map is not supported");
		}
	}

	private Object getEdgeProperty(final OEdge edge) {
		return edge.getPropertyNames().stream().filter(prop -> ! (prop.equalsIgnoreCase("IN") || prop.equalsIgnoreCase("OUT")))
									.map(edge::getProperty).findAny()
									.orElseThrow(MetadataException::new);
	}
}
