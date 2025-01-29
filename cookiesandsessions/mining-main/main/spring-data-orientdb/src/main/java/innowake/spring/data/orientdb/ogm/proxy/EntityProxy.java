/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.proxy;

import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.record.ODirection;
import com.orientechnologies.orient.core.record.OElement;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.core.SessionManager;
import innowake.spring.data.orientdb.commons.exception.AccessorMethodNotFoundException;
import innowake.spring.data.orientdb.commons.exception.EntityProxyMappingException;
import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinitionMapper;
import innowake.spring.data.orientdb.ogm.mapping.EdgeInformation;
import innowake.spring.data.orientdb.ogm.mapping.ObjectMapper;
import org.springframework.cglib.proxy.Enhancer;
import org.springframework.cglib.proxy.MethodInterceptor;
import org.springframework.cglib.proxy.MethodProxy;

import java.beans.IntrospectionException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import static org.apache.commons.lang3.StringUtils.substringAfter;
import static org.apache.commons.lang3.StringUtils.uncapitalize;

/**
 * Acts as a bridge between entity class and element class. 
 */
class EntityProxy implements IEntityProxy, MethodInterceptor {
	private static final Logger LOGGER = LoggerFactory.getLogger(EntityProxy.class);
	private final Object proxiedObject;
	private final Class<?> baseClass;
	private final OElement baseElement;
	private final ClassDefinition classDefinition;
	private final Map<String, Boolean> lazyLinkLoadStatus = new ConcurrentHashMap<>();
	private final Map<String, Object> lazyLinkLockTable = new ConcurrentHashMap<>();
	private final SessionManager sessionManager;
	private final OrientOperations<?> orientOperations;

	/**
	 * Initializes proxy object for the entity instance.
	 * 
	 * @param targetClass the type of entity instance
	 * @param oElement the orient element to which the proxy object would be mapped to
	 */
	EntityProxy(final Class<?> targetClass, final OElement oElement, final OrientOperations<?> orientOperations, final SessionManager sessionManager) { 
		baseClass = targetClass;
		baseElement = oElement;
		final Enhancer enhancer = new Enhancer();
		enhancer.setSuperclass(targetClass);
		enhancer.setCallback(this);
		enhancer.setInterfaces(new Class[] {IEntityProxy.class});
		classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(baseClass);
		final Class<?>[] constructorParameterTypes = classDefinition.getConstructorParameterTypes();
		final int constructorParamsSize = constructorParameterTypes.length;
		if (constructorParamsSize > 0) {
			final Object[] constructorParamValues = populateDefaultValues(constructorParameterTypes);
			proxiedObject =
					enhancer.create(constructorParameterTypes,
							constructorParamValues);
		} else {
			proxiedObject = enhancer.create(new Class[0], new Object[0]);
		}
		this.populateLazyLinkLoadStatus();
		this.orientOperations = orientOperations;
		this.sessionManager = sessionManager;
	}
	
	private Object[] populateDefaultValues(final Class<?>[] constructorParameterTypes) {
		final Object[] defaultConstructorValues = new Object[constructorParameterTypes.length];
		for (int i = 0; i < defaultConstructorValues.length; i++) {
			final Class<?> constructorType = constructorParameterTypes[i];
			if (constructorType.isPrimitive()) {
				if (constructorType.isAssignableFrom(boolean.class)) {
					defaultConstructorValues[i] = Boolean.FALSE;
				} else if (constructorType.isAssignableFrom(byte.class)) {
					defaultConstructorValues[i] = Byte.valueOf(Byte.MIN_VALUE);
				}
			}
		}
		return defaultConstructorValues;
	}

	/**
	 * Defines methods to be intercepted by the dynamic proxy class and methods that needs to be delegated to parent class.
	 */
	@Override
	@Nullable
	public Object intercept(@Nullable final Object object, @Nullable final Method method, @Nullable final Object[] args,
			@Nullable final MethodProxy methodProxy) throws Throwable {
		try {
			if (method != null) {
				final String methodName = method.getName();
				if (methodName.startsWith("__")) {
					return invokeEntityMethod(methodName, args);
				} else if (methodProxy != null) {
					final String fieldName = uncapitalize(substringAfter(methodName, "get"));
					if (methodName.startsWith("get") && classDefinition.getLinks().containsKey(fieldName)) {
						final Field field = classDefinition.getLinks().get(fieldName);
							final Object mappedEntity;
							synchronized (lazyLinkLockTable.get(fieldName)) {
								if ( ! lazyLinkLoadStatus.get(fieldName).booleanValue()) {
									mappedEntity = orientOperations.getRespectiveEntityForFieldType(classDefinition, __getElement(), field);
									this.__setFieldValue(mappedEntity, field);
									lazyLinkLoadStatus.put(fieldName, Boolean.TRUE);
								} else {
									return methodProxy.invokeSuper(object, args);
								}
							}
							return mappedEntity;
						} else {
							return methodProxy.invokeSuper(object, args);
						}
				}
			}
			return null;
		} finally {
			sessionManager.closeNonTransactionalThreadDatabase();
		}
	}

	@Override
	public OElement __getElement() {
		return baseElement;
	}

	@Override
	public ORID __getRid() {
		return baseElement.getIdentity();
	}

	@Override
	public void __injectRid() {
		@Nullable
		final Field ridField = classDefinition.getRidField();
		if (ridField != null) {
			try {
				ridField.setAccessible(true);
				ridField.set(proxiedObject, baseElement.getIdentity().toString());
			} catch (final IllegalArgumentException | IllegalAccessException ex) {
				throw new IllegalStateException("Error setting rid to proxy object ", ex);
			}
		} 
	}

	@Override
	public Class<?> __getBaseClass() {
		return baseClass;
	}

	@Override
	public Object __getProxiedObject() {
		return proxiedObject;
	}

	@Override
	public void __reload() {
		baseElement.reload();
	}

	/**
	 * Set the field's value in object. 
	 *
	 * @param value of the field
	 * @param field whose value needs to be set
	 * 
	 * @throws AccessorMethodNotFoundException if it cannot set the field's value
	 */
	@Override
	public void __setFieldValue(@Nullable final Object value, final Field field) {
		try {
			classDefinition.getWriteMethod(field).invoke(proxiedObject, value);
		} catch (final IllegalAccessException | IllegalArgumentException | InvocationTargetException | IntrospectionException e) {
			throw new AccessorMethodNotFoundException("Error Accessing setter method" + field.getName() + " " + baseClass.getSimpleName(), e);
		}
	}

	@Override
	@Nullable public Object __getFieldValue(final Field field) {
		try {
			return classDefinition.getReadMethod(field).invoke(proxiedObject);
		} catch (final IllegalArgumentException | IllegalAccessException | InvocationTargetException | IntrospectionException e) {
			throw new AccessorMethodNotFoundException("Error Accessing getter method" + field.getName() + " " + baseClass.getSimpleName(), e);
		}
	}

	@Override
	public <S> void __populateObjectFields(final S entity) {
		classDefinition.getObjectFields().forEach(field -> {
			try {
				__setFieldValue(classDefinition.getReadMethod(field).invoke(entity), field);
			} catch (final IllegalArgumentException | IllegalAccessException | InvocationTargetException | IntrospectionException e) {
				throw new IllegalStateException("Error while setting field's on the proxy object", e);
			}
		});
	}

	@Override
	public void __populateLazyCollectionReference(final String fieldName) {
		final Optional<Field> fLink = classDefinition.getFieldByName(fieldName);
		if (fLink.isPresent()) {
			final Field field = fLink.get();
			final Class<?> listClassType = field.getType();
			try {
				@Nullable
				final EdgeInformation edgeInformation = classDefinition.getEdgeInformation(field);
				final ODirection direction;
				final String relationshipFieldName;
				if (edgeInformation != null) {
					direction = edgeInformation.getDirection();
					relationshipFieldName = edgeInformation.getName();
				} else {
					direction = ODirection.OUT;
					relationshipFieldName = fieldName;
				}
				final Class<?> collectionInstanceType = CollectionProxyType.getCollectionType(listClassType);
				final OElement oElement = __getElement();
				final Object lazyCollection;
				if (List.class.isAssignableFrom(collectionInstanceType)) {
					final Class<?> listClass = ObjectMapper.getObjectMapper().getListType(field);
					LOGGER.debug(() -> "Element::" + oElement);
					lazyCollection = new CollectionProxyInterceptor(collectionInstanceType, oElement, relationshipFieldName, null, listClass, direction,
							this.orientOperations, this.sessionManager).getProxiedObject();
				} else if (Map.class.isAssignableFrom(collectionInstanceType)) {
					final ParameterizedType listType = (ParameterizedType) field.getGenericType();
					final Class<?> keyClass = (Class<?>) listType.getActualTypeArguments()[0];
					final Class<?> valClass = (Class<?>) listType.getActualTypeArguments()[1];
					lazyCollection = new CollectionProxyInterceptor(collectionInstanceType, oElement, relationshipFieldName, keyClass, valClass, direction,
							this.orientOperations, this.sessionManager).getProxiedObject();
				} else {
					throw new IllegalAccessException("Could not create a proxy instance for field " + fieldName + " with entity " + collectionInstanceType);
				}
				__setFieldValue(lazyCollection, field);
			} catch (final SecurityException | IllegalArgumentException | IllegalAccessException ex) {
				throw new EntityProxyMappingException("Error while setting proxy collection fields", ex);
			}
		} else {
			throw new UnsupportedQueryTypeException("Unsupported value assigned for type " + fLink);
		}
	}
	
	private void populateLazyLinkLoadStatus() {
		classDefinition.getLinks().forEach((key, value) -> {
			this.lazyLinkLoadStatus.put(key, Boolean.FALSE);
			this.lazyLinkLockTable.put(key, new Object());
		});
	}
	
	@Nullable
	private Object invokeEntityMethod(final String methodName, @Nullable final Object[] args) {
		switch (methodName) {
			case "__getElement" :
				return __getElement();
			case "__getRid" :
				return __getRid();
			case "__injectRid":
				this.__injectRid();
				break;
			case "__getBaseClass":
				return __getBaseClass();
			case "__getProxiedObject":
				return __getProxiedObject();
			case "__reload":
				__reload();
				break;
			case "__setFieldValue":
				if (args !=  null && args.length == 2) {
					__setFieldValue(args[0], (Field) args[1]);
				}
				break;
			case "__getFieldValue":
				if (args !=  null && args.length > 0) {
					return __getFieldValue((Field) args[0]);
				}
				break;
			case "__populateObjectFields":
				if (args !=  null && args.length > 0) {
					__populateObjectFields(args[0]);
				}
				break;
			case "__populateLazyCollectionReference":
				if (args !=  null && args.length > 0) {
					__populateLazyCollectionReference((String) args[0]);
				}
				break;
			default :
				throw new EntityProxyMappingException("No such method found" + methodName);
		}
		return null;
	}
}
