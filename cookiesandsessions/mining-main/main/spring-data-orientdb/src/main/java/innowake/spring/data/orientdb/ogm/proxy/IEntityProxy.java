/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.proxy;

import java.lang.reflect.Field;

import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.record.OElement;

import innowake.lib.core.api.lang.Nullable;
import innowake.spring.data.orientdb.commons.exception.AccessorMethodNotFoundException;
import innowake.spring.data.orientdb.commons.exception.EntityProxyMappingException;

/**
 * Define methods to be accessed by the proxy object.
 * The methods are named with "__" in order to prevent method name conflicts with actual entity class.
 */
public interface IEntityProxy {

	/**
	 * Returns the element node mapped to the entity object.
	 * 
	 * @return the {@link OElement} if the record is present in DB.
	 */
	OElement __getElement();

	/**
	 * Returns {@link ORID} if the record is persist in the database.
	 * 
	 * @return {@link ORID} with record id as {@link String} saved in DB
	 */
	ORID __getRid();

	/**
	 * Once the data is persist in DB, it sets the field annotated with {@link ORID} in entity class with the record id.
	 */
	void __injectRid();

	/**
	 * Gets the base domain class.
	 * 
	 * @return entity class
	 */
	Class<?> __getBaseClass();

	/**
	 * Returns a proxy object for a given entity class.
	 * 
	 * @return proxy object of type {@link IEntityProxy} for a given entity class
	 */
	Object __getProxiedObject();

	/**
	 * Updates the {@code baseElement} with values from DB
	 */
	void __reload();

	/**
	 * Set the field's value in object. 
	 *
	 * @param value of the field
	 * @param field whose value needs to be set
	 * 
	 * @throws AccessorMethodNotFoundException if it cannot set the field's value
	 */
	public void __setFieldValue(@Nullable final Object value, final Field field);
	
	/**
	 * Get the field's value from object. 
	 *
	 * @param field value needs to be set
	 * @return the value of the field
	 * 
	 * @throws EntityProxyMappingException if it cannot get the field's value
	 */
	@Nullable public Object __getFieldValue(final Field field);
	
	/**
	 * Sets the fields to the newly created proxy object.
	 *
	 * @param entity real object
	 */
	public <S> void __populateObjectFields(final S entity);
	
	
	/**
	 * Sets the collection fields in proxy object with lazy collection references and the values are loaded lazily.
	 *
	 * @param fieldName name of the field in entity class
	 */
	public void __populateLazyCollectionReference(final String fieldName);

}
