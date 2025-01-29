/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping.custom;

import java.util.List;
import java.util.Map;

import com.orientechnologies.orient.core.db.ODatabaseSession;
import com.orientechnologies.orient.core.record.OElement;
/**
 * Provides read, save or update support for  {@link CustomProperty}
 */
public interface CustomPropertyService {

	/**
	 * Retrieves name and type information of custom properties associated with an entity class from it's equivalent CustomProperty classes. 
	 *
	 * @param recordId the rId of MiningEntity
	 * @param session the session for database with a specific user
	 * @return collection of class and it's {@link CustomProperty} associated with the given class
	 */
	public Map<String, List<CustomProperty>> readCustomProperties(String recordId, ODatabaseSession session);

	/**
	 * Saves or Updates the {@link CustomProperty} associated with MiningEntity
	 *
	 * @param oElement the rId of MiningEntity
	 * @param customProperties the collection of {@link CustomProperty} to be saved or updated
	 * @param session the session for database with a specific user
	 */
	public void saveOrUpdate(OElement oElement, Map<String, List<CustomProperty>> customProperties, ODatabaseSession session);

}
