/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.mapping;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.api.lang.Nullable;

/**
 * Entity class and it's corresponding projection interface information.
 */
public class EntityClassMapper {

	private Class<?> entityClass;
	@Nullable
	private Class<?> projectionClass;
	private String rootName = "";
	@Nullable
	private Integer index;

	/**
	 * Instantiates {@link EntityClassMapper} object.
	 * 
	 * @param entityClass actual domain class
	 */
	public EntityClassMapper(final Class<?> entityClass) {
		this.entityClass = entityClass;
	}

	/**
	 * Instantiates {@link EntityClassMapper} object.
	 * @param entityClass actual domain class
	 * @param projectionClass the projected class
	 */
	public EntityClassMapper(final Class<?> entityClass, final Class<?> projectionClass) {
		this.entityClass = entityClass;
		this.projectionClass = projectionClass;
	}

	/**
	 * Returns the entity class.
	 *
	 * @return the entity class
	 */
	public Class<?> getEntityClass() {
		return entityClass;
	}
	
	/**
	 * Set entity class.
	 *
	 * @param entityClass sets the entity class
	 */
	public void setEntityClass(final Class<?> entityClass) {
		this.entityClass = entityClass;
	}

	/**
	 * Returns the projection class.
	 *
	 * @return the projection class
	 */
	public Class<?> getProjectionClass() {
		return assertNotNull(projectionClass);
	}

	/**
	 * Sets the projection class.
	 *
	 * @param projectionClass the projected interface class
	 */
	public void setProjectionClass(final Class<?> projectionClass) {
		this.projectionClass = projectionClass;
	}

	/**
	 * Returns the root class name.
	 *
	 * @return the root class name
	 */
	public String getRootName() {
		return rootName;
	}

	/**
	 * Set's the root name of the property.
	 *
	 * @param rootName the name  of the parent field
	 */
	public void setRootName(final String rootName) {
		this.rootName = rootName;
	}

	/**
	 * Returns the index value.
	 *
	 * @return the index value
	 */
	@Nullable
	public Integer getIndex() {
		return index;
	}

	/**
	 * Set's the index position.
	 *
	 * @param index the index of the list record
	 */
	public void setIndex(final int index) {
		this.index = Integer.valueOf(index);
	}
}
