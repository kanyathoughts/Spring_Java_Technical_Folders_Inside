/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.proxy;

/**
 * Define methods to be accessed by the proxy object.
 * The methods are named with "__" in order to prevent method name conflicts with actual entity class.
 */
public interface ICollectionProxy {
	
	/**
	 * Returns whether the collection has been loaded lazily or not.
	 *
	 * @return true if collection has been lazy loaded, false otherwise
	 */
	public boolean __isLazyLoaded();
}
