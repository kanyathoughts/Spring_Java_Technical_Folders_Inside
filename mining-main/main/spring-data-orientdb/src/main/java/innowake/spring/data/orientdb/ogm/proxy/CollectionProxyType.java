/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.proxy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Class to check for the supported collection types.
 */
public class CollectionProxyType {

	private static final Map<Class<?>, Class<?>> LAZY_COLLECTION = new IdentityHashMap<>();

	static {
		LAZY_COLLECTION.put(List.class, ArrayList.class);
		LAZY_COLLECTION.put(ArrayList.class, ArrayList.class);
		LAZY_COLLECTION.put(LinkedList.class, LinkedList.class);

		LAZY_COLLECTION.put(Set.class, HashSet.class);

		LAZY_COLLECTION.put(HashMap.class, HashMap.class);
		LAZY_COLLECTION.put(Map.class, HashMap.class);
	}

	/**
	 * Returns true if a collection type has a corresponding lazy loading class type.
	 * @param collectionType java collection class
	 *  
	 * @return true if the collection type is supported
	 */
	public static boolean isCollectionTypeSupported(final Class<?> collectionType) {
		return LAZY_COLLECTION.containsKey(collectionType);
	}

	/**
	 * Returns the type of instance class for corresponding collection type.
	 *
	 * @param collectionType type of collection class
	 * @return the type of instance to be created
	 */
	public static Class<?> getCollectionType(final Class<?> collectionType) {
		return LAZY_COLLECTION.get(collectionType);
	}

	private CollectionProxyType() {
	}

}
