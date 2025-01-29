/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.discovery.attribute;

import java.util.Comparator;

/**
 * Helper class that holds ModelAttributeKey and CallType comparators.
 */
public class ModelAttributeComparator {
private static final Comparator<ModelAttributeKey> keyComparator;
private static final Comparator<ModelAttributeValue.ModelAttributeValueEnum> valueComparator;
	
	private ModelAttributeComparator() {}
	
	static {
		keyComparator = (o1, o2) -> o1.toString().compareTo(o2.toString());
		valueComparator = (o1, o2) -> o1.toString().compareTo(o2.toString());
	}
    
    public static Comparator<ModelAttributeKey> getKeyComparator() {
    	return keyComparator;
    }
    
    public static Comparator<ModelAttributeValue.ModelAttributeValueEnum> getValueComparator() {
    	return valueComparator;
    }

}
