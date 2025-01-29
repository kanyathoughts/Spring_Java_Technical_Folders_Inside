/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.springframework.core.annotation.MergedAnnotations;

/**
 * Utility class for introspection property annotations via reflection.
 */
public class PropertyReflectionUtil {
	
	private PropertyReflectionUtil() {
		/* no instance can be created */
	}

	/**
	 * Retrieves a collection of all annotations that are either define on the field, the write method
	 * or the read method of the given property.
	 * <p>
	 * A {@link PropertyDescriptor} can be obtained from a {@link BeanInfo} which can in turn be obtained
	 * with {@link Introspector#getBeanInfo(Class)}.
	 * 
	 * @param prop a {@link PropertyDescriptor}
	 * @return the collection of annotations on the property
	 */
	public static Collection<MergedAnnotations> getAnnotationsFromProperty(final PropertyDescriptor prop) {
		final List<MergedAnnotations> annotations = new ArrayList<>();
		final Method readMethod = prop.getReadMethod();
		final Method writeMethod = prop.getWriteMethod();
		Field field;
		if (readMethod != null) {
			try {
				field = readMethod.getDeclaringClass().getDeclaredField(prop.getName());
			} catch (final NoSuchFieldException e) {
				field = null;
			}
		} else {
			field = null;
		}

		if (readMethod != null) {
			annotations.add(MergedAnnotations.from(readMethod));
		}
		if (writeMethod != null) {
			annotations.add(MergedAnnotations.from(writeMethod));
		}
		if (field != null) {
			annotations.add(MergedAnnotations.from(field));
		}

		return annotations;
	}
}
