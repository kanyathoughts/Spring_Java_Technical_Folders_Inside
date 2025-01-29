package fw2.orm.xlsx.io;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.BeanUtilsBean2;
import org.apache.commons.beanutils.converters.DateConverter;
import org.apache.commons.beanutils.converters.DateTimeConverter;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;

import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.MapElement;
import fw2.orm.xlsx.Mapping;
import fw2.orm.xlsx.ReferenceMap;
import fw2.orm.xlsx.io.mapper.MappingException;

public class RepositoryImpl implements Repository {

	public static BeanUtilsBean2 beanUtilsBean = null;

	static {

		final DateTimeConverter dtConverter = new DateConverter();
		dtConverter.setPattern("MM/dd/yyyy");

		beanUtilsBean = new BeanUtilsBean2();
		beanUtilsBean.getConvertUtils().register(dtConverter, java.util.Date.class);

	}

	Mapping classMapMapping;

	EPackage ePackage;

	Map<String, List<?>> lookupMap = new HashMap<String, List<?>>();

	@Override
	public Map<String, List<?>> getLookupMap() {
		return lookupMap;
	}

	public RepositoryImpl(final Mapping classMapMapping, EPackage ePackage) {
		this.classMapMapping = classMapMapping;
		this.ePackage = ePackage;

	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T createInstance(final Class<T> type) {
		if (ePackage != null) {
			final EClass eClass = (EClass) this.ePackage.getEClassifier(type.getSimpleName());
			return (T) this.ePackage.getEFactoryInstance().create(eClass);
		} else {
			try {
				return (T) type.newInstance();
			} catch (IllegalAccessException | InstantiationException e) {
				throw new AssertionError(e);
			} 
		}

	}

	@Override
	public ClassMap getClassMapFor(final Class<?> type) {
		return this.getClassMapFor(type.getSimpleName());
	}

	@Override
	public ClassMap getClassMapFor(final Object object) {

		if (object instanceof EObject) {
			return this.getClassMapFor(((EObject) object).eClass().getName());
		} else {
			return this.getClassMapFor(object.getClass());
		}
	}

	public ClassMap getParentClass(ClassMap c) {
		// TODO what about multiple parents???
		final List<ClassMap> allClasses = this.classMapMapping.getClassMaps();
		for (final ClassMap classMap : allClasses) {
			for (final ReferenceMap reference : classMap.getReferenceMaps()) {
				if ((null != reference.getRelatedClassMap()) && reference.getRelatedClassMap().equals(c)) {
					return classMap;
				}
			}
		}

		return null;
	}

	@Override
	public ClassMap getClassMapFor(final String simpleName) {
		for (final ClassMap map : this.classMapMapping.getClassMaps()) {
			if (map.getName().equals(simpleName)) {
				return map;
			}
		}
		return null;
	}

	@Override
	public Object lookupObject(final String root, final List<NameValuePair> lookupKeys) {
		final List<NameValuePair> list = new ArrayList<NameValuePair>(lookupKeys);
		if (list.size() > 0) {
			final NameValuePair key = list.get(0);
			final String name = key.getValue();
			final List<?> objects = this.getLookupObjects(root);
			if (objects != null) {
				for (final Object part : objects) {
					if (part instanceof MapElement) {
						if (((MapElement) part).getName().equals(name)) {
							return part;
						}
					}
				}
			} else {
				return this.getClassMapFor(name);// Fallback
			}
		}
		return null;

	}

	public List<?> getLookupObjects(final String root) {
		return this.lookupMap.get(root);
	}

	@Override
	public Object getProperty(Object object, String propertyName) {
		try {
			if (object instanceof EObject) {
				EObject eObject = ((EObject) object);
				EClass eClass = eObject.eClass();
				EStructuralFeature eStructuralFeature = eClass.getEStructuralFeature(propertyName);
				if (eStructuralFeature == null) {
					throw new AssertionError("Property:" + propertyName + " Not found in:" + eClass.getName());
				}
				return eObject.eGet(eStructuralFeature);

			} else {
				return beanUtilsBean.getProperty(object, propertyName);
			}
		} catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
			throw new MappingException(e);
		}
	}

	@Override
	public void setProperty(Object object, String propertyName, Object value) {
		try {
			if (object instanceof EObject) {
				EObject eObject = ((EObject) object);
				EClass eClass = eObject.eClass();
				EStructuralFeature eStructuralFeature = eClass.getEStructuralFeature(propertyName);
				if (eStructuralFeature == null) {
					throw new AssertionError("Property:" + propertyName + " Not found in:" + eClass.getName());
				}
				EClassifier eType = eStructuralFeature.getEType();
				Object convertedValue = value;
				if (eType instanceof EDataType) {
					convertedValue = EcoreUtil.createFromString((EDataType) eType, (String) value);
				}
				eObject.eSet(eStructuralFeature, convertedValue);
			} else {
				beanUtilsBean.setProperty(object, propertyName, value);
			}
		} catch (IllegalAccessException | InvocationTargetException e) {
			throw new MappingException(e);
		}
	}

	@Override
	public void addLookupObjects(String name, List<Object> objects) {
		lookupMap.put(name, objects);
	}

}
