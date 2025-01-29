/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.io.discovery.config;

import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.IOException;
import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.springframework.core.annotation.MergedAnnotations;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.PropertyReflectionUtil;
import innowake.mining.shared.discovery.config.Properties;
import innowake.mining.shared.discovery.config.PropertyEntry;

/**
 * Configuration used by the DNA process. 
 */
public class Configurable {
	
	@Nullable
	private final Supplier<String> configSupplier;
	
	/**
	 * Create a new configurable instance which will not load or write properties.
	 * This constructor is used for unit test purposes where the defaults of the configurable classes are used.
	 */
	public Configurable() {
		this.configSupplier = null;
	}
	
	/**
	 * Create a new instance of the configurable with the base folder for the configuration.
	 * 
	 * @param configSupplier Supplier of configuration data.
	 */
	public Configurable(final Supplier<String> configSupplier) {
		this.configSupplier = configSupplier;
	}
	
	/**
	 * Load the properties if some are available and apply them to the current class instance.
	 */
	public void loadAndApplyConfiguration() {
		final Properties props = loadConfiguration();
		if (props != null) {
			setProperties(props);
		}
	}
	
	/**
	 * Load the configuration for this class.
	 * It is recommended to create a initial configuration with the possible values first.
	 *
	 * @return The loaded properties.
	 */
	@Nullable
	public Properties loadConfiguration() {
		try {
			if (configSupplier != null) {
				final String data = configSupplier.get();
				if (data != null) {
					final var jc = JAXBContext.newInstance(Properties.class);
					final var unmarshaller = jc.createUnmarshaller();
					final var dbf = DocumentBuilderFactory.newInstance();
					dbf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
					final var db = dbf.newDocumentBuilder();
					final var document = db.parse(new InputSource(new StringReader(data)));
					return (Properties) unmarshaller.unmarshal(document);
				}
			}
			return null;
		} catch (final JAXBException | SAXException | ParserConfigurationException | IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
	protected void setProperties(final Properties props) {
		for (final Map.Entry<PropertyDescriptor, String> entry: getConfigurableProperties().entrySet()) {
			props.get(entry.getValue()).ifPresent(property -> set(entry.getKey(), property));
		}
	}
	
	public void loadMap(final Map<String, Object> props) {
		for (final Map.Entry<PropertyDescriptor, String> entry: getConfigurableProperties().entrySet()) {
			if (props.containsKey(entry.getValue())) {
				set(entry.getKey(), props.get(entry.getValue()));
			}
		}
	}
	
	public Map<String, Object> toMap() {
		final HashMap<String, Object> props = new HashMap<>();
		for (final Map.Entry<PropertyDescriptor, String> entry: getConfigurableProperties().entrySet()) {
			try {
				props.put(entry.getValue(), entry.getKey().getReadMethod().invoke(this));
			} catch (final Exception e) {
				throw new IllegalStateException(e);
			}
		}
		return props;
	}
	
	private void set(final PropertyDescriptor desc, final PropertyEntry property) {
		final String propVal = property.getValue();
		final Object propValConverted;
		final Class<?> propertyType = desc.getPropertyType();
		
		if (propertyType.isAssignableFrom(Double.class) || propertyType.isAssignableFrom(double.class)) {
			propValConverted = Double.valueOf(propVal);
		} else if (propertyType.isAssignableFrom(Float.class) || propertyType.isAssignableFrom(float.class) ) {
			propValConverted = Float.valueOf(propVal);
		} else if (propertyType.isAssignableFrom(Long.class) || propertyType.isAssignableFrom(long.class) ) {
			propValConverted = Long.valueOf(propVal);
		} else if (propertyType.isAssignableFrom(Integer.class) || propertyType.isAssignableFrom(int.class) ) {
			propValConverted = Integer.valueOf(propVal);
		} else if (propertyType.isAssignableFrom(Boolean.class) || propertyType.isAssignableFrom(boolean.class) ) {
			propValConverted = Boolean.valueOf(propVal);
		} else if (propertyType.isAssignableFrom(String.class)) {
			propValConverted = propVal;
		} else {
			throw new IllegalStateException("Unable to apply value for property annotated with @Configuration:"
					+ " the property " + desc.getName() + " of " + getClass().getName()
					+ " has an unsupported type: " + propertyType.getName());
		}
		
		set(desc, propValConverted);
	}
	
	private void set(final PropertyDescriptor desc, final Object propValConverted) {
		final var writeMethod = desc.getWriteMethod();
		if (writeMethod == null) {
			throw new IllegalStateException("Unable to apply value for property annotated with @Configuration:"
					+ " the property " + desc.getName() + " of " + getClass().getName()
					+ " can not be written.");
		}
		
		try {
			writeMethod.invoke(this, propValConverted);
		} catch (final IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new IllegalStateException("Unable to apply value for property annotated with @Configuration", e);
		}
	}
	
	private Map<PropertyDescriptor, String> getConfigurableProperties() {
		final Map<PropertyDescriptor, String> ret = new HashMap<>();
		try {
			for (final PropertyDescriptor prop: Introspector.getBeanInfo(getClass()).getPropertyDescriptors()) {
				final Collection<MergedAnnotations> annotationsFromProperty = PropertyReflectionUtil.getAnnotationsFromProperty(prop);
				final Optional<MergedAnnotations> configurationAnnotation = annotationsFromProperty.stream()
						.filter(annotations -> annotations.isPresent(Configuration.class))
						.findAny();
				if (configurationAnnotation.isPresent()) {
					final var propertyName = configurationAnnotation.get().get(Configuration.class).getString("name");
					ret.put(prop, propertyName);
				}
			}
		} catch (final IntrospectionException e) {
			throw new IllegalStateException("unable to introspect configurable properties of " + getClass().getName(), e);
		}
		return ret;
	}
}
