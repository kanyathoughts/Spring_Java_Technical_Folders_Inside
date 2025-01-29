/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.datapoints.builder;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.stream.Collectors;

import innowake.mining.shared.datapoints.definition.MiningSchemaClass;
import org.apache.commons.lang3.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.PropertyReflectionUtil;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointBuilder.DataTypeBuilder;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataPointIgnore;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import org.springframework.core.annotation.MergedAnnotation;
import org.springframework.core.annotation.MergedAnnotations;

/**
 * Implementation of {@link DataTypeBuilder}.
 */
class DataTypeBuilderImpl implements DataTypeBuilder {

	private static final Logger LOG = LoggerFactory.getLogger(DataTypeBuilderImpl.class);

	private final MiningDataPointBuilderImpl parentBuilder;
	@Nullable
	private final String name;

	@Nullable
	private long[] projectIds;
	@Nullable
	private Class<?> representedBy;
	private boolean withDefaultProperties;

	/**
	 * Create new DataTypeBuilder. The defined data type will be registered on the {@code parentBuilder}. The parent builder instance
	 * is also returned from {@link #add()}.
	 * 
	 * @param parentBuilder the parent builder
	 * @param name name of the type to define
	 */
	DataTypeBuilderImpl(final MiningDataPointBuilderImpl parentBuilder, @Nullable final String name) {
		this.parentBuilder = parentBuilder;
		this.name = name;
	}

	@Override
	public DataTypeBuilder onlyOnProjects(final long... projectIds) {
		this.projectIds = projectIds;
		return this;
	}

	@Override
	public DataTypeBuilder representedBy(final Class<?> klass) {
		this.representedBy = klass;
		return this;
	}

	@Override
	public DataTypeBuilder withDefaultProperties() {
		this.withDefaultProperties = true;
		return this;
	}

	@Override
	public MiningDataPointBuilder add() {
		addAndReturnTypeName();
		return parentBuilder;
	}

	String addAndReturnTypeName() {
		validateExistingTypeDefinitionForClass();

		String actualName;

		if (name != null) {
			actualName = name;
		} else if (representedBy != null) {
			actualName = getClassName(representedBy);
		} else {
			throw new IllegalArgumentException("Either a name must be set for the type or a class must be given via representedBy()");
		}

		if (representedBy != null && withDefaultProperties) {
			actualName = introspectClass(actualName, getActualClass(representedBy));
		} else {
			final MiningDataTypeDefinition type = new MiningDataTypeDefinition(actualName, representedBy);
			type.setProjectIds(projectIds);
			parentBuilder.addTypeDefinition(type);
		}

		return actualName;
	}

	/**
	 * Validates that the Java class {@code representedBy} was not already registered under a different type name.
	 *
	 * @throws IllegalArgumentException if a name inconsistency is detected.
	 */
	private void validateExistingTypeDefinitionForClass() {
		if (name != null && representedBy != null) {
			final String nameRepresentedBy = representedBy.getName();
			final MiningSchemaClass existingTypeDefinitionForClass = parentBuilder.getKnownClasses().get(nameRepresentedBy);
			if (existingTypeDefinitionForClass != null && ! existingTypeDefinitionForClass.getName().equals(name)) {
				throw new IllegalArgumentException("The class '" + nameRepresentedBy
						+ "' was registered by '" + existingTypeDefinitionForClass.getProvidedBy() + "' with type name '" + existingTypeDefinitionForClass.getName()
						+ "' but '" + parentBuilder.getProviderName() + "' requested to add it with type name '" + name + "'.");
			}
		}
	}

	private String introspectClass(final String name, final Class<?> klass) {
		LOG.debug("Introspecting " + name + " represented by " + klass.getName());

		if (klass.isEnum()) {
			@SuppressWarnings("unchecked") /* already asserted that class is enum */
			final Class<? extends Enum<?>> enumKlass = (Class<? extends Enum<?>>) klass;
			return introspectEnum(name, enumKlass);
		}

		if (parentBuilder.getKnownClasses().containsKey(klass.getName())) {
			return parentBuilder.getKnownClasses().get(klass.getName()).getName();
		}
		LOG.debug("Adding type definition " + name + " represented by " + klass.getName());
		final MiningDataTypeDefinition typeDefinition = new MiningDataTypeDefinition(name, klass);
		typeDefinition.addProvidedBy(klass.getName());
		parentBuilder.addTypeDefinition(typeDefinition);

		try {
			final BeanInfo beanInfo = Introspector.getBeanInfo(klass);
			final PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();
			for (final PropertyDescriptor prop : propertyDescriptors) {

				if (prop.getReadMethod() == null) {
					/* skip write-only properties */
					continue;
				}

				if (prop.getReadMethod().getDeclaringClass().getName().startsWith("java.lang")) {
					/* omit properties inherited from java classes like java.lang.Object or java.lang.Enum
					 * more sophisticated blacklisting may be required */
					continue;
				}

				if (shouldIgnoreProperty(prop)) {
					continue;
				}

				final MergedAnnotation<MiningDataPoint> miningDataPointAnnotation = PropertyReflectionUtil.getAnnotationsFromProperty(prop)
						.stream()
						.map(annotations -> annotations.get(MiningDataPoint.class))
						.filter(MergedAnnotation::isPresent)
						.findAny()
						.orElse(MergedAnnotation.missing());

				final DataPointBuilderImpl dataPointBuilder = DataPointAnnotationUtil.createDataPointBuilderFromAnnotation(parentBuilder, name, prop.getName(),
						prop.getReadMethod().getGenericReturnType(), miningDataPointAnnotation);

				DataPointAnnotationUtil.fillAdditionalPropertiesFromAnnotations(PropertyReflectionUtil.getAnnotationsFromProperty(prop), dataPointBuilder);

				final MiningDataPointDefinitionWithCustomFetch dataPoint = dataPointBuilder.createDataPoint();
				dataPoint.addProvidedBy(klass.getName());
				parentBuilder.addDataPointDefinition(dataPoint);
			}
		} catch (final IntrospectionException e) {
			LOG.error("Failed to introspect " + name + " represented by " + klass.getName());
			throw new IllegalStateException("Failed to introspect " + klass.getName(), e);
		}
		return name;
	}

	private String introspectEnum(final String name, final Class<? extends Enum<?>> enumKlass) {
		if (parentBuilder.getKnownClasses().containsKey(enumKlass.getName())) {
			return parentBuilder.getKnownClasses().get(enumKlass.getName()).getName();
		}
		final Enum<?>[] enumConstants = enumKlass.getEnumConstants();
		LOG.debug("Adding type definition " + name + " represented by " + enumKlass.getName());
		final MiningEnumDefinition enumDefinition = new MiningEnumDefinition(name, enumKlass,
				Arrays.stream(enumConstants).map(Enum::name).collect(Collectors.toList()));
		enumDefinition.addProvidedBy(enumKlass.getName());
		parentBuilder.addEnumDefinition(enumDefinition);
		return name;
	}

	private String getClassName(final Class<?> klass) {
		final Class<?> componentClass = klass.isArray() ? klass.getComponentType() : klass;
		final MergedAnnotation<MiningDataType> dataTypeAnnotation = MergedAnnotations.from(componentClass, MergedAnnotations.SearchStrategy.SUPERCLASS).get(MiningDataType.class);
		if (dataTypeAnnotation.isPresent() && StringUtils.isNotEmpty(dataTypeAnnotation.getString("name"))) {
			return dataTypeAnnotation.getString("name");
		} else {
			return componentClass.getName().replace(".", "_").replace("$", "__");
		}
	}

	private Class<?> getActualClass(final Class<?> klass) {
		final Class<?> componentClass = klass.isArray() ? klass.getComponentType() : klass;
		final MergedAnnotation<MiningDataType> dataTypeAnnotation = MergedAnnotations.from(componentClass, MergedAnnotations.SearchStrategy.SUPERCLASS).get(MiningDataType.class);
		if ( ! dataTypeAnnotation.isPresent()) {
			return klass;
		} else {
			final Class<?> declaringClass = (Class<?>) dataTypeAnnotation.getSource();
			return declaringClass != null ? declaringClass : klass;
		}
	}

	private boolean shouldIgnoreProperty(final PropertyDescriptor prop) {
		return PropertyReflectionUtil.getAnnotationsFromProperty(prop).stream().anyMatch(annotations -> annotations.isPresent(MiningDataPointIgnore.class));
	}
}
