/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints.builder;

import java.lang.reflect.Type;
import java.util.Collection;

import innowake.mining.data.datapoints.MiningDataPointBuilder.DataPointBuilder;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.annotation.MergedAnnotation;
import org.springframework.core.annotation.MergedAnnotations;

import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;

/**
 * Utility for adding additional properties to a {@link MiningDataPointDefinition} that are defined
 * via a {@link MiningDataPoint} annotation or a {@link Usage} annotation.
 */
class DataPointAnnotationUtil {
	
	private DataPointAnnotationUtil() {
		/* only static use */
	}

	static DataPointBuilderImpl createDataPointBuilderFromAnnotation(final MiningDataPointBuilderImpl parentBuilder,
			final String typeName, final String defaultName, final Type defaultType,
			final MergedAnnotation<MiningDataPoint> miningDataPointAnnotation) {

		if (miningDataPointAnnotation.isPresent()) {
			final String name = miningDataPointAnnotation.getString("name");
			final ScalarType scalarType = miningDataPointAnnotation.getEnum("scalarType", MiningDataPointDefinition.ScalarType.class);
			final String referenceTypeName = miningDataPointAnnotation.getString("referenceTypeName");
			final Class<?> dataPointClass = miningDataPointAnnotation.getClass("type");

			final DataPointBuilderImpl dataPointBuilder;
			if (StringUtils.isEmpty(name)) {
				dataPointBuilder = (DataPointBuilderImpl) parentBuilder.defineDataPoint(typeName, defaultName);
			} else {
				dataPointBuilder = (DataPointBuilderImpl) parentBuilder.defineDataPoint(typeName, name);
			}

			if (scalarType != ScalarType.UNDEFINED) {
				dataPointBuilder.type(scalarType);
			} else if ( ! StringUtils.isEmpty(referenceTypeName)) {
				dataPointBuilder.type(referenceTypeName);
			} else if ( ! void.class.equals(dataPointClass)) {
				dataPointBuilder.type(dataPointClass);
			} else {
				dataPointBuilder.type(defaultType);
			}

			return dataPointBuilder;
		} else {
			return (DataPointBuilderImpl) parentBuilder.defineDataPoint(typeName, defaultName).type(defaultType);
		}
	}
	
	static void fillAdditionalPropertiesFromAnnotations(final Collection<MergedAnnotations> annotations, final DataPointBuilder builder) {
		annotations.forEach(annotation -> fillAdditionalPropertiesFromAnnotations(annotation, builder));
	}
	
	static void fillAdditionalPropertiesFromAnnotations(final MergedAnnotations annotations, final DataPointBuilder builder) {
		final MergedAnnotation<MiningDataPoint> annotation = annotations.get(MiningDataPoint.class);
		if (annotation.isPresent()) {
			final String displayName = annotation.getString("displayName");
			if ( ! StringUtils.isEmpty(displayName)) {
				builder.withDisplayName(displayName);
			}
			final String description = annotation.getString("description");
			if ( ! StringUtils.isEmpty(description)) {
				builder.withDescription(description);
			}
		}
		annotations.stream(Usage.class).forEach(usageAnnotation -> {
			final String usage = usageAnnotation.getString("value");
			builder.withUsage(usage);
			for (final MergedAnnotation<UsageAttribute> attr : usageAnnotation.getAnnotationArray("attributes", UsageAttribute.class)) {
				builder.withUsageAttribute(usage, attr.getString("key"), attr.getString("value"));
			}
		});
	}
}
