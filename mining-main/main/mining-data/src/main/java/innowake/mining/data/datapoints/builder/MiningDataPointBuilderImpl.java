/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints.builder;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import graphql.schema.DataFetchingEnvironment;
import innowake.mining.data.datapoints.registry.RegistryView;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationResult;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.annotation.MergedAnnotation;
import org.springframework.core.annotation.MergedAnnotations;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.BatchMapping;
import org.springframework.graphql.data.method.annotation.SchemaMapping;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.shared.datapoints.annotations.MiningDataPointIgnore;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import innowake.mining.shared.datapoints.definition.MiningSchemaClass;
import innowake.mining.shared.datapoints.definition.MiningSchemaElement;
import software.amazon.awssdk.utils.StringUtils;

/**
 * Implementation of {@link MiningDataPointBuilder}.
 */
public class MiningDataPointBuilderImpl implements MiningDataPointBuilder {

	private static final Logger LOG = LoggerFactory.getLogger(MiningDataPointBuilderImpl.class);

	private final List<MiningDataTypeDefinition> typeDefinitions = new ArrayList<>();
	private final List<MiningEnumDefinition> enumDefinitions = new ArrayList<>();
	private final List<MiningDataPointDefinitionWithCustomFetch> dataPointDefinitions = new ArrayList<>();
	private final List<MiningDataPointDefinition> queryDefinitions = new ArrayList<>();
	private final Map<String, MiningSchemaClass> knownClasses = new HashMap<>();

	private final String providerName;
	private final Optional<Long> projectId;
	private final RegistryView registryView;
	
	/**
	 * Create new MiningDataPointBuilder. It will collect definitions created by sub-builders.
	 * 
	 * @param providerName a string identifying the entity (e.g. class) that is using this builder to provide data points. Used for diagnostic purposes only.
	 * @param projectId Optionally, request that only data points for a specific project be provided.
	 * @param registryView the registry view to which data point definitions from this builder are being added
	 */
	public MiningDataPointBuilderImpl(final String providerName, final Optional<Long> projectId, final RegistryView registryView) {
		this.providerName = providerName;
		this.projectId = projectId;
		this.registryView = registryView;
	}
	
	@Override
	public DataTypeBuilder defineType() {
		return new DataTypeBuilderImpl(this, null);
	}
	
	@Override
	public DataTypeBuilder defineType(final String name) {
		return new DataTypeBuilderImpl(this, name);
	}

	@Override
	public EnumBuilder defineEnum(final String name) {
		return new EnumBuilderImpl(this, name);
	}
	
	@Override
	public DataPointBuilder defineQuery(final String name) {
		return new QueryBuilderImpl(this, name);
	}

	@Override
	public DataPointBuilder defineDataPoint(final String on, final String name) {
		return new DataPointBuilderImpl(this, name, on);
	}
	
	@Override
	public AliasBuilder defineAlias(final String on, final String name) {
		return new AliasBuilderImpl(this, name, on);
	}

	@Override
	public <E extends Enum<E>> MiningDataPointBuilder defineAggregations(final String on, final String fieldName, final Class<E> aggregationFields) {
		final String typeName = on + "_" + fieldName + "_aggregations";
		defineType(typeName).add();
		defineDataPoint(on, fieldName).arrayOfType(typeName).add();

		final String groupByTypeName = typeName + "_groupBy";
		defineType(groupByTypeName).add();
		defineDataPoint(typeName, "groupBy").type(groupByTypeName)
				/* custom fetch required: in AggregationRequest the field is called "groupBy" but in AggregationResult it is called "group" */
				.withCustomFetch(env -> {
					final AggregationResult<?> result = env.getSource();
					return result.getGroup();
				})
				.add();

		final String fieldsTypeName = typeName + "_fields";
		defineType(fieldsTypeName).add();
		defineDataPoint(typeName, "fields").type(fieldsTypeName)
				.add();

		for (final E aggregationField : aggregationFields.getEnumConstants()) {
			final String operatorTypeName = typeName + "_" + aggregationField.name() + "_operators";
			defineType(operatorTypeName).add();
			//TODO: define other operators from AggregationOperator enum here
			defineDataPoint(operatorTypeName, AggregationOperator.COUNT.name()).type(ScalarType.LONG)
					.withCustomFetch(DataFetchingEnvironment::getSource)
					.add();
			defineDataPoint(operatorTypeName, AggregationOperator.LIST.name()).arrayOfType(ScalarType.JSON)
					.withCustomFetch(DataFetchingEnvironment::getSource)
					.add();

			defineDataPoint(groupByTypeName, aggregationField.name()).type(ScalarType.JSON).add();
			defineDataPoint(fieldsTypeName, aggregationField.name()).type(operatorTypeName).add();
		}

		return this;
	}

	@Override
	public MiningDataPointBuilder defineDataPointsFromSchemaMappingAnnotations(final Object obj) {
		defineDataPointsFromSchemaMappingAnnotations(obj.getClass());
		return this;
	}
	@Override
	public MiningDataPointBuilder defineDataPointForGivenMethod(final Class<?> klass, final Method method) {
		final MergedAnnotations annotationsOnMethod = MergedAnnotations.from(method);
		final MergedAnnotation<SchemaMapping> schemaMapping = annotationsOnMethod.get(SchemaMapping.class);
		final MergedAnnotation<BatchMapping> batchMapping = annotationsOnMethod.get(BatchMapping.class);

		final String typeName;
		String field;
		final Type returnType;
		if (schemaMapping.isPresent()) {
			typeName = schemaMapping.getString("typeName");
			field = schemaMapping.getString("field");
			returnType = method.getGenericReturnType();
		} else {
			typeName = batchMapping.getString("typeName");
			field = batchMapping.getString("field");
			returnType = getReturnTypeOfBatchMappingMethod(method.getGenericReturnType());
		}
		if (StringUtils.isEmpty(typeName)) {
			LOG.error("Unable to define data point for @SchemaMapping method " + method.getName() + " on " + klass + ": parent type name not specified");
			return this;
		}
		if (returnType == null) {
			LOG.error("Unable to define data point for @SchemaMapping method " + method.getName() + " on " + klass
					+ ": unable to determine method return type");
			return this;
		}
		if (StringUtils.isEmpty(field)) {
			field = method.getName();
		}

		final DataPointBuilderImpl dataPointBuilder = DataPointAnnotationUtil.createDataPointBuilderFromAnnotation(this, typeName, field,
				returnType, annotationsOnMethod.get(MiningDataPoint.class));
		DataPointAnnotationUtil.fillAdditionalPropertiesFromAnnotations(annotationsOnMethod, dataPointBuilder);

		final MiningDataPointDefinitionWithCustomFetch dataPoint = dataPointBuilder.createDataPoint();

		final Parameter[] parameters = method.getParameters();
		final String[] parameterNames = new DefaultParameterNameDiscoverer().getParameterNames(method);
		for (int i = 0; i < parameters.length; i++) {
			final Parameter parameter = parameters[i];
			final MergedAnnotations parameterAnnotations = MergedAnnotations.from(parameter);
			final MergedAnnotation<Argument> argument = parameterAnnotations.get(Argument.class);
			if ( ! argument.isPresent()) {
				continue;
			}
			String parameterName = argument.getString("name");
			if (StringUtils.isEmpty(parameterName)) {
				if (parameterNames == null || parameterNames[i] == null) {
					LOG.error("Unable to determine name of parameter " + i + " of method " + method.getName() + " on " + klass
							+ ". Parameter will be ignored.");
					continue;
				}
				parameterName = parameterNames[i];
			}
			final MergedAnnotation<MiningDataPoint> miningDataPointAnnotation = parameterAnnotations.get(MiningDataPoint.class);
			final Type parameterType = parameter.getParameterizedType() != null ? parameter.getParameterizedType() : parameter.getType();
			final DataPointBuilderImpl parameterBuilder = DataPointAnnotationUtil.createDataPointBuilderFromAnnotation(this, typeName,
					parameterName, parameterType, miningDataPointAnnotation);
			dataPoint.addParameter(parameterBuilder.createDataPoint());
		}

		dataPoint.addProvidedBy(klass.getName());
		if ("Query".equals(typeName)) {
			addQueryDefinition(dataPoint);
		} else {
			addDataPointDefinition(dataPoint);
		}
		return this;
	}
	
	@Override
	public MiningDataPointBuilder defineDataPointsFromSchemaMappingAnnotations(final Class<?> klass) {
		for (final Method method : klass.getMethods()) {
			final MergedAnnotations annotationsOnMethod = MergedAnnotations.from(method);
			final MergedAnnotation<SchemaMapping> schemaMapping = annotationsOnMethod.get(SchemaMapping.class);
			final MergedAnnotation<BatchMapping> batchMapping = annotationsOnMethod.get(BatchMapping.class);
			final MergedAnnotation<MiningDataPointIgnore> miningDataPointIgnore = annotationsOnMethod.get(MiningDataPointIgnore.class);

			if ( ! (schemaMapping.isPresent() || batchMapping.isPresent()) || miningDataPointIgnore.isPresent()) {
				continue;
			}
			defineDataPointForGivenMethod(klass, method);
		}
		return this;
	}

	@Nullable
	private Type getReturnTypeOfBatchMappingMethod(final Type type) {
		if ( ! (type instanceof ParameterizedType)) {
			return null;
		}
		final ParameterizedType parameterizedType = (ParameterizedType) type;
		final Class<?> klass = (Class<?>) parameterizedType.getRawType();
		if ( ! Map.class.isAssignableFrom(klass)) {
			return null;
		}
		return parameterizedType.getActualTypeArguments()[1];
	}

	@Override
	public DataPointBuilder extend(final String on, final String name) {
		Optional<MiningDataPointDefinitionWithCustomFetch> existing = dataPointDefinitions.stream()
				.filter(dp -> dp.getParentTypeName().equals(on) && dp.getName().equals(name))
				.findAny();
		if ( ! existing.isPresent()) {
			existing = Optional.ofNullable(registryView.getDataPointDefinitions(Optional.empty()).get(on))
					.flatMap(existingType -> Optional.ofNullable(existingType.get(name)));
			if ( ! existing.isPresent()) {
				throw new IllegalArgumentException("extend() was used, but data point " + on + "." + name + " was not yet defined");
			}
		}
		return new DataPointBuilderImpl(this, existing.get());
	}

	@Override
	public List<MiningDataTypeDefinition> getTypeDefinitions() {
		return typeDefinitions;
	}

	@Override
	public List<MiningEnumDefinition> getEnumDefinitions() {
		return enumDefinitions;
	}

	@Override
	public List<MiningDataPointDefinitionWithCustomFetch> getDataPointDefinitions() {
		return dataPointDefinitions;
	}
	
	@Override
	public List<MiningDataPointDefinition> getQueryDefinitions() {
		return queryDefinitions;
	}

	void addTypeDefinition(final MiningDataTypeDefinition def) {
		validateDefinition(def);
		updateKnownClasses(def);
		typeDefinitions.add(def);
	}
	
	void addEnumDefinition(final MiningEnumDefinition def) {
		validateDefinition(def);
		updateKnownClasses(def);
		enumDefinitions.add(def);
	}
	
	void addDataPointDefinition(final MiningDataPointDefinitionWithCustomFetch def) {
		validateDefinition(def);
		dataPointDefinitions.add(def);
	}

	void replaceDataPointDefinition(final MiningDataPointDefinitionWithCustomFetch def) {
		/* used with extend() to replace an existing definition */
		validateDefinition(def);
		dataPointDefinitions.removeIf(existing -> existing.getId().equals(def.getId()));
		dataPointDefinitions.add(def);
	}
	
	void addQueryDefinition(final MiningDataPointDefinition def) {
		if (projectId.isPresent()) {
			throw new IllegalArgumentException(def.getName() + ": queries cannot be defined in a project specific context (" + projectId.get() + ")");
		}
		final long[] prjs = def.getProjectIds();
		if (prjs != null && prjs.length > 0) {
			throw new IllegalArgumentException(def.getName() + ": queries cannot be defined specific to a project (" + prjs + ")");
		}
		queryDefinitions.add(def);
	}
	
	Map<String, MiningSchemaClass> getKnownClasses() {
		final Map<String, MiningSchemaClass> ret = new HashMap<>(registryView.getKnownClasses());
		ret.putAll(knownClasses);
		return Collections.unmodifiableMap(ret);
	}

	String getProviderName() {
		return providerName;
	}
	
	private void updateKnownClasses(final MiningSchemaClass def) {
		if (def.getClassName() != null) {
			knownClasses.put(def.getClassName(), def);
		}
	}
	
	private void validateDefinition(final MiningSchemaElement def) {
		if (projectId.isPresent()) {
			final long[] prjs = def.getProjectIds();
			if (prjs == null || prjs.length != 1 || prjs[0] != projectId.get()) {
				throw new IllegalArgumentException("Expected definition specific to project " + projectId.get()
					+ " but got " + prjs + " for " + def.getName());
			}
		}
		def.addProvidedBy(providerName);
	}
	
	@Override
	public Optional<Long> getProjectId() {
		return projectId;
	}

}
