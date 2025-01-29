/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints.builder;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.regex.Pattern;

import innowake.mining.shared.datapoints.definition.AliasDefinition;
import org.apache.commons.lang3.CharUtils;
import org.springframework.data.domain.Page;

import graphql.execution.DataFetcherResult;
import graphql.schema.DataFetcher;
import graphql.schema.PropertyDataFetcher;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointBuilder.DataPointBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.MiningDataPointFilterCallbacks;
import innowake.mining.shared.datapoints.definition.MiningDataPointSortCallback;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * Implementation of {@link DataPointBuilder}.
 */
class DataPointBuilderImpl implements DataPointBuilder {

	private static final Pattern ESCAPE_NAME_PATTERN = Pattern.compile("[^a-zA-Z0-9_]");
	
	private static final Logger LOG = LoggerFactory.getLogger(DataPointBuilderImpl.class);
	
	private static final String PAGE_TYPE_PREFIX = "PAGE_";
	private static final String PAGED_TYPE_PREFIX = "PAGED_";
	private static final String MAP_TYPE_PREFIX = "MAP_";

	protected final MiningDataPointBuilderImpl parentBuilder;
	
	private final String name;
	private final String parentTypeName;
	@Nullable
	private ScalarType scalarType;
	@Nullable
	private String referenceTypeName;
	@Nullable
	private long[] projectIds;
	private boolean isArray;
	private boolean isNullable = true;
	private String displayName = "";
	private String description = "";
	@Nullable
	private DataFetcher<?> customFetch;
	
	private final Set<String> usages = new HashSet<>();
	private final Map<String, Map<String, String>> usageAttributes = new HashMap<>();
	
	private final List<MiningDataPointDefinition> parameters = new ArrayList<>();

	private final Map<String, MiningDataPointFilterCallbacks> filterCallbacks = new HashMap<>();
	private final Map<String, MiningDataPointSortCallback<?>> sortCallbacks = new HashMap<>();
	private final Map<String, MiningDataPointBuilder.DynamicFieldBuilder<?>> dynamicFieldBuilders = new HashMap<>();
	
	private final boolean isReplacing;
	@Nullable
	private final Set<String> previousProviders;

	/* aliasDefinition is set only when extending an existing data point alias with extend() - in that case isReplacing will be true */
	@Nullable
	private final AliasDefinition aliasDefinition;

	/**
	 * Create new DataPointBuilder. The defined data point will be registered on the {@code parentBuilder}. The parent builder instance
	 * is also returned from {@link #add()}.
	 * 
	 * @param parentBuilder the parent builder
	 * @param name name of the data point to define
	 * @param parentTypeName name of the type on which to define the data point
	 */
	DataPointBuilderImpl(final MiningDataPointBuilderImpl parentBuilder, final String name, final String parentTypeName) {
		this.parentBuilder = parentBuilder;
		this.name = name;
		this.parentTypeName = parentTypeName;
		isReplacing = false;
		previousProviders = null;
		aliasDefinition = null;
	}

	/**
	 * Initializes the builder from an existing data point definition, for modifying or extending the data point.
	 * @param parentBuilder the parent builder
	 * @param existingDataPoint the existing data point definition - builder will be initialized with the properties of the data point
	 */
	DataPointBuilderImpl(final MiningDataPointBuilderImpl parentBuilder, final MiningDataPointDefinitionWithCustomFetch existingDataPoint) {
		this.parentBuilder = parentBuilder;
		this.name = existingDataPoint.getName();
		this.parentTypeName = existingDataPoint.getParentTypeName();
		this.scalarType = existingDataPoint.getScalarType();
		this.referenceTypeName = existingDataPoint.getReferenceTypeName();
		this.projectIds = existingDataPoint.getProjectIds();
		this.isArray = existingDataPoint.isArray();
		this.isNullable = existingDataPoint.isNullable();
		this.displayName = existingDataPoint.getDisplayName();
		this.description = existingDataPoint.getDescription();
		this.customFetch = existingDataPoint.getCustomFetch();
		this.usages.addAll(existingDataPoint.getUsages());
		this.usageAttributes.putAll(existingDataPoint.getUsageAttributes());
		this.parameters.addAll(existingDataPoint.getParameters());
		this.filterCallbacks.putAll(existingDataPoint.getFilterCallbacks());
		this.sortCallbacks.putAll(existingDataPoint.getSortCallbacks());
		this.previousProviders = existingDataPoint.getProvidedBy();
		aliasDefinition = existingDataPoint.getAliasFor();
		isReplacing = true;
	}

	@Override
	public DataPointBuilder onlyOnProjects(@Nullable final long... projectIds) {
		this.projectIds = projectIds; 
		return this;
	}

	@Override
	public DataPointBuilder type(final String type) {
		referenceTypeName = type;
		scalarType = null;
		isArray = false;
		return this;
	}

	@Override
	public DataPointBuilder type(final ScalarType type) {
		scalarType = type;
		referenceTypeName = null;
		isArray = false;
		return this;
	}
	
	@Override
	public DataPointBuilder type(final Type rawType) {
		final Type type = unwrapControllerReturnValue(rawType);
		final Class<?> klass;
		if (type instanceof ParameterizedType) {
			klass = (Class<?>) ((ParameterizedType) type).getRawType();
		} else {
			/* I hope this will always go well ... */
			klass = (Class<?>) type;
		}
		
		if (Map.class.isAssignableFrom(klass)) {
			/* Maps are represented as arrays of key-value pairs */
			isArray = true;
			referenceTypeName = defineMapType((ParameterizedType) type);
			/* define custom fetch logic which converts a Map to a list of Map entries */
			final PropertyDataFetcher<Map<?, ?>> fetcher = PropertyDataFetcher.fetching(name);
			withCustomFetch(context -> {
				final Map<?, ?> map = fetcher.get(context);
				if (map == null) {
					return Collections.emptyList();
				} else {
					return map.entrySet();
				}
			});
		} else if (Date.class.isAssignableFrom(klass)) {
			scalarType = ScalarType.DATETIME;
			/* convert Date field to UTC date/time */
			final PropertyDataFetcher<Date> fetcher = PropertyDataFetcher.fetching(name);
			withCustomFetch(context -> Optional.ofNullable(fetcher.get(context)).map(d -> d.toInstant().atZone(ZoneOffset.UTC)));
		} else if (Collection.class.isAssignableFrom(klass)) {
			/* all collections will be represented as arrays of their element type */
			isArray = true;
			/* not the most robust reflection magic here: we only support simple cases like a method returning List<Foo>
			 * should be sufficient for now */
			inspectElementType((ParameterizedType) type);
		} else if (Optional.class.isAssignableFrom(klass)) {
			/* if the value is wrapped in Optional<> then it is represented as nullable in the schema */
			isNullable = true;
			inspectElementType((ParameterizedType) type);
		} else if (Page.class.isAssignableFrom(klass)) {
			referenceTypeName = definePageType((ParameterizedType) type, true);
		} else if (Paged.class.isAssignableFrom(klass)) {
			referenceTypeName = definePageType((ParameterizedType) type, false);
		} else if (type instanceof ParameterizedType) {
			/* parameterized types (e.g. Foo<Bar>) that are not collections or maps are currently unsupported */
			LOG.warn(() -> "Unsupported parameterized type of " + name + " on " + parentTypeName);
		} else {
			scalarType = getScalarType(klass);
			if (scalarType == null) {
				referenceTypeName = ((DataTypeBuilderImpl) parentBuilder.defineType().representedBy(klass).withDefaultProperties()).addAndReturnTypeName();
			}
		}
		
		if ( ! isArray && klass.isArray()) {
			isArray = true;
		}
		
		return this;
	}

	private Type unwrapControllerReturnValue(final Type rawType) {
		/* GraphQl controllers may return DataFetcherResult<T> - in that case the type of the data point is T
		 * they may also return Mono<T> or Flux<T> or CompletableFuture<T> or a combination of these and DataFetcherResult */
		Type type = rawType;
		boolean unwrapped;
		do {
			unwrapped = false;
			if (type instanceof ParameterizedType) {
				final ParameterizedType parameterizedType = (ParameterizedType) type;
				if (Mono.class.isAssignableFrom((Class<?>) parameterizedType.getRawType())
						|| Flux.class.isAssignableFrom((Class<?>) parameterizedType.getRawType())
						|| CompletableFuture.class.isAssignableFrom((Class<?>) parameterizedType.getRawType())
						|| DataFetcherResult.class.isAssignableFrom((Class<?>) parameterizedType.getRawType())) {
					type = parameterizedType.getActualTypeArguments()[0];
					unwrapped = true;
				}
			}
		} while (unwrapped);

		return type;
	}

	private void inspectElementType(final ParameterizedType type) {
		final Type[] actualTypeArguments = type.getActualTypeArguments();
		if (actualTypeArguments.length == 1) {
			if (actualTypeArguments[0] instanceof Class<?>) {
				final Class<?> elementType = (Class<?>) actualTypeArguments[0];
				scalarType = getScalarType(elementType);
				if (scalarType == null) {
					referenceTypeName = ((DataTypeBuilderImpl) parentBuilder.defineType().representedBy(elementType).withDefaultProperties()).addAndReturnTypeName();
				}
			} else if (actualTypeArguments[0] instanceof ParameterizedType
					&& Map.class.isAssignableFrom((Class<?>) ((ParameterizedType) actualTypeArguments[0]).getRawType())) {
				referenceTypeName = defineMapType((ParameterizedType) actualTypeArguments[0]);
			} else {
				LOG.warn(() -> "Unsupported collection type of " + name + " on " + parentTypeName);
			}
		} else {
			LOG.warn(() -> "Unsupported collection type of " + name + " on " + parentTypeName);
		}
	}

	@Override
	public DataPointBuilder arrayOfType(final String type) {
		referenceTypeName = type;
		scalarType = null;
		isArray = true;
		return this;
	}

	@Override
	public DataPointBuilder arrayOfType(final ScalarType type) {
		scalarType = type;
		referenceTypeName = null;
		isArray = true;
		return this;
	}
	
	@Override
	public DataPointBuilder nullable() {
		isNullable = true;
		return this;
	}

	@Override
	public DataPointBuilder notNull() {
		isNullable = false;
		return this;
	}
	
	@Override
	public DataPointBuilder withDisplayName(final String displayName) {
		this.displayName = displayName; 
		return this;
	}
	
	@Override
	public DataPointBuilder withDescription(final String description) {
		this.description = description;
		return this;
	}
	
	@Override
	public DataPointBuilder withUsage(final String dataPointUsage) {
		usages.add(dataPointUsage);
		return this;
	}
	
	@Override
	public DataPointBuilder withUsageAttribute(final String dataPointUsage, final String key, final String value) {
		withUsage(dataPointUsage);
		usageAttributes.computeIfAbsent(dataPointUsage, k -> new HashMap<>()).put(key, value);
		return this;
	}

	@Override
	public DataPointBuilder withCustomFetch(final DataFetcher<?> dataFetcher) {
		customFetch = dataFetcher;
		return this;
	}
	
	@Override
	public DataPointBuilder withOptionalParameter(final String name, final Class<?> cls) {
		parameters.add(new MiningDataPointDefinition(name, "", parentBuilder.getKnownClasses().get(cls.getName()).getName(), false, true));
		return this;
	}
	
	@Override
	public DataPointBuilder withOptionalParameter(final String name, final ScalarType type) {
		parameters.add(new MiningDataPointDefinition(name, "", type, false, true));
		return this;
	}

	@Override
	public DataPointBuilder withOptionalParameter(final String name, final String type) {
		parameters.add(new MiningDataPointDefinition(name, "", type, false, true));
		return this;
	}
	
	@Override
	public DataPointBuilder withOptionalParameterArray(final String name, final ScalarType type) {
		parameters.add(new MiningDataPointDefinition(name, "", type, true, true));
		return this;
	}
	
	@Override
	public DataPointBuilder withOptionalParameterArray(final String name, final String type) {
		parameters.add(new MiningDataPointDefinition(name, "", type, true, true));
		return this;
	}
	
	@Override
	public DataPointBuilder withRequiredParameter(final String name, final Class<?> cls) {
		parameters.add(new MiningDataPointDefinition(name, "", parentBuilder.getKnownClasses().get(cls.getName()).getName(), false, false));
		return this;
	}
	
	@Override
	public DataPointBuilder withRequiredParameter(final String name, final ScalarType type) {
		parameters.add(new MiningDataPointDefinition(name, "", type, false, false));
		return this;
	}

	@Override
	public DataPointBuilder withRequiredParameter(final String name, final String type) {
		parameters.add(new MiningDataPointDefinition(name, "", type, false, false));
		return this;
	}
	
	@Override
	public DataPointBuilder withRequiredParameterArray(final String name, final ScalarType type) {
		parameters.add(new MiningDataPointDefinition(name, "", type, true, false));
		return this;
	}
	
	@Override
	public DataPointBuilder withRequiredParameterArray(final String name, final String type) {
		parameters.add(new MiningDataPointDefinition(name, "", type, true, false));
		return this;
	}

	@Override
	public DataPointBuilder withFiltering(final String queryName, final Consumer<MiningDataPointBuilder.FilterBuilder> builderConsumer) {
		if (filterCallbacks.containsKey(queryName)) {
			throw new IllegalStateException("Duplicate withFiltering() defined for " + queryName
					+ ", should only be called once. You should define a lambda with function block and call multiple methods on your lambda parameter.");
		}
		/* required, or FilterObjectService ignores the data point when building the FilterObject schema */
		withUsage(Usages.GRAPHQL_QUERY_PREFIX + queryName);

		final var filterBuilder = new FilterBuilderImpl(null, null);
		builderConsumer.accept(filterBuilder);
		filterCallbacks.put(queryName, filterBuilder.build());
		return this;
	}

	@Override
	public DataPointBuilder withFiltering(final String queryName, final ScalarType filterArgumentType,
			final Consumer<MiningDataPointBuilder.FilterBuilder> builderConsumer) {
		/* required, or FilterObjectService ignores the data point when building the FilterObject schema */
		withUsage(Usages.GRAPHQL_QUERY_PREFIX + queryName);

		final var filterBuilder = new FilterBuilderImpl(filterArgumentType, null);
		builderConsumer.accept(filterBuilder);
		filterCallbacks.put(queryName, filterBuilder.build());
		return this;
	}

	@Override
	public DataPointBuilder withFiltering(final String queryName, final String filterArgumentType,
			final Consumer<MiningDataPointBuilder.FilterBuilder> builderConsumer) {
		/* required, or FilterObjectService ignores the data point when building the FilterObject schema */
		withUsage(Usages.GRAPHQL_QUERY_PREFIX + queryName);

		final var filterBuilder = new FilterBuilderImpl(null, filterArgumentType);
		builderConsumer.accept(filterBuilder);
		filterCallbacks.put(queryName, filterBuilder.build());
		return this;
	}

	@Override
	public <B> DataPointBuilder withSorting(final String queryName, final MiningDataPointSortCallback<B> cb) {
		/* required, or SortObjectService ignores the data point when building the SortObject schema */
		withUsage(Usages.GRAPHQL_QUERY_PREFIX + queryName);
		sortCallbacks.put(queryName, cb);
		return this;
	}
	
	@Override
	public <B> DataPointBuilder withDynamicFieldBuilder(final String referenceType, final MiningDataPointBuilder.DynamicFieldBuilder<B> b) {
		dynamicFieldBuilders.put(referenceType, b);
		return this;
	}
	
	@Override
	public MiningDataPointBuilder add() {
		final MiningDataPointDefinitionWithCustomFetch dataPoint = createDataPoint();
		LOG.debug(() -> "Adding data point definition " + name + " on " + parentTypeName + ": " + dataPoint);
		if (previousProviders != null) {
			previousProviders.forEach(dataPoint::addProvidedBy);
		}
		if (isReplacing) {
			parentBuilder.replaceDataPointDefinition(dataPoint);
		} else {
			parentBuilder.addDataPointDefinition(dataPoint);
		}
		return parentBuilder;
	}

//	@Override
//	public DataPointBuilder withAlias(final String aliasName) {
//		final var aliasDefinition = this.parentBuilder.defineAlias(this.parentTypeName, aliasName).forDataPoint(this.name);
//		this.usages.forEach(aliasDefinition::withUsage);
//		aliasDefinition.add();
//		this.aliasDefinition = new AliasDefinition(aliasName, StringUtils.trimToEmpty(""), StringUtils.trimToEmpty(""), Arrays.asList());
//		return this;
//	}

	MiningDataPointDefinitionWithCustomFetch createDataPoint() {
		final MiningDataPointDefinitionWithCustomFetch dataPoint;

		final String escapedName = escapeName(name);

		if (scalarType == ScalarType.DATETIME) {
			/* additional field to retrieve a Date as milliseconds timestamp */
			parentBuilder.defineDataPoint(parentTypeName, escapedName + "Timestamp").type(ScalarType.TIMESTAMP)
				.onlyOnProjects(projectIds)
				.withCustomFetch(PropertyDataFetcher.fetching(name))
				.add();
		}

		if (scalarType == ScalarType.UNDEFINED) {
			throw new IllegalArgumentException("Data Point " + name + " on " + parentTypeName + " has type set to UNDEFINED");
		}

		if (aliasDefinition != null) {
			dataPoint = new MiningDataPointDefinitionWithCustomFetch(new MiningDataPointDefinition(escapedName, parentTypeName, aliasDefinition,
					filterCallbacks, sortCallbacks));
		} else if (scalarType != null) {
			dataPoint = new MiningDataPointDefinitionWithCustomFetch(new MiningDataPointDefinition(escapedName, parentTypeName, scalarType, isArray, isNullable,
					filterCallbacks, sortCallbacks), customFetch, dynamicFieldBuilders);
		} else if (referenceTypeName != null) {
			dataPoint = new MiningDataPointDefinitionWithCustomFetch(new MiningDataPointDefinition(escapedName, parentTypeName, referenceTypeName,
					isArray, isNullable, filterCallbacks, sortCallbacks), customFetch, dynamicFieldBuilders);
		} else {
			throw new IllegalArgumentException("Data Point " + name + " on " + parentTypeName + " has no type()");
		}

		dataPoint.setProjectIds(projectIds);
		dataPoint.setDisplayName(displayName);
		dataPoint.setDescription(description);
		dataPoint.addUsages(usages);
		for (final Map.Entry<String, Map<String, String>> entry : usageAttributes.entrySet()) {
			dataPoint.addUsageAttributes(entry.getKey(), entry.getValue());
		}
		dataPoint.addParameters(parameters);
		return dataPoint;
	}
	
	@Nullable
	private ScalarType getScalarType(final Class<?> propType) {
		final Class<?> componentClass = propType.isArray() ? propType.getComponentType() : propType;
		if (float.class.isAssignableFrom(componentClass)
				|| Float.class.isAssignableFrom(componentClass)
				|| double.class.isAssignableFrom(componentClass)
				|| Double.class.isAssignableFrom(componentClass)) {
			return ScalarType.FLOAT;
		} else if (int.class.isAssignableFrom(componentClass)
				|| Integer.class.isAssignableFrom(componentClass)
				|| byte.class.isAssignableFrom(componentClass)
				|| Byte.class.isAssignableFrom(componentClass)) {
			return ScalarType.INT;
		} else if (long.class.isAssignableFrom(componentClass)
				|| Long.class.isAssignableFrom(componentClass)) {
			return ScalarType.LONG;
		} else if (boolean.class.isAssignableFrom(componentClass)
				|| Boolean.class.isAssignableFrom(componentClass)) {
			return ScalarType.BOOLEAN;
		} else if (String.class.isAssignableFrom(componentClass)
				|| char.class.isAssignableFrom(componentClass)
				|| Character.class.isAssignableFrom(componentClass)) {
			return ScalarType.STRING;
		} else if (Date.class.isAssignableFrom(componentClass)
				|| Instant.class.isAssignableFrom(componentClass)
				|| OffsetDateTime.class.isAssignableFrom(componentClass)
				|| ZonedDateTime.class.isAssignableFrom(componentClass)) {
			return ScalarType.DATETIME;
		} else if (UUID.class.isAssignableFrom(componentClass)) {
			return ScalarType.UUID;
		} else if (EntityId.class.isAssignableFrom(componentClass)) {
			return ScalarType.ENTITY_ID;
		}
		return null;
	}
	
	@Nullable
	private String defineMapType(final ParameterizedType type) {
		final Type[] actualTypeArguments = type.getActualTypeArguments();
		if (actualTypeArguments.length != 2) {
			LOG.warn(() -> "Expected Map type to have two type arguments (for key and value), but had " + actualTypeArguments.length);
			LOG.warn(() -> "Unsupported type of " + name + " on " + parentTypeName + ": property will be ignored");
			return null;
		}
		
		final Class<?> keyType;
		if (actualTypeArguments[0] instanceof Class<?>) {
			keyType = (Class<?>) actualTypeArguments[0];
		} else {
			LOG.warn(() -> "Unexpected type arguments for Map type:" + actualTypeArguments[0].getTypeName() + ", " + actualTypeArguments[1].getTypeName()
					+ ": parameterized key type is not supported");
			LOG.warn(() -> "Unsupported type of " + name + " on " + parentTypeName + ": property will be ignored");
			return null;
		}
		
		final Class<?> valueType;
		final boolean arrayValue;
		if (actualTypeArguments[1] instanceof Class<?>) {
			/* value type is "simple type" */
			valueType = (Class<?>) actualTypeArguments[1];
			arrayValue = false;
		} else if (actualTypeArguments[1] instanceof ParameterizedType
				&& ((ParameterizedType) actualTypeArguments[1]).getActualTypeArguments().length == 1
				&& ((ParameterizedType) actualTypeArguments[1]).getActualTypeArguments()[0] instanceof Class<?>
				&& ((ParameterizedType) actualTypeArguments[1]).getRawType() instanceof Class<?>
				&& Collection.class.isAssignableFrom((Class<?>) ((ParameterizedType) actualTypeArguments[1]).getRawType())) {
			/* value type is a Collection with one type parameter which is of simple type, e.g. List<Something> or Set<Something> */
			valueType = (Class<?>) ((ParameterizedType) actualTypeArguments[1]).getActualTypeArguments()[0];
			arrayValue = true;
		} else {
			/* value type is something else which we don't support at the moment */
			LOG.warn(() -> "Unexpected type arguments for Map type:" + actualTypeArguments[0].getTypeName() + ", " + actualTypeArguments[1].getTypeName()
					+ ": value type is an unsupported parameterized type.");
			LOG.warn(() -> "Unsupported type of " + name + " on " + parentTypeName + ": property will be ignored");
			return null;
		}
		
		final ScalarType keyScalarType = getScalarType(keyType);
		final String keyReferenceTypeName = keyScalarType == null ? ((DataTypeBuilderImpl) parentBuilder.defineType().representedBy(keyType).withDefaultProperties()).addAndReturnTypeName() : null;
		final ScalarType valueScalarType = getScalarType(valueType);
		final String valueReferenceTypeName = valueScalarType == null ? ((DataTypeBuilderImpl) parentBuilder.defineType().representedBy(valueType).withDefaultProperties()).addAndReturnTypeName() : null;
		
		return defineMapTypeFor(keyScalarType, keyReferenceTypeName, valueScalarType, valueReferenceTypeName, arrayValue);
	}
	
	private String defineMapTypeFor(@Nullable final ScalarType keyScalarType, @Nullable final String keyReferenceTypeName, 
			@Nullable final ScalarType valueScalarType, @Nullable final String valueReferenceTypeName,
			final boolean arrayValue) {
		
		final String keyTypeName = keyScalarType != null ? keyScalarType.name() : keyReferenceTypeName;
		final String valueTypeName = valueScalarType != null ? valueScalarType.name() : valueReferenceTypeName;
		final String mapTypeName = MAP_TYPE_PREFIX + keyTypeName + (arrayValue ? "_ARRAY_OF_" : "_") + valueTypeName;
		
		final MiningDataTypeDefinition mapType = new MiningDataTypeDefinition(mapTypeName);
		mapType.addProvidedBy(parentTypeName);

		parentBuilder.addTypeDefinition(mapType);
		
		final MiningDataPointDefinitionWithCustomFetch keyDataPoint;
		final MiningDataPointDefinitionWithCustomFetch valueDataPoint;
		
		if (keyScalarType != null) {
			keyDataPoint = new MiningDataPointDefinitionWithCustomFetch(new MiningDataPointDefinition("key", mapTypeName, keyScalarType, false, true));
		} else if (keyReferenceTypeName != null){
			keyDataPoint = new MiningDataPointDefinitionWithCustomFetch(new MiningDataPointDefinition("key", mapTypeName, keyReferenceTypeName, false, true));
		} else {
			throw new IllegalArgumentException("Either a scalar type or a reference type name must be given for the map key.");
		}
		if (valueScalarType != null) {
			valueDataPoint = new MiningDataPointDefinitionWithCustomFetch(new MiningDataPointDefinition("value", mapTypeName, valueScalarType, arrayValue, true));
		} else if (valueReferenceTypeName != null){
			valueDataPoint = new MiningDataPointDefinitionWithCustomFetch(new MiningDataPointDefinition("value", mapTypeName, valueReferenceTypeName, arrayValue, true));
		} else {
			throw new IllegalArgumentException("Either a scalar type or a reference type name must be given for the contents of the page.");
		}
		keyDataPoint.addProvidedBy(parentTypeName);
		valueDataPoint.addProvidedBy(parentTypeName);
		
		parentBuilder.addDataPointDefinition(keyDataPoint);
		parentBuilder.addDataPointDefinition(valueDataPoint);
		
		return mapTypeName;
	}
	
	@Nullable
	private String definePageType(final ParameterizedType type, final boolean isSpringFrameworkPaged) {
		/* for org.springframework.data.domain.Page */
		final Type[] actualTypeArguments = type.getActualTypeArguments();
		if (actualTypeArguments.length != 1) {
			LOG.warn(() -> "Expected Page type to have one type argument, but had " + actualTypeArguments.length);
			LOG.warn(() -> "Unsupported type of " + name + " on " + parentTypeName + ": property will be ignored");
			return null;
		}
		
		final Class<?> contentType = (Class<?>) actualTypeArguments[0];
		final ScalarType contentScalarType = getScalarType(contentType);
		final String contentReferenceTypeName =  contentScalarType == null ? ((DataTypeBuilderImpl) parentBuilder.defineType().representedBy(contentType).withDefaultProperties()).addAndReturnTypeName() : null;

		if (isSpringFrameworkPaged) {
			return definePageTypeFor(contentScalarType, contentReferenceTypeName);
		} else {
			return definePagedTypeFor(contentScalarType, contentReferenceTypeName);
		}
	}
	
	private String definePageTypeFor(@Nullable final ScalarType contentScalarType, @Nullable final String contentReferenceTypeName) {
		/* for org.springframework.data.domain.Page */
		final String contentTypeName = contentScalarType != null ? contentScalarType.name() : contentReferenceTypeName;
		final String pageTypeName = PAGE_TYPE_PREFIX + contentTypeName;
		parentBuilder.defineType(pageTypeName).add();
		parentBuilder.defineDataPoint(pageTypeName, "totalPages").type(ScalarType.INT).add();
		parentBuilder.defineDataPoint(pageTypeName, "totalElements").type(ScalarType.INT).add();
		parentBuilder.defineDataPoint(pageTypeName, "numberOfElements").type(ScalarType.INT).add();
		parentBuilder.defineDataPoint(pageTypeName, "number").type(ScalarType.INT).add();
		parentBuilder.defineDataPoint(pageTypeName, "size").type(ScalarType.INT).add();
		parentBuilder.defineDataPoint(pageTypeName, "hasNext").type(ScalarType.BOOLEAN).add();
		parentBuilder.defineDataPoint(pageTypeName, "hasPrevious").type(ScalarType.BOOLEAN).add();
		parentBuilder.defineDataPoint(pageTypeName, "isFirst").type(ScalarType.BOOLEAN).add();
		parentBuilder.defineDataPoint(pageTypeName, "isLast").type(ScalarType.BOOLEAN).add();
		if (contentScalarType != null) {
			parentBuilder.defineDataPoint(pageTypeName, "content").arrayOfType(contentScalarType).add();
		} else if (contentReferenceTypeName != null) {
			parentBuilder.defineDataPoint(pageTypeName, "content").arrayOfType(contentReferenceTypeName).add();
		} else {
			throw new IllegalArgumentException("Either a scalar type or a reference type name must be given for the contents of the page.");
		}
		return pageTypeName;
	}

	private String definePagedTypeFor(@Nullable final ScalarType contentScalarType, @Nullable final String contentReferenceTypeName) {
		/* for innowake.mining.shared.access.Paged */
		final String contentTypeName = contentScalarType != null ? contentScalarType.name() : contentReferenceTypeName;
		final String pageTypeName = PAGED_TYPE_PREFIX + contentTypeName;
		parentBuilder.defineType(pageTypeName).add();
		parentBuilder.defineDataPoint(pageTypeName, "totalPages").type(ScalarType.INT)
				/* Paged class has no getter for this, so using customFetch */
				.withCustomFetch(env -> {
					final Paged<?> paged = env.getSource();
					final Integer limit = paged.getLimit();
					final long totalElements = Optional.ofNullable(paged.getTotalElements()).orElse(0L);
					if (limit.equals(0)) {
						/* avoid division by zero */
						return 0;
					}
					return Math.ceil(totalElements / limit.doubleValue());
				})
				.add();
		parentBuilder.defineDataPoint(pageTypeName, "totalElements").type(ScalarType.INT)
				.withCustomFetch(env -> {
					final Paged<?> paged = env.getSource();
					final Long totalElements = paged.getTotalElements();
					if (totalElements == null) {
						return Long.valueOf(0);
					}
					return totalElements;
				})
				.add();
		parentBuilder.defineDataPoint(pageTypeName, "number").type(ScalarType.INT)
				/* Paged class has no getter for this, so using customFetch */
				.withCustomFetch(env -> {
					final Paged<?> paged = env.getSource();
					final Integer limit = paged.getLimit();
					if (limit.equals(0)) {
						/* avoid division by zero */
						return 0;
					}
					return paged.getOffset() / limit;
				})
				.add();
		parentBuilder.defineDataPoint(pageTypeName, "offset").type(ScalarType.INT).add();
		parentBuilder.defineDataPoint(pageTypeName, "size").type(ScalarType.INT).add();
		parentBuilder.defineDataPoint(pageTypeName, "limit").type(ScalarType.INT).add();
		if (contentScalarType != null) {
			parentBuilder.defineDataPoint(pageTypeName, "content").arrayOfType(contentScalarType).add();
		} else if (contentReferenceTypeName != null) {
			parentBuilder.defineDataPoint(pageTypeName, "content").arrayOfType(contentReferenceTypeName).add();
		} else {
			throw new IllegalArgumentException("Either a scalar type or a reference type name must be given for the contents of the page.");
		}
		return pageTypeName;
	}

	private String escapeName(final String name) {
		/* names in GraphQL may only contain letters, digits and underscore and must not start with a digit:
		 * https://spec.graphql.org/October2021/#sec-Names */

		final String nameEscaped = ESCAPE_NAME_PATTERN.matcher(name).replaceAll("_");
		if (nameEscaped.isEmpty() || CharUtils.isAsciiNumeric(nameEscaped.charAt(0))) {
			return "_" + nameEscaped;
		} else {
			return nameEscaped;
		}
	}
}
