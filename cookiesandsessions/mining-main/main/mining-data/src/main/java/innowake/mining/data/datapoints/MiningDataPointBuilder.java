/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.springframework.graphql.data.method.annotation.SchemaMapping;

import graphql.schema.DataFetcher;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.builder.MiningDataPointDefinitionWithCustomFetch;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.shared.datapoints.annotations.MiningDataPointIgnore;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.MiningDataPointFilterCallback;
import innowake.mining.shared.datapoints.definition.MiningDataPointSortCallback;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import innowake.mining.shared.datapoints.definition.usages.Usages;

/**
 * Builder for mining data points, used by {@link MiningDataPointSource}. This builder can be used to define 4 different things:
 * <ul>
 * <li> A {@link MiningDataTypeDefinition} which defines a collection of data points (think "class")
 * <li> A {@link MiningDataPointDefinition} which defines a data point inside a {@link MiningDataTypeDefinition} (think "member" or "property")
 * <li> A {@link MiningEnumDefinition} for enumeration types
 * <li> A {@link MiningDataPointDefinition} which defines a query. Queries are the entry points for data retrieval.
 * 		Each query must be implemented by a method annotated with {@code MiningQueryMapping} on a class annotated with {@code GraphQlController}.
 * </ul>
 * Data points may be defined specific to a project and the {@link DataPointRegistry} may request sources to provide data points for a specific project only.<br>
 * A {@link MiningDataPointSource} defining project specific data points must therefore call {@link #getProjectId()} to determine
 * if the current building context is restricted to a certain project.
 */
public interface MiningDataPointBuilder {

	/**
	 * Builder for {@link MiningDataTypeDefinition}.
	 */
	interface DataTypeBuilder {
		/**
		 * Defines that this type is only valid for specific projects.
		 * @param projectIds list of project ids on which this data point is going to exist
		 * @return this builder
		 */
		DataTypeBuilder onlyOnProjects(long... projectIds);
		/**
		 * Defines that this type represents the given Java class.
		 * @param klass the class
		 * @return this builder
		 */
		DataTypeBuilder representedBy(Class<?> klass);
		/**
		 * Use together with {@link #representedBy(Class)} to automatically define datapoints for all public properties of the class,
		 * unless the property is annotated with {@link MiningDataPointIgnore}.
		 * @return this builder
		 */
		DataTypeBuilder withDefaultProperties();
		/**
		 * Adds the configured type to the global data point registry.
		 * @return the {@link MiningDataPointBuilder}
		 */
		MiningDataPointBuilder add();
	}
	
	/**
	 * Builder for {@link MiningEnumDefinition}.
	 */
	interface EnumBuilder {
		/**
		 * Defines that this enum is only valid for specific projects.
		 * @param projectIds list of project ids on which this data point is going to exist
		 * @return this builder
		 */
		EnumBuilder onlyOnProjects(long... projectIds);
		/**
		 * Defines that this enum type represents the given Java enum. Automatically adds all values from the Java enum to this enum definition.
		 * @param klass the enum class
		 * @return this builder
		 */
		EnumBuilder representedBy(Class<? extends Enum<?>> klass);
		/**
		 * Manually define the values for the enum.
		 * @param values the enum values
		 * @return this builder
		 */
		EnumBuilder withValues(String... values);
		/**
		 * Adds the configured enum type to the global data point registry.
		 * @return the {@link MiningDataPointBuilder}
		 */
		MiningDataPointBuilder add();
	}
	
	/*
	 * Applies a dynamic selection to a query.
	 */
	interface DynamicFieldBuilder<T> {
		/***
		 * Adds a dynamic property to a query.
		 * @param fieldAlias Name for the property.
		 * @param args Query arguments for the data-point. 
		 * @param builder Data access builder.
		 */
		void accept(String fieldAlias, Map<String, Object> args, T builder);
	}
	
	/**
	 * Builder for {@link MiningDataPointDefinition}.
	 */
	interface DataPointBuilder {
		
		
		/**
		 * Defines that this data point is only valid for specific projects.
		 * @param projectIds list of project ids on which this data point is going to exist
		 * @return this builder
		 */
		DataPointBuilder onlyOnProjects(@Nullable long... projectIds);
		/**
		 * Defines that the type of this data point is an existing {@link MiningDataTypeDefinition}.
		 * @param type the name of the {@link MiningDataTypeDefinition}
		 * @return this builder
		 */
		DataPointBuilder type(String type);
		/**
		 * Defines that this data point has a scalar (primitive) type.
		 * @param type the scalar type
		 * @return this builder
		 */
		DataPointBuilder type(ScalarType type);
		/**
		 * Defines that the type of this data point is determined by the give Java type or class.
		 *
		 * @param type a Java type
		 * @return this builder
		 */
		DataPointBuilder type(Type type);
		/**
		 * Defines that the type of this data point is an array of existing {@link MiningDataTypeDefinition}.
		 * @param type the name of the {@link MiningDataTypeDefinition}
		 * @return this builder
		 */
		DataPointBuilder arrayOfType(String type);
		/**
		 * Defines that this data point is an array of a scalar (primitive) type.
		 * @param type the scalar type
		 * @return this builder
		 */
		DataPointBuilder arrayOfType(ScalarType type);
		/**
		 * Defines that the data point may contain a null value (this is the default).
		 *
		 * @return this builder
		 */
		DataPointBuilder nullable();
		/**
		 * Defines that the data point does not contain a null value.
		 *
		 * @return this builder
		 */
		DataPointBuilder notNull();
		/**
		 * Sets the name of the data point to be displayed on the UI.
		 *
		 * @param displayName display name of the data point
		 * @return this builder
		 */
		DataPointBuilder withDisplayName(String displayName);
		/**
		 * Sets the description of the data point to be displayed on the UI.
		 *
		 * @param description description of the data point
		 * @return this builder
		 */
		DataPointBuilder withDescription(String description);
		/**
		 * Defines a usage for this data point. Multiple usages can be added.
		 * <p>
		 * For the list of built-in usages, see {@link Usages}.
		 *
		 * @param dataPointUsage the usage for the data point
		 * @return this builder
		 * @see Usages
		 */
		DataPointBuilder withUsage(String dataPointUsage);
		/**
		 * Defines a usage attribute for this data point.
		 *
		 * @param dataPointUsage the usage of the attribute
		 * @param key the key of the attribute
		 * @param value the value of the attribute
		 * @return this builder
		 */
		DataPointBuilder withUsageAttribute(String dataPointUsage, String key, String value);
		/**
		 * Defines custom fetching logic for this data point.
		 * @param dataFetcher the data fetcher which will be invoked to produce the data for this data point
		 * @return this builder
		 */
		DataPointBuilder withCustomFetch(DataFetcher<?> dataFetcher);
		/**
		 * Adds the configured data point to the global data point registry.
		 * @return the {@link MiningDataPointBuilder}
		 */
		MiningDataPointBuilder add();
		/**
		 * Defines an optional input parameter for the query with a previously registered class type.
		 * @param name name of the parameter
		 * @param cls the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withOptionalParameter(String name, Class<?> cls);
		/**
		 * Defines an optional input parameter for the query with scalar type.
		 * @param name the name of the parameter
		 * @param type the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withOptionalParameter(String name, ScalarType type);
		/**
		 * Defines an optional input parameter for the query with named type.
		 * @param name the name of the parameter
		 * @param type the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withOptionalParameter(String name, String type);
		/**
		 * Defines an optional input parameter for the query that is an array of scalar type.
		 * @param name the name of the parameter
		 * @param type the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withOptionalParameterArray(String name, ScalarType type);
		/**
		 * Defines an optional input parameter for the query that is an array of named type.
		 * @param name the name of the parameter
		 * @param type the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withOptionalParameterArray(String name, String type);
		/**
		 * Defines a required input parameter for the query with a previously registered class type.
		 * @param name Name of the parameter
		 * @param cls Type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withRequiredParameter(String name, Class<?> cls);
		/**
		 * Defines a required input parameter for the query with scalar type.
		 * @param name the name of the parameter
		 * @param type the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withRequiredParameter(String name, ScalarType type);
		/**
		 * Defines a required input parameter for the query with named type.
		 * @param name the name of the parameter
		 * @param type the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withRequiredParameter(String name, String type);
		/**
		 * Defines a required input parameter for the query that is an array of scalar type.
		 * @param name the name of the parameter
		 * @param type the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withRequiredParameterArray(String name, ScalarType type);
		/**
		 * Defines a required input parameter for the query that is an array of named type.
		 * @param name the name of the parameter
		 * @param type the type of the parameter
		 * @return this builder
		 */
		DataPointBuilder withRequiredParameterArray(String name, String type);
		/**
		 * Defines filtering operations for this data point.
		 * @param queryName the name of the GraphQL query for which to define filter operations on this data point
		 * @param builderConsumer a consumer that will receive {@link FilterBuilder} to configure the filtering operations
		 * @return this builder
		 */
		DataPointBuilder withFiltering(String queryName, Consumer<FilterBuilder> builderConsumer);
		/**
		 * Defines filtering operations for this data point, overriding the type of the filter argument.
		 * @param queryName the name of the GraphQL query for which to define filter operations on this data point
		 * @param filterArgumentType defines the scalar type to use for the filter argument
		 * @param builderConsumer a consumer that will receive {@link FilterBuilder} to configure the filtering operations
		 * @return this builder
		 */
		DataPointBuilder withFiltering(String queryName, ScalarType filterArgumentType, Consumer<FilterBuilder> builderConsumer);
		/**
		 * Defines filtering operations for this data point, overriding the type of the filter argument.
		 * @param queryName the name of the GraphQL query for which to define filter operations on this data point
		 * @param filterArgumentType defines the reference type name to use for the filter argument
		 * @param builderConsumer a consumer that will receive {@link FilterBuilder} to configure the filtering operations
		 * @return this builder
		 */
		DataPointBuilder withFiltering(String queryName, String filterArgumentType, Consumer<FilterBuilder> builderConsumer);
		/**
		 * Defines a sorting operation for this data point.
		 * @param queryName the name of the GraphQL query for which to define a sorting operation on this data point
		 * @param cb a callback that configures sorting for this data point
		 * @return this builder
		 * @param <B> the type of the "order builder" that the callback uses
		 */
		<B> DataPointBuilder withSorting(final String queryName, MiningDataPointSortCallback<B> cb);
		/**
		 * Defines a method for dynamically acquiring a property during query execution.
		 * @param <B> Type of the data access builder.
		 * @param referenceType Root data point this definition applies to.
		 * @param b Access extending method.
		 * @return this builder
		 */
		<B> DataPointBuilder withDynamicFieldBuilder(String referenceType, DynamicFieldBuilder<B> b);
	}

	/**
	 * Builder for specifying the available filter operations and corresponding filter callbacks for a data point.
	 */
	interface FilterBuilder {
		/**
		 * Specifies the callback for the "eq" (equals) filter
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder eq(MiningDataPointFilterCallback<B, T> cb);
		/**
		 * Specifies the callback for the "notEq" (not equals) filter
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder notEq(MiningDataPointFilterCallback<B, T> cb);
		/**
		 * Specifies the callback for the "in" (in list of values) filter
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T extends Collection<?>> FilterBuilder in(MiningDataPointFilterCallback<B, T>  cb);
		/**
		 * Specifies the callback for the "notIn" (not in list of value) filter
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T extends Collection<?>> FilterBuilder notIn(MiningDataPointFilterCallback<B, T>  cb);
		/**
		 * Specifies the callback for the "is" filter with value "true" (for boolean data points)
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder isTrue(MiningDataPointFilterCallback<B, T>  cb);
		/**
		 * Specifies the callback for the "is" filter with value "false" (for boolean data points)
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder isFalse(MiningDataPointFilterCallback<B, T>  cb);
		/**
		 * Specifies the callback for the "is" filter with value "null" (filtering on the absence of a datapoint)
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder isAbsent(MiningDataPointFilterCallback<B, T>  cb);

		/**
		 * Specifies the callback for the "gte" (greater than or equals) filter
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder gte(MiningDataPointFilterCallback<B, T>  cb);

		/**
		 * Specifies the callback for the "gt" (greater than) filter
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder gt(MiningDataPointFilterCallback<B, T>  cb);

		/**
		 * Specifies the callback for the "lte" (less than or equals) filter
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder lte(MiningDataPointFilterCallback<B, T>  cb);

		/**
		 * Specifies the callback for the "lt" (less than) filter
		 * @param cb the callback used to apply the filter
		 * @return this builder
		 * @param <B> the type of the inquiry builder used by the callback
		 * @param <T> the type of the filter argument - same as the type of the data point
		 */
		<B, T> FilterBuilder lt(MiningDataPointFilterCallback<B, T>  cb);
	}
	
	/**
	 * /**
	 * Builder for {@link MiningDataPointDefinition} that is created as an alias for an existing data point definition.
	 * <p>
	 * An alias serves two main purposes:
	 * <ul>
	 * <li> for parameterized data points, it allows to provide suggested parameter combinations, along with their own set of display name, description
	 * and usage definitions
	 * <li> it allows to provide a custom display name, description and usage definitions for a sub-selection
	 * </ul>
	 * Example for the first use-case: the alias allows us to define a custom display name and description for a certain parameter assignment
	 * of a parameterized data point. The alias is created like so:
	 * <pre>
	 * builder.defineAlias("Module", "calledModules")
	 *	.forDataPoint("dependencyCount")
	 *	.withParameter("type", Relationship.CALLS)
	 *	.withParameter("direction", EdgeDirection.OUT)
	 *	.withDisplayName("Called Modules")
	 *	.withDescription("Number of Modules called by this Module")
	 *	.add();
	 * </pre>
	 * In this example "dependencyCount" is a parameterized data point. Depending on the parameters that are used, the data point returns a different kind
	 * of dependency information. The alias definition created here suggests that the datapoint 
	 * {@code dependencyCount(type: CALLS, direction: OUT)} should be presented to the user with the name
	 * "Called Modules" and the description "Number of Modules called by this Module".
	 * <p>
	 * Example for the second use-case: the alias allows us to define a custom display name and description for a data point that is reachable via a 
	 * sub-selection. The alias is created like so:
	 * <pre>
	 * builder.defineAlias("Module", "namesOfCalledModules")
	 *	.forDataPoint("outCalls")
	 *	.withSubSelection("in.name")
	 *	.withDisplayName("Names of Called Modules")
	 *	.withDescription("The list of names of Modules that are called by this Module")
	 *	.add();
	 *</pre>
	 * In this example "outCalls" is a data point that requires a sub-selection. The alias that is defined indicates that the selection
	 * {@code outCalls { in { name } } } should be presented to the user with the name "Names of Called Modules"
	 * and the description "The list of names of Modules that are called by this Module".
	 */
	interface AliasBuilder {
		/**
		 * Defines the name of the data point for which this data point will be an alias.
		 * <p>
		 * Parameters defined via {@linkplain #withParameter(String, boolean) withParameter()} are passed to the target datapoint.
		 * Currently the target data point must be on the same type. 
		 * Example:
		 * <pre>
		 * builder.defineAlias("Module", "fooAlias")
		 *	.forDataPoint("foo")
		 *	.withParameter("bar", true)
		 *	.add();
		 * </pre>
		 * Now {@code fooBarAlias} will be an alias for {@code foo(bar: true) }
		 *
		 * @param name the name of the actual data point
		 * @return this builder
		 */
		AliasBuilder forDataPoint(String name);

		/**
		 * Defines that this alias is present only for specific projects.
		 * Setting this is <i>required</i> when the target of the alias (i.e. the aliased data point) is also project-specific.
		 * @param projectIds list of project ids on which this alias is going to exist
		 * @return this builder
		 */
		AliasBuilder onlyOnProjects(@Nullable long... projectIds);

		/**
		 * Appends an additional sub-selection after the datapoint set via {@link #forDataPoint(String)}. Use dot notation for nested selections.
		 * <p>
		 * Example:
		 * <pre>
		 * builder.defineAlias("Module", "fooBarAlias")
		 *	.forDataPoint("foo")
		 *	.withParameter("bar", true)
		 *	.withSubSelection("baz.qux")
		 *	.add();
		 * </pre>
		 * Now {@code fooBarAlias} will be an alias for {@code foo(bar: true) { baz { qux } } }.
		 *
		 * @param selection the sub-selection to append to the target datapoint (in dot notation)
		 * @return this builder
		 */
		AliasBuilder withSubSelection(String selection);
		/**
		 * Defines that a path from inside a JSON structure should be selected when this data point is selected. Only valid
		 * when this data points aliases a data point of type {@link ScalarType#JSON}.
		 * Use dot notation for nested selections.
		 * <p>
		 * Example:
		 * <pre>
		 * builder.defineAlias("Module", "fooBarAlias")
		 *	.forDataPoint("foo") // "foo" is of type ScalarType.JSON
		 *	.withJsonPath("baz.qux") // if value of "foo" is { "baz": { "qux": 42 } } then 42 is returned when you select "fooBarAlias"
		 *	.add();
		 * </pre>
		 *
		 * @param jsonPath the jsonPath to select from the aliased datapoint
		 * @return this builder
		 */
		AliasBuilder withJsonPath(String jsonPath);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, Integer value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, Long value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, long value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, Float value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, Double value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, double value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, String value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, Boolean value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, boolean value);
		/**
		 * Defines that a parameter is passed to the alias data point.
		 *
		 * @param name the name of the parameter
		 * @param value the parameter value
		 * @return this builder
		 */
		AliasBuilder withParameter(String name, Enum<?> value);
		/**
		 * Sets the name of the data point to be displayed on the UI.
		 *
		 * @param displayName display name of the data point
		 * @return this builder
		 */
		AliasBuilder withDisplayName(String displayName);
		/**
		 * Sets the description of the data point to be displayed on the UI.
		 *
		 * @param description description of the data point
		 * @return this builder
		 */
		AliasBuilder withDescription(String description);
		/**
		 * Defines a usage for this data point. Multiple usages can be added.
		 * <p>
		 * For the list of built-in usages, see {@link Usages}.
		 *
		 * @param dataPointUsage the usage for the data point
		 * @return this builder
		 * @see Usages
		 */
		AliasBuilder withUsage(String dataPointUsage);
		/**
		 * Defines a usage attribute for this data point.
		 *
		 * @param dataPointUsage the usage of the attribute
		 * @param key the key of the attribute
		 * @param value the value of the attribute
		 * @return this builder
		 */
		AliasBuilder withUsageAttribute(String dataPointUsage, String key, String value);
		/**
		 * Adds the configured alias to the global data point registry.
		 * @return the {@link MiningDataPointBuilder}
		 */
		MiningDataPointBuilder add();
	}
	
	/**
	 * Obtains a builder to define a {@link MiningDataTypeDefinition}.
	 * @return a {@link DataTypeBuilder}
	 */
	DataTypeBuilder defineType();
	/**
	 * Obtains a builder to define a {@link MiningDataTypeDefinition} with a given name.
	 * @param name name for the new type
	 * @return a {@link DataTypeBuilder}
	 */
	DataTypeBuilder defineType(String name);
	/**
	 * Obtains a builder to define a {@link MiningEnumDefinition} with a given name.
	 * @param name name for the new enum
	 * @return a {@link EnumBuilder}
	 */
	EnumBuilder defineEnum(String name);
	/**
	 * Obtains a builder to define a {@link MiningDataPointDefinition} with a given name on an existing type.
	 * @param on name of the type on which to define the data point
	 * @param name name for the new data point
	 * @return a {@link DataPointBuilder}
	 */
	DataPointBuilder defineDataPoint(String on, String name);
	/**
	 * Obtains a builder to define a {@link MiningDataPointDefinition} for a query.
	 * @param name name for the new query
	 * @return a {@link DataPointBuilder} for query definition
	 */
	DataPointBuilder defineQuery(String name);
	
	/**
	 * Obtains a builder to define an alias for an existing datapoint.
	 *
	 * @param on name of the type on which to define the alias
	 * @param name the name of the alias
	 * @return a {@link AliasBuilder} for defining the alias
	 */
	AliasBuilder defineAlias(String on, String name);

	/**
	 * Defines a data point and type for requesting aggregations on the fields defined by the provided {@code aggregationFields} enum.
	 * The type of the generated data point mimics the structure of {@link innowake.mining.shared.model.aggregations.AggregationRequest}.
	 * @param on the enclosing type on which the new data point is defined
	 * @param fieldName the name for the new data point
	 * @param aggregationFields an enum class enumerating the aggregation fields
	 * @return this builder
	 * @param <E> type of the enum class enumerating the aggregation fields
	 */
	<E extends Enum<E>> MiningDataPointBuilder defineAggregations(String on, String fieldName, Class<E> aggregationFields);
	
	/**
	 * Automatically discover data points based on {@link SchemaMapping} annotations present
	 * on the class of the given object.
	 *
	 * @param obj the object to inspect
	 * @return this builder
	 */
	MiningDataPointBuilder defineDataPointsFromSchemaMappingAnnotations(final Object obj);

	/**
	 * Define datapoints for the given method.
	 *
	 * @param klass the class to inspect
	 * @param method the method to inspect
	 * @return this builder
	 */
	MiningDataPointBuilder defineDataPointForGivenMethod(final Class<?> klass, final Method method);
	
	/**
	 * Automatically discover data points based on {@link SchemaMapping} annotations present
	 * on the given class.
	 *
	 * @param klass the class to inspect
	 * @return this builder
	 */
	MiningDataPointBuilder defineDataPointsFromSchemaMappingAnnotations(final Class<?> klass);

	/**
	 * Extends or modifies an existing data point definition. Currently, this works only for data points that were defined through this
	 * same builder instance. It is therefore useful only for adding additional attributes (or sorting and filtering callbacks) to data points
	 * that were defined declaratively.
	 *
	 * @param on name of the type on which the data point exists
	 * @param name name of the existing data point
	 * @return a builder for modifying or extending the data point - modification will be applied when its {@linkplain DataPointBuilder#add() add()} method
	 * 		   is called
	 */
	DataPointBuilder extend(String on, String name);
	
	/**
	 * Determine the ID of the project to which this building context is restricted. 
	 *
	 * @return An optional projectId. If present, only data points for the specified project may be defined using this builder. 
	 */
	Optional<Long> getProjectId();
	
	List<MiningDataTypeDefinition> getTypeDefinitions();
	List<MiningEnumDefinition> getEnumDefinitions();
	List<MiningDataPointDefinitionWithCustomFetch> getDataPointDefinitions();
	List<MiningDataPointDefinition> getQueryDefinitions();
}
