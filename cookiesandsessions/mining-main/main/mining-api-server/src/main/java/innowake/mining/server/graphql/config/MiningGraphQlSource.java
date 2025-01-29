/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.config;

import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.graphql.GraphQlAutoConfiguration;
import org.springframework.boot.autoconfigure.graphql.GraphQlProperties;
import org.springframework.boot.autoconfigure.graphql.GraphQlSourceBuilderCustomizer;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.event.EventListener;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.data.domain.Sort;
import org.springframework.data.util.Optionals;
import org.springframework.graphql.execution.DataFetcherExceptionResolver;
import org.springframework.graphql.execution.GraphQlSource;
import org.springframework.graphql.execution.RuntimeWiringConfigurer;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.servlet.HandlerMapping;

import graphql.GraphQL;
import graphql.execution.instrumentation.Instrumentation;
import graphql.scalars.ExtendedScalars;
import graphql.schema.GraphQLSchema;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.SortObjectService;
import innowake.mining.data.datapoints.builder.MiningDataPointDefinitionWithCustomFetch;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.data.event.DataPointRegistryUpdatedEvent;
import innowake.mining.data.graphql.ProjectAwareGraphQlSource;
import innowake.mining.server.graphql.GraphQLEntityId;
import innowake.mining.server.graphql.GraphQLInstantDateTimeScalar;
import innowake.mining.server.graphql.GraphQLTimestamp;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;

/**
 * GraphQlSource capable of providing a separate {@link GraphQL} and {@link GraphQLSchema} instance per mining project.
 * <p>
 * This class implicitly disables {@link GraphQlAutoConfiguration} due to its {@code @ConditionalOnMissingBean} annotation.
 */
@Service
@EnableConfigurationProperties(GraphQlProperties.class)
public class MiningGraphQlSource implements ProjectAwareGraphQlSource {

	private static final Logger LOG = LoggerFactory.getLogger(MiningGraphQlSource.class);
	
	private static final String NO_PROJECT_ID_ON_REQUEST = "unspecified (no 'projectId' query parameter present on request)";
	
	private static final String INDENT = "\t";
	
	@Autowired(required = false)
	private final List<DataFetcherExceptionResolver> exceptionResolvers = Collections.emptyList();
	
	@Autowired(required = false)
	private final List<Instrumentation> instrumentations = Collections.emptyList();
	
	@Autowired(required = false)
	private final List<GraphQlSourceBuilderCustomizer> sourceCustomizers = Collections.emptyList();
	
	@Autowired(required = false)
	private final List<RuntimeWiringConfigurer> wiringConfigurers = Collections.emptyList();
	
	@Autowired
	private HttpServletRequest request;
	
	private final AtomicReference<GraphQlSource> defaultSource = new AtomicReference<>();
	
	private final Map<Long, GraphQlSource> projectSources = new ConcurrentHashMap<>();

	private final ThreadLocal<Long> threadLocalProjectId = new ThreadLocal<>();
	
	@Autowired
	private DataPointRegistry dataPointRegistry;

	@Autowired
	private FilterObjectService filterObjectService;

	@Autowired
	private SortObjectService sortObjectService;

	@Autowired
	private ProjectService projectService;
	
	private GraphQlSource getSource(final Optional<Long> projectId) {
		if (projectId.isPresent()) {
			return projectSources.computeIfAbsent(projectId.get(), k -> buildSource(projectId));
		} else {
			GraphQlSource src = defaultSource.get();
			if (src == null) {
				src = buildSource(Optional.empty());
				defaultSource.set(src);
			}
			return src;
		}
	}
	
	@EventListener
	public void invalidateSource(final DataPointRegistryUpdatedEvent event) {
		Optionals.ifPresentOrElse(event.getProjectId().map(projectService::getNid),
				projectSources::remove,
				() -> {
					projectSources.clear();
					defaultSource.set(null);
				});
	}

	@Override
	public GraphQL graphQl() {
		final Optional<Long> projectId = getProjectIdFromRequestOrThreadLocal();
		LOG.info(() -> "Handling GraphQl request for project: " + projectId.map(Object::toString).orElse(NO_PROJECT_ID_ON_REQUEST));
		return getSource(projectId).graphQl();
	}

	@Override
	public GraphQLSchema schema() {
		final Optional<Long> projectId = getProjectIdFromRequestOrThreadLocal();
		LOG.info(() -> "Handling GraphQl schema request for project: " + projectId.map(Object::toString).orElse(NO_PROJECT_ID_ON_REQUEST));
		return getSource(projectId).schema();
	}
	
	private Optional<Long> getProjectIdFromRequestOrThreadLocal() {
		LOG.debug(() -> "request: " + (RequestContextHolder.getRequestAttributes() != null 
				? request.getRequestURI() + "?" + request.getQueryString() : "(no context)"));
		LOG.debug(() -> "threadLocalProjectId=" + threadLocalProjectId.get());
		if (threadLocalProjectId.get() != null) {
			/* explicit value set in thread-local */
			return Optional.ofNullable(threadLocalProjectId.get());
		}
		if (RequestContextHolder.getRequestAttributes() == null) {
			/* no request is active */
			return Optional.empty();
		}
		try {
			if (request.getParameter("projectId") != null) {
				return Optional.of(projectService.getNid(EntityId.of(request.getParameter("projectId"))));
			}
		} catch (final NumberFormatException e) {
			throw new IllegalArgumentException("Could not parse projectId query parameter!", e);
		}
		final Object pathVariables = request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
		if (pathVariables instanceof Map) {
			try {
				final String projectId = ((Map<?, ?>) pathVariables).get("projectId").toString();
				if (projectId != null) {
					return Optional.of(projectService.getNid(EntityId.of(projectId)));
				}
			} catch (final NumberFormatException e) {
				throw new IllegalArgumentException("Could not parse projectId path parameter!", e);
			}
		}
		return Optional.empty();
	}
	
	private GraphQlSource buildSource(final Optional<Long> projectId) {
		final GraphQlSource.SchemaResourceBuilder builder = GraphQlSource.schemaResourceBuilder()
				.exceptionResolvers(exceptionResolvers)
				.instrumentation(instrumentations);
		/* main schema */
		configureSchema(projectId, builder);
		builder.configureRuntimeWiring(runtimeWiringBuilder -> configureRuntimeWiring(projectId, runtimeWiringBuilder));
		/* possibly additional schemas */
		wiringConfigurers.forEach(builder::configureRuntimeWiring);
		sourceCustomizers.forEach(customizer -> customizer.customize(builder));
		return builder.build();
	}

	private void configureSchema(final Optional<Long> projectId, final GraphQlSource.SchemaResourceBuilder builder) {
		final StringBuilder sb = new StringBuilder();
		sb.append("scalar Long\n");
		sb.append("scalar DateTime\n");
		sb.append("scalar Timestamp\n");
		sb.append("scalar UUID\n");
		sb.append("scalar JSON\n");
		sb.append("scalar EntityId\n");
		sb.append("enum SortDirection { " + Sort.Direction.ASC.name() + " " + Sort.Direction.DESC.name() + " "
			+ SortDirection.ASCENDING.name() + " " + SortDirection.DESCENDING.name() + " }\n");

		/* declare input types for sort and filter arguments */
		final Map<String, Map<String, String>> inputTypes = new HashMap<>();
		inputTypes.putAll(sortObjectService.buildSortObjectsSchema(projectId));
		inputTypes.putAll(filterObjectService.buildFilterObjectsSchema(projectId));
		for (final Map.Entry<String, Map<String, String>> inputType: inputTypes.entrySet()) {
			sb.append("input " + inputType.getKey() + " {\n");
			for (final Map.Entry<String, String> entry : inputType.getValue().entrySet()) {
				sb.append(INDENT + entry.getKey() + ": " + entry.getValue() + "\n");
			}
			sb.append("}\n\n");
		}
		
		/* create GraphQl schema for MiningDataTypeDefinitions containing MiningDataPointDefinitions */
		final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> allDataPoints = dataPointRegistry.getDataPointDefinitions(projectId);
		final Map<String, MiningEnumDefinition> allEnums = dataPointRegistry.getEnumDefinitions(projectId);
		final Map<String, MiningDataTypeDefinition> typeDefinitions = dataPointRegistry.getTypeDefinitions(projectId);
		final Map<String, MiningDataTypeDefinition> allTypes = typeDefinitions.entrySet().stream()
			.filter(def -> allDataPoints.containsKey(def.getKey()))
			.filter(def -> allDataPoints.get(def.getKey()).values().stream().anyMatch(dataPoint -> {
				/* Filter MiningDataTypeDefinitions that reference a MiningDataTypeDefinition for which no MiningDataPointDefinition is available */
				if (dataPoint.getReferenceTypeName() != null && typeDefinitions.containsKey(dataPoint.getReferenceTypeName())) {
					return allDataPoints.containsKey(dataPoint.getReferenceTypeName());
				}

				return true;
			}))
			.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
		for (final Map.Entry<String, MiningDataTypeDefinition> typeDef: allTypes.entrySet()) {
			final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPointDefs = allDataPoints.get(typeDef.getKey());
			if (dataPointDefs != null) {
				sb.append("type " + typeDef.getValue().getName() + " {\n");
				for (final MiningDataPointDefinitionWithCustomFetch dataPointDef : dataPointDefs.values()) {
					if (dataPointDef.isAlias()) {
						/* aliases are not added to the GraphQl schema */
						continue;
					}
					if (dataPointDef.getReferenceTypeName() == null 
							|| allEnums.containsKey(dataPointDef.getReferenceTypeName())
							|| allTypes.containsKey(dataPointDef.getReferenceTypeName())) {
						sb.append(INDENT + dataPointToGraphQlSchema(dataPointDef));
					}
				}
				sb.append("}\n\n");
			}
		}
		
		/* create GraphQl schema for MiningEnumDefinitions */
		for (final MiningEnumDefinition enumDef : allEnums.values()) {
			sb.append("enum " + enumDef.getName() + " {\n");
			for (final String value : enumDef.getValues()) {
				sb.append(INDENT + value + "\n");
			}
			sb.append("}\n\n");
		}
		
		/* create GraphQl schema for query definitions */
		sb.append("type Query {\n");
		if (dataPointRegistry.getQueryDefinitions().isEmpty()) {
			/* server startup would fail otherwise if no queries are defined */
			sb.append(INDENT + "noQueriesHaveBeenDefined: String\n");
		}
		for (final MiningDataPointDefinition queryDef : dataPointRegistry.getQueryDefinitions().values()) {
			sb.append(INDENT + dataPointToGraphQlSchema(queryDef));
		}
		sb.append("}\n");

		LOG.debug(() -> "Compiled GraphQL Schema for Project " + projectId + ": \n" + sb.toString());
		builder.schemaResources(new ByteArrayResource(sb.toString().getBytes(StandardCharsets.UTF_8)));
	}
	
	private void configureRuntimeWiring(final Optional<Long> projectId, final graphql.schema.idl.RuntimeWiring.Builder builder) {
		/* register custom scalar types */
		builder.scalar(GraphQLInstantDateTimeScalar.INSTANCE);
		builder.scalar(ExtendedScalars.GraphQLLong);
		builder.scalar(ExtendedScalars.Json);
		builder.scalar(ExtendedScalars.UUID);
		builder.scalar(GraphQLTimestamp.INSTANCE);
		builder.scalar(GraphQLEntityId.INSTANCE);
		
		/* register custom fetch logic from MiningDataPointDefinitionWithCustomFetch */
		for (final Map.Entry<String,Map<String,MiningDataPointDefinitionWithCustomFetch>> dpEntry :
				dataPointRegistry.getDataPointDefinitions(projectId).entrySet()) {
			for (final MiningDataPointDefinitionWithCustomFetch dataPoint : dpEntry.getValue().values()) {
				if (dataPoint.getCustomFetch() != null) {
					builder.type(dpEntry.getKey(), typeBuilder -> typeBuilder.dataFetcher(dataPoint.getName(), dataPoint.getCustomFetch()));
				}
			}
		}
	}

	/**
	 * Converts a {@link MiningDataPointDefinition} into a string conforming to GraphQL schema syntax.
	 * <p>
	 * Examples:
	 * <pre>hello: String</pre>
	 * <pre>modules(projectId: Int!, filter: String = ""): [Module]!</pre>
	 * 
	 * @param dataPointDef the data point definition to convert
	 * @return GraphQL property schema string
	 */
	private String dataPointToGraphQlSchema(final MiningDataPointDefinition dataPointDef) {
		final StringBuilder sb = new StringBuilder();
		
		final String comment = Stream.of(dataPointDef.getDisplayName(), dataPointDef.getDescription())
				.filter(s -> ! s.isEmpty())
				.collect(Collectors.joining(": "));
		if ( ! comment.isEmpty()) {
			sb.append("\"");
			sb.append(comment);
			sb.append("\"");
			sb.append("\n" + INDENT);
		}
		
		sb.append(dataPointDef.getName());
		if ( ! dataPointDef.getParameters().isEmpty()) {
			sb.append("(");
			final Iterator<MiningDataPointDefinition> iter = dataPointDef.getParameters().iterator();
			while (iter.hasNext()) {
				final MiningDataPointDefinition parameter = iter.next();
				sb.append(parameter.getName());
				sb.append(": ");
				if (parameter.isArray()) {
					sb.append("[");
				}
				sb.append(parameter.getScalarType() != null ? parameter.getScalarType() : parameter.getReferenceTypeName());
				if (parameter.isArray()) {
					sb.append("]");
				}
				if ( ! parameter.isNullable()) {
					sb.append("!");
				} else {
					/* GraphQl demands a default value for optional parameters */
					if (parameter.isArray()) {
						sb.append(" = []");
					} else {
						sb.append(" = null");
					}
				}
				if (iter.hasNext()) {
					sb.append(",");
				}
			}
			sb.append(")");
		}
		sb.append(":");
		if (dataPointDef.isArray()) {
			sb.append("[");
		}
		sb.append(dataPointDef.getScalarType() != null ? dataPointDef.getScalarType() : dataPointDef.getReferenceTypeName());
		if (dataPointDef.isArray()) {
			sb.append("]");
		}
		if ( ! dataPointDef.isNullable()) {
			sb.append("!");
		}
		sb.append("\n");
		
		return sb.toString();
	}

	@Override
	public void setThreadLocalProjectId(final Long projectId) {
		threadLocalProjectId.set(projectId);
	}

	@Override
	public void removeThreadLocalProjectId() {
		threadLocalProjectId.remove();
	}

}
