/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.dependency;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.extensions.export.callchain.model.CallChainGraph.CallChainEdge;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Utility class containing method that exports {@linkplain CallChainGraph CallChainGraphs} as a {@linkplain DependencyGraph} in JSON-format.
 */
public class DependencyGraphExportUtil {

	private static final SimpleModule entityIdSerializingModule = new SimpleModule();

	static {
		entityIdSerializingModule.setSerializerModifier(new BeanSerializerModifier() {
			@Override
			public JsonSerializer<?> modifySerializer(final SerializationConfig config, final BeanDescription beanDesc, final JsonSerializer<?> defaultSerializer) {
				if (beanDesc.getBeanClass() == EntityId.class) {
					return new JsonSerializer<EntityId>() {
						@Override
						public void serialize(final EntityId eid, final JsonGenerator gen, final SerializerProvider serializers) throws IOException {
							if (eid.hasUid()) {
								var uid = eid.getUid();
								serializers.findValueSerializer(UUID.class).serialize(uid, gen, serializers);
							} else if (eid.hasNid()) {
								var nid = eid.getNid();
								serializers.findValueSerializer(Long.class).serialize(nid, gen, serializers);
							} else {
								serializers.getDefaultNullValueSerializer().serialize(null, gen, serializers);
							}
						}
					};
				}
				return super.modifySerializer(config, beanDesc, defaultSerializer);
			}
		});
		//		probably not needed, as there is no import util
		//		entityIdSerializingModule.setDeserializerModifier(new BeanDeserializerModifier() {
		//			@Override
		//			public JsonDeserializer<?> modifyDeserializer(final DeserializationConfig config, final BeanDescription beanDesc, final JsonDeserializer<?> deserializer) {
		//				if (beanDesc.getBeanClass() == EntityId.class) {
		//					return new JsonDeserializer<EntityId>() {
		//						@Override
		//						public EntityId deserialize(final JsonParser p, final DeserializationContext ctxt) throws IOException, JacksonException {
		//							final String val = p.getValueAsString();
		//							return EntityId.of(val);
		//						}
		//					};
		//				}
		//				return super.modifyDeserializer(config, beanDesc, deserializer);
		//			}
		//		});
	}

	private DependencyGraphExportUtil() {
		/* Hide implicit constructor */
	}

	/**
	 * Builds a {@linkplain DependencyGraph} based on {@linkplain CallChainGraph CallChainGraphs} and writes it to an {@linkplain OutputStream} in JSON-format.
	 *
	 * @param callChainGraphs list of {@linkplain CallChainGraph CallChainGraphs} that will be merged into one {@linkplain DependencyGraph}
	 * @param out {@linkplain OutputStream} the result will be written to
	 * @param projectId ID of project that this call-chains are derived from
	 * @param moduleService {@linkplain ModuleService} used to access the module data
	 * @throws IOException on writing to out
	 */
	public static void exportDependencyGraph(final Optional<List<CallChainGraph>> callChainGraphs, final OutputStream out, final EntityId projectId,
			final ModuleService moduleService) throws IOException {
		if (callChainGraphs.isPresent()) {
			final List<CallChainGraph> callChainGraphList = callChainGraphs.get();
			final List<DependencyGraph> dependencyGraphs = new ArrayList<>();
			for (final CallChainGraph graph : callChainGraphList) {
				if ( ! graph.getEdgeMap().isEmpty()) {
					dependencyGraphs.add(callChainToDependencyGraph(projectId, graph, moduleService));
				}
			}
			final DependencyGraph dep = new DependencyGraph(dependencyGraphs);

			final var objectMapper = new ObjectMapper()
					.registerModule(entityIdSerializingModule)
					.registerModule(new Jdk8Module());
			objectMapper.writeValue(out, dep);
		}
	}

	/**
	 * Creates a new {@link DependencyGraph} based on a {@link CallChainGraph}.
	 *
	 * @param projectId ID of project that this call-chain is derived from
	 * @param callChain {@link CallChainGraph} the DependencyGraph will be based on
	 * @param moduleService {@link ModuleService} used to find the modules
	 * @return {@link DependencyGraph} object representing this call chain
	 */
	private static DependencyGraph callChainToDependencyGraph(final EntityId projectId, final CallChainGraph callChain, final ModuleService moduleService) {
		if (! projectId.hasUid() || ! projectId.hasNid()) {
			throw new IllegalArgumentException("Project ID needs to contain UID and NID when creating a Pojo, but was: " + projectId);
		}
		final List<ModuleLightweightPojo> modules = new ArrayList<>();
		final List<ModuleRelationshipPojo> references = new ArrayList<>();
		final Set<Long> ids = new HashSet<>();
		final Set<UUID> moduleIds = new HashSet<>();
		for (final Entry<ModuleLightweightPojo, CallChainEdge> entry : callChain.getEdgeMap().entries()) {
			final ModuleLightweightPojo module = entry.getKey();
			if (ids.add(module.getId())) { /* if module does not exist then do not add to the list */
				modules.add(module);
			}

			final CallChainEdge edge = entry.getValue();
			final ModuleLightweightPojo targetModule = edge.getTarget();
			if (ids.add(targetModule.getId())) { /* if module does not exist then do not add to the list */
				modules.add(targetModule);
			}

			moduleIds.add(module.getUid());
			if (callChain.getDirection() == CallChainDirection.IN) {
				references.add(new GraphModuleRelationshipPojo(
						transformToValidId(edge.getId()),
						targetModule.getUid(),
						targetModule.getName(),
						null,
						module.getUid(),
						module.getName(),
						null,
						edge.getRelationship(),
						edge.getRelationshipAttributes(),
						null, null, Collections.emptyList()));
			} else {
				references.add(new GraphModuleRelationshipPojo(
						transformToValidId(edge.getId()),
						module.getUid(),
						module.getName(),
						null,
						targetModule.getUid(),
						targetModule.getName(),
						null,
						edge.getRelationship(),
						edge.getRelationshipAttributes(),
						null, null, Collections.emptyList()));
			}
		}

		/* set the modules with missing dependencies */
		final List<Long> modulesWithMissingDependencies = moduleService
				.findModuleIds(x -> x.byUids(moduleIds).withDestinationRelationshipsTo(y -> y.withIdentified(false))).stream().map(EntityId::getNid).toList();

		/* set the error count for each module */
		final Set<EntityId> moduleIdsEntity = moduleIds.stream()
				.map(EntityId::of)
				.collect(Collectors.toSet());
		final Map<UUID, Long> moduleErrorCount = moduleService.countErrorMarkersByModule(x -> x.ofModules(moduleIdsEntity));

		return new DependencyGraph(buildModulePojoFromLightweight(modules, projectId, moduleErrorCount), references,
				Collections.singleton(callChain.getRoot().getId()), modulesWithMissingDependencies);
	}

	/**
	 * Removes the artificial reference-IDs that are used for CSV and GraphML exports.
	 * F.e. "230_1" gets transformed to 230
	 *
	 * @param id id that needs to be fixed
	 * @return the id without the artificial reference-ID as a {@linkplain Long}
	 */
	private static UUID transformToValidId(final String id) {
		final int index = id.indexOf('_');
		return UUID.fromString(index == -1 ? id : id.substring(0, index));
	}

	/**
	 * Builds a list of {@linkplain ModulePojo ModulePojos} from a list of {@linkplain ModuleLightweightPojo ModuleLightweightPojos}.
	 *
	 * @param modules list of {@linkplain ModuleLightweightPojo ModuleLightweightPojos} that will be transformed
	 * @param projectId ID of project that this call-chains are derived from
	 * @param moduleErrorCount map containing the error count for each module
	 * @return list of {@linkplain ModulePojo ModulePojos}
	 */
	private static List<ModulePojo> buildModulePojoFromLightweight(final List<ModuleLightweightPojo> modules, final EntityId projectId,
			final Map<UUID, Long> moduleErrorCount) {
		return modules.stream().map(moduleLightweight -> new ModulePojo(
				moduleLightweight.getUid(),
				moduleLightweight.getId(),
				CustomPropertiesMap.empty(),
				projectId, null, null,
				moduleLightweight.getName(),
				"path",
				moduleLightweight.getTechnology(),
				moduleLightweight.getType(),
				Storage.UNDEFINED,
				Origin.CUSTOM,
				Creator.DISCOVERY,
				Identification.IDENTIFIED,
				null,
				"description",
				null,
				null,
				"link Hash",
				null,
				null,
				false,
				null,
				null,
				null,
				null,
				moduleErrorCount.getOrDefault(moduleLightweight.getUid(), 0L).intValue(),
				0,
				0,
				false,
				null, null, null,
				null,
				null)).toList();
	}

	public static class GraphModuleRelationshipPojo extends ModuleRelationshipPojo {

		private final String srcName;
		private final String dstName;

		public GraphModuleRelationshipPojo(
				final UUID id,
				final UUID srcModule,
				final String srcName,
				@Nullable final ModuleLocation srcLocation,
				final UUID dstModule,
				final String dstName,
				@Nullable final ModuleLocation dstLocation,
				final RelationshipType type,
				@Nullable final Map<String, Object> properties,
				@Nullable final Binding dependencyBinding,
				@Nullable final String dependencyAttributes,
				final List<UUID> validIfReachedFrom) {
			super(id, srcModule, srcLocation, dstModule, dstLocation, type, null,
					properties, dependencyBinding, dependencyAttributes, validIfReachedFrom, null, null, null);

			this.srcName = srcName;
			this.dstName = dstName;
		}

		/**
		 * @return the name of the source module
		 */
		public String getSrcName() {
			return srcName;
		}

		/**
		 * @return the name of the destination module
		 */
		public String getDstName() {
			return dstName;
		}
	}
}
