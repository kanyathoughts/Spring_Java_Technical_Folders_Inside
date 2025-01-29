/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.hazelcast.shaded.org.json.JSONObject;
import com.hazelcast.shaded.org.json.XML;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.scheduler.SchedulerConditionEntryResolver;
import innowake.mining.server.scheduler.SchedulerEntryResolver;
import innowake.mining.server.scheduler.SchedulerJobEntryResolver;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipFilter;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipFilter.EntryFilter;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojoPrototype;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.hashing.LinkHash;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.scheduler.SchedulerEntryResolver.DEFAULT_SCHEDULER_IDENTIFIER;
import static innowake.mining.shared.entities.scheduler.SchedulerImportPojo.PROPERTY_CREATE_MODULE_IF_MISSING;
import static innowake.mining.shared.entities.scheduler.SchedulerImportPojo.PROPERTY_ESTABLISH_MODULE_RELATIONSHIP;

/**
 * This service is responsible for importing Control-M data and resolving the relationships within the data. The data is imported from the given source
 * and the relationships are resolved using the resolvers. The resolvers are queried in the order they are present in the list. If the resolver could not
 * resolve the relationship, a warning message is generated and the relationship is skipped. The service is transactional and the data is imported in a
 * single transaction.
 */
@Service
public class ControlMImportService {

	private final ModuleService moduleService;
	private final SchedulerInfoService schedulerInfoService;
	private final List<SchedulerJobEntryResolver> jobEntryResolvers;
	private final List<SchedulerConditionEntryResolver> conditionEntryResolvers;
	private final List<SchedulerEntryResolver> entryResolvers;
	private final SourceService sourceService;

	private static final Logger LOG = LoggerFactory.getLogger(ControlMImportService.class);
	private static final int PAGE_LIMIT = 1000;

	@Autowired
	public ControlMImportService(final ModuleService moduleService, final SchedulerInfoService schedulerInfoService,
			final List<SchedulerJobEntryResolver> jobEntryResolvers, final List<SchedulerConditionEntryResolver> conditionEntryResolvers,
			final List<SchedulerEntryResolver> entryResolvers, final SourceService sourceService) {
		this.moduleService = moduleService;
		this.schedulerInfoService = schedulerInfoService;
		this.jobEntryResolvers = jobEntryResolvers;
		this.conditionEntryResolvers = conditionEntryResolvers;
		this.entryResolvers = entryResolvers;
		this.sourceService = sourceService;
	}

	/**
	 * This method imports the Control-M data from the given prototype and resolves the relationships within the data. The data is imported in a single transaction.
	 *
	 * @param prototype The prototype containing the source and project information
	 * @param progressMonitor The progress monitor to report the progress of the import
	 * @param warningHandler The handler to handle the warning messages generated during the import
	 *
	 * @throws IOException If an error occurs while reading the source data
	 */
	@Transactional("postgres")
	public void importData(final SchedulerImportPojoPrototype prototype, final ProgressMonitor progressMonitor, final Consumer<String> warningHandler) throws IOException {
		final String importer = prototype.importerUsed.getNonNull();
		final EntityId project = prototype.project.getNonNull();
		final Map<String, Object> properties = prototype.properties.getOrSet(Map::of);
		final boolean establishModuleRelationship = Boolean.parseBoolean(properties.getOrDefault(PROPERTY_ESTABLISH_MODULE_RELATIONSHIP, false).toString());
		final SchedulerJobEntryResolver jobEntryResolver = applicableJobEntryResolver(importer);
		final SchedulerConditionEntryResolver conditionEntryResolver = applicableConditionEntryResolver(importer);
		final List<SchedulerEntryResolver> entryResolvers = this.entryResolvers.stream()
				.filter(x -> x.resolverSchedulerIdentifier()
						.equals(importer))
				.collect(Collectors.toList());

		progressMonitor.setJobDescription("Importing Control-M data and resolve module for applicable entries");
		LOG.info("Importing Control-M data for project: {}", project);
		final UUID sourceId = prototype.source.getNonNull();
		final BinaryString source = sourceService.getContent(sourceId, project);

		final UUID importId = schedulerInfoService.createSchedulerImport(prototype.setImportedOn(Instant.now()));

		/* we pass the first node name as root because XmlMapper discards it while deserializing. it does not bring any value for our use case, hence passing
		* a generic name: root. this makes the identifier set to UNKNOWN, unfortunately. */
		addSchedulerEntry(prototype, readJson(source), "root", importId, null, jobEntryResolver, entryResolvers, warningHandler);

		progressMonitor.setJobDescription("Resolving relationships within Control-M data");
		LOG.info("Resolving relationships within Control-M data");
		resolveRelationships(importId, project, conditionEntryResolver, warningHandler);

		if (establishModuleRelationship) {
			progressMonitor.setJobDescription("Establishing module relationships");
			LOG.info("Establishing module relationships");
			final int establishedRelationships = establishModuleRelationship(importId);
			LOG.info("Established {} module relationships", establishedRelationships);
		}
	}

	@Nullable
	public EntityId createModule(final EntityId project, final String importerIdentifier, final SchedulerEntryPojo entry, final Consumer<String> warningHandler) {
		return createModule(project, importerIdentifier, entry.getContent(), warningHandler);
	}

	@Nullable
	public EntityId createModule(final EntityId project, final String importerIdentifier, final Map<String, String> content, final Consumer<String> warningHandler) {
		final ModuleType missingModuleType = applicableJobEntryResolver(importerIdentifier).missingModuleType(content);
		final Optional<ModuleFilter> moduleFilter = applicableJobEntryResolver(importerIdentifier).resolveModule(project, content);
		if (moduleFilter.isEmpty() || moduleFilter.get().getNames().isEmpty()) {
			warningHandler.accept("No module name found for entry: " + content + " with module filter: " + moduleFilter);
			return null;
		}
		final String moduleName;
		if (moduleFilter.get().getNames().size() > 1) {
			moduleName = moduleFilter.get().getNames().iterator().next();
			warningHandler.accept(
					"Multiple module names found for entry: " + content + " with module filter: " + moduleFilter + " using the first one: " + moduleName);
		} else {
			moduleName = moduleFilter.get().getNames().iterator().next();
		}
		final ModulePojoPrototype modulePojoPrototype = new ModulePojoPrototype()
				.setProject(project)
				.setType(missingModuleType.getType())
				.setTechnology(missingModuleType.getTechnology())
				.setName(moduleName)
				.setCreator(Creator.SCHEDULER_INFO)
				.setStorage(Storage.UNDEFINED)
				.setOrigin(Origin.CUSTOM)
				.setIdentification(Identification.MISSING)
				.setLinkHash(LinkHash.calculateLinkHash(moduleName, missingModuleType.getTechnology(), missingModuleType.getType()));

		return moduleService.create(modulePojoPrototype);
	}

	public int establishModuleRelationship(final UUID importId) {
		Pagination paged = new Pagination(PAGE_LIMIT);
		int size;
		final AtomicInteger count = new AtomicInteger(0);
		do {
			final Paged<SchedulerEntryRelationshipPojo> relationshipPojoPaged = schedulerInfoService.findRelationships(paged, q -> q.ofImportId(importId));

			relationshipPojoPaged.getContent().stream()
					.filter(x -> x.getPredecessorModule() != null && x.getSuccessorModule() != null)
					.distinct()
					.filter(x -> moduleService.findAnyRelationship(
							y -> y.ofSource(Objects.requireNonNull(x.getPredecessorModule())).ofDestination(Objects.requireNonNull(x.getSuccessorModule())).withType(RelationshipType.PRECEDES)).isEmpty())
					.map(x -> new ModuleRelationshipPojoPrototype().setRelationship(RelationshipType.PRECEDES)
							.setSrcModule(EntityId.of(Objects.requireNonNull(x.getPredecessorModule()))).setDstModule(EntityId.of(Objects.requireNonNull(x.getSuccessorModule()))))
					.map(moduleService::createRelationship)
					.forEach(x -> count.incrementAndGet());
			size = relationshipPojoPaged.getSize();
			paged = paged.nextPage();

		} while (size == PAGE_LIMIT);
		return count.get();
	}

	private JsonNode readJson(final BinaryString source) throws JsonProcessingException {
		final JSONObject jsonObject = XML.toJSONObject(source.toString());
		final ObjectMapper objectMapper = new ObjectMapper();
		return objectMapper.readTree(jsonObject.toString());
	}

	private void addSchedulerEntry(final SchedulerImportPojoPrototype prototype, final JsonNode node, final String nodeName, final UUID importId, @Nullable final UUID parentId,
			final SchedulerJobEntryResolver jobEntryResolver, final List<SchedulerEntryResolver> entryResolvers, final Consumer<String> warningHandler) {
		final boolean createModuleIfMissing = Boolean.parseBoolean(prototype.properties.getNonNull().getOrDefault(PROPERTY_CREATE_MODULE_IF_MISSING, false).toString());
		if (node.isArray()) {
			/* for array nodes */
			final Iterator<JsonNode> elements = node.elements();
			while (elements.hasNext()) {
				addSchedulerEntry(prototype, elements.next(), nodeName, importId, parentId, jobEntryResolver, entryResolvers, warningHandler);
			}
			return;
		}
		final Map<String, String> content = new HashMap<>();
		final Iterator<Map.Entry<String, JsonNode>> fields = node.fields();

		/* The elements stored in the below map will be the children of the current node */
		final Map<String, List<JsonNode>> childrenNodes = new HashMap<>();
		while (fields.hasNext()) {
			final Map.Entry<String, JsonNode> field = fields.next();
			if (field.getValue()
					.isContainerNode()) {
				childrenNodes.computeIfAbsent(field.getKey(), x -> new ArrayList<>())
						.add(field.getValue());
			} else {
				content.put(field.getKey(), field.getValue()
						.asText());
			}
		}

		final SchedulerEntryType entryType = resolveSchedulerEntryType(entryResolvers, nodeName, content);
		final SchedulerEntryPojoPrototype entry = new SchedulerEntryPojoPrototype();
		if (entryType == SchedulerEntryType.JOB) {
			final EntityId project = prototype.project.getNonNull();
			jobEntryResolver.resolveModule(project, content)
					.ifPresent(moduleFilter -> {
						final EntityId resolvedModule = resolveModuleFromFilter(project, moduleFilter);
						if (resolvedModule != null) {
							entry.setModule(resolvedModule);
						} else if (createModuleIfMissing) {
							final var createdModule = createModule(project, prototype.importerUsed.getNonNull(), content, warningHandler);
							if (createdModule != null) {
								entry.setModule(createdModule);
							}
						} else {
							warningHandler.accept("Module not found for entry: " + nodeName + " with module filter: " + moduleFilter + " content: " + content);
						}
					});
		}

		entry.setIdentifier(nodeName)
				.setType(resolveSchedulerEntryType(entryResolvers, nodeName, content))
				.setSchedulerImport(importId)
				.setContent(content);
		if (parentId != null) {
			entry.setContainedIn(parentId);
		}
		/* we first create the entry for the current node and then we provide the UUID of that entry node to the children nodes */
		final UUID entryId = schedulerInfoService.createSchedulerEntry(entry);
		childrenNodes.forEach((key, value) -> value.forEach(y -> addSchedulerEntry(prototype, y, key, importId, entryId, jobEntryResolver, entryResolvers, warningHandler)));
	}

	@Nullable
	private EntityId resolveModuleFromFilter(final EntityId project, final ModuleFilter filter) {
		final List<Tuple2<Technology, Type>> types = filter.getTypes()
				.stream()
				.map(x -> Tuple2.of(x.getTechnology(), x.getType()))
				.collect(Collectors.toList());
		return moduleService.findAnyModuleId(q -> q.ofProject(project).withNames(filter.getNames())
				.withTechnologiesAndTypes(types)).orElse(null);
	}

	private void resolveRelationships(final UUID importId, final EntityId project, final SchedulerConditionEntryResolver conditionEntryResolver, final Consumer<String> warningHandler) {
		Pagination paged = new Pagination(PAGE_LIMIT);
		int size;
		do {
			/* Retrieve entries that are of type condition */
			final Paged<SchedulerEntryPojo> conditionEntries = schedulerInfoService.findEntries(paged, q -> q.ofSchedulerImportId(importId)
					.withType(SchedulerEntryType.CONDITION));

			final List<SchedulerEntryRelationshipFilter> relationshipsFromResolver = conditionEntries.getContent().stream()
					.filter(x -> x.getContainedIn() != null)
					.flatMap(x -> conditionEntryResolver.resolveRelationships(project, x, moduleFromEntry(assertNotNull(x.getContainedIn()))).stream())
					.collect(Collectors.toList());

			schedulerInfoService.createSchedulerEntryRelationships(resolveRelationshipsForInsert(importId, project, relationshipsFromResolver, warningHandler));
			size = conditionEntries.getSize();
			paged = paged.nextPage();

		} while (size == PAGE_LIMIT);
	}

	private List<SchedulerEntryRelationshipPojoPrototype> resolveRelationshipsForInsert(final UUID importId, final EntityId project,
			final List<SchedulerEntryRelationshipFilter> relationships, final Consumer<String> warningHandler) {
		final List<SchedulerEntryRelationshipPojoPrototype> relationshipsToCreate = new ArrayList<>();

		for (final SchedulerEntryRelationshipFilter relationship : relationships) {
			final List<UUID> predecessors = resolveEntryIdFromFilter(project, importId, relationship.getPredecessorFilter());
			final List<UUID> successors = resolveEntryIdFromFilter(project, importId, relationship.getSuccessorFilter());
			if (predecessors.isEmpty() || successors.isEmpty()) {
				warningHandler.accept("Predecessor or successor module not found for relationship: " + relationship);
				continue;
			}
			for (final UUID predecessor : predecessors) {
				for (final UUID successor : successors) {
					final SchedulerEntryRelationshipPojoPrototype relationshipToCreate = new SchedulerEntryRelationshipPojoPrototype().setUid(UUID.randomUUID())
							.setPredecessor(predecessor)
							.setSuccessor(successor)
							.setIsOk(relationship.isOk());
					final String identifier = relationship.getIdentifier();
					if (identifier != null) {
						relationshipToCreate.setIdentifier(identifier);
					}
					relationshipsToCreate.add(relationshipToCreate);
				}
			}
		}

		return relationshipsToCreate;
	}

	private List<UUID> resolveEntryIdFromFilter(final EntityId project, final UUID importId, final EntryFilter entryFilter) {
		if ( ! entryFilter.getEntryIds()
				.isEmpty()) {
			return entryFilter.getEntryIds();
		}
		final EntityId[] moduleIds;
		if ( ! entryFilter.getModuleNames().isEmpty()) {
			BuildingConsumer<ModuleService.ModuleInquiryBuilder> builder = q -> q.ofProject(project).withNames(entryFilter.getModuleNames());
			if ( ! entryFilter.getTechnologyTypes().isEmpty()) {
				builder = builder.andThen(q -> q.withTechnologiesAndTypes(entryFilter.getTechnologyTypes()));
			}
			moduleIds = moduleService.findModulesLightweight(builder)
					.stream()
					.map(ModuleLightweightPojo::getUid)
					.map(EntityId::of)
					.toArray(EntityId[]::new);
		} else if ( ! entryFilter.getModuleIds().isEmpty()) {
			moduleIds = entryFilter.getModuleIds()
					.stream()
					.map(EntityId::of)
					.toArray(EntityId[]::new);
		} else {
			throw new UnsupportedOperationException("Filters are not supported yet");
		}

		return schedulerInfoService.findEntries(q -> q.ofProject(project)
						.ofSchedulerImportId(importId)
						.withModules(moduleIds))
				.stream()
				.map(SchedulerEntryPojo::getUid)
				.collect(Collectors.toList());
	}

	private Supplier<ModuleLightweightPojo> moduleFromEntry(final UUID entryId) {
		final Optional<SchedulerEntryPojo> moduleId = schedulerInfoService.findAnyEntry(x -> x.byId(entryId));
		return () -> moduleId.map(SchedulerEntryPojo::getModule)
				.map(EntityId::of)
				.map(moduleService::getModuleLightweight)
				.orElse(null);
	}

	private SchedulerJobEntryResolver applicableJobEntryResolver(final String schedulerIdentifier) {
		return applicableResolver(jobEntryResolvers, schedulerIdentifier);
	}

	private SchedulerConditionEntryResolver applicableConditionEntryResolver(final String schedulerIdentifier) {
		return applicableResolver(conditionEntryResolvers, schedulerIdentifier);
	}

	/* This method resolves the applicable resolver for the given scheduler identifier. If no resolver is found for the given identifier, the default resolver
	 * is returned. If multiple resolvers are found for the given identifier, an exception is thrown.
	 */
	private static <T extends SchedulerEntryResolver> T applicableResolver(final List<T> entryResolvers, final String identifier) {
		final List<T> matchingResolvers = entryResolvers.stream()
				.filter(x -> x.resolverSchedulerIdentifier()
						.equals(identifier))
				.toList();
		if (matchingResolvers.size() > 1) {
			final String message = String.format("Multiple resolvers found: %s with identifier '%s' ", matchingResolvers.stream()
					.map(SchedulerEntryResolver::getClass)
					.map(Class::getName)
					.collect(Collectors.joining(", ")), identifier);
			LOG.error(message);
			throw new IllegalStateException(message);
		}
		if (matchingResolvers.size() == 1) {
			return matchingResolvers.get(0);
		}
		final List<T> defaultResolvers = entryResolvers.stream()
				.filter(x -> x.resolverSchedulerIdentifier()
						.equals(DEFAULT_SCHEDULER_IDENTIFIER))
				.toList();

		if (defaultResolvers.size() != 1) {
			final String message = String.format("Multiple default resolvers found: %s", defaultResolvers.stream()
					.map(SchedulerEntryResolver::getClass)
					.map(Class::getName)
					.collect(Collectors.joining(", ")));
			LOG.error(message);
			throw new IllegalStateException(message);
		}

		return defaultResolvers.get(0);
	}

	/* This method resolves the scheduler entry type for the given node name and content. The resolvers are queried in the order they are present in the list.
	 * If could not be determined from the resolvers then the UNKNOWN is returned */
	private SchedulerEntryType resolveSchedulerEntryType(final List<SchedulerEntryResolver> entryResolvers, final String nodeName, final Map<String, String> content) {
		return entryResolvers.stream()
				.map(x -> x.resolveSchedulerEntryType(nodeName, content))
				.filter(x -> x != SchedulerEntryType.UNKNOWN)
				.findFirst()
				.orElse(SchedulerEntryType.UNKNOWN);
	}
}
