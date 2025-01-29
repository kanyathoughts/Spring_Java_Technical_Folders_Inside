/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;

import javax.persistence.EntityNotFoundException;
import javax.persistence.PersistenceException;

import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.google.common.io.CharStreams;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.access.postgres.MiningJobInfoPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.data.access.postgres.ProjectPgDao;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.event.MarkedForDeletionEvent;
import innowake.mining.server.event.ProjectCreatedEvent;
import innowake.mining.server.event.ProjectDeletedEvent;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.ProjectNature;

/**
 * Central point for accessing and modifying Project entities.
 */
@Service
public class ProjectServiceImpl implements ProjectService {
	
	private static final Logger LOG = LoggerFactory.getLogger(ProjectServiceImpl.class);
	
	private static final String BASE_CONFIG_LOCATION = "/db/migration/discovery-config/";
	
	private final ProjectPgDao projectDao;
	private final ModulePgDao moduleDao;
	private final MiningJobInfoPgDao miningJobInfoDao;

	@Autowired
	private AuthorizationManagementService authorizationManagementService;
	
	@Autowired
	private MiningJobService miningJobService;
	
	@Autowired
	private TaxonomyService taxonomyService;
	
	@Autowired
	private TaxonomyModelService taxonomyModelService;
	
	@Autowired
	private ApplicationEventPublisher eventPublisher;
	@Autowired
	private JobManager jobManager;

	@Value("${mining.taxonomies.technicalTaxonomyCategoryName: Technical Taxonomies}")
	private String technicalTaxonomyCategoryName = "";
	
	@Value("${mining.taxonomies.defaultTaxonomyCategoryName: Business Taxonomies}")
	private String defaultTaxonomyCategoryName = "";
	
	@Autowired
	public ProjectServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		projectDao = new ProjectPgDao(jdbcTemplate);
		moduleDao = new ModulePgDao(jdbcTemplate);
		miningJobInfoDao = new MiningJobInfoPgDao(jdbcTemplate);
	}
	
	@Override
	public List<ProjectPojo> find(final BuildingConsumer<ProjectInquiryBuilder> builder) {
		return projectDao.find(builder);
	}
	
	@Override
	public Paged<ProjectPojo> find(final Pagination paging, final BuildingConsumer<ProjectInquiryBuilder> builder) {
		return projectDao.find(paging, builder);
	}
	
	@Override
	public Optional<ProjectPojo> find(final UUID uid) {
		return projectDao.find(uid);
	}
	
	@Override
	public Optional<ProjectPojo> find(final EntityId projectId) {
		return projectDao.find(projectId);
	}
	
	@Override
	public Long count(final BuildingConsumer<ProjectInquiryBuilder> builder) {
		return projectDao.count(builder);
	}
	
	@Override
	public ProjectPojo get(final UUID uid) {
		return projectDao.find(uid)
				.orElseThrow(() -> new MiningEntityNotFoundException(ProjectPojo.class, uid.toString()));
	}
	
	@Override
	public ProjectPojo get(final EntityId projectId) {
		return projectDao.find(projectId)
				.orElseThrow(() -> new MiningEntityNotFoundException(ProjectPojo.class, projectId.toString()));
	}
	
	@Override
	public List<ProjectPojo> getAll(@Nullable final Collection<Long> projectIds) {
		return projectDao.find(q -> {
			if (projectIds != null) {
				q.withIds(projectIds);
			} else {
				q.withIdAbove(Long.valueOf(0l));
			}
			q.sortNid(SortDirection.ASCENDING);
		});
	}
	
	@Override
	public Paged<ProjectPojo> getAll(final Pagination paging, @Nullable final Consumer<ProjectOrderBuilder> order,
			@Nullable final Collection<Long> projectIds) {
		return projectDao.find(paging, q -> {
			if (projectIds != null) {
				q.withIds(projectIds);
			} else {
				q.withIdAbove(Long.valueOf(0l));
			}
			if (order != null) {
				order.accept(q);
			}
		});
	}
	
	@Override
	public List<UUID> getUids(final BuildingConsumer<ProjectInquiryBuilder> builder) {
		return projectDao.findUids(builder);
	}
	
	@Override
	public List<Long> getNids(final BuildingConsumer<ProjectInquiryBuilder> builder) {
		return projectDao.findNids(builder);
	}
	
	@Override
	public Long incrementSourceCodeRevision(EntityId projectId) {
		return projectDao.incrementSourceCodeRevision(projectId);
	}

	@Override
	public EntityId update(ProjectPojoPrototype project) {
		return projectDao.update(project);
	}

	@Override
	public void putConfig(EntityId projectId, String name, Object value) {
		projectDao.putConfig(projectId, name, value);
	}

	@Override
	public EntityId update(BuildingConsumer<ProjectPojoPrototype> projectBuilder) {
		ProjectPojoPrototype project = new ProjectPojoPrototype();
		projectBuilder.accept(project);
		return projectDao.update(project);
	}

	@Override
	public boolean isValid(EntityId projectId) {
		return projectDao.count(q -> q.withId(projectId)).longValue() > 0;
	}

	@Override
	public Map<String, Map<String, Object>> getConfigs(EntityId projectId) {
		return projectDao.fetchConfigs(projectId);
	}

	@Override
	public <T> Optional<T> getConfigByName(final EntityId projectId, final Class<T> valueType, final String name) {
		return projectDao.fetchConfigByName(projectId, valueType, name);
	}

	@Override
	public String getXmlConfig(EntityId projectId, String name) {
		return projectDao.fetchConfigByKeyAndName(projectId, PLACEHOLDER_XML_CONFIG_KEY, name)
				.orElseThrow(() -> new EntityNotFoundException("Configuration " + name + " for project " + projectId.toString()));
	}

	@Override
	public Map<String, String> getXmlConfigs(final EntityId projectId, final Collection<String> names) {
		return projectDao.fetchConfigsByKeyAndNames(projectId, PLACEHOLDER_XML_CONFIG_KEY, names);
	}

	@Override
	public void resetConfiguration(EntityId projectId) {
		for (ConfigResources res : ConfigResources.values()) {
			try {
				putConfig(projectId, res.getResourceName(), Collections.singletonMap(PLACEHOLDER_XML_CONFIG_KEY, CharStreams.toString(new InputStreamReader(
						Objects.requireNonNull(ConfigResources.class.getResourceAsStream(BASE_CONFIG_LOCATION + res.getResourceName())),
						StandardCharsets.UTF_8))));
			} catch (IOException e) {
				throw new PersistenceException("Default configuration for " + res.name() + " (" + res.getResourceName() + ") could not be uploaded.", e);
			}
		}
		putConfig(projectId, ReachabilityAnalysisConfig.CONFIG_NAME, ReachabilityAnalysisConfig.defaultConfig());
	}

	@Override
	@Transactional("postgres")
	public ProjectPojo create(final ProjectPojoPrototype project) {
		return create(project, true);
	}

	@Override
	@Transactional("postgres")
	public ProjectPojo create(final ProjectPojoPrototype project, final boolean createAuthorization) {
		if ( ! project.searchOrders.isDefined()) {
			project.searchOrders.set(Collections.singletonList(new SearchOrder()));
		}
		if (createAuthorization && ! project.natures.isDefined()) {
			throw new ConstraintViolationException("Project Natures must be defined for creation.");
		}
		
		final EntityId createdProjectId = projectDao.create(project);
		resetConfiguration(createdProjectId);
		
		final Long defaultTaxonomyCategoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName(defaultTaxonomyCategoryName).setProject(createdProjectId));
		final Long technicalTaxonomyCategoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName(technicalTaxonomyCategoryName).setProject(createdProjectId));
		
		final ProjectPojo createdProject = get(update(p -> p.withId(createdProjectId)
				.setDefaultTaxonomyCategory(defaultTaxonomyCategoryId)
				.setTechnicalTaxonomyCategory(technicalTaxonomyCategoryId)));
		
		taxonomyModelService.createTechnicalTaxonomies(createdProject);
		
		if (createAuthorization) {
			authorizationManagementService.createProjectAttributes(createdProject.getClientNid(), createdProject.getId(), project.natures.getNonNull());
		}
		
		TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
			@Override
			public void afterCommit() {
				eventPublisher.publishEvent(new ProjectCreatedEvent(createdProjectId));
			}
		});
		
		return createdProject;
	}
	
	private void markedForDeletion(final ProjectPojo project, final boolean triggerJob) {
		try {
			authorizationManagementService.deleteProjectAttributes(project.getClientNid(), project.getId());
		} catch (final Exception e) {
			LOG.error(() -> String.format(
					"Deletion of project roles and groups for specified project %d failed. Please check the Keycloak server logs for more information.",
					project.getId()), e);
		}
		if (triggerJob) {
			eventPublisher.publishEvent(new MarkedForDeletionEvent());
		}
		eventPublisher.publishEvent(new ProjectDeletedEvent(project.identity()));
	}
	
	@Override
	public EntityId markForDeletion(final EntityId projectId, final boolean andStartDeletion) {
		final ProjectPojo project = get(projectId);
		projectDao.markForDeletion(project.identity());
		markedForDeletion(project, andStartDeletion);
		return project.identity();
	}
	
	@Override
	public void deleteDirectly(final EntityId projectId) {
		final ProjectPojo project = get(projectId);
		if (project.isMarkedDeleted() == Boolean.FALSE) {
			projectDao.markForDeletion(project.identity());
			markedForDeletion(project, false);
		}
		projectDao.delete(project.identity());
	}

	@Override
	public List<ProjectNature> findProjectNatures(final EntityId projectId) {
		final ProjectPojo project = get(projectId);
		return Arrays.asList(authorizationManagementService
				.findProjectNatures(project.getClientNid(), project.getId()));
	}

	@Override
	public void changeProjectNatures(final EntityId projectId, final Set<ProjectNature> natures) {
		if (natures.isEmpty()) {
			throw new IllegalArgumentException("Project Natures have not been defined.");
		}
		final ProjectPojo project = get(projectId);
		authorizationManagementService.changeProjectNatures(project.getClientNid(), project.getId(), natures);
	}
	
	@Override
	public void deleteProjectCascading(final Long clientId, final Long projectId) {
		final List<EntityId> moduleIds = moduleDao.findModuleIds(q -> q.ofProject(EntityId.of(projectId)));
		LOG.info(() -> "Deleting Job entities for project: " + projectId);
		final List<UUID> jobIds = miningJobInfoDao.findJobId(q -> q.ofModules(EntityId.allUids(moduleIds)));
		jobManager.delete(q -> q.byIds(jobIds));

		LOG.info(() -> "Deleting Job Information for " + projectId);
		miningJobService.deleteByProjectId(EntityId.of(projectId));
		LOG.info(() -> "Deleting project " + projectId);
		projectDao.delete(EntityId.of(projectId));
	}
	
	@Override
	public Long getNid(final EntityId projectId) {
		if ( ! projectId.hasNid()) {
			final var projects = find(b -> b.withId(projectId));
			if (projects.isEmpty()) {
				throw new MiningEntityNotFoundException("Project does not exists with id: " + projectId);
			}

			return projects.get(0).getId();
		}

		return projectId.getNid();
	}
}
