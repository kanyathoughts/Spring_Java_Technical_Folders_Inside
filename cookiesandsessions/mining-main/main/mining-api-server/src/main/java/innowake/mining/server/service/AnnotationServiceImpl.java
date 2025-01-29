/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.persistence.EntityNotFoundException;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.access.postgres.AnnotationPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.data.access.postgres.ProjectPgDao;
import innowake.mining.data.annotation.ProjectIdArgument;
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationFieldName;
import innowake.mining.shared.model.AnnotationReport;
import innowake.mining.shared.model.AnnotationReportSearchParameter;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * Central point for accessing and modifying Annotation entities.
 */
@Service
public class AnnotationServiceImpl implements AnnotationService {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DATA);

	protected final AnnotationPgDao dao;
	protected final ModulePgDao moduleDao;
	protected final ProjectPgDao projectDao;
	protected final CustomPropertiesService customPropertiesService;
	
	@Autowired
	public AnnotationServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate, final CustomPropertiesService customPropertiesService) {
		dao = new AnnotationPgDao(jdbcTemplate);
		moduleDao = new ModulePgDao(jdbcTemplate);
		projectDao = new ProjectPgDao(jdbcTemplate);
		this.customPropertiesService = customPropertiesService;
	}
	
	@Override
	public AnnotationPojo get(final EntityId id) {
		return findAny(q -> q.byId(id)).orElseThrow(() -> new MiningEntityNotFoundException(AnnotationPojo.class, id.toString()));
	}
	
	@Override
	public AnnotationPojo get(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return findAny(builder).orElseThrow(EntityNotFoundException::new);
	}
	
	@Override
	public List<AnnotationPojo> find(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return dao.find(builder);
	}
	
	@Override
	public Optional<AnnotationPojo> findAny(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return dao.findAny(builder);
	}

	@Cacheable(cacheNames = "aggregatedValues", cacheResolver = "cacheResolver", keyGenerator = "projectKeyGenerator")
	@Override
	/* project is required for caching */
	public List<AggregationResult<AnnotationFieldName>> getAggregations(@ProjectIdArgument final EntityId project, final AggregationRequest<AnnotationFieldName> request) {
		return dao.getAggregations(q -> {
			request.getFilterObject().forEach((fieldName, filter) -> filter.forEach((operator, value) -> {
				switch (fieldName) {
					case ID:
						q.byId(operator, value);
						break;
					case NAME:
						q.withName(operator, value);
						break;
					case PROJECT_ID:
						q.ofProject(operator, value);
						break;
					case CATEGORY:
						q.withCategory(operator, value);
						break;
					case STATE:
						q.withState(operator, value);
						break;
					case TYPE:
						q.withType(operator, value);
						break;
					case CREATED_BY_USER_ID:
						q.withCreatedByUserId(operator, value);
						break;
					case UPDATED_BY_USER_ID:
						q.withUpdatedByUserId(operator, value);
						break;
					case SOURCE_ATTACHMENT:
						q.withSourceAttachment(operator, value);
						break;
					case MODULE_TECHNOLOGY:
						q.withModuleTechnology(operator, value);
						break;
					case MODULE_TYPE:
						q.withModuleType(operator, value);
						break;
					case METADATA:
						q.withMetadata(operator, value);
						break;
					case TAXONOMY_ID:
						q.withTaxonomy(operator, value);
						break;
					default:
						throw new UnsupportedOperationException("Unknown field name for Annotation " + fieldName);
				}
			}));
			request.getFields().forEach(q :: aggregate);
			request.getGroupBy().forEach(q :: groupBy);
			request.getOrderBy().forEach(q :: orderBy);
		}).map(table -> AggregationResult.getResultFromTable(table, request, AnnotationFieldName.class))
		  .orElse(Collections.emptyList());
	}

	@Override
	public long count(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return dao.count(builder);
	}
	
	@Override
	public Paged<AnnotationPojo> find(final Pagination page, final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return dao.find(page, builder);
	}
	
	@Override
	public Map.Entry<EntityId, EntityId> delete(final EntityId project, final EntityId annotation) {
		return dao.delete(q -> q.ofProject(project).byId(annotation)).entrySet().stream().findFirst()
				.orElseThrow(() -> new MiningEntityNotFoundException(AnnotationPojo.class, annotation));
	}
	
	@Override
	public Map<EntityId, EntityId> delete(final BuildingConsumer<AnnotationInquiryBuilder> builder) {
		return dao.delete(builder);
	}
	
	@Override
	public EntityId create(final AnnotationPojoPrototype annotation) {
		if ( ! annotation.createdByUserId.isPresent()) {
			annotation.setCreatedByUserId(SchemaConstants.SYSTEM_USER);
		}

		/* updatedByUserId is ignored in old impl when creating annotations */
		annotation.updatedByUserId.unset();

		customPropertiesService.validateModuleBound(annotation, annotation.module::getNonNull);
		return dao.put(annotation, true);
	}
	
	@Override
	public EntityId update(final AnnotationPojoPrototype annotation) {
		customPropertiesService.validateModuleBound(annotation, () -> annotation.module.orElseNonNull(() -> get(annotation.identityProvisional()).getModule()));
		return dao.put(annotation, false);
	}
	
	@Override
	public int[][] update(final List<AnnotationPojoPrototype> annotations, final int batchSize) {
		annotations.forEach(a -> customPropertiesService.validateModuleBound(a, () -> a.module.orElseNonNull(() -> get(a.identityProvisional()).getModule())));
		return dao.put(annotations, false, batchSize);
	}
	
	@Override
	public AnnotationCategory getCategory(final EntityId project, final Long id) {
		return dao.findCategory(q -> q.ofProject(project).byId(id))
					.orElseThrow(() -> new MiningEntityNotFoundException(AnnotationCategory.class, id));
	}
	
	@Override
	public Optional<AnnotationCategory> findCategory(final BuildingConsumer<AnnotationCategoryInquiryBuilder> builder) {
		return dao.findCategory(builder);
	}

	@Override
	public List<AnnotationCategory> findCategories(final BuildingConsumer<AnnotationCategoryInquiryBuilder> builder) {
		return dao.findCategories(builder);
	}

	@Override
	public List<Long> findCategoryIds(final BuildingConsumer<AnnotationCategoryInquiryBuilder> builder) {
		return dao.findCategoryIds(builder);
	}
	
	@Override
	public List<AnnotationType> findDeclaredTypes(final EntityId project) {
		return dao.findDeclaredTypes(project);
	}
	
	@Override
	public long createCategory(final EntityId project, final String name, final Collection<AnnotationType> types) {
		return dao.createCategory(project, name, types);
	}
	
	@Override
	public void updateCategory(final EntityId project, final Long id, Optional<String> name, final Optional<Collection<AnnotationType>> types) {
		dao.updateCategory(project, id, name, types);
	}
	
	@Override
	public void deleteCategory(final EntityId project, final Long id) {
		dao.deleteCategory(project, id);
	}
	
	@Override
	public Paged<AnnotationReport> getReport(final EntityId project, final AnnotationReportSearchParameter parameters, final int limit) {
		return dao.getReport(project, parameters, limit);
	}
	
	@Override
	@Nullable
	public String getContent(final AnnotationPojo annotation, final EntityId projectId, final EntityId moduleId) {
		/* if source attachment is available directly return it */
		if (annotation.getSourceAttachment().isPresent()) {
			return annotation.getSourceAttachment().get().toString();
		}

		/* save on DB access if location information is not present */
		final var location = annotation.getLocation();
		if (location.isEmpty()) {
			return null;
		}

		/* otherwise try retrieving the source from the associated module by querying the module content */
		final var module = moduleDao.findAnyModule(q -> q.ofProject(projectId).byId(moduleId).includeContent(true));
		if (module.isEmpty()) {
			return null;
		}

		final var content = module.get().getContent();
		if (content.isEmpty()) {
			return null;
		}

		final int startOffset = location.get().getOffset().intValue();
		final int endOffset = startOffset + location.get().getLength().intValue();

		/* extract sub-string with module location from content */
		if (startOffset < 0 || startOffset > endOffset || endOffset > content.get().length()) {
			LOG.warn(() -> String.format(
					"Invalid location information (offset=%d,length=%d) for the content (length=%d) of Module (id=%d) when trying to extract the content "
							+ "for the Annotation creation.",
							location.get().getOffset(), location.get().getLength(), content.get().length(), moduleId));
		}

		/* if the substring would be empty we just want to return null, this saves a database access */
		return StringUtils.defaultIfEmpty(StringUtils.substring(content.get(), startOffset, endOffset), null);
	}

	@Override
	public Long getNid(final EntityId annotationId) {
		if ( ! annotationId.hasNid()) {
			final var annotations = findAny(b -> b.byId(annotationId));
			if (annotations.isEmpty()) {
				throw new MiningEntityNotFoundException("Annotation does not exists with id: " + annotationId);
			}

			return annotations.get().getId();
		}

		return annotationId.getNid();
	}

	@Override
	public Map<Long, Object> getCustomProperties(final EntityId projectId, final String customPropertyName) {
		final Long projectNid;
		if ( ! projectId.hasNid()) {
			final var nids = projectDao.findNids(q -> q.withId(projectId));
			if (nids.isEmpty()) {
				throw new MiningEntityNotFoundException("Project must exist for id: " + projectId);
			}

			projectNid = nids.get(0);
		} else {
			projectNid = projectId.getNid();
		}

		final String entityClassName = customPropertiesService.getDefaultClassName(projectNid, "Annotation");
		final List<AnnotationPojo> annotations = find(q -> q.ofProject(projectId)
															.withCustomPropertyPresent(List.of(entityClassName, customPropertyName), true));
		return annotations.stream().collect(Collectors.toMap(a -> a.getId(), a -> {
			@SuppressWarnings("unchecked")
			final Map<String, Object> properties = (Map<String, Object>) a.getCustomProperties().get(entityClassName);
			return properties.get(customPropertyName);
		}));
	}
}
