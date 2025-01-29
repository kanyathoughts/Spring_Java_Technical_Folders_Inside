/* Copyright (c) 2023 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.service;

import java.time.Instant;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.data.access.postgres.TaxonomyPgDao;
import innowake.mining.data.annotation.ProjectIdArgument;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.TaxonomyFieldName;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

@Service
public class TaxonomyServiceImpl implements TaxonomyService {

	private final TaxonomyPgDao dao;
	private final ModulePgDao moduleDao;

	@Autowired
	public TaxonomyServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		dao = new TaxonomyPgDao(jdbcTemplate);
		moduleDao = new ModulePgDao(jdbcTemplate);
	}

	@Override
	public long count(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.count(builder);
	}

	@Override
	public List<TaxonomyTypePojo> findTypes(final BuildingConsumer<TaxonomyTypeInquiryBuilder> builder) {
		return dao.findTypes(builder);
	}

	@Override
	public List<TaxonomyCategoryPojo> findCategories(final BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder) {
		return dao.findCategories(builder);
	}

	@Override
	public List<TaxonomyPojo> find(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.find(builder);
	}
	
	@Override
	public Optional<TaxonomyPojo> findAny(BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.findAny(builder);
	}

	@Override
	public List<EntityId> findIds(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.findIds(builder);
	}

	@Override
	public Paged<TaxonomyPojo> find(final Pagination paging, final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.find(paging, builder);
	}

	@Cacheable(cacheNames = "aggregatedValues", cacheResolver = "cacheResolver", keyGenerator = "projectKeyGenerator")
	@Override
	/* project is required for caching */
	public List<AggregationResult<TaxonomyFieldName>> getAggregations(@ProjectIdArgument final EntityId project, final AggregationRequest<TaxonomyFieldName> aggregationRequest) {
		return dao.getAggregations(q -> {
			aggregationRequest.getFilterObject().forEach((fieldName, filter) -> filter.forEach((operator, value) -> {
				switch (fieldName) {
					case PROJECT_ID:
						q.ofProject(operator, value);
						break;
					case ID:
						q.byId(operator, value);
						break;
					case NAME:
						q.withName(operator, value);
						break;
					case TYPE_NAME:
						q.ofTypeName(operator, value);
						break;
					case CATEGORY_NAME:
						q.ofCategoryName(operator, value);
						break;
					case MODULE_ID:
						q.ofModuleId(operator, value);
						break;
					case MODULE_NAME:
						q.ofModuleName(operator, value);
						break;
					case MODULE_TECHNOLOGY:
						q.ofModuleTechnology(operator, value);
						break;
					case MODULE_TYPE:
						q.ofModuleType(operator, value);
						break;
					case MODULE_REPRESENTATION:
						q.ofModuleRepresentation(operator, value);
						break;
					case MODULE_COMPLEXITY:
						q.ofModuleComplexity(operator, value);
						break;
					case MODULE_LINES_OF_CODE:
						q.ofModuleCodeLines(operator, value);
						break;
					case MODULE_LINES_OF_COMMENT:
						q.ofModuleCommentLines(operator, value);
						break;
					case MODULE_LINES_OF_DEAD_CODE:
						q.ofModuleDeadCodeLines(operator, value);
						break;
					default:
						throw new IllegalArgumentException("Unknown field: " + fieldName);
				}
			}));
			aggregationRequest.getFields().forEach(q :: aggregate);
			aggregationRequest.getGroupBy().forEach(q :: groupBy);
			aggregationRequest.getOrderBy().forEach(q :: orderBy);
		}).map(table -> AggregationResult.getResultFromTable(table, aggregationRequest, TaxonomyFieldName.class))
		  .orElse(Collections.emptyList());
	}

	@Override
	public EntityId create(final TaxonomyPojoPrototype taxonomy) {
		if (taxonomy.name.orElseNonNull("").isEmpty()) {
			throw new IllegalArgumentException("Taxonomy name must not be empty.");
		}
		return dao.create(taxonomy);
	}

	@Override
	public List<EntityId> create(final List<TaxonomyPojoPrototype> taxonomies) {
		return dao.create(taxonomies);
	}

	@Override
	public UUID createType(final TaxonomyTypePojoPrototype proto) {
		try {
			return dao.createType(proto);
		} catch (DataIntegrityViolationException e) {
			throw new ConstraintViolationException("Original Message: " + e.getMessage(), e);
		}
	}

	@Override
	public void update(final TaxonomyPojoPrototype taxonomy) {
		dao.update(taxonomy);
	}

	@Override
	public void updateType(final TaxonomyTypePojoPrototype taxonomyType) {
		dao.updateType(taxonomyType);
	}

	@Override
	public void updateCategory(final TaxonomyCategoryPojoPrototype taxonomyCategory) {
		dao.updateCategory(taxonomyCategory);
	}

	@Override
	public Long upsertCategory(final TaxonomyCategoryPojoPrototype taxonomyCategory) {
		return dao.upsertCategory(taxonomyCategory);
	}

	@Override
	public TaxonomyPojo get(final EntityId id) {
		final List<TaxonomyPojo> taxonomies = find(q -> q.byId(id));
		if (taxonomies.isEmpty()) {
			throw new MiningEntityNotFoundException("id: " + id);
		}
		return taxonomies.get(0);
	}

	@Override
	public TaxonomyCategoryPojo getCategory(final Long id) {
		final List<TaxonomyCategoryPojo> categories = findCategories(q -> q.byId(id));
		if (categories.isEmpty()) {
			throw new MiningEntityNotFoundException("id: " + id);
		}
		return categories.get(0);
	}

	@Override
	public TaxonomyTypePojo getType(final UUID id) {
		final List<TaxonomyTypePojo> types = findTypes(q -> q.byId(id));
		if (types.isEmpty()) {
			throw new MiningEntityNotFoundException("id: " + id);
		}
		return types.get(0);
	}

	@Override
	public long delete(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.delete(builder);
	}

	@Override
	public long deleteCategory(final BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder) {
		return dao.deleteCategory(builder);
	}
	
	@Override
	public long deleteType(final BuildingConsumer<TaxonomyTypeInquiryBuilder> builder) {
		return dao.deleteType(builder);
	}

	@Override
	public Map<EntityId, List<TaxonomyPojo>> findTaxonomiesPerModule(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.findTaxonomiesPerModule(builder);
	}

	@Override
	public Map<EntityId, Set<EntityId>> findTaxonomyIdPerModule(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.findTaxonomyIdPerModule(builder);
	}

	@Override
	public Map<TaxonomyPojo, Set<EntityId>> findModulesPerTaxonomy(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.fetchTaxonomyToModules(builder);
	}

	@Override
	public long createModuleLinks(final EntityId module, final Collection<EntityId> taxonomies) {
		return dao.createModuleLinks(module, taxonomies);
	}

	@Transactional("postgres") /* delete taxonomy links and update module modification dates in one transaction */
	@Override
	public long createModuleLinks(final Collection<EntityId> modules, final Collection<EntityId> taxonomies) {
		final long result = dao.createModuleLinks(modules, taxonomies);
		moduleDao.updateModules(q -> q.byIds(modules), new ModulePojoPrototype()
															.setModifiedDate(Instant.now()));
		return result;
	}
	
	@Transactional("postgres") /* delete taxonomy links and update module modification dates in one transaction */
	@Override
	public long createModuleLink(final UUID module, final EntityId taxonomy) {
		final long result = dao.createModuleLink(module, taxonomy);
		moduleDao.updateModules(q -> q.byUid(module), new ModulePojoPrototype()
															.setModifiedDate(Instant.now()));
		return result;
	}

	@Transactional("postgres") /* delete taxonomy links and update module modification dates in one transaction */
	@Override
	public long deleteModuleLinks(final BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		final List<UUID> deleteModuleLinks = dao.deleteModuleLinks(builder);
		moduleDao.updateModules(q -> q.byUids(new HashSet<>(deleteModuleLinks)), new ModulePojoPrototype()
																						.setModifiedDate(Instant.now()));
		return deleteModuleLinks.size();
	}

	@Override
	public Map<Long, Long> countModulesPerCategory(final BuildingConsumer<TaxonomyCategoryInquiryBuilder> builder) {
		return dao.fetchCategoryModuleCount(builder);
	}

	@Override
	public Map<TaxonomyTypePojo, Long> countModulesPerType(BuildingConsumer<TaxonomyTypeInquiryBuilder> builder) {
		return dao.fetchTypeModuleCount(builder);
	}
	
	@Override
	public Map<String, Long> countSourceMetricsCodeLinesByTypeName(final EntityId project) {
		return dao.countSourceMetricsCodeLinesByTypeName(project);
	}

	@Override
	public List<EntityId> findTaxonomyModulesIds(BuildingConsumer<TaxonomyInquiryBuilder> builder) {
		return dao.findTaxonomyModulesIds(builder);
	}

	@Override
	public long createCategory(final TaxonomyCategoryPojoPrototype taxonomyCategory) {
		return dao.createCategory(taxonomyCategory);
	}

	@Override
	public Map<UUID, Long> getTaxonomyModuleCounts(final EntityId projectId, @Nullable final Collection<UUID> moduleIds) {
		return dao.getTaxonomyModuleCounts(projectId, moduleIds);
	}
	
}
