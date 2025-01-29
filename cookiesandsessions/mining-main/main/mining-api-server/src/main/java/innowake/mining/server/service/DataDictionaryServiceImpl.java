/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.mining.data.core.SchemaConstants.SYSTEM_USER;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.DataDictionaryPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.data.annotation.ProjectIdArgument;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.event.AnnotationUpdatedEvent;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.DataDictionaryFieldName;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


/**
 * Central point for accessing and modifying Data Dictionary entities.
 */
@Service
public class DataDictionaryServiceImpl implements DataDictionaryService {

	private final ApplicationEventPublisher eventPublisher;
	private final DataDictionaryPgDao dataDictionaryDao;
	private final ModulePgDao moduleDao;
	private final CustomPropertiesService customPropertiesService;
	private final AnnotationService annotationService;

	@Autowired
	public DataDictionaryServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate, final CustomPropertiesService customPropertiesService, final AnnotationService annotationService,
			ApplicationEventPublisher eventPublisher) {
		this.eventPublisher = eventPublisher;
		this.dataDictionaryDao = new DataDictionaryPgDao(jdbcTemplate);
		this.moduleDao = new ModulePgDao(jdbcTemplate);
		this.customPropertiesService = customPropertiesService;
		this.annotationService = annotationService;
	}

	@Override
	public Long count(final BuildingConsumer<DataDictionaryInquiryBuilder> qb) {
		return dataDictionaryDao.count(qb);
	}

	@Override
	public DataDictionaryPojo get(final BuildingConsumer<DataDictionaryInquiryBuilder> qb) {
		return dataDictionaryDao.findAny(qb).orElseThrow(() -> new MiningEntityNotFoundException("Could not find Data Dictionary for the matching filters."));
	}

	@Cacheable(cacheNames = "aggregatedValues", cacheResolver = "cacheResolver", keyGenerator = "projectKeyGenerator")
	@Override
	/* project is required for caching */
	public List<AggregationResult<DataDictionaryFieldName>> getAggregations(@ProjectIdArgument final EntityId project, final AggregationRequest<DataDictionaryFieldName> request) {
		return dataDictionaryDao.getAggregations(q ->  {
			request.getFilterObject().forEach((fieldName, filter) -> filter.forEach((operator, value) -> {
				switch (fieldName) {
					case PROJECT_ID:
						q.ofProject(operator, value);
						break;
					case ID:
						q.byId(operator, value);
						break;
					case DATA_ELEMENT_NAME:
						q.withName(operator, value);
						break;
					case DESCRIPTION:
						q.withDescription(operator, value);
						break;
					case FORMAT:
						q.withFormat(operator, value);
						break;
					case LENGTH:
						q.withLength(operator, value);
						break;
					case OTHER_SCOPE_LINK:
						q.withOtherScope(operator, value);
						break;
					case OTHER_SCOPE_SOURCE:
						q.withOtherScopeSource(operator, value);
						break;
					case CREATED_BY_USER_ID:
						q.withCreatedByUserId(operator, value);
						break;
					case UPDATED_BY_USER_ID:
						q.withUpdatedByUserId(operator, value);
						break;
					case MODULE_TECHNOLOGY:
						q.ofModuleTechnology(operator, value);
						break;
					case MODULE_TYPE:
						q.ofModuleType(operator, value);
						break;
					case SCOPE_LINK:
						q.withScope(operator, value);
						break;
					case IS_CANDIDATE:
						q.withIsCandidate(operator, value);
						break;
					case PIC_CLAUSE:
						q.withPicClause(operator, value);
						break;
					case DEFINED_LOCATION:
						q.withDefinedLocation(operator, value);
						break;
					case STATE:
						q.withState(operator, value);
						break;
					case IS_BUSINESS:
						q.withIsBusiness(operator, value);
						break;
					case FIELD_TRANSFORMATION:
						q.withFieldTransformation(operator, value);
						break;
					case SOURCE_INPUT:
						q.withSourceInput(operator, value);
						break;
					case TARGET_OUTPUT:
						q.withTargetOutput(operator, value);
						break;
					case IS_REFERENCED:
						q.withIsReferenced(operator, value);
						break;
					case FIELD_USAGE:
						q.withFieldUsage(operator, value);
						break;
					case SCOPE_ATTRIBUTES:
						q.withScopeAccessType(operator, value);
						break;
					case TAXONOMY_ID:
						q.withTaxonomy(operator, value);
						break;
					default:
						throw new UnsupportedOperationException(String.format("Unsupported field name: %s for DataDictionaryFieldName", fieldName));
				}
			}));
			request.getFields().forEach(q :: aggregate);
			request.getGroupBy().forEach(q :: groupBy);
			request.getOrderBy().forEach(q :: orderBy);
		}).map(rows -> AggregationResult.getResultFromTable(rows, request, DataDictionaryFieldName.class))
		  .orElse(Collections.emptyList());
	}

	@Override
	public List<DataDictionaryPojo> find(final BuildingConsumer<DataDictionaryInquiryBuilder> qb) {
		return dataDictionaryDao.find(qb);
	}

	@Override
	public Paged<DataDictionaryPojo> find(final Pagination pagination, final BuildingConsumer<DataDictionaryInquiryBuilder> qb) {
		return dataDictionaryDao.find(pagination, qb);
	}

	@Override
	public Optional<DataDictionaryPojo> findAny(final BuildingConsumer<DataDictionaryInquiryBuilder> qb) {
		return dataDictionaryDao.findAny(qb);
	}

	@Override
	@Transactional("postgres")
	public DataDictionaryPojo create(final DataDictionaryPojoPrototype dde) {
		final List<DataDictionaryPojo> exitingReferences = find(q -> q.ofModule(dde.module.getNonNull()));
		final ModuleLocation location = dde.location.getNonNull();
		if (exitingReferences.stream().anyMatch(d -> location.overlapsWith(d.getLocation().orElseThrow(() -> new NoSuchElementException("Location is required"))))) {
			throw new ConstraintViolationException("Data dictionary source location overlaps with location of existing Data Dictionary");
		}

		if ( ! dde.createdByUserId.isPresent()) {
			dde.setCreatedByUserId(SYSTEM_USER);
		}

		customPropertiesService.validateModuleBound(dde, dde.module::getNonNull);

		return get(q -> q.byId(dataDictionaryDao.create(dde)));
	}

	@Override
	@Transactional("postgres")
	public Long createCandidates(final Collection<DataDictionaryPojoPrototype> identifiedEntries) {
		final var moduleIds = identifiedEntries.stream().map(dde -> dde.module.getNonNull()).distinct().collect(Collectors.toList());
		final List<DataDictionaryPojo> existingDdes = find(q -> q.ofModules(moduleIds));
		final Map<DataDictionaryPojo, DataDictionaryPojoPrototype> existingAndNewDdes = new HashMap<>();
		final List<DataDictionaryPojoPrototype> newEntries = new ArrayList<>();
		final List<DataDictionaryPojoPrototype> toBeUpdatedEntries = new ArrayList<>();
		identifiedEntries.forEach(identified -> {
			final EntityId iModule = identified.module.getNonNull();
			existingDdes.stream()
					.filter(prototype -> identified.location.getNonNull().equals(prototype.getLocation().orElse(null))
							&& moduleMatches(iModule, prototype.getModule()))
					.findFirst()
					.ifPresentOrElse(e -> existingAndNewDdes.put(e, identified), () -> newEntries.add(identified));

			if ( ! identified.createdByUserId.isPresent()) {
				identified.setCreatedByUserId(SYSTEM_USER);
			}
			if ( ! identified.updatedByUserId.isPresent()) {
				identified.setUpdatedByUserId(SYSTEM_USER);
			}
		});

		if ( ! newEntries.isEmpty()) {
			dataDictionaryDao.create(newEntries);
		}

		existingAndNewDdes.forEach((k, v) -> updateExistingDataDictionary(k, v).ifPresent(toBeUpdatedEntries :: add));

		if ( ! toBeUpdatedEntries.isEmpty()) {
			dataDictionaryDao.updateExistingCandidates(toBeUpdatedEntries);
		}

		return (long) (toBeUpdatedEntries.size() + newEntries.size());
	}

	@Override
	@Transactional("postgres")
	public DataDictionaryPojo update(final DataDictionaryPojoPrototype dde) {
		customPropertiesService.validateModuleBound(dde, () -> dde.module.
				orElseNonNull(() -> get(q -> q.byId(dde.identityProvisional())).getModule()));
		dde.createdByUserId.unset();
		return get(q -> q.byId(dataDictionaryDao.update(dde)));
	}

	@Override
	public int update(final BuildingConsumer<DataDictionaryInquiryBuilder> builder, final DataDictionaryPojoPrototype values) {
		values.createdByUserId.unset();
		return dataDictionaryDao.update(builder, values);
	}

	@Override
	@Transactional("postgres")
	public void markAsBusinessVariables(final Collection<EntityId> ids) {
		dataDictionaryDao.markAsBusinessVariables(ids.stream().map(EntityId :: getUid).toList());
	}

	@Override
	@Transactional("postgres")
	public int delete(final BuildingConsumer<DataDictionaryInquiryBuilder> qb) {
		return dataDictionaryDao.delete(qb);
	}

	@Override
	@Transactional("postgres")
	public void linkAnnotations(final EntityId dde, final EntityId annotation) {
		dataDictionaryDao.linkAnnotations(dde, annotation);
	}

	@Override
	public List<String> findDataDictionaryOtherScope(final EntityId project) {
		return dataDictionaryDao.findDataDictionaryOtherScope(project);
	}

	private Optional<DataDictionaryPojoPrototype> updateExistingDataDictionary(final DataDictionaryPojo pojo, final DataDictionaryPojoPrototype prototype) {
		final var updatedBy = pojo.getUpdatedByUserId();
		if ( ! (SYSTEM_USER.equals(pojo.getCreatedByUserId()) && (! updatedBy.isPresent() || SYSTEM_USER.equals(updatedBy.get())))) {
			return Optional.empty();
		}
		final Collection<DataDictionaryVariableScope> scopes = CollectionUtils.union(pojo.getScopes().keySet(), prototype.scopes.getNonNull().keySet());

		if (scopes.size() > 1 && scopes.contains(DataDictionaryVariableScope.OTHER)) {
			scopes.remove(DataDictionaryVariableScope.OTHER);
		}

		final Map<DataDictionaryVariableScope, Map<String, String>> combinedScopeAttributes = getCombinedScopeAttributes(pojo, prototype, scopes);
		scopes.forEach(scope -> combinedScopeAttributes.putIfAbsent(scope, Map.of()));

		return Optional.of(new DataDictionaryPojoPrototype()
				.setUid(pojo.getUid())
				.setUpdatedByUserId(SYSTEM_USER)
				.setScopes(combinedScopeAttributes)
				.setSourceInput(getCombinedString(pojo.getSourceInput().orElse(null), prototype.sourceInput.orElse(null)))
				.setTargetOutput(getCombinedString(pojo.getTargetOutput().orElse(null), prototype.targetOutput.orElse(null))));
	}

	private Map<DataDictionaryVariableScope, Map<String, String>> getCombinedScopeAttributes(final DataDictionaryPojo oldEntry,
			final DataDictionaryPojoPrototype newEntry, final Collection<DataDictionaryVariableScope> scopes) {
		final Map<DataDictionaryVariableScope, Map<String, String>> combinedScopeAttributes = new EnumMap<>(DataDictionaryVariableScope.class);
		for (final DataDictionaryVariableScope scope : scopes) {
			final Map<String, String> oldScopeAttributes = Optional.ofNullable(oldEntry.getScopes().get(scope)).orElse(Collections.emptyMap());
			final Map<String, String> newScopeAttributes = Optional.ofNullable(newEntry.scopes.getNonNull().get(scope)).orElse(Collections.emptyMap());
			final Collection<String> allKeys = CollectionUtils.union(oldScopeAttributes.keySet(), newScopeAttributes.keySet());
			final Map<String, String> updatedAttributes = new HashMap<>();
			for (final String key : allKeys) {
				final String oldAttribute = oldScopeAttributes.get(key);
				final String newAttribute = newScopeAttributes.get(key);
				final Collection<String> updatedValue;
				if (oldAttribute == null && newAttribute == null) {
					continue;
				} else if (oldAttribute == null) {
					updatedValue = CollectionUtils.union(Collections.emptyList(), Arrays.asList(newAttribute.split(",")));
				} else if (newAttribute == null) {
					updatedValue = CollectionUtils.union(Arrays.asList(oldAttribute.split(",")), Collections.emptyList());
				} else {
					updatedValue = CollectionUtils.union(Arrays.asList(oldAttribute.split(",")), Arrays.asList(newAttribute.split(",")));
				}
				if ( ! updatedValue.isEmpty()) {
					updatedAttributes.put(key, updatedValue.stream().collect(Collectors.joining(",")));
				}
			}
			if ( ! updatedAttributes.isEmpty()) {
				combinedScopeAttributes.put(scope, updatedAttributes);
			}
		}
		return combinedScopeAttributes;
	}

	@Nullable
	private String getCombinedString(final @Nullable String oldValue, final @Nullable String newValue) {
		if (oldValue == null || oldValue.isEmpty()) {
			return newValue;
		} else if (newValue == null || newValue.isEmpty()) {
			return oldValue;
		} else {
			final Collection<String> combinedValue = CollectionUtils.union(Arrays.asList(oldValue.split(";")), Arrays.asList(newValue.split(";")));
			return combinedValue.stream().collect(Collectors.joining(";"));
		}
	}

	private boolean moduleMatches(final EntityId left, final EntityId right) {
		if (left.isEmpty() || right.isEmpty()) {
			throw new IllegalStateException("Module entity id must not be empty");
		}

		if (left.hasUid() && right.hasUid()) {
			return left.equals(right);
		} else if (left.hasNid() && right.hasNid()) {
			return left.getNid().equals(right.getNid());
		} else {
			final EntityId l = left.hasUid() ? left : right;
			final Long r = left.hasUid() ? right.getNid() : left.getNid();

			final List<EntityId> matches = moduleDao.findModuleIds(q -> q.byNid(r));
			if (matches.isEmpty()) {
				throw new MiningEntityNotFoundException(DataDictionaryPojo.class, r.toString());
			}

			return l.equals(matches.get(0));
		}
	}

	@Override
	public void updateRelatedAnnotationsEnglishTranslation(final EntityId projectId, final EntityId ddeId, final DataDictionaryPojoPrototype dataDictionaryEntry, 
			final String userId) {
		/* Check if the translation field has actually changed */
		boolean translationUpdated = false;
		Optional<String> oldTranslatedField = Optional.empty();
		final Optional<DataDictionaryPojo> oldDDE = this.findAny(q -> q.byId(ddeId));
		if (oldDDE.isPresent()) {
			oldTranslatedField = oldDDE.get().getTranslatedFieldValue();

			if (oldTranslatedField.isPresent()) {
				translationUpdated = ! oldTranslatedField.get().equals(dataDictionaryEntry.translatedFieldValue.orElse(null));
			} else {
				translationUpdated = dataDictionaryEntry.translatedFieldValue.isPresent();
			}
		}
		
		updateInternal(translationUpdated, ddeId, dataDictionaryEntry, oldTranslatedField, userId);
		
	}

	@Override
	public List<EntityId> getDataDictionaryIdsFromDataFlowIds(final List<String> dataFlowIds) {
		return dataDictionaryDao.findDataDictionaryIds(q -> q.withDataFlowIds(dataFlowIds));
	}

	private void updateInternal(final boolean translationUpdated, final EntityId ddeId, final DataDictionaryPojoPrototype dataDictionaryEntry, 
			final Optional<String> oldTranslatedField, final String userId) {
		if (translationUpdated && dataDictionaryEntry.translatedFieldValue.isPresent()) {
			for (final AnnotationPojo anno : annotationService.find(q -> q.ofDataDictionaryEntry(ddeId))) {
				final Optional<String> englishTranslation = anno.getEnglishTranslation();
				if ( ! englishTranslation.isEmpty()) {
					final AnnotationPojoPrototype annotation = anno.convertToPrototype();
					annotation.createdByUserId.unset();
					annotation.setUpdatedByUserId(userId);
					final String translatedValue = dataDictionaryEntry.translatedFieldValue.get();
					if (translatedValue != null) {

						/* If we already replaced the name, we now need to replace the old translation */
						final String sToReplace = oldTranslatedField.isPresent() && ! oldTranslatedField.get().equals("") ? 
								oldTranslatedField.get() : dataDictionaryEntry.name.get();
						final String sToReplaceWith = translatedValue.equals("") ? dataDictionaryEntry.name.get() : translatedValue;

						/* update the English translation of all annotations */
						Optional<String> englishTranslationOptional = anno.getEnglishTranslation();
						englishTranslationOptional.ifPresent(s -> annotation.setEnglishTranslation(s.replaceAll(sToReplace, sToReplaceWith)));
						annotationService.update(annotation);
						eventPublisher.publishEvent(new AnnotationUpdatedEvent(anno.getProject(), anno.identity()));
					}
				}
			}
		}
	}
}
