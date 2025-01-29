/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import javax.persistence.EntityNotFoundException;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.servlet.HandlerMapping;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.AuthenticationFacade;
import innowake.mining.server.error.MiningAuthenticationException;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.FieldName;
import innowake.mining.shared.model.UserIdentifiableCreator;
import innowake.mining.shared.model.UserIdentifiableUpdater;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.service.UserRoleService;

/**
 * Base implementation for REST controllers.
 * <p>
 * For checking the existence of the entities all controller methods should call {@link #validate}.
 */
public abstract class BaseController {
	
	@Autowired
	protected ClientService clientService;
	@Autowired
	protected ProjectService projectService;
	@Autowired
	protected ModuleService moduleService;
	@Autowired
	protected SourceService sourceService;
	@Autowired
	protected AnnotationService annotationService;
	@Autowired
	protected DataDictionaryService dataDictionaryService;
	@Autowired
	protected TaxonomyService taxonomyService;
	@Autowired
	protected CustomPropertiesService customPropertiesService;	
	@Autowired
	protected Environment environment;
	@Autowired
	protected AuthenticationFacade authentication;
	@Autowired
	protected UserRoleService user;
	@Autowired
	protected UserNameUtil userUtil;
	
	protected static final String URI_VAR_CLIENT_ID = "clientId";
	protected static final String URI_VAR_PROJECT_ID = "projectId";
	protected static final String URI_VAR_MODULE_ID = "moduleId";
	protected static final String URI_VAR_REFERENCE_ID = "referenceId";
	protected static final String URI_VAR_ANNOTATION_ID = "annotationId";
	protected static final String URI_VAR_ANNOTATION_CATEGORY_ID = "annotationCategoryId";
	protected static final String URI_VAR_TAXONOMY_ID = "taxonomyId";
	protected static final String URI_VAR_META_MODEL_CLASS = "metaModelClass";
	protected static final String URI_VAR_REFERENCED_MODULE_ID = "referencedModuleId";
	protected static final String URI_VAR_UID = "uid";
	
	/**
	 * The name of the request attribute containing the user name.
	 */
	public static final String USER_ATTRIBUTE = "User";
	
	protected static class UriVarsValidation {
		private final Map<String, String> uriVars;
		private final Set<String> uriVarsPassed;
		
		@SuppressWarnings("unchecked")
		private UriVarsValidation(final HttpServletRequest request) {
			uriVars = (Map<String, String>) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
			uriVarsPassed = new HashSet<>(uriVars.size());
		}
		
		public String get(final String v) {
			return uriVars.get(v);
		}
		
		public void pass(final String ... v) {
			uriVarsPassed.addAll(Arrays.asList(v));
		}
		
		public boolean needCheck(final String ... v) {
			return ! uriVarsPassed.containsAll(Arrays.asList(v)) && uriVars.keySet().containsAll(Arrays.asList(v));
		}
		
		public boolean anyUnchecked() {
			return ! uriVarsPassed.containsAll(uriVars.keySet());
		}
		
		public List<String> getUnchecked() {
			return uriVars.keySet().stream().filter(v -> ! uriVarsPassed.contains(v)).collect(Collectors.toList());
		}
	}
	
	/**
	 * Validates the request by checking for the existence of the primary entities based on the request URI.
	 * <p>
	 * The know-existence of those entities will lead to a {@link HttpStatus#NOT_FOUND} status response.
	 *
	 * @param request the HTTP request
	 * @param passUriVars URI variables not to be checked by this method.
	 */
	protected void validate(final HttpServletRequest request, final String ... passUriVars) {
		checkUnauthenticatedRequest(request);
		
		final var validation = new UriVarsValidation(request);
		validation.pass(passUriVars);
		
		validateUriVars(validation);
		
		if (validation.anyUnchecked()) {
			throw new IllegalArgumentException("Unauthorized arguments in request: " + validation.getUnchecked());
		}
	}

	/**
	 * Validates the given {@code aggregationRequest} and verifies that all {@code ORDER BY} fields are either present in the {@code GROUP BY} list or in the
	 * {@code FIELDS} map.
	 *
	 * @param request the {@link AggregationRequest} to validate and extend
	 * @param <F> The concrete type of project {@link FieldName}
	 */
	protected static <F extends Enum<F> & FieldName> void validateAggregationRequest(final AggregationRequest<F> request) {
		final Set<F> validOrderBys = new HashSet<>();
		validOrderBys.addAll(request.getGroupBy());
		validOrderBys.addAll(request.getFields().keySet());
		if ( ! validOrderBys.containsAll(request.getOrderBy())) {
			throw new IllegalArgumentException(String.format("Order By field must be either present in the aggregation or group By fields. "
					+ "Valid Order By fields %s, provided Order By fields %s", validOrderBys, request.getOrderBy()));
		}
	}

	/**
	 * Validates the given {@code aggregationRequest} and verifies that all {@code ORDER BY} fields are either present in the {@code GROUP BY} list or in the
	 * {@code FIELDS} map.
	 * <p>Adds a project ID filter with the given {@code projectId} to the {@code filterObject} in {@code aggregationRequest}.</p>
	 *
	 * @param request the {@link AggregationRequest} to validate and extend
	 * @param projectField the project ID {@link FieldName} 
	 * @param projectId the project {@link EntityId} to set
	 * @param <F> The concrete type of project {@link FieldName}
	 */
	protected static <F extends Enum<F> & FieldName> void validateAggregationRequest(final AggregationRequest<F> request, final F projectField, final EntityId projectId) {
		validateAggregationRequest(request);

		final Map<String, Object> filter = request.getFilterObject().get(projectField);
		/* Like before the Postgres migration, we do not check, if a project filter for project(s) is present.
		 * A user could set a filter for one or more projects, he has no access. */
		if (filter == null) {
			request.getFilterObject().put(projectField, Map.of(FilterOperators.OPERATOR_EQ, projectId));
		} else {
			filter.put(FilterOperators.OPERATOR_EQ, projectId);
		}
	}

	protected void validateUriVars(final UriVarsValidation validation) {
		if (validation.needCheck(URI_VAR_META_MODEL_CLASS)) {
			if (customPropertiesService.countPropertyDefinitions(q -> q.withName(validation.get(URI_VAR_META_MODEL_CLASS)).ofParent(null)) < 1) {
				throw new EntityNotFoundException(String.format("class with the name '%s' not found", validation.get(URI_VAR_META_MODEL_CLASS)));
			}
			validation.pass(URI_VAR_META_MODEL_CLASS);
		}
		
		if (validation.needCheck(URI_VAR_CLIENT_ID)) {
			checkClientValid(EntityId.of(validation.get(URI_VAR_CLIENT_ID)));
			validation.pass(URI_VAR_CLIENT_ID);
		}

		if (validation.needCheck(URI_VAR_PROJECT_ID)) {
			checkProjectValid(EntityId.of(validation.get(URI_VAR_PROJECT_ID)));
			validation.pass(URI_VAR_PROJECT_ID);
		}
		
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_MODULE_ID)) {
			checkModuleValid(EntityId.of(validation.get(URI_VAR_PROJECT_ID)), EntityId.of(validation.get(URI_VAR_MODULE_ID)));
			validation.pass(URI_VAR_MODULE_ID);
		}
		
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_MODULE_ID, URI_VAR_REFERENCED_MODULE_ID)) {
			checkModuleValid(EntityId.of(validation.get(URI_VAR_PROJECT_ID)), EntityId.of(validation.get(URI_VAR_REFERENCED_MODULE_ID)));
			validation.pass(URI_VAR_REFERENCED_MODULE_ID);
		}
		
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_MODULE_ID, URI_VAR_REFERENCE_ID)) {
			validation.pass(URI_VAR_REFERENCE_ID);
		}

		/* Functional Blocks */
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_UID)) {
			validation.pass(URI_VAR_UID);
		}
		
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_ANNOTATION_ID)) {
			validation.pass(URI_VAR_ANNOTATION_ID);
		}
		
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_ANNOTATION_CATEGORY_ID)) {
			validation.pass(URI_VAR_ANNOTATION_CATEGORY_ID);
		}
		
		if (validation.needCheck(URI_VAR_PROJECT_ID, URI_VAR_TAXONOMY_ID)) {
			checkTaxonomyValid(EntityId.of(validation.get(URI_VAR_TAXONOMY_ID)));
			validation.pass(URI_VAR_TAXONOMY_ID);
		}
	}
	
	private void checkModuleValid(final EntityId projectId, final EntityId moduleId) {
		final var module = moduleService.findAnyModuleLightweight(q -> q.ofProject(projectId).byId(moduleId));
		if (module.isEmpty()) {
			throw new MiningEntityNotFoundException("Module not found for identifier: " + moduleId + " and project: " + projectId);
		}
	}

	/**
	 * Verifies that the project with the given ID exists and was not marked as to be deleted.
	 *
	 * @param projectId the ID to verify
	 * @throws MiningEntityNotFoundException if the project doesn't exists or was marked as to be deleted
	 */
	private void checkProjectValid(final EntityId projectId) {
		if ( ! projectService.isValid(projectId)) {
			throw new MiningEntityNotFoundException(ProjectPojo.class, projectId);
		}
	}
	
	/**
	 * Verifies that the taxonomy with the given ID exists.
	 *
	 * @param taxonomyId the ID to verify
	 * @throws MiningEntityNotFoundException if the project doesn't exists or was marked as to be deleted
	 */
	private void checkTaxonomyValid(final EntityId taxonomyId) {
		taxonomyService.get(taxonomyId);
	}

	/**
	 * Verifies that the client with the given ID exists and was not marked as to be deleted.
	 *
	 * @param clientId the client ID to verify
	 * @throws MiningEntityNotFoundException if the client doesn't exists or was marked as to be deleted
	 */
	protected void checkClientValid(final EntityId clientId) {
		clientService.get(clientId, true);
	}

	/**
	 * Check if the request is not authenticated.
	 * <p>
	 * In case of proper authentication, the user name will be added into the request, otherwise an appropriate
	 * error message will be send to the client.
	 *
	 * @param request for adding the appropriate user name into the request, for later usage 
	 */
	private void checkUnauthenticatedRequest(final ServletRequest request) {
		final Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		final boolean noAuthorizationActive = Arrays.stream(environment.getActiveProfiles()).anyMatch(Profiles.NO_AUTH::equals);
		final boolean anonymousUser = auth != null && auth.getAuthorities().stream()
				                                                                  .anyMatch(authority -> "ROLE_ANONYMOUS".equals(authority.getAuthority()));
		if (auth == null || auth.getName().isEmpty()) {
			throw new MiningAuthenticationException("Not authenticated");
		} else if (noAuthorizationActive && anonymousUser) {
			/* This is only relevant for functional integration tests for which no authentication is active. */
			((HttpServletRequest) request).setAttribute(USER_ATTRIBUTE, "admin");
		} else {
			((HttpServletRequest) request).setAttribute(USER_ATTRIBUTE, auth.getName());
		}
	}
	
	protected interface SortStringMapper<T> {
		@Nullable Consumer<SortDirection> accept(T builder, String field);
	}
	
	protected <T> void buildOrderFromString(final T builder, final Collection<String> sortBy, final SortStringMapper<T> mapper) {
		if (sortBy != null) {
			for (final String sort : sortBy) {
				final String[] sortArgs = sort.split(";");
				final Consumer<SortDirection> mapping = mapper.accept(builder, sortArgs[0]);
				if (mapping == null) {
					throw new IllegalArgumentException("Bad sort specification: " + sort);
				}
				mapping.accept(SortDirection.of(sortArgs[1]));
			}
		}
	}
	
	protected <T> T resolveUserNames(final T entity) {
		if (entity instanceof UserIdentifiableCreator) {
			((UserIdentifiableCreator) entity).setCreatedByUserName(userUtil::getUserName);
		}
		if (entity instanceof UserIdentifiableUpdater) {
			((UserIdentifiableUpdater) entity).setUpdatedByUserName(userUtil::getUserName);
		}
		return entity;
	}
	
	protected <T> List<T> resolveUserNames(final List<T> entities) {
		for (final T entity : entities) {
			resolveUserNames(entity);
		}
		return entities;
	}
	
	protected void validateProject(final EntityId projectId, final AnnotationPojoPrototype annotation) {
		final EntityId actualProject = moduleService.getModule(annotation.module.optional()
				.orElseGet(() -> annotationService.get(annotation.identityProvisional()).getModule()))
			.getProject();
		if (! projectId.equals(actualProject)) {
			throw new IllegalArgumentException("Project " + projectId + " does not match " + actualProject);
		}
	}
	
}
