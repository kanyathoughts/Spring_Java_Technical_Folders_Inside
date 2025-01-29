/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.security.RoleType.CLIENT_ADMIN;
import static java.util.stream.Collectors.toList;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import org.aopalliance.intercept.MethodInvocation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.security.access.AccessDecisionVoter;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.expression.method.DefaultMethodSecurityExpressionHandler;
import org.springframework.security.access.prepost.PreInvocationAttribute;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Mining specific {@link AccessDecisionVoter} evaluating the annotations
 * 
 * <ul>
 *  <li>{@link Nature @Nature}
 *  <li>{@link Role @Role}
 *  <li>{@link ManualSecurityWithoutProjectAssociation @ManualSecurityWithoutProjectAssociation}
 * </ul>
 * 
 * for determining the access.
 */
@Component
public class MiningVoter implements AccessDecisionVoter<MethodInvocation> {

	private static final Long NOT_FOUND = Long.valueOf(-1);
	private static final Logger LOG = LoggerFactory.getLogger(MiningVoter.class);
	
	@Autowired
	private ApplicationContext applicationContext;
	
	@Nullable
	private ProjectService projectService;
	
	@Nullable
	private ClientService clientService;
	
	private synchronized ClientService getClientService() {
		if (clientService == null) {
			clientService = applicationContext.getBean(ClientService.class);
		}

		return assertNotNull(clientService);
	}

	private synchronized ProjectService getProjectService() {
		if (projectService == null) {
			projectService = applicationContext.getBean(ProjectService.class);
		}

		return assertNotNull(projectService);
	}
	
	@Override
	public boolean supports(@Nullable final ConfigAttribute attribute) {
		return attribute instanceof PreInvocationAttribute;
	}
	
	@Override
	public boolean supports(@Nullable final Class<?> clazz) {
		return MethodInvocation.class.isAssignableFrom(clazz);
	}

	@Override
	public int vote(@Nullable final Authentication authentication, @Nullable final MethodInvocation methodInvocation,
			@Nullable final Collection<ConfigAttribute> attributes) {
		LOG.debug(() -> "MiningVoter#vote");
		if (methodInvocation == null) {
			LOG.error(() -> "MethodInvocation required but was null. Denying access.");
			return ACCESS_DENIED;
		}
		if (authentication == null) {
			LOG.error(() -> "Authentication required but was null. Denying access.");
			return ACCESS_DENIED;
		}

		final Method javaMethod = methodInvocation.getMethod();
		final Role role = javaMethod.getDeclaredAnnotation(Role.class);
		final Nature nature = javaMethod.getDeclaredAnnotation(Nature.class);
		final ManualSecurityWithoutProjectAssociation manualSecurityWithoutPrjAssociation = javaMethod.getDeclaredAnnotation(ManualSecurityWithoutProjectAssociation.class);

		boolean securityAnnotationsMissing = false;

		if (role == null) {
			LOG.warn(() -> String.format("No @Role provided for %s", methodInvocation));
			securityAnnotationsMissing = true;
		}

		if (nature == null) {
			LOG.warn(() -> String.format("No @Nature provided for %s", methodInvocation));
			securityAnnotationsMissing = true;
		}

		if (securityAnnotationsMissing) {
			LOG.warn(() -> String.format(
					"Mandatory security annotations are missing. Please make sure that %s and %s are present. Denying access.",
					Role.class.getSimpleName(), Nature.class.getSimpleName()));
			return ACCESS_DENIED;
		}
		
		if (isAdmin(authentication)) {
			return ACCESS_GRANTED;
		}
		
		/* For methods returning collections which are not project bound
		 * we short circuit here, as the collections will be filtered within
		 * the database queries. */
		if (manualSecurityWithoutPrjAssociation != null) {
			return ACCESS_GRANTED;
		}
		
		final Long projectId = retrieveProjectId(methodInvocation, authentication);
		LOG.debug(() -> String.format("Project ID: %d", projectId));
		
		final Long clientId = getClientId(authentication, methodInvocation, projectId);
		LOG.debug(() -> String.format("Client ID: %d", clientId));
		
		if (isClientAdmin(authentication, clientId, assertNotNull(role))) {
			return ACCESS_GRANTED;
		}
		
		final Nature nonNullNature = assertNotNull(nature);
		if (projectId == null && ! nonNullNature.onAnyProject()) {
			LOG.warn(() -> String.format("Could not retrieve the project ID when invoking %s", methodInvocation.getMethod().toGenericString()));
			return ACCESS_DENIED;
		}

		final boolean natureValid = checkNatures(authentication, nonNullNature, clientId, projectId);
		final boolean userRoleValid = checkRoles(authentication, assertNotNull(role), clientId, projectId, nonNullNature.onAnyProject());
		
		if (natureValid && userRoleValid) {
			return ACCESS_GRANTED;
		}

		return ACCESS_DENIED;
	}
	
	public static List<MiningRole> getMiningRoles(final Collection<? extends GrantedAuthority> authorities) {
		return authorities.stream().filter(MiningRole.class::isInstance).map(MiningRole.class::cast).collect(toList());
	}

	private boolean checkRoles(final Authentication authentication, final Role userRole, Long clientId, @Nullable Long projectId, final boolean anyProject) {
		final RoleType[] roles = userRole.value();
		final Collection<? extends GrantedAuthority> authorities = authentication.getAuthorities();

		if (LOG.isDebugEnabled()) {
			printRequiredRoles(userRole.annotationType().getSimpleName(), Arrays.stream(roles).map(RoleType::name).collect(toList()));
			printActualRoles(authorities);
		}

		final List<RoleType> requiredRoleList = Arrays.asList(roles);
		final List<MiningRole> actualRoles = getMiningRoles(authorities);
		
		final boolean userAccessGranted = getRoleValues(clientId, projectId, anyProject, actualRoles)
				.map(String::toLowerCase)
				.filter(RoleType.VALUE_SET::contains)
				.map(String::toUpperCase)
				.map(RoleType::valueOf)
				.anyMatch(actualUserRole -> requiredRoleList.stream().anyMatch(actualUserRole::contains));

		LOG.debug(() -> String.format("User authorization granted: %b", Boolean.valueOf(userAccessGranted)));
		return userAccessGranted;
	}

	private boolean checkNatures(final Authentication authentication, final Nature nature, final Long clientId, @Nullable final Long projectId) {
		final NatureType[] natures = nature.value();
		final Collection<? extends GrantedAuthority> authorities = authentication.getAuthorities();

		if (LOG.isDebugEnabled()) {
			printRequiredRoles(nature.annotationType().getSimpleName(), Arrays.stream(natures).map(NatureType::name).collect(toList()));
			printActualRoles(authorities);
		}

		final List<NatureType> requiredNatureList = Arrays.asList(natures);
		final List<MiningRole> actualRoles = getMiningRoles(authorities);
		
		final boolean userAccessGranted = getRoleValues(clientId, projectId, nature.onAnyProject(), actualRoles)
				.map(String::toLowerCase)
				.filter(NatureType.VALUE_SET::contains)
				.map(NatureType::fromName)
				.anyMatch(requiredNatureList::contains);

		LOG.debug(() -> String.format("User authorization granted: %b", Boolean.valueOf(userAccessGranted)));
		return userAccessGranted;
	}
	
	@Nullable
	private Long retrieveProjectId(final MethodInvocation method, final Authentication authentication) {
		final DefaultMethodSecurityExpressionHandler methodSecurityExpressionHandler = new DefaultMethodSecurityExpressionHandler();
		final EvaluationContext evaluationContext = methodSecurityExpressionHandler.createEvaluationContext(authentication, method);
		final ExpressionParser parser = new SpelExpressionParser();
		final Nature natureAnnotation = method.getMethod().getAnnotation(Nature.class);
		final Expression expression = parser.parseExpression(natureAnnotation.projectId());
		final Object methodValue = expression.getValue(evaluationContext, Object.class);
		
		final EntityId id;
		if (methodValue instanceof EntityId) {
			id = (EntityId) methodValue;
		} else if (methodValue instanceof String) {
			id = EntityId.of((String) methodValue);
		} else if (methodValue instanceof Long) {
			return (Long) methodValue;
		} else {
			return null;
		}
		
		if (id.hasNid()) {
			return id.getNid();
		} else {
			return getProjectService().find(id).map(ProjectPojo::getId).orElse(null);
		}
	}
	
	private Long getClientId(final Authentication authentication, final MethodInvocation methodInvocation, @Nullable final Long projectId) {
		if (projectId != null) {
			/* Try getting it based on the project */
			final Long clientIdFromProject = getClientId(projectId);
			return clientIdFromProject != null ? clientIdFromProject : getClientId(methodInvocation, authentication);
		} else {
			/* if we could not retrieve a project ID initially we directly try the method context for the client ID */
			return getClientId(methodInvocation, authentication);
		}
	}

	@Nullable
	private Long getClientId(final Long projectId) {
		try {
			final ProjectPojo project = getProjectService().get(EntityId.of(projectId));
			return assertNotNull(project.getClientNid());
		} catch (final MiningEntityNotFoundException e) {
			LOG.trace(e::getLocalizedMessage, e);
			return null;
		}
	}
	
	private Long getClientId(final MethodInvocation methodInvocation, final Authentication authentication) {
		final Long clientIdFromMethodContext = retrieveClientId(methodInvocation, authentication);
		if (clientIdFromMethodContext != null) {
			return clientIdFromMethodContext;
		} else {
			return NOT_FOUND;
		}
	}
	
	@Nullable
	private Long retrieveClientId(final MethodInvocation method, final Authentication authentication) {
		final DefaultMethodSecurityExpressionHandler methodSecurityExpressionHandler = new DefaultMethodSecurityExpressionHandler();
		final EvaluationContext evaluationContext = methodSecurityExpressionHandler.createEvaluationContext(authentication, method);
		final ExpressionParser parser = new SpelExpressionParser();
		final Role natureAnnotation = method.getMethod().getAnnotation(Role.class);
		final Expression expression = parser.parseExpression(natureAnnotation.clientId());
		final Object methodValue = expression.getValue(evaluationContext, Object.class);

		if (methodValue != null) {
			if (methodValue instanceof Long) {
				return (Long) methodValue;
			}

			if (methodValue instanceof EntityId) {
				return ((EntityId) methodValue).apply(uid -> getClientService().find(uid).map(ClientPojo::getId).orElse(null), Long::valueOf);
			}

			if (methodValue instanceof String) {
				/* We assume the string is a record ID */
				return EntityId.of((String) methodValue).apply(uid -> getClientService().find(uid).map(ClientPojo::getId).orElse(null), Long::valueOf);
			}
		}

		return null;
	}

	private boolean isAdmin(final Authentication authentication) {
		return getMiningRoles(authentication.getAuthorities()).stream().anyMatch(role -> role.value().equals(RoleType.ADMIN.getValue()));
	}
	
	private boolean isClientAdmin(final Authentication authentication, final Long clientId, final Role requiredRole) {
		final List<MiningRole> userRoles = getMiningRoles(authentication.getAuthorities());
		if (requiredRole.onAnyClient() && hasClientAdminRole(userRoles)) {
			/* Short-circuit mainly for endpoints which have no project-association but where it's still relevant
			 * that the user have proper user roles. */
			return true;
		}
		final boolean requiresClientAdminOrLower = Arrays.stream(requiredRole.value()).anyMatch(CLIENT_ADMIN::contains);
		return requiresClientAdminOrLower && userRoles.stream()
				.filter(userRole -> isClientRole(userRole, clientId))
				.anyMatch(userRole -> userRole.value().equals(CLIENT_ADMIN.getValue()));
	}
	
	private boolean hasClientAdminRole(final List<MiningRole> roles) {
		return roles.stream()
				.map(MiningRole::value)
				.anyMatch(role -> CLIENT_ADMIN.getValue().equals(role));
	}

	private boolean isClientRole(final MiningRole role, final Long clientId) {
		final Optional<Long> actualClientId = role.clientId();
		return actualClientId.isPresent() && actualClientId.get().equals(clientId);
	}

	private Stream<String> getRoleValues(final Long clientId, @Nullable final Long projectId, final boolean anyProject, final List<MiningRole> actualRoles) {
		return actualRoles.stream()
				.filter(role -> filterClients(clientId, role))
				.filter(role -> filterProjects(projectId, anyProject, role))
				.map(MiningRole::value);
	}

	private boolean filterProjects(@Nullable final Long projectId, final boolean anyProject, final MiningRole role) {
		final Optional<Long> idOptinal = role.projectId();
		return anyProject || idOptinal.isPresent() && idOptinal.get().equals(projectId);
	}

	private boolean filterClients(final Long clientId, final MiningRole role) {
		final Optional<Long> idOptional = role.clientId();
		return clientId.equals(NOT_FOUND) || idOptional.isPresent() && idOptional.get().equals(clientId);
	}

	private void printRequiredRoles(final String roleType, final Collection<String> requiredRoles) {
		LOG.debug(() -> String.format("Required %s", roleType));
		for (final String requiredRole : requiredRoles) {
			LOG.debug(() -> String.format("\t%s", requiredRole));
		}
	}
	
	private void printActualRoles(final Collection<? extends GrantedAuthority> authorities) {
		LOG.debug(() -> "Actual user roles");
		for (final GrantedAuthority grantedAuthority : authorities) {
			LOG.debug(() -> String.format("\t%s", grantedAuthority.toString()));
		}
	}

}
