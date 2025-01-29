/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.draft.idm.configuration;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.aopalliance.intercept.MethodInvocation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.expression.method.DefaultMethodSecurityExpressionHandler;
import org.springframework.security.access.prepost.PreInvocationAuthorizationAdvice;
import org.springframework.security.access.prepost.PreInvocationAuthorizationAdviceVoter;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Component;

@Component
public class MiningVoter extends PreInvocationAuthorizationAdviceVoter {
	
	public MiningVoter(@Autowired PreInvocationAuthorizationAdvice pre) {
		super(pre);
	}

	private static final Logger LOG = LoggerFactory.getLogger(MiningVoter.class);

	@Override
	public int vote(Authentication authentication, MethodInvocation method, Collection<ConfigAttribute> attributes) {
		LOG.info("MiningVoter#vote");
		final Method javaMethod = method.getMethod();
		final Role role = javaMethod.getDeclaredAnnotation(Role.class); 
		final Nature nature = javaMethod.getDeclaredAnnotation(Nature.class);
		
		boolean securityAnnotationsMissing = false;
		
		if (role == null) {
			LOG.warn("No @Role provided for {}", method);
			securityAnnotationsMissing = true;
		}
		
		if (nature == null) {
			LOG.warn("No @Nature provided for {}", method);
			securityAnnotationsMissing = true;
		}
		
		if (securityAnnotationsMissing) {
			final String message = String.format("Mandatory security annotations are missing. Please make sure that %s and %s are present. Denying access.",
					Role.class.getSimpleName(), Nature.class.getSimpleName());
			LOG.warn(message);
			return ACCESS_DENIED;
		}
		
		checkRoles(authentication, role);
		
		final Object projectId = evaluate(method, authentication);
		
		LOG.info("Project ID: {}", projectId);
	
		checkNatures(nature);

		return 0;
	}
	
	private void checkRoles(final Authentication authentication, final Role role) {
		final Role.Type[] roles = role.value();
		LOG.info("Required {}", role.annotationType().getSimpleName());
		
		if (LOG.isInfoEnabled()) {
			for (final Role.Type type : roles) {
				LOG.info("\t{}", type.name());
			}
		}
		
		final List<String> requiredRoles = Arrays.stream(roles)
				.map(requiredRole -> requiredRole.name())
				.map(String::toUpperCase)
				.collect(Collectors.toList());
		
		
		LOG.info("Actual user rules");
		final Collection<? extends GrantedAuthority> authorities = authentication.getAuthorities();
		if (LOG.isInfoEnabled()) {
			for (GrantedAuthority grantedAuthority : authorities) {
				LOG.info("\t{}", grantedAuthority.getAuthority());
			}
		}
		
		boolean userAccessGranted = authorities.stream()
		.map(GrantedAuthority::getAuthority)
		.anyMatch(requiredRoles::contains);
		
		LOG.info("User authorization granted: {}", Boolean.valueOf(userAccessGranted));
	}

	private void checkNatures(final Nature nature) {
		final Nature.Type[] natures = nature.value();		
		LOG.info(nature.annotationType().getSimpleName());

		for (final Nature.Type type : natures) {
			if (LOG.isInfoEnabled()) {
				LOG.info("\t{}", type.name());
			}
		}
		
		/* Check for actual nature of project with the given project ID */
		
		/* if check was not successful then check if the user has an overriding permission */
	}
	
    private static Object evaluate(final MethodInvocation method, final Authentication authentication) {
		final DefaultMethodSecurityExpressionHandler methodSecurityExpressionHandler = new DefaultMethodSecurityExpressionHandler();
		final EvaluationContext evaluationContext = methodSecurityExpressionHandler.createEvaluationContext(authentication, method);
        final ExpressionParser parser = new SpelExpressionParser();
        final Nature natureAnnotation = method.getMethod().getAnnotation(Nature.class);
        final Expression expression = parser.parseExpression(natureAnnotation.projectId());
        return expression.getValue(evaluationContext, Object.class);
    }

}
