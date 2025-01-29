/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server;

import org.junit.jupiter.api.Test;

import java.util.Set;

import javax.annotation.security.RolesAllowed;

import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.core.env.StandardEnvironment;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.mining.server.controller.MiningUnsecuredRestController;
import innowake.mining.server.controller.MiningUnsecuredRestEndpoint;

import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.MethodMetadata;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.security.access.annotation.Secured;

/**
 * Test class to check for potentially insecurely implemented Controller endpoints.
 */
class ControllerSecurityTest {
	
	private static final String CONTROLLER_BASE_PACKAGE = "innowake.mining.server.controller";
	private static final String GRAPHQL_CONTROLLER_BASE_PACKAGE = "innowake.mining.server.graphql.controller";
	
	@Test
	void verifyRestControllers() {
		verifyControllersInPackage(CONTROLLER_BASE_PACKAGE);
	}
	
	@Test
	void verifyGraphQlControllers() {
		verifyControllersInPackage(GRAPHQL_CONTROLLER_BASE_PACKAGE);
	}
	
	private void verifyControllersInPackage(final String packageName) {
		final ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(/* use default filter */ false);
		scanner.addIncludeFilter(new AnnotationTypeFilter(Controller.class, /* consider meta annotations (indirect) */ true));
		scanner.setEnvironment(new StandardEnvironment() {
			@Override
			protected boolean isProfileActive(String profile) {
				/* evaluate all components regardless of profile */
				return true;
			}
		});
		
		final IllegalStateException securityIssues = new IllegalStateException("Improperly secured endpoints found."
				+ " Please make sure to use either @MiningRestController or @MiningUnsecuredRestController on classes"
				+ " and @Role, @Nature, @RolesAllowed or @MiningUnsecuredRestEndpoint on methods.");
		
		/* check Controllers in specified package and sub-packages */
		final Set<BeanDefinition> beanDefinitions = scanner.findCandidateComponents(packageName);
		beanDefinitions.forEach(def -> {
			if ( ! (def instanceof AnnotatedBeanDefinition)) {
				throw new IllegalStateException("Unable to inspect " + def.getBeanClassName());
			}
			final AnnotatedBeanDefinition annotatedDef = (AnnotatedBeanDefinition) def;
			
			/* find REST endpoints on the Controller */
			final Set<MethodMetadata> requestMappingMethods = annotatedDef.getMetadata().getAnnotatedMethods(RequestMapping.class.getName());
			if ( ! (requestMappingMethods.isEmpty() || annotatedDef.getMetadata().isAnnotated(Secured.class.getName()))) {
				/* if class has @RequestMapping methods but is not annotated with @Secured ... */
				if ( ! annotatedDef.getMetadata().isAnnotated(MiningUnsecuredRestController.class.getName())) {
					securityIssues.addSuppressed(new Exception("Controller " + def.getBeanClassName() + " is not annotated properly."));
				}
				
				/* ... each @RequestMapping method must itself be secured or explicitly be not secured. */
				requestMappingMethods.stream().forEach(method -> { 
					if ( ! (method.isAnnotated(Secured.class.getName()) || method.isAnnotated(RolesAllowed.class.getName())
							|| method.isAnnotated(MiningUnsecuredRestEndpoint.class.getName()))) {
						securityIssues.addSuppressed(new Exception(
								"REST endpoint " + method.getMethodName() + "() in controller " + def.getBeanClassName() + " is not properly secured."));
					}
				});
			}
			
			/* find GraphQL endpoints on the Controller */
			final Set<MethodMetadata> queryMappingMethods = annotatedDef.getMetadata().getAnnotatedMethods(QueryMapping.class.getName());
			if ( ! (queryMappingMethods.isEmpty() || annotatedDef.getMetadata().isAnnotated(Secured.class.getName()))) {
				/* if class has @QueryMapping methods but is not annotated with @Secured */
				/* then each @QueryMapping method must itself be annotated with @Secured. */
				queryMappingMethods.stream().forEach(method -> {
					if ( ! method.isAnnotated(Secured.class.getName())) {
						securityIssues.addSuppressed(new Exception(
								"GraphQL endpoint " + method.getMethodName() + "in controller " + def.getBeanClassName() + " is not properly secured."
								+ " Please make sure to use @MiningQueryMapping instead of @QueryMapping."));
					}
				});
			}
		});
		
		if (securityIssues.getSuppressed().length > 0) {
			throw securityIssues;
		}
	}
	
}
