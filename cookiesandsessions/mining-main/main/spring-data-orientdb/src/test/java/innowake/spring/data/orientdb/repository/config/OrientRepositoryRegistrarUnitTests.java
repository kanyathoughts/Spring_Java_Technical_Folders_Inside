/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.config;

import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.annotation.AnnotationBeanNameGenerator;
import org.springframework.core.env.StandardEnvironment;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.lang.Nullable;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * Unit tests for {@link OrientRepositoryRegistrar}.
 */
public class OrientRepositoryRegistrarUnitTests {

	@Nullable private BeanDefinitionRegistry registry;
	@Nullable private AnnotationMetadata annotationMetadata;

	/**
	 * Initialize test set up.
	 */
	@Before
	public void setUp() {
		annotationMetadata = AnnotationMetadata.introspect(Config.class);
		registry = new DefaultListableBeanFactory();
	}

	/**
	 * Test for repository scanning.
	 */
	@Test
	public void testRepositoriesConfiguration() {

		final OrientRepositoryRegistrar orientRepositoryRegistrar = new OrientRepositoryRegistrar();
		orientRepositoryRegistrar.setResourceLoader(new DefaultResourceLoader());
		orientRepositoryRegistrar.setEnvironment(new StandardEnvironment());
		orientRepositoryRegistrar.registerBeanDefinitions(Assert.assertNotNull(annotationMetadata), Assert.assertNotNull(registry), AnnotationBeanNameGenerator.INSTANCE);
		final List<String> names = Arrays.asList(Assert.assertNotNull(registry).getBeanDefinitionNames());
		assertTrue(names.contains("employeeRepository"));
	}
}

@EnableOrientRepositories(basePackageClasses = EmployeeRepository.class)
class Config {

}

