/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.config;

import static org.junit.jupiter.api.Assertions.fail;

import java.util.Collection;

import org.junit.Test;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.core.env.Environment;
import org.springframework.core.env.StandardEnvironment;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.config.AnnotationRepositoryConfigurationSource;
import org.springframework.data.repository.config.RepositoryConfiguration;
import org.springframework.data.repository.config.RepositoryConfigurationSource;

import innowake.mining.shared.springdata.annotations.Entity;
import innowake.spring.data.orientdb.repository.OrientRepository;

/**
 * Unit tests for {@link OrientRepositoryConfigurationExtension}.
 */
public class OrientRepositoryConfigExtensionUnitTests {

	private final AnnotationMetadata metadata = AnnotationMetadata.introspect(Config.class);
	private final ResourceLoader loader = new PathMatchingResourcePatternResolver();
	private final Environment environment = new StandardEnvironment();
	private final BeanDefinitionRegistry registry = new DefaultListableBeanFactory();
	private final RepositoryConfigurationSource configurationSource = new AnnotationRepositoryConfigurationSource(metadata,
			EnableOrientRepositories.class, loader, environment, registry, null);

	/**
	 * Test case to ensure if the repository is scanned, if it has a specific entity.
	 */
	@Test
	public void isStrictMatchIfDomainTypeIsAnnotatedWithEntity() {
		final OrientRepositoryConfigurationExtension extension = new OrientRepositoryConfigurationExtension();
		assertHasRepo(SampleRepository.class, extension.getRepositoryConfigurations(configurationSource, loader, true));
	}

	/**
	 * Test case to ensure if the repository is scanned, for a generic entity with {@link OrientRepository}.
	 */
	@Test 
	public void isStrictMatchIfRepositoryExtendsStoreSpecificBase() {
		final OrientRepositoryConfigurationExtension extension = new OrientRepositoryConfigurationExtension();
		assertHasRepo(StoreRepository.class, extension.getRepositoryConfigurations(configurationSource, loader, true));
	}

	/**
	 * Test case to ensure the repository is not scanned, if it has a generic entity.
	 */
	@Test
	public void isNotStrictMatchIfDomainTypeIsNotAnnotatedWithEntity() {
		final OrientRepositoryConfigurationExtension extension = new OrientRepositoryConfigurationExtension();
		assertDoesNotHaveRepo(UnannotatedRepository.class,
				extension.getRepositoryConfigurations(configurationSource, loader, true));
	}

	private static void assertHasRepo(final Class<?> repositoryInterface,
			final Collection<RepositoryConfiguration<RepositoryConfigurationSource>> configs) {
		for (final RepositoryConfiguration<?> config : configs) {
			if (config.getRepositoryInterface().equals(repositoryInterface.getName())) {
				return;
			}
		}
		fail("Expected to find config for repository interface ".concat(repositoryInterface.getName()).concat(" but got ")
				.concat(configs.toString()));
	}

	private static void assertDoesNotHaveRepo(final Class<?> repositoryInterface,
			final Collection<RepositoryConfiguration<RepositoryConfigurationSource>> configs) {
		for (final RepositoryConfiguration<?> config : configs) {
			if (config.getRepositoryInterface().equals(repositoryInterface.getName())) {
				fail("Expected not to find config for repository interface ".concat(repositoryInterface.getName()));
			}
		}
	}

	@EnableOrientRepositories(considerNestedRepositories = true)
	static class Config {

	}

	@Entity
	static class Sample {}

	interface SampleRepository extends Repository<Sample, String> {}

	interface UnannotatedRepository extends Repository<Object, String> {}

	interface StoreRepository extends OrientRepository<Object> {}
}
