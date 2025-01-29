/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.config;

import java.lang.annotation.Annotation;

import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.data.repository.config.RepositoryBeanDefinitionRegistrarSupport;
import org.springframework.data.repository.config.RepositoryConfigurationExtension;

/**
 * Orient specific implementation of {@link ImportBeanDefinitionRegistrar}.
 */
public class OrientRepositoryRegistrar extends RepositoryBeanDefinitionRegistrarSupport {

	@Override
	protected Class<? extends Annotation> getAnnotation() {
		return EnableOrientRepositories.class;
	}

	@Override
	protected RepositoryConfigurationExtension getExtension() {
		return new OrientRepositoryConfigurationExtension();
	}

}
