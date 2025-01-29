/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.config;

import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.Collections;

import org.springframework.data.repository.config.RepositoryConfigurationExtension;
import org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport;

import innowake.mining.shared.springdata.annotations.Entity;
import innowake.spring.data.orientdb.repository.OrientRepository;
import innowake.spring.data.orientdb.repository.support.OrientRepositoryFactoryBean;

/**
 * Orient specific configuration extension parsing custom attributes from {@link EnableOrientRepositories} annotation.
 * {@link RepositoryConfigurationExtension} for OrientDB.
 */
public class OrientRepositoryConfigurationExtension extends RepositoryConfigurationExtensionSupport {

	private static final String MODULE_NAME = "orient";

	@Override
	public String getRepositoryFactoryBeanClassName() {
		return OrientRepositoryFactoryBean.class.getName();
	}

	@Override
	protected String getModulePrefix() {
		return MODULE_NAME;
	}

	@Override
	protected Collection<Class<? extends Annotation>> getIdentifyingAnnotations() {
		return Collections.singleton(Entity.class);
	}
	
	@Override
	protected Collection<Class<?>> getIdentifyingTypes() {
		return Collections.singleton(OrientRepository.class);
	}

}
