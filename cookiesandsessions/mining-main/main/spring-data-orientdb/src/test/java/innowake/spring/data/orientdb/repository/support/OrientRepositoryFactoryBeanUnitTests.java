/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.support;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import org.springframework.test.util.ReflectionTestUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.repository.OrientRepository;

/**
 * Unit tests for {@link OrientRepositoryFactoryBean} class.
 */
@RunWith(MockitoJUnitRunner.class)
public class OrientRepositoryFactoryBeanUnitTests {

	/**
	 * Test default query listener.
	 *
	 */
	@Test
	@SuppressWarnings({
		"rawtypes", "unchecked"
	})
	public void testQueryCreationListenerByDefault() {
		final List<Object> listeners = getListenersFromFactory(new OrientRepositoryFactoryBean(ContactRepository.class));
		assertThat(assertNotNull(listeners).size()).isEqualTo(1);
	}

	@SuppressWarnings({
		"rawtypes", "unchecked"
	})
	@Nullable private List<Object> getListenersFromFactory(final OrientRepositoryFactoryBean factoryBean) {
		factoryBean.setLazyInit(true);
		factoryBean.afterPropertiesSet();
		final RepositoryFactorySupport factory = factoryBean.doCreateRepositoryFactory();
		return (List<Object>) ReflectionTestUtils.getField(factory, "queryPostProcessors");
	}

	interface ContactRepository extends OrientRepository<Employee> {

	}

}
