/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.customer;

import static innowake.lib.core.lang.Assert.assertEqual;
import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.map.HashedMap;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import com.orientechnologies.orient.core.record.OEdge;
import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Calls;
import innowake.spring.data.orientdb.integration.repository.domain.Customer;
import innowake.spring.data.orientdb.integration.repository.domain.Includes;
import innowake.spring.data.orientdb.integration.repository.domain.ModuleLocation;
import innowake.spring.data.orientdb.integration.repository.domain.Reference;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import innowake.spring.data.orientdb.repository.CustomerRepository;
import innowake.spring.data.orientdb.repository.ReferenceRepository;

/**
 * Test cases for testing inheritance of edges.
 */
public class EdgeInheritanceTests extends AbstractIntegrationTests {
	
	@Autowired
	private ReferenceRepository referenceRepository;
	@Autowired
	private CustomerRepository customerRepository;
	
	/**
	 * Clears data saved in oreintDB.
	 */
	@Before
	public void init() {
		clearData("Customer");
	}
	
	/**
	 * Test fetching {@link Calls} using {@link Reference} repository.
	 */
	@Test
	public void testRetrieveFunction() {
		final Customer customer1 = new Customer("customer 1");
		final Customer customer2 = new Customer("customer 2");
		
		final Calls call = new Calls(customer2, customer1);
		customer1.setOutCalls(call);
		customerRepository.save(customer1);
		
		final List<Reference> referenceList = (List<Reference>) referenceRepository.findAll();
		assertTrue(referenceList.get(0) instanceof Calls);
		final Calls calls = ((Calls) referenceList.get(0));
		assertEqual("customer 2", assertNotNull(calls.getIn()).getName());
		assertEqual("customer 1", assertNotNull(calls.getOut()).getName());
	}
	
	
	/**
	 * Test fetching calls using reference repository.
	 */
	@Test
	public void testSaveCallsWithReferenceValues() {
		final Customer customer1 =  new Customer("Customer 1");
		final Customer customer2 = new Customer("Customer 2");
		final Customer customer3 = new Customer("Customer 3");
		final Customer customer4 = new Customer("Customer 4");
		
		/* Save entities */
		final Customer fromEntity = customerRepository.save(customer1);
		final Customer toEntity = customerRepository.save(customer2);
		final Customer fromEntity1 = customerRepository.save(customer3);
		final Customer toEntity1 = customerRepository.save(customer4);
		
		((IEntityProxy) fromEntity).__injectRid();
		((IEntityProxy) toEntity).__injectRid();
		((IEntityProxy) fromEntity1).__injectRid();
		((IEntityProxy) toEntity1).__injectRid();
		
		final List<OEdge> edgesBefore = getEdgeFromVertex(((IEntityProxy) fromEntity).__getElement().asVertex().get());
		assertEqual(Integer.valueOf(0), Integer.valueOf(edgesBefore.size()));
		
		final ModuleLocation fromModuleLocationLink = new ModuleLocation(Integer.valueOf(10), Integer.valueOf(15));
		final ModuleLocation toModuleLocation = new ModuleLocation(Integer.valueOf(20), Integer.valueOf(30));
		final ModuleLocation fromModuleLocationLink1 = new ModuleLocation(Integer.valueOf(15), Integer.valueOf(32));
		final ModuleLocation toModuleLocation1 = new ModuleLocation(Integer.valueOf(5), Integer.valueOf(10));
		
		final Map<String, String> properties = new HashedMap<>();
		properties.put("key 1", "value 1");
		properties.put("key 2", "value 2");
		final Calls calls = new Calls(fromEntity, toEntity, fromModuleLocationLink, toModuleLocation, properties);
		final Includes includes = new Includes(fromEntity1, toEntity1, fromModuleLocationLink1, toModuleLocation1, properties);
		
		referenceRepository.save(calls);
		referenceRepository.save(includes);
		
		final List<Reference> referenceList = (List<Reference>) referenceRepository.findAll();
		if (referenceList.get(0) instanceof Calls) {
			assertCallsData((Calls) referenceList.get(0));
			assertIncludesData((Includes) referenceList.get(1));
		} else if (referenceList.get(0) instanceof Includes) {
			assertIncludesData((Includes) referenceList.get(0));
			assertCallsData((Calls) referenceList.get(1));
		}
		
		final Long count = Long.valueOf(referenceRepository.count());
		assertEqual(Long.valueOf(2), count);
	}
	
	private void assertCallsData(final Calls calls) {
		assertEqual("Customer 1", assertNotNull(calls.getIn()).getName());
		assertEqual("Customer 2", assertNotNull(calls.getOut()).getName());
	}
	
	private void assertIncludesData(final Includes includes) {
		assertEqual("Customer 3", assertNotNull(includes.getIn()).getName());
		assertEqual("Customer 4", assertNotNull(includes.getOut()).getName());
	}
	

}
