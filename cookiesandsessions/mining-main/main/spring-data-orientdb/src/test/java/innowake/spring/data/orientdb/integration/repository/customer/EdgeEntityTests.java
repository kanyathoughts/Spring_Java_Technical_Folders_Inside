/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.customer;

import static innowake.lib.core.lang.Assert.assertEqual;
import static innowake.lib.core.lang.Assert.assertFalse;
import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.apache.commons.collections4.map.HashedMap;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.internal.matchers.apachecommons.ReflectionEquals;
import org.springframework.beans.factory.annotation.Autowired;
import com.orientechnologies.orient.core.record.OEdge;
import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Address;
import innowake.spring.data.orientdb.integration.repository.domain.Calls;
import innowake.spring.data.orientdb.integration.repository.domain.Customer;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.integration.repository.domain.Reference;
import innowake.spring.data.orientdb.integration.repository.domain.Reviewed;
import innowake.spring.data.orientdb.integration.repository.domain.Reviewed.RatingEnum;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import innowake.spring.data.orientdb.repository.CustomerRepository;
import innowake.spring.data.orientdb.repository.ProductRepository;
import innowake.spring.data.orientdb.repository.ReferenceRepository;
import innowake.spring.data.orientdb.repository.ReviewedRepository;

/**
 * Test cases for edge as entity class. Considered below relationship.
 * 
 * Customer ---- Reviewed ----> Product
 */
public class EdgeEntityTests extends AbstractIntegrationTests {
	
	@Autowired
	private ReviewedRepository edgeRepo;
	@Autowired
	private CustomerRepository fromEntityRepo;
	@Autowired
	private ProductRepository toEntityRepo;
	@Autowired
	private ReferenceRepository referenceRepository;
	
	/**
	 * Clears data saved in oreintDB.
	 */
	@Before
	public void init() {
		clearData("Customer", "Product", "Employee");
	}
	
	/**
	 * Link two vertices persisted in database by an edge.
	 */
	@Test
	public void testCreationOfEdge() {
		final Customer customer =  new Customer("Customer 1");
		final Product product = new Product("Product 1", Long.valueOf(1));
		
		/* Save entities */
		final Customer fromEntity = fromEntityRepo.save(customer);
		final Product toEntity = toEntityRepo.save(product);
		
		((IEntityProxy) fromEntity).__injectRid();
		((IEntityProxy) toEntity).__injectRid();
		final List<OEdge> edgesBefore = getEdgeFromVertex(((IEntityProxy) fromEntity).__getElement().asVertex().get());
		assertEqual(Integer.valueOf(0), Integer.valueOf(edgesBefore.size()));
		
		final Address address = new Address("12", "MG Road", "Bangalore", "560037");
		final Map<String, String> properties = new HashedMap<>();
		properties.put("key 1", "value 1");
		final Reviewed reviewed = new Reviewed(fromEntity, toEntity);
		reviewed.setDescription("Some description");
		reviewed.setRating(Arrays.asList(RatingEnum.THREE));
		reviewed.setSellerAddress(address);
		reviewed.setProperties(properties);
		final Reviewed savedEdge = edgeRepo.save(reviewed);
		
		assertNotNull(((IEntityProxy) savedEdge).__getRid());
		Optional<Customer> findById = fromEntityRepo.findById(fromEntity.getRid());
		assertEqual(Integer.valueOf(1), Integer.valueOf(assertNotNull(assertNotNull(findById.get()).getReviewed()).size()));
		assertTrue(new ReflectionEquals(savedEdge, "id", "rid", "sellerAddress").matches(reviewed));
		assertTrue(new ReflectionEquals(savedEdge.getSellerAddress()).matches(reviewed.getSellerAddress()));
	}
	

	/**
	 * Update the properties of an edge already created.
	 */
	@Test
	public void testUpdationOfEdge() {
		final Customer customer =  new Customer("Customer 1");
		final Product product = new Product("Product 1", Long.valueOf(1));
		
		/* Save entities */
		final Reviewed reviewed1 = new Reviewed(customer, product);
		customer.setReviewed(Arrays.asList(reviewed1));
		final Customer fromEntity = fromEntityRepo.save(customer);
		((IEntityProxy) fromEntity).__injectRid();
		final List<OEdge> edgesBefore = getEdgeFromVertex(((IEntityProxy) fromEntity).__getElement().asVertex().get());
		assertEqual(Integer.valueOf(1), Integer.valueOf(edgesBefore.size()));
		
		final List<Product> savedProducts = (List<Product>) toEntityRepo.findAll();
		final Address address = new Address("12", "MG Road", "Bangalore", "560037");
		final Map<String, String> properties = new HashedMap<>();
		properties.put("key 1", "value 1");
		final Reviewed reviewed = new Reviewed(fromEntity, savedProducts.get(0));
		reviewed.setDescription("Some description");
		reviewed.setRating(Arrays.asList(RatingEnum.THREE));
		reviewed.setSellerAddress(address);
		reviewed.setProperties(properties);
		final Reviewed savedEdge = edgeRepo.save(reviewed);
		
		final List<OEdge> edgesAfter = getEdgeFromVertex(((IEntityProxy) fromEntity).__getElement().asVertex().get());
		assertEqual(Integer.valueOf(1), Integer.valueOf(edgesAfter.size()));
		assertEqual(edgesBefore.get(0).getIdentity(), ((IEntityProxy) savedEdge).__getRid().getIdentity());
		
		assertTrue(new ReflectionEquals(savedEdge, "id", "rid", "sellerAddress").matches(reviewed));
		assertTrue(new ReflectionEquals(savedEdge.getSellerAddress()).matches(reviewed.getSellerAddress()));
	}
	
	/**
	 * Test case to validate if properties are updated on the edge.
	 */
	@Ignore("Will be removed as a part of projections code")
	@Test
	public void testUpdatePropertiesOnEdge() {
		final Customer customer =  new Customer("Customer 1");
		final Product product = new Product("Product 1", Long.valueOf(1));
		
		/* Save entities */
		final Customer fromEntity = fromEntityRepo.save(customer);
		final Product toEntity = toEntityRepo.save(product);
		
		((IEntityProxy) fromEntity).__injectRid();
		((IEntityProxy) toEntity).__injectRid();
		final List<OEdge> edgesBefore = getEdgeFromVertex(((IEntityProxy) fromEntity).__getElement().asVertex().get());
		assertEqual(Integer.valueOf(0), Integer.valueOf(edgesBefore.size()));
		
		final Address address = new Address("12", "MG Road", "Bangalore", "560037");
		final Map<String, String> properties = new HashedMap<>();
		properties.put("key 1", "value 1");
		final Reviewed reviewed = new Reviewed(fromEntity, toEntity);
		reviewed.setDescription("Some description");
		reviewed.setRating(Arrays.asList(RatingEnum.THREE));
		reviewed.setSellerAddress(address);
		reviewed.setProperties(properties);
		final Reviewed savedEdge = edgeRepo.save(reviewed);
		
		assertNotNull(((IEntityProxy) savedEdge).__getRid());
		
		final List<Reviewed> fetchedReviews = (List<Reviewed>) edgeRepo.findAll();
		final Reviewed fetchedReview = fetchedReviews.get(0);
		fetchedReview.setDescription("Modified Description");
		
		/* Try to modify customer object */
		final Customer customerNew =  new Customer("Customer 2");
		final Customer newEntity = fromEntityRepo.save(customerNew);
		((IEntityProxy) newEntity).__injectRid();
		fetchedReview.setOut(newEntity);
		final Reviewed updatedEdge = edgeRepo.save(fetchedReview);
		/* The data in DB is not modified but the object what user passed is returned */
		assertEqual("Customer 2", assertNotNull(updatedEdge.getOut()).getName());
		assertEqual("Modified Description", updatedEdge.getDescription());
	}
	
	/**
	 * Update the properties of an edge already created considering real entities
	 */
	@Test
	public void testUpdationOfEdgeWithModifiedVertices() {
		final Customer customer =  new Customer("Customer 1");
		final Product product = new Product("Product 1", Long.valueOf(1));
		
		/* Save entities */
		final Customer fromEntity = fromEntityRepo.save(customer);
		final Product toEntity = toEntityRepo.save(product);
		
		final String fromRid = ((IEntityProxy) fromEntity).__getRid().toString();
		final Long fromId = fromEntity.getCustomerId();
		final Long toId = toEntity.getProductId();
		final List<OEdge> edgesBefore = getEdgeFromVertex(((IEntityProxy) fromEntity).__getElement().asVertex().get());
		assertEqual(Integer.valueOf(0), Integer.valueOf(edgesBefore.size()));
		
		final Address address = new Address("12", "MG Road", "Bangalore", "560037");
		final Map<String, String> properties = new HashedMap<>();
		properties.put("key 1", "value 1");
		
		final Customer newCustomerWithRid = new Customer("Customer 2");
		newCustomerWithRid.setCustomerId(assertNotNull(fromId));
		final Product newProductWithRid = new Product("Product 1", Long.valueOf(1));
		newProductWithRid.setProductId(assertNotNull(toId));
		
		final Reviewed reviewed = new Reviewed(newCustomerWithRid, newProductWithRid);
		reviewed.setDescription("Some description");
		reviewed.setRating(Arrays.asList(RatingEnum.THREE));
		reviewed.setSellerAddress(address);
		reviewed.setProperties(properties);
		final Reviewed savedEdge = edgeRepo.save(reviewed);
		
		assertNotNull(((IEntityProxy) savedEdge).__getRid());
		Optional<Customer> findById = fromEntityRepo.findById(fromRid);
		assertEqual(Integer.valueOf(1), Integer.valueOf(assertNotNull(assertNotNull(findById.get()).getReviewed()).size()));
		assertTrue(new ReflectionEquals(savedEdge, "id", "rid", "sellerAddress", "in", "out").matches(reviewed));
		assertTrue(new ReflectionEquals(savedEdge.getSellerAddress()).matches(reviewed.getSellerAddress()));
	}
	
	/**
	 * Test case to fetch already saved edges via vertices.
	 */
	@Test
	public void testFindAllAndCount() {
		final Customer customer1 = new Customer("customer 1");
		final Customer customer2 = new Customer("customer 2");
		
		final Product product1 = new Product("product 1 1", Long.valueOf(1));
		final Product product2 = new Product("product 1 2", Long.valueOf(1));
		final Product product4 = new Product("product 3 1", Long.valueOf(3));
		
		final Reviewed reviewed1 = new Reviewed(customer1, product1);
		final Reviewed reviewed2 = new Reviewed(customer1, product2);
		final Reviewed reviewed3 = new Reviewed(customer2, product4);
		customer1.setReviewed(Arrays.asList(reviewed1, reviewed2));
		customer2.setReviewed(Arrays.asList(reviewed3));
		
		final List<Customer> customers = Arrays.asList(customer1, customer2);
		fromEntityRepo.saveAll(customers);
		
		final long totalEdges = edgeRepo.count();
		assertTrue(totalEdges == 3);
		
		final List<Reviewed> reviewed = (List<Reviewed>) edgeRepo.findAll();
		assertTrue(reviewed.size() == 3);
		assertTrue(assertNotNull(reviewed.get(0).getOut()).getName().startsWith("customer "));
		assertTrue(assertNotNull(reviewed.get(0).getIn()).getProductName().startsWith("product "));
	}
	
	/**
	 * Test case to delete edge.
	 */
	@Test
	public void testDeleteEdge() {
		final Customer customer1 = new Customer("customer 1");
		final Product product1 = new Product("product 1 1", Long.valueOf(1));
		final Reviewed reviewed1 = new Reviewed(customer1, product1);
		customer1.setReviewed(Arrays.asList(reviewed1));
		final Customer savedCustomer = fromEntityRepo.save(customer1);
		((IEntityProxy) savedCustomer).__injectRid();
		
		final Customer fetchedCustomerBeforeEdgeDeletion = fromEntityRepo.findById(savedCustomer.getRid()).orElseThrow(RuntimeException::new);
		assertNotNull(fetchedCustomerBeforeEdgeDeletion.getReviewed());
		
		final List<Reviewed> reviewed = (List<Reviewed>) edgeRepo.findAll();
		final String rid = assertNotNull(reviewed.get(0).getRid());
		assertTrue(edgeRepo.existsById(rid));
		edgeRepo.deleteById(rid);
		assertFalse(edgeRepo.existsById(rid));
		
		final Customer fetchedCustomer = fromEntityRepo.findById(savedCustomer.getRid()).orElseThrow(RuntimeException::new);
		assertTrue(fetchedCustomer.getReviewed() == null);
	}
	
	/**
	 * Test case for linked list.
	 */
	@Test
	public void testLinkedList() {
		final Customer customer1 = new Customer("customer 1");
		final Customer customer2 = new Customer("customer 2");
		
		final Calls calls1 = new Calls(customer1, customer2);
		customer1.setOutCalls(calls1);
		fromEntityRepo.save(customer1);
		
		final List<Reference> referenceList = (List<Reference>) referenceRepository.findAll();
		assertTrue(referenceList.get(0) instanceof Calls);
		final Calls calls = ((Calls) referenceList.get(0));
		
		final Employee employee1 = new Employee("user 1", "user 1", "user 1");
		final Employee employee2 = new Employee("user 2", "user 2", "user 2");
		final Employee[] employees = {employee1, employee2};
		calls.setLinkList(Arrays.asList(employees));
		referenceRepository.save(calls);
		
		final List<Reference> referenceListWithCustomProperties = (List<Reference>) referenceRepository.findAll();
		final Calls callsReference = assertNotNull((Calls) referenceListWithCustomProperties.get(0));
		final List<Employee> fetchedList = assertNotNull(callsReference.getLinkList());
		assertTrue(fetchedList.size() == 2);
		assertNotNull(fetchedList.get(0).getRid());
	}
	
}
