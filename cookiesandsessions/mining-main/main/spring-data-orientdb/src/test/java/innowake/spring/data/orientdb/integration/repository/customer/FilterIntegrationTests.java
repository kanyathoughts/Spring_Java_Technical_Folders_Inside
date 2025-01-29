/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.customer;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import innowake.mining.shared.springdata.EdgeDirection;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;
import innowake.spring.data.orientdb.api.query.clause.OrientOperator;
import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;
import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Account;
import innowake.spring.data.orientdb.integration.repository.domain.Address;
import innowake.spring.data.orientdb.integration.repository.domain.Customer;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.integration.repository.domain.Reviewed;
import innowake.spring.data.orientdb.repository.CustomerRepository;
import innowake.spring.data.orientdb.repository.ProductRepository;

/**
 * Test cases for filtering data using spring data clauses.
 */
public class FilterIntegrationTests extends AbstractIntegrationTests {

	@Autowired
	private CustomerRepository customerRepository;
	
	@Autowired
	private ProductRepository productRepository;

	/**
	 * Clears data saved in oreintDB.
	 */
	@Before
	public void init() {
		clearData("Customer", "Product", "Account");
	}

	/**
	 * Test case for collection of customer objects returned.
	 */
	@Test
	public void testFiltering() {

		final Date date = new Date();
		final Account account1 = new Account("AE123445", "American Express", date);
		final Account account2 = new Account("HC12345", "HSBC", date);
		final Account account3 = new Account("AE2445", "American Express", date);

		final Product product1 = new Product("product 1 1", Long.valueOf(1));
		final Product product2 = new Product("product 1 2", Long.valueOf(1));
		final Product product3 = new Product("product 2 1", Long.valueOf(2));
		final Product product4 = new Product("product 3 1", Long.valueOf(3));

		final String pincode = "123455";
		final Address address1 = new Address("12", "street1", "city 1", pincode);
		final Address address2 = new Address("14", "street2", "city 2", pincode);
		final Address address3 = new Address("14", "street2", "city 2", "34546");

		final Customer customer1 = new Customer("customer 1", address1);
		final Customer customer2 = new Customer("customer 2", address2);
		final Customer customer3 = new Customer("customer 3", address3);

		customer1.setCustomerAccount(account1);
		customer2.setCustomerAccount(account2);
		customer3.setCustomerAccount(account3);

		customer1.setProducts(Arrays.asList(product1, product3, product2));
		customer2.setProducts(Arrays.asList(product3, product4));
		customer3.setProducts(Arrays.asList(product2, product4));

		final Reviewed reviewed1 =  new Reviewed(customer1, product1);
		final Reviewed reviewed2 =  new Reviewed(customer1, product2);
		final Reviewed reviewed3 =  new Reviewed(customer2, product4);
		customer1.setReviewed(Arrays.asList(reviewed1, reviewed2));
		customer2.setReviewed(Arrays.asList(reviewed3));

		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);
		customerRepository.saveAll(customers);

		/* Filter by name, city, account */
		final OrientClause nameClause = OrientClause.clause(Customer.class, "customerName", OrientOperator.LIKE, "customer%");
		final OrientClause cityClause = OrientClause.clause(Customer.class, "address.city", OrientOperator.EQ, "city 2");
		final OrientClause accountClause = OrientClause.clause(Customer.class, "account.cardName", OrientOperator.EQ, "American Express");
		final OrientClause clauses = OrientClause.or(nameClause, cityClause);
		final OrientClause clauseNew = OrientClause.and(clauses, accountClause);

		final PageRequest oneElementPageRequest = PageRequest.of(0, 10);
		final Page<Customer> page = customerRepository.findAll(clauseNew, oneElementPageRequest);
		assertEquals(Integer.valueOf(2), Integer.valueOf(page.getContent().size()));
		assertEquals("American Express", assertNotNull(page.getContent().get(0).getCustomerAccount()).getCardName());

		/* Filter by products reviewed */
		final OrientClause subClause = OrientClause.clause(Product.class, "productName", OrientOperator.CONTAINS, "product 1 1");
		final OrientClause edgeClause = OrientClause.edgeClause("reviewed", EdgeDirection.OUT, subClause);
		final Page<Customer> result = customerRepository.findAll(edgeClause, oneElementPageRequest);
		assertEquals(Integer.valueOf(1), Integer.valueOf(result.getContent().size()));
		assertEquals("product 1 1", assertNotNull(assertNotNull(result.getContent().get(0).getReviewed()).get(0).getIn()).getProductName());

		/* Filter link lists */
		final OrientClause linkClause = OrientClause.clause(Customer.class, "products.productName", OrientOperator.CONTAINS, "product 2 1");
		final Page<Customer> linkResult = customerRepository.findAll(linkClause, oneElementPageRequest);
		assertEquals(Integer.valueOf(2), Integer.valueOf(linkResult.getContent().size()));
	}

	/**
	 * Test case for embedded map filter.
	 */
	@Test
	public void testFilterWithEmbeddedMap() {
		final Customer customer1 = new Customer("User 1");
		final Customer customer2 = new Customer("User 2");
		final Customer customer3 = new Customer("User 3");
		final Customer customer4 = new Customer("User 4");
		final Map<String, String> embeddedMap1 =  new HashMap<>();
		final Map<String, String> embeddedMap2 =  new HashMap<>();
		final Map<String, String> embeddedMap3 =  new HashMap<>();
		final Map<String, String> embeddedMap4 =  new HashMap<>();
		final String key1 = "Key 1";
		embeddedMap1.put(key1, "Value 1");
		embeddedMap1.put("Key 2", "Value 2");
		customer1.setEmbeddedMap(embeddedMap1);

		embeddedMap2.put(key1, "Value 3");
		embeddedMap2.put("Key 2", "Value 2");
		customer2.setEmbeddedMap(embeddedMap2);

		embeddedMap3.put("Key 3", "Value 1");
		customer3.setEmbeddedMap(embeddedMap3);

		embeddedMap4.put(key1, "Value 1");
		customer4.setEmbeddedMap(embeddedMap4);

		final List<Customer> customers = new LinkedList<>(Arrays.asList(customer1, customer2, customer3, customer4));
		final List<Customer> savedCustomer = (List<Customer>) customerRepository.saveAll(customers);
		assertCollectionObjects(customers, savedCustomer, "customerId");

		final OrientClause mapClause = OrientClause.embeddedMapClasue(Customer.class, "embeddedMap", key1, "Value 1");
		final Page<Customer> fetchedCustomers = customerRepository.findAll(mapClause, Pageable.unpaged());
		assertEquals(2, fetchedCustomers.getSize());
	}

	/**
	 * Test case to execute embedded set clause.
	 */
	@Test
	public void testFilterWithEmbeddedSetClause() {
		final Customer customer1 = new Customer("User 19");
		final Customer customer2 = new Customer("User 20");
		final Customer customer3 = new Customer("User 21");
		final String commonValue = "Data 4";
		final Set<String> embeddedSet1 = new HashSet<>();
		embeddedSet1.add("Data 3");
		embeddedSet1.add(commonValue);
		customer1.setEmbeddedSet(embeddedSet1);
		final Set<String> embeddedSet2 = new HashSet<>();
		embeddedSet2.add("Data 5");
		embeddedSet2.add("Data 6");
		customer2.setEmbeddedSet(embeddedSet2);
		final Set<String> embeddedSet3 = new HashSet<>();
		embeddedSet3.add(commonValue);
		embeddedSet3.add("Data 7");
		customer3.setEmbeddedSet(embeddedSet3);

		final List<Customer> customers = new LinkedList<>(Arrays.asList(customer1, customer2, customer3));
		final List<Customer> savedCustomer = (List<Customer>) customerRepository.saveAll(customers);
		assertCollectionObjects(customers, savedCustomer, "customerId");

		final OrientClause setClause = OrientClause.embeddedSetClause(Customer.class, "embeddedSet", commonValue);

		final Page<Customer> fetchedCustomers = customerRepository.findAll(setClause, Pageable.unpaged());
		assertEquals(2, fetchedCustomers.getSize());
		assertTrue(assertNotNull(fetchedCustomers.getContent().get(0).getEmbeddedSet()).contains(commonValue));
	}

	/**
	 * Test case to execute embedded set clause - error.
	 */
	@Test(expected = UnsupportedQueryTypeException.class)
	public void testFilterWithEmbeddedSetClauseError() {
		final Customer customer1 = new Customer("User 19");
		final Customer customer2 = new Customer("User 20");
		final Customer customer3 = new Customer("User 21");
		final String commonValue = "Data 4";
		final Set<String> embeddedSet1 = new HashSet<>();
		embeddedSet1.add("Data 3");
		embeddedSet1.add(commonValue);
		customer1.setEmbeddedSet(embeddedSet1);
		final Set<String> embeddedSet2 = new HashSet<>();
		embeddedSet2.add("Data 5");
		embeddedSet2.add("Data 6");
		customer2.setEmbeddedSet(embeddedSet2);
		final Set<String> embeddedSet3 = new HashSet<>();
		embeddedSet3.add(commonValue);
		embeddedSet3.add("Data 7");
		customer3.setEmbeddedSet(embeddedSet3);

		final List<Customer> customers = new LinkedList<>(Arrays.asList(customer1, customer2, customer3));
		final List<Customer> savedCustomer = (List<Customer>) customerRepository.saveAll(customers);
		assertCollectionObjects(customers, savedCustomer, "customerId");

		final OrientClause setClause = OrientClause.embeddedSetClause(Customer.class, "embeddedSet", Long.valueOf(15));
		customerRepository.findAll(setClause, Pageable.unpaged());
	}

	/**
	 * Test CONTAINS clause for linked collection fields.
	 */
	@Test
	public void testFilterByContainsClause() {
		final Product product1 = new Product("IPhone", Long.valueOf(06));
		final Product product2 = new Product("Oneplus", Long.valueOf(23));
		final Product product3 = new Product("Nokia", Long.valueOf(17));
		final Customer customer1 = new Customer("User 11", Arrays.asList(product1, product3));
		final Customer customer2 = new Customer("User 12", Arrays.asList(product3, product2));
		final Customer customer3 = new Customer("User 13", Arrays.asList(product1, product2));
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());

		final OrientClause subClause = OrientClause.clause(Product.class, "productName", OrientOperator.EQ, "IPhone");
		final OrientClause clause = OrientClause.containsClause(Customer.class, "products", subClause);
		final Page<Customer> fetchedCustomers = customerRepository.findAll(clause, Pageable.unpaged());
		assertEquals(2, fetchedCustomers.getSize());
	}

	/**
	 * Test CONTAINS clause for linked collection fields - Negative.
	 */
	@Test(expected = UnsupportedQueryTypeException.class)
	public void testFilterByContainsClauseError() {
		final Product product1 = new Product("IPhone", Long.valueOf(06));
		final Product product2 = new Product("Oneplus", Long.valueOf(23));
		final Product product3 = new Product("Nokia", Long.valueOf(17));
		final Customer customer1 = new Customer("User 11", Arrays.asList(product1, product3));
		final Customer customer2 = new Customer("User 12", Arrays.asList(product3, product2));
		final Customer customer3 = new Customer("User 13", Arrays.asList(product1, product2));
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());

		final OrientClause subClause = OrientClause.clause(Product.class, "productName", OrientOperator.EQ, "IPhone");
		final OrientClause clause = OrientClause.containsClause(Customer.class, "product", subClause);
		customerRepository.findAll(clause, Pageable.unpaged());
	}

	/**
	 * Test IN clause.
	 */
	@Test
	public void testFilterByInClause() {
		final Customer customer1 = new Customer("Customer 1");
		final Customer customer2 = new Customer("Customer 2");
		final Customer customer3 = new Customer("Customer 3");
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());

		final String[] values = {"Customer 1", "Customer 3"};
		final OrientClause clause = OrientClause.inClause(Customer.class, "customerName", values);
		final Page<Customer> fetchedCustomers = customerRepository.findAll(clause, Pageable.unpaged());
		assertEquals(2, fetchedCustomers.getSize());
	}

	/**
	 * Test IN clause - Negative.
	 */
	@Test(expected = UnsupportedQueryTypeException.class)
	public void testFilterByInClauseError() {
		final Customer customer1 = new Customer("Customer 1");
		final Customer customer2 = new Customer("Customer 2");
		final Customer customer3 = new Customer("Customer 3");
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());

		final Integer[] values = {Integer.valueOf(1), Integer.valueOf(3)};
		final OrientClause clause = OrientClause.inClause(Customer.class, "customerName", values);
		customerRepository.findAll(clause, Pageable.unpaged());
	}

	/**
	 * Test IN clause with List data.
	 */
	@Test
	public void testFilterByInClauseWithList() {
		final Customer customer1 = new Customer("Customer 1");
		final Customer customer2 = new Customer("Customer 2");
		final Customer customer3 = new Customer("Customer 3");
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());

		final List<String> filterCustomers = new ArrayList<>();
		filterCustomers.add("Customer 1");
		filterCustomers.add("Customer 3");
		final OrientClause clause = OrientClause.inClause(Customer.class, "customerName", filterCustomers);
		final Page<Customer> fetchedCustomers = customerRepository.findAll(clause, Pageable.unpaged());
		assertEquals(2, fetchedCustomers.getSize());
	}

	/**
	 * Test IN clause with List values - Negative.
	 */
	@Test(expected = UnsupportedQueryTypeException.class)
	public void testFilterByInClauseListError() {
		final Customer customer1 = new Customer("Customer 1");
		final Customer customer2 = new Customer("Customer 2");
		final Customer customer3 = new Customer("Customer 3");
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());

		final List<Integer> filterCustomers = new ArrayList<>();
		filterCustomers.add(Integer.valueOf(1));
		filterCustomers.add(Integer.valueOf(3));
		final OrientClause clause = OrientClause.inClause(Customer.class, "customerName", filterCustomers);
		customerRepository.findAll(clause, Pageable.unpaged());
	}

	/**
	 * Test NOT IN clause.
	 */
	@Test
	public void testFilterByNotInClause() {
		final Customer customer1 = new Customer("Customer 1");
		final Customer customer2 = new Customer("Customer 2");
		final Customer customer3 = new Customer("Customer 3");
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());

		final String[] values = {"Customer 1", "Customer 3"};
		final OrientClause clause = OrientClause.notInClause(Customer.class, "customerName", values);
		final Page<Customer> fetchedCustomers = customerRepository.findAll(clause, Pageable.unpaged());
		assertEquals(1, fetchedCustomers.getSize());

		/* checking with list */
		final List<String> filterCustomers = new ArrayList<>();
		filterCustomers.add("Customer 1");
		filterCustomers.add("Customer 3");
		final OrientClause listClause = OrientClause.notInClause(Customer.class, "customerName", filterCustomers);
		final Page<Customer> fetchedListCustomers = customerRepository.findAll(listClause, Pageable.unpaged());
		assertEquals(1, fetchedListCustomers.getSize());
	}

	/**
	 * Test NOT Equal clause.
	 */
	@Test
	public void testFilterByNotEqualClause() {
		final Customer customer1 = new Customer("Customer 1");
		final Customer customer2 = new Customer("Customer 2");
		final Customer customer3 = new Customer("Test Customer 1");
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());

		final OrientClause clause = OrientClause.not(OrientClause.clause(Customer.class, "customerName", OrientOperator.LIKE, "T%"));
		final Page<Customer> fetchedCustomers = customerRepository.findAll(clause, Pageable.unpaged());
		assertEquals(2, fetchedCustomers.getSize());
	}
	
	/**
	 * Test greater/smaller than, greater/smaller than and equal clause.
	 */
	@Test
	public void testFilterByComparisonClauses() {
		final Product product1 = new Product("product 1", Long.valueOf(1));
		final Product product2 = new Product("product 2", Long.valueOf(2));
		final Product product3 = new Product("product 3", Long.valueOf(3));
		
		final List<Product> products = Arrays.asList(product1, product2, product3);

		final List<Product> savedProducts = (List<Product>) productRepository.saveAll(products);
		assertEquals(3, savedProducts.size());
		
		/* Greater than */
		final OrientClause clause = OrientClause.clause("productCode ", OrientOperator.GT, "2");
		final Page<Product> fetchedProducts = productRepository.findAll(clause, Pageable.unpaged());
		assertEquals(1, fetchedProducts.getSize());
		
		/* Smaller than */
		final OrientClause clause1 = OrientClause.clause("productCode ", OrientOperator.LT, "2");
		final Page<Product> fetchedProducts1 = productRepository.findAll(clause1, Pageable.unpaged());
		assertEquals(1, fetchedProducts1.getSize());
		
		/* Greater than or equal */
		final OrientClause clause2 = OrientClause.clause("productCode ", OrientOperator.GE, "2");
		final Page<Product> fetchedProducts2 = productRepository.findAll(clause2, Pageable.unpaged());
		assertEquals(2, fetchedProducts2.getSize());
		
		/* Smaller than or equal */
		final OrientClause clause3 = OrientClause.clause("productCode ", OrientOperator.LE, "2");
		final Page<Product> fetchedProducts3 = productRepository.findAll(clause3, Pageable.unpaged());
		assertEquals(2, fetchedProducts3.getSize());
	}

}
