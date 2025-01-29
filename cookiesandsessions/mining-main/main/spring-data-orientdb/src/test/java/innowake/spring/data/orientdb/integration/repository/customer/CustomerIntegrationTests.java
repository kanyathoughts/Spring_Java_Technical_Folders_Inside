/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.customer;

import static innowake.lib.core.lang.Assert.assertFalse;
import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.Before;
import org.junit.Test;
import org.mockito.internal.matchers.apachecommons.ReflectionEquals;
import org.springframework.beans.factory.annotation.Autowired;
import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.record.ODirection;
import com.orientechnologies.orient.core.record.OVertex;
import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Account;
import innowake.spring.data.orientdb.integration.repository.domain.Address;
import innowake.spring.data.orientdb.integration.repository.domain.Customer;
import innowake.spring.data.orientdb.integration.repository.domain.OrderedBy;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.integration.repository.domain.Reviewed;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import innowake.spring.data.orientdb.repository.CustomerRepository;
import innowake.spring.data.orientdb.repository.ProductRepository;

/**
 * Test cases for {@link CustomerRepository} class.
 */
public class CustomerIntegrationTests extends AbstractIntegrationTests {

	@Autowired
	private CustomerRepository customerRepository;
	@Autowired
	private ProductRepository productRepository;
	private final Product product1 = new Product("IPhone", Long.valueOf(12));
	private final Product product2 = new Product("Samsung galaxy", Long.valueOf(15));
	private final Address address = new Address("221B", "Baker Street", "London", "560037");
	private final Customer customer = new Customer("User 4", Arrays.asList(product1, product2));
	private static final String IGNORE_PRODUCTS_FIELD = "products";
	private static final String IGNORE_CUSTOMER_ID_FIELD = "customerId";

	/**
	 * Clears data saved in oreintDB.
	 */
	@Before
	public void init() {
		clearData("Customer", "Product", "Account");
	}

	/**
	 * Saves entity containing relationship entities without edges, as direct links.
	 * Entity returned after saving does not contain rid and generated id's. 
	 * The relationship entities rid and generated id's are updated only if its a collection type.
	 */
	@Test
	public void testSaveCustomer() {
		final Customer savedCustomer = customerRepository.save(customer);
		assertTrue(new ReflectionEquals(customer, IGNORE_PRODUCTS_FIELD, "customerId").matches(savedCustomer));
		assertCustomerProducts(savedCustomer);
	}

	/**
	 * Retrieves entity containing relationship entities without edges, as direct links.
	 * Only the retrieved values have populated rid and id's.
	 */
	@Test
	public void testRetrieveCustomer() {
		final Customer savedCustomer = customerRepository.save(customer);
		final ORID rid = ((IEntityProxy)savedCustomer).__getRid();
		final Customer fetchedCustomer = customerRepository.findById(rid).orElseThrow(NullPointerException::new);
		assertTrue(new ReflectionEquals(savedCustomer, "rid", IGNORE_PRODUCTS_FIELD, IGNORE_CUSTOMER_ID_FIELD, "customProperties").matches(fetchedCustomer));
		assertCustomerProducts(fetchedCustomer);
	}

	/**
	 * Deletes the customer, but not it's links.
	 */
	@Test
	public void testDeleteCustomer() {
		final Customer savedCustomer = customerRepository.save(customer);
		final ORID rid = ((IEntityProxy)savedCustomer).__getRid();
		assertTrue(customerRepository.existsById(rid));
		final List<Product> products = assertNotNull(savedCustomer.getProducts());
		customerRepository.deleteById(rid);
		assertFalse(customerRepository.existsById(rid));
		products.stream().forEach(product -> assertTrue(customerRepository.existsById(assertNotNull(product.getRid()))));
	}

	/**
	 * Test case to save a embedded and update the embedded using the entity repository.
	 */
	@Test
	public void testUpdateCustomerLink() {
		final Customer customer1 = new Customer("User 9", address);
		final Customer savedCustomer = customerRepository.save(customer1);
		assertTrue(new ReflectionEquals(customer1, "customerId", "customerAddress").matches(savedCustomer));
		assertNotNull(savedCustomer.getCustomerAddress()).setCity("Bangalore"); 
		final Customer modifiedCustomer = customerRepository.save(savedCustomer);
		assertEquals("Bangalore", assertNotNull(modifiedCustomer.getCustomerAddress()).getCity());
	}

	/**
	 * Test native query with embedded type.
	 */
	@Test
	public void testFindCustomerByCity() {
		final Address address2 = new Address("221B", "Baker Street", "Senden", "560037");
		final Customer customer1 = new Customer("User 10", address);
		final Customer customer2 = new Customer("User 11", address2);
		final List<Customer> customers = Arrays.asList(customer1, customer2);
		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertCollectionObjects(customers, new TreeSet<>(savedCustomers), "customerAddress", "customerId");
		final Customer fetchedCustomer = customerRepository.findCustomerByAddressCity("Senden");
		assertEquals("User 11", fetchedCustomer.getName());
	}

	/**
	 * Test native query with linkLists.
	 */
	@Test
	public void testFindCustomerByProductName() {
		final Product product3 = new Product("Oneplus", Long.valueOf(23));
		final Product product4 = new Product("Nokia", Long.valueOf(17));
		final Customer customer2 = new Customer("User 12", Arrays.asList(product3, product4));
		final Customer customer3 = new Customer("User 13", Arrays.asList(product1, product4));
		final List<Customer> customers = Arrays.asList(customer, customer2, customer3);

		final List<Customer> savedCustomers = (List<Customer>) customerRepository.saveAll(customers);
		assertEquals(3, savedCustomers.size());
		final List<Customer> fetchedCustomers = customerRepository.findCustomerByProductsProductName("IPhone");
		assertEquals(2, fetchedCustomers.size());
	}

	/**
	 * Test case to save and retrieve an link entity.
	 *
	 * @throws ParseException if the date format is not valid
	 */
	@Test
	public void testCustomerWithLinkAccount() throws ParseException {
		final Customer customer1 = new Customer("User 15");
		final Date date = new SimpleDateFormat("yyyy-MM-dd").parse("2020-04-28");
		final Account account = new Account("123456", "American Express", date);
		customer1.setCustomerAccount(account);

		final Customer savedCustomer = customerRepository.save(customer1);
		assertTrue(new ReflectionEquals(customer1, "customerAccount", "customerId").matches(savedCustomer));

		final List<Customer> fetchedCustomers = customerRepository.findCustomerByAccountCardName("American Express");
		final Account fetchedAccount = fetchedCustomers.get(0).getCustomerAccount();
		assertTrue(new ReflectionEquals(account, "rid", IGNORE_CUSTOMER_ID_FIELD).matches(fetchedAccount));
	}

	/**
	 * Test case for save and retrieval for embedded map.
	 */
	@Test
	public void testCustomerWithEmbeddedMap() {
		final Customer customer1 = new Customer("User 16");
		final Map<String, String> embeddedMap =  new HashMap<>();
		final String key1 = "Key 1";
		final String key2 = "Key 2";
		embeddedMap.put(key1, "Value 1");
		embeddedMap.put("Key 2", "Value 2");
		customer1.setEmbeddedMap(embeddedMap);

		final Map<String, Set<String>> embeddedMapSet = new HashMap<>();
		final Set<String> stringSet = new HashSet<>();
		stringSet.add("value1");
		stringSet.add("value2");
		embeddedMapSet.put("key1", stringSet);

		customer1.setCustomerEmbeddedMapSet(embeddedMapSet);

		final Customer savedCustomer = customerRepository.save(customer1);
		assertTrue(new ReflectionEquals(customer1, "customerId").matches(savedCustomer));

		final List<Customer> fetchedCustomers = (List<Customer>) customerRepository.findAll();
		final Map<String, String> fetchedEmbeddedMap = assertNotNull(fetchedCustomers.get(0).getEmbeddedMap());
		assertEquals(embeddedMap.get(key1), fetchedEmbeddedMap.get(key1));
		assertEquals(embeddedMap.get(key2), fetchedEmbeddedMap.get(key2));
		assertTrue(embeddedMapSet.equals(fetchedCustomers.get(0).getCustomerEmbeddedMapSet()));
	}

	/**
	 * Test case for save and retrieval of embedded set.
	 */
	@Test
	public void testCustomerWithEmbeddedSet() {
		final Customer customer1 = new Customer("User 18");
		final Set<String> embeddedSet =  new HashSet<>();
		embeddedSet.add("Data 1");
		embeddedSet.add("Data 2");
		embeddedSet.add("Data 1");
		customer1.setEmbeddedSet(embeddedSet);

		final Customer savedCustomer = customerRepository.save(customer1);
		assertTrue(new ReflectionEquals(customer1, "customerId").matches(savedCustomer));

		final List<Customer> fetchedCustomers = (List<Customer>) customerRepository.findAll();
		final Set<String> fetchedEmbeddedSet = assertNotNull(fetchedCustomers.get(0).getEmbeddedSet());
		assertEquals(embeddedSet, fetchedEmbeddedSet);
	}

	/**
	 * Test case to execute named query on embedded set.
	 */
	@Test
	public void testNamedQueryWithEmbeddedSet() {
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

		final List<Customer> fetchedCustomers = customerRepository.findCustomerByEmbeddedSet(commonValue);
		customers.remove(1);
		assertCollectionObjects(customers, new TreeSet<>(fetchedCustomers), "rid", "customProperties", IGNORE_CUSTOMER_ID_FIELD);
	}

	/**
	 * Test case to execute named query on embedded map.
	 */
	@Test
	public void testNamedQueryWithEmbeddedMap() {
		final Customer customer1 = new Customer("User 19");
		final Customer customer2 = new Customer("User 20");
		final Customer customer3 = new Customer("User 21");
		final String key1 = "Key1";
		final String key2 = "Key2";
		final String value = "Value1";
		final Map<String, String> embeddedMap1 = new HashMap<>();
		embeddedMap1.put(key1, value);
		embeddedMap1.put(key2, "Value2");
		customer1.setEmbeddedMap(embeddedMap1);
		final Map<String, String> embeddedMap2 = new HashMap<>();
		embeddedMap2.put(key1, value);
		embeddedMap2.put(key2, value);
		customer2.setEmbeddedMap(embeddedMap2);
		final Map<String, String> embeddedMap3 = new HashMap<>();
		embeddedMap3.put(key1, "Value5");
		embeddedMap3.put(key2, value);
		customer3.setEmbeddedMap(embeddedMap3);

		final List<Customer> customers = new LinkedList<>(Arrays.asList(customer1, customer2, customer3));
		final List<Customer> savedCustomer = (List<Customer>) customerRepository.saveAll(customers);
		assertCollectionObjects(customers, savedCustomer, "customerId");

		final List<Customer> fetchedCustomers = customerRepository.findCustomerByEmbeddedMap(key1, value);
		customers.remove(2);
		assertCollectionObjects(customers, new TreeSet<>(fetchedCustomers), "rid", "customProperties", IGNORE_CUSTOMER_ID_FIELD);
	}

	/**
	 * Test case to validate graph relationship. It also tests named query with graph relation.
	 */
	@Test
	public void testGraphRelationshipWithDirectEntity() {
		final Customer customer1 = new Customer("customer 1");
		final Customer customer2 = new Customer("customer 2");
		final Customer customer3 = new Customer("customer 3");
		final Customer customer4 = new Customer("customer 4");

		final Product product1 = new Product("product 1", Long.valueOf(1));
		final Product product2 = new Product("product 2", Long.valueOf(1));
		final Product product3 = new Product("product 3", Long.valueOf(2));

		customer1.setProductsReviewed(Arrays.asList(product1, product2));
		customer2.setProductsReviewed(Arrays.asList(product3));
		customer4.setProductsReviewed(Arrays.asList(product2));
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3, customer4);
		
		saveAndFindEntitiesLinkedByEdges(customers);
	}
	
	/**
	 * Test case to validate behavior with edge and entity retrieval.
	 */
	@Test
	public void testGraphUpdate() {
		final Customer customer1 = new Customer("customer 1");
		final Product product1 = new Product("product 1", Long.valueOf(1));
		final Reviewed reviewed1 = new Reviewed(customer1, product1);
		customer1.setReviewed(Arrays.asList(reviewed1));
		final Customer savedCustomer = customerRepository.save(customer1);
		final Product product = assertNotNull(assertNotNull(savedCustomer.getReviewed()).get(0).getIn());
		product.setProductName("product 2");
		productRepository.save(product);
		((IEntityProxy) savedCustomer).__injectRid();
		final Customer updatedCustomer = customerRepository.findById(savedCustomer.getRid()).orElseThrow(NullPointerException::new);
		assertEquals("product 2", assertNotNull(assertNotNull(updatedCustomer.getReviewed()).get(0).getIn()).getProductName());
	}
	 

	/**
	 * Test case to validate graph relationship. It also tests named query with graph relation.
	 */
	@Test
	public void testGraphRelationship() {
		final Customer customer1 = new Customer("customer 1");
		final Customer customer2 = new Customer("customer 2");
		final Customer customer3 = new Customer("customer 3");
		final Customer customer4 = new Customer("customer 4");

		final Product product1 = new Product("product 1", Long.valueOf(1));
		final Product product2 = new Product("product 2", Long.valueOf(1));
		final Product product3 = new Product("product 3", Long.valueOf(2));

		final Reviewed reviewed1 = new Reviewed(customer1, product1);
		final Reviewed reviewed2 = new Reviewed(customer1, product2);
		final Reviewed reviewed3 = new Reviewed(customer2, product3);
		final Reviewed reviewed4 = new Reviewed(customer4, product2);
		customer1.setReviewed(Arrays.asList(reviewed1, reviewed2));
		customer2.setReviewed(Arrays.asList(reviewed3));
		customer4.setReviewed(Arrays.asList(reviewed4));
		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3, customer4);

		saveAndFindEntitiesLinkedByEdges(customers);
	}

	/**
	 * Test case for named query with a embedded field.
	 */
	@Test
	public void testEmbeddedNamedQuery() {
		final String pincode = "123455";
		final Address address1 = new Address("12", "street1", "city 1", pincode);
		final Address address2 = new Address("14", "street2", "city 2", pincode);
		final Address address3 = new Address("14", "street2", "city 2", "34546");

		final Customer customer1 = new Customer("customer 1", address1);
		final Customer customer2 = new Customer("customer 2", address2);
		final Customer customer3 = new Customer("customer 3", address3);

		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);
		customerRepository.saveAll(customers);

		final List<Customer> fetchedCustomers = customerRepository.findCustomerByCustomerAddressPincode(pincode);
		assertEquals(2, fetchedCustomers.size());
		final Address fetchedAddress = assertNotNull(fetchedCustomers.get(0).getCustomerAddress());
		assertEquals(pincode, fetchedAddress.getPincode());
	}

	/**
	 * Test case for named query with an link field.
	 */
	@Test
	public void testLinkNamedQuery() {
		final Date date = new Date();
		final Account account1 = new Account("AE123445", "American Express", date);
		final Account account2 = new Account("HC12345", "HSBC", date);
		final Account account3 = new Account("AE2445", "American Express", date);

		final Customer customer1 = new Customer("customer 1");
		final Customer customer2 = new Customer("customer 2");
		final Customer customer3 = new Customer("customer 3");

		customer1.setCustomerAccount(account1);
		customer2.setCustomerAccount(account2);
		customer3.setCustomerAccount(account3);

		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);
		customerRepository.saveAll(customers);

		final List<Customer> fetchedCustomers = customerRepository.findCustomerByCustomerAccountCardNumberLike("AE%");
		assertEquals(2, fetchedCustomers.size());
		final Account fetchedAccount = assertNotNull(fetchedCustomers.get(0).getCustomerAccount());
		assertEquals("American Express", fetchedAccount.getCardName());
	}

	/**
	 * Test case for named query with a link list field.
	 */
	@Test
	public void testLinkListNamedQuery() {
		final Product product1 = new Product("product 1 1", Long.valueOf(1));
		final Product product2 = new Product("product 1 2", Long.valueOf(1));
		final Product product3 = new Product("product 2 1", Long.valueOf(2));
		final Product product4 = new Product("product 3 1", Long.valueOf(3));

		final Customer customer1 = new Customer("customer 1");
		final Customer customer2 = new Customer("customer 2");
		final Customer customer3 = new Customer("customer 3");

		customer1.setProducts(Arrays.asList(product1, product3));
		customer2.setProducts(Arrays.asList(product3, product4));
		customer3.setProducts(Arrays.asList(product2, product4));

		final List<Customer> customers = Arrays.asList(customer1, customer2, customer3);
		customerRepository.saveAll(customers);

		final List<Customer> fetchedCustomers = customerRepository.findCustomerByProductsProductCode(Long.valueOf(1));
		assertEquals(2, fetchedCustomers.size());
		final List<Product> fetchedProducts = assertNotNull(fetchedCustomers.get(0).getProducts());
		assertTrue(assertNotNull(fetchedProducts.get(0).getProductName()).startsWith("product 1 "));
	}

	/**
	 * Test case to validate number of link vertices created.
	 */
	@Test
	public void testInjectingLink() {
		final Product product1 = new Product("product 1", Long.valueOf(1));
		final Product savedProduct = productRepository.save(product1);
		final Customer customer1 = new Customer("customer 1");
		customer1.setCustomerSubscribed(savedProduct);
		final Customer customer2 = new Customer("customer 2");
		customer2.setProducts(Arrays.asList(savedProduct));
		final Customer customer3 = new Customer("customer 3");
		final Reviewed reviewed = new Reviewed(customer3, savedProduct);
		customer3.setReviewed(Arrays.asList(reviewed));
		customerRepository.saveAll(Arrays.asList(customer1, customer2, customer3));

		final List<Customer> fetchedCustomers = (List<Customer>) customerRepository.findAll();
		final List<Product> fetchedProducts = (List<Product>) productRepository.findAll();
		assertEquals(3, fetchedCustomers.size());
		assertEquals(1, fetchedProducts.size());
	}
	
	/**
	 * Test case to validate number of link vertices created.
	 */
	@Test
	public void testUpdateExistingEntity() {
		final Product product1 = new Product("product 1", Long.valueOf(1));
		final Product savedProduct = productRepository.save(product1);
		final Customer customer1 = new Customer("customer 1");
		customer1.setCustomerSubscribed(savedProduct);
		final Customer customer2 = new Customer("customer 2");
		customer2.setProducts(Arrays.asList(savedProduct));
		final Customer customer3 = new Customer("customer 3");
		final Reviewed reviewed = new Reviewed(customer3, savedProduct);
		customer3.setReviewed(Arrays.asList(reviewed));
		customerRepository.saveAll(Arrays.asList(customer1, customer2, customer3));

		final List<Customer> fetchedCustomers = (List<Customer>) customerRepository.findAll();
		final List<Product> fetchedProducts = (List<Product>) productRepository.findAll();
		assertEquals(3, fetchedCustomers.size());
		assertEquals(1, fetchedProducts.size());
		final Long productId = assertNotNull(fetchedProducts.get(0).getProductId());
		final Product product2 = new Product("product 1", Long.valueOf(2));
		product2.setProductId(productId);
		
		final Customer customer4 = new Customer("customer 4");
		customer4.setCustomerSubscribed(product2);
		customerRepository.save(customer4);
		final List<Customer> fetchedCustomers1 = (List<Customer>) customerRepository.findAll();
		final List<Product> fetchedProducts1 = (List<Product>) productRepository.findAll();
		assertEquals(4, fetchedCustomers1.size());
		assertEquals(1, fetchedProducts1.size());
		assertEquals(Long.valueOf(2), fetchedProducts1.get(0).getProductCode());
	}
	
	/**
	 * Test case to validate graph relationship. It also tests named query with graph relation.
	 */
	@Test
	public void testGraphRelationshipWithDirection() {
		final Customer customer1 = new Customer("customer 1");
		final Product product1 = new Product("product 1", Long.valueOf(1));
		final Product product2 = new Product("product 2", Long.valueOf(1));

		final OrderedBy orderedBy = new OrderedBy(product1, customer1);
		final OrderedBy orderedBy2 = new OrderedBy(product2, customer1);
		product1.setCustomer(orderedBy);
		product2.setCustomer(orderedBy2);
		final List<Product> products = Arrays.asList(product1, product2);

		final List<Product> savedProducts =  (List<Product>) productRepository.saveAll(products);
		final OVertex vertex = ((IEntityProxy) savedProducts.get(0)).__getElement().asVertex().orElseThrow(RuntimeException::new);
		assertFalse(vertex.getEdges(ODirection.OUT, "orderedBy").iterator().hasNext());
		assertTrue(vertex.getEdges(ODirection.IN, "orderedBy").iterator().hasNext());
		final List<Product> fetchedProducts = (List<Product>) productRepository.findAll();
		assertTrue(assertNotNull(assertNotNull(fetchedProducts.get(0).getCustomer()).getOut()).getName().startsWith("customer "));
	}
	
	/**
	 * Test case to validate graph relationship. It also tests named query with graph relation.
	 */
	@Test
	public void testGraphRelationshipReturningEntity() {
		final Customer customer1 = new Customer("customer 1");
		final Customer customer2 = new Customer("Customer 2");
		
		customer1.setCallsCustomers(Collections.singletonList(customer2));
		customerRepository.save(customer1);
		
		final Customer fetchedCustomer = customerRepository.findByName("customer 1");
		final String customerName = assertNotNull(fetchedCustomer.getCallsCustomers()).get(0).getName();
		assertEquals("Customer 2", customerName);
	}
	
	/**
	 * Test case to validate update method.
	 */
	@Test
	public void testUpdateMethod() {
		final Customer customer1 = new Customer("customer 1");
		final Customer savedCustomer1 = customerRepository.save(customer1);
		final Product product1 = new Product("product 1", Long.valueOf(1));
		final Product product2 = new Product("product 2", Long.valueOf(1));
		
		final OrderedBy orderedBy = new OrderedBy(product1, savedCustomer1);
		final OrderedBy orderedBy2 = new OrderedBy(product2, savedCustomer1);
		product1.setCustomer(orderedBy);
		product2.setCustomer(orderedBy2);
		final List<Product> products = Arrays.asList(product1, product2);
		productRepository.saveAll(products);
		
		assertEquals("customer 1", savedCustomer1.getName());
		savedCustomer1.setName("Modified customer");
		final Customer updatedCustomer = customerRepository.save(savedCustomer1);
		assertEquals("Modified customer", updatedCustomer.getName());
		
		final List<Product> fetchedProducts = (List<Product>) productRepository.findAll();
		final OrderedBy orderedByFecthed = assertNotNull(fetchedProducts.get(0).getCustomer());
		assertEquals("Modified customer", assertNotNull(orderedByFecthed.getOut()).getName());
	}
	
	private void assertCustomerProducts(final Customer customer) {
		final List<Product> products = assertNotNull(customer.getProducts());
		assertTrue(new ReflectionEquals(product1, "rid", "productId").matches(products.get(0)));
		assertTrue(new ReflectionEquals(product2, "rid", "productId").matches(products.get(1)));
	}
	
	private void saveAndFindEntitiesLinkedByEdges(final List<Customer> customers) {
		final List<Customer> savedCustomer = (List<Customer>) customerRepository.saveAll(customers);
		assertCollectionObjects(customers, new TreeSet<>(savedCustomer), IGNORE_CUSTOMER_ID_FIELD, "rid", "productsReviewed", "reviewed");
		final List<Customer> fetchedCustomer = (List<Customer>) customerRepository.findAll();
		assertEquals(4, fetchedCustomer.size());
		assertCollectionObjects(customers, new TreeSet<>(fetchedCustomer), IGNORE_CUSTOMER_ID_FIELD, "rid", "productsReviewed", "reviewed",  "customProperties");

		final List<Customer> reviewers = customerRepository.findCustomerByReviewedInProductName("product 2");
		assertEquals(2, reviewers.size());
		assertTrue(assertNotNull(reviewers.get(0).getReviewed()).size() > 0);
		assertTrue(assertNotNull(reviewers.get(1).getReviewed()).size() > 0);
	}
	
	/**
	 * Tests lazy loading while being accessed by multiple threads.
	 *
	 * @throws InterruptedException if an execution of a thread is interrupted mid run
	 * @throws ParseException if there is an exception in parsing date
	 */
	@Test
	public void testConcurrentAccess() throws InterruptedException, ParseException {
		final Customer customer1 = new Customer("User 15");
		final Date date = new SimpleDateFormat("yyyy-MM-dd").parse("2020-04-28");
		final Account account = new Account("123456", "American Express", date);
		customer1.setCustomerAccount(account);
		
		final Customer savedCustomer = customerRepository.save(customer1);
		((IEntityProxy) savedCustomer).__injectRid();
		String rid = savedCustomer.getRid();
		Optional<Customer> findById = customerRepository.findById(rid);
		Customer returnedCustomer = findById.get();
		final int noOfStatements = 50;
		final ExecutorService pool = Executors.newFixedThreadPool(noOfStatements);
		final AtomicInteger numberOfErrors = new AtomicInteger();
		final CountDownLatch latch = new CountDownLatch(noOfStatements);
		for (int i = 0; i < noOfStatements; i++) {
			pool.submit(() -> {
				try {
					assertNotNull(returnedCustomer.getCustomerAccount());
				} catch (final Exception e) {
					numberOfErrors.incrementAndGet();
				} finally {
					latch.countDown();
				}
			});
		}
		latch.await();
		assertEquals(0, numberOfErrors.get());
	}
}
