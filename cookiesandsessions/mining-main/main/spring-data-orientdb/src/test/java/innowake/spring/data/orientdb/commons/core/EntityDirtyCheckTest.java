/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.core;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.record.OEdge;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.CustomPropertyDataType;
import innowake.mining.shared.springdata.annotations.Relationship;
import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;
import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Account;
import innowake.spring.data.orientdb.integration.repository.domain.Address;
import innowake.spring.data.orientdb.integration.repository.domain.Calls;
import innowake.spring.data.orientdb.integration.repository.domain.Customer;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee.Designation;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.integration.repository.domain.Reviewed;
import innowake.spring.data.orientdb.integration.repository.domain.Reviewed.RatingEnum;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomProperty;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import innowake.spring.data.orientdb.repository.CustomerRepository;
import innowake.spring.data.orientdb.repository.EmployeeRepository;
import innowake.spring.data.orientdb.repository.OrientRepository;
import innowake.spring.data.orientdb.repository.ProductRepository;
import innowake.spring.data.orientdb.repository.ReviewedRepository;

/**
 * Tests for EntityDirtyCheck class.
 * These tests validate if the vertex or edge is dirty and requires update operation in database.
 */
@Transactional(transactionManager = "orientdb-transaction", noRollbackFor = { NoRecordFoundException.class })
class EntityDirtyCheckTest extends AbstractIntegrationTests {

	private static final String CUSTOMER_NAME = "Customer";
	private static final String CUSTOMER_NAME_2 = "Customer 2";

	private static final String IPHONE = "IPhone";
	private static final String IPHONE_11 = "IPhone 11";
	private static final String SAMSUNG_GALAXY = "Samsung galaxy";
	private static final String ONE_PLUS = "OnePlus";

	private static final Long IPHONE_PRODUCT_CODE = Long.valueOf(12);
	private static final Long IPHONE_11_PRODUCT_CODE = Long.valueOf(14);
	private static final Long GALAXY_PRODUCT_CODE = Long.valueOf(15);
	private static final Long ONEPLUS_PRODUCT_CODE = Long.valueOf(30);

	private static final String PRODUCT_REVIEW_DESCRIPTION = "Some description";
	private static final String PRODUCT_REVIEW_MODIFIED_DESCRIPTION = "Modified description";
	private static final String PRODUCT_CUSTOM_PROPERTY_CLASS = "ProductCustomProperties";
	private static final String PRODUCT_CUSTOM_PROPERTY_FIELD_STRING = "customProductStringProperty";

	@Autowired
	private CustomerRepository customerRepository;
	@Autowired
	private ProductRepository productRepository;
	@Autowired
	private EmployeeRepository employeeRepository;
	@Autowired
	private ReviewedRepository reviewedRepository;

	@Nullable
	private EntityDirtyCheck entityDirtyCheck = null;

	/**
	 * Clears data saved in oreintDB.
	 */
	@BeforeEach
	void init() {
		clearData(CUSTOMER_NAME, "Product", "Account", "Employee");
		entityDirtyCheck = new EntityDirtyCheck(new HashSet<>());
	}

	/**
	 * Tests vertex entity is dirty when a primitive field is changed.
	 */
	@Test
	void testVertexEntityForModifiedPrimitiveField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the primitive field. */
		fetchedCustomer.setName(CUSTOMER_NAME_2);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is not dirty when entity is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedPrimitiveField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when a linked field is changed.
	 */
	@Test
	void testVertexEntityForModifiedLinkedField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Account sbiAccount = new Account("10248203048", "MasterCard", new Date());
		customer.setCustomerAccount(sbiAccount);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Link field i.e. Account to a new account. */
		final Account iciciAccount = new Account("34382221138", "Visa", new Date());
		fetchedCustomer.setCustomerAccount(iciciAccount);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when a linked field is changed.
	 */
	@Test
	void testVertexEntityForNewLinkField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Product savedProduct = saveEntity(product, productRepository);
		fetchedCustomer.setCustomerSubscribed(savedProduct);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when a property of linked field is changed.
	 */
	@Test
	void testVertexEntityForModifiedLinkedFieldProperty() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Account sbiAccount = new Account("10248203048", "MasterCard", new Date());
		customer.setCustomerAccount(sbiAccount);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Link field i.e. Account's property. */
		assertNotNull(fetchedCustomer.getCustomerAccount()).setCardName("Visa");
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is not dirty when entity is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedLinkedField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Account sbiAccount = new Account("10248203048", "MasterCard", new Date());
		customer.setCustomerAccount(sbiAccount);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);		
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when embedded field is changed.
	 */
	@Test
	void testVertexEntityForModifiedEmbeddedField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Address address = new Address("221B", "Baker Street", "London", "560037");
		customer.setCustomerAddress(address);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Embedded field i.e. Address to a new address. */
		final Address newAddress = new Address("221B", "Baker Street", "New York", "560037");
		fetchedCustomer.setCustomerAddress(newAddress);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when property of embedded field is changed.
	 */	
	@Test
	void testVertexEntityForModifiedEmbeddedFieldProperty() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Address address = new Address("221B", "Baker Street", "London", "560037");
		customer.setCustomerAddress(address);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Embedded field i.e. Address's property. */
		assertNotNull(fetchedCustomer.getCustomerAddress()).setCity("New York");
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is not dirty when entity is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedEmbeddedField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Address address = new Address("221A", "Cake Street", "London", "560037");
		customer.setCustomerAddress(address);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);		
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when link list field is changed.
	 */
	@Test
	void testVertexEntityForModifiedLinkListFields() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product1 = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Product product2 = new Product(SAMSUNG_GALAXY, GALAXY_PRODUCT_CODE);
		customer.setProducts(Arrays.asList(product1, product2));
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		final Product product3 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		final Product savedProduct3 = saveEntity(product3, productRepository);
		/* Replace a product in the list. */
		assertNotNull(fetchedCustomer.getProducts()).set(1, savedProduct3);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when property of link list field is changed.
	 */	
	@Test
	void testVertexEntityForModifiedLinkListFieldProperty() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product1 = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Product product2 = new Product(SAMSUNG_GALAXY, GALAXY_PRODUCT_CODE);
		customer.setProducts(Arrays.asList(product1, product2));
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify property of a list element. */
		assertNotNull(fetchedCustomer.getProducts()).get(0).setProductName(IPHONE_11);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity dirty check throws exception when link list field contains duplicate {@link IEntityProxy} objects.
	 */
	@Test
	void testVertexEntityWithDuplicateListFieldsThrowsException() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product1 = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Product product2 = new Product(SAMSUNG_GALAXY, GALAXY_PRODUCT_CODE);
		customer.setProducts(Arrays.asList(product1, product2));
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		final Product product3 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		final Product savedProduct3 = saveEntity(product3, productRepository);
		/* Modify the list to store duplicate items. */
		final List<Product> fetchedProductsList = fetchedCustomer.getProducts();
		assertNotNull(fetchedProductsList).set(0, savedProduct3);
		assertNotNull(fetchedProductsList).set(1, savedProduct3);
		final EntityDirtyCheck entityDirtyCheckNotNullCopy = assertNotNull(entityDirtyCheck);
		final Exception thrownException = assertThrows(IllegalStateException.class,
				() -> entityDirtyCheckNotNullCopy.isDirty((IEntityProxy) fetchedCustomer));
		assertTrue(thrownException.getMessage().contains("Duplicate key"));
		assertTrue(thrownException.getMessage().contains("productName=OnePlus, productCode=30"));
	}

	/**
	 * Tests vertex entity is not dirty when entity is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedLinkListFields() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product1 = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Product product2 = new Product(SAMSUNG_GALAXY, GALAXY_PRODUCT_CODE);
		customer.setProducts(Arrays.asList(product1, product2));
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);		
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is not dirty when entity is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedReversedLinkList() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product1 = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Product product2 = new Product(SAMSUNG_GALAXY, GALAXY_PRODUCT_CODE);
		customer.setProducts(Arrays.asList(product1, product2));
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Reverse the list. */
		Collections.reverse(assertNotNull(fetchedCustomer.getProducts()));
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when enum field is changed.
	 */
	@Test
	void testVertexEntityForModifiedEnumFields() {
		final Employee employee = new Employee(StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY);
		employee.setEmpDesignation(Designation.CONSULTANT);
		final Employee fetchedEmployee = saveEntity(employee, employeeRepository);
		/* Modify enum field i.e. employee's designation. */
		fetchedEmployee.setEmpDesignation(Designation.MANAGER);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedEmployee));
	}

	/**
	 * Tests vertex entity is not dirty when entity is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedEnumFields() {
		final Employee employee = new Employee(StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY);
		employee.setEmpDesignation(Designation.CONSULTANT);
		final Employee fetchedEmployee = saveEntity(employee, employeeRepository);		
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedEmployee));
	}

	/**
	 * Tests vertex entity is dirty when new edge is added from it to another vertex.
	 */
	@Test
	void testVertexEntityForModifiedEdgeList() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		/* Modify the edges. */
		final Product product2 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		final Product savedProduct2 = saveEntity(product2, productRepository);
		final Reviewed secondReviewed = new Reviewed(fromEntity, savedProduct2);
		assertNotNull(fromEntity.getReviewed()).add(secondReviewed);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fromEntity));
	}

	/**
	 * Tests vertex entity is not dirty when both db and entity has same edges.
	 */
	@Test
	void testVertexEntityForUnmodifiedEdgeList() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		/* Modify the edges. */
		final Product product2 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		final Product savedProduct2 = saveEntity(product2, productRepository);
		final Reviewed secondReviewed = new Reviewed(fromEntity, savedProduct2);
		saveEntity(secondReviewed, reviewedRepository);
		assertEquals(Integer.valueOf(2), Integer.valueOf(assertNotNull(fromEntity.getReviewed()).size()));
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fromEntity));
	}

	/**
	 * Tests vertex entity is dirty when new edge is added from it to another vertex, while replacing old edge.
	 */
	@Test
	void testVertexEntityForModifiedEdgeField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		/* Modify the edges. */
		final Product product2 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		final Product savedProduct2 = saveEntity(product2, productRepository);
		final Reviewed secondReviewed = new Reviewed(fromEntity, savedProduct2);
		final Reviewed savedSecondReviewed = reviewedRepository.save(secondReviewed);
		fromEntity.setReviewed(Arrays.asList(savedSecondReviewed));
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fromEntity));		
	}
	
	/**
	 * Tests vertex entity is dirty when new edge is added to vertex entity.
	 */
	@Test
	void testVertexEntityForNewEdge() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Customer savedCustomer = saveEntity(customer, customerRepository);
		final Customer customer2 = new Customer(CUSTOMER_NAME_2);
		final Customer savedCustomer2 = saveEntity(customer2, customerRepository);
		/* Create a new edge. */
		final Calls outCall = new Calls(savedCustomer2, savedCustomer);
		savedCustomer.setOutCalls(outCall);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) savedCustomer));		
	}

	/**
	 * Tests vertex entity is not dirty when order of edges is reversed.
	 */
	@Test
	void testVertexEntityForUnmodifiedEdges() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		final Product product2 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		final Reviewed secondReviewed = new Reviewed(customer, product2);
		customer.setReviewed(Arrays.asList(reviewed, secondReviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		/* Reverse the list of edges. */
		Collections.reverse(fromEntity.getReviewed());
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fromEntity));		
	}

	/**
	 * Tests vertex entity is dirty when embedded set is changed.
	 */
	@Test
	void testVertexEntityForModifiedEmbeddedSet() {
		final Customer customer = new Customer(CUSTOMER_NAME); 
		customer.setEmbeddedSet(new HashSet<String>(Arrays.asList(IPHONE, ONE_PLUS, SAMSUNG_GALAXY)));
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Embedded Set. */
		fetchedCustomer.setEmbeddedSet(new HashSet<String>(Arrays.asList(IPHONE_11, ONE_PLUS, SAMSUNG_GALAXY)));
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when embedded set size is changed.
	 */
	@Test
	void testVertexEntityForModifiedEmbeddedSetSize() {
		final Customer customer = new Customer(CUSTOMER_NAME); 
		customer.setEmbeddedSet(new HashSet<String>(Arrays.asList(IPHONE, ONE_PLUS, SAMSUNG_GALAXY)));
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Embedded Set size. */
		assertNotNull(customer.getEmbeddedSet()).remove(IPHONE);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is not dirty when embedded set is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedEmbeddedSet() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Set<String> customerPhones = new HashSet<String>( Arrays.asList(IPHONE, ONE_PLUS, SAMSUNG_GALAXY));
		customer.setEmbeddedSet(customerPhones);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		fetchedCustomer.setEmbeddedSet(customerPhones);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when embedded map key is changed.
	 */
	@Test
	void testVertexEntityForModifiedEmbeddeMapKey() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Map<String, String> embeddedMap = new HashMap<>();
		embeddedMap.put(IPHONE, String.valueOf(IPHONE_PRODUCT_CODE));
		embeddedMap.put(ONE_PLUS, String.valueOf(ONEPLUS_PRODUCT_CODE));
		embeddedMap.put(SAMSUNG_GALAXY, String.valueOf(GALAXY_PRODUCT_CODE));
		customer.setEmbeddedMap(embeddedMap);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Embedded Map Key. */
		embeddedMap.remove(IPHONE);
		embeddedMap.put(IPHONE_11, String.valueOf(IPHONE_PRODUCT_CODE));
		fetchedCustomer.setEmbeddedMap(embeddedMap);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when embedded map value is changed.
	 */
	@Test
	void testVertexEntityForModifiedEmbeddedMapValue() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Map<String, String> embeddedMap = new HashMap<>();
		embeddedMap.put(IPHONE, String.valueOf(IPHONE_PRODUCT_CODE));
		embeddedMap.put(ONE_PLUS, String.valueOf(ONEPLUS_PRODUCT_CODE));
		embeddedMap.put(SAMSUNG_GALAXY, String.valueOf(GALAXY_PRODUCT_CODE));
		customer.setEmbeddedMap(embeddedMap);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Embedded Map Key. */
		assertNotNull(fetchedCustomer.getEmbeddedMap()).put(IPHONE, String.valueOf(IPHONE_11_PRODUCT_CODE));
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is dirty when embedded map size is changed.
	 */
	@Test
	void testVertexEntityForModifiedEmbeddedMapSize() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Map<String, String> embeddedMap = new HashMap<>();
		embeddedMap.put(IPHONE, String.valueOf(IPHONE_PRODUCT_CODE));
		embeddedMap.put(ONE_PLUS, String.valueOf(ONEPLUS_PRODUCT_CODE));
		embeddedMap.put(SAMSUNG_GALAXY, String.valueOf(GALAXY_PRODUCT_CODE));
		customer.setEmbeddedMap(embeddedMap);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the Embedded Map Key. */
		assertNotNull(fetchedCustomer.getEmbeddedMap()).remove(IPHONE);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity is not dirty when embedded map is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedEmbeddedMap() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Map<String, String> embeddedMap = new HashMap<>();
		embeddedMap.put(IPHONE, String.valueOf(IPHONE_PRODUCT_CODE));
		embeddedMap.put(ONE_PLUS, String.valueOf(ONEPLUS_PRODUCT_CODE));
		embeddedMap.put(SAMSUNG_GALAXY, String.valueOf(GALAXY_PRODUCT_CODE));
		customer.setEmbeddedMap(embeddedMap);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));
	}

	/**
	 * Tests vertex entity dirty check throws exception when list of vertices having {@link Relationship} annotation is modified.
	 */
	@Test
	void testVertexEntityWithEdgeAnnotationThrowsException() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product1 = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Product product2 = new Product(SAMSUNG_GALAXY, GALAXY_PRODUCT_CODE);
		customer.setProductsReviewed(Arrays.asList(product1, product2));
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		final Product product3 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		final Product savedProduct3 = saveEntity(product3, productRepository);
		/* Replace a product in the list. */
		assertNotNull(fetchedCustomer.getProductsReviewed()).set(1, savedProduct3);
		final EntityDirtyCheck entityDirtyCheckNotNullCopy = assertNotNull(entityDirtyCheck);
		final Exception thrownException = assertThrows(IllegalStateException.class, () -> entityDirtyCheckNotNullCopy.isDirty((IEntityProxy) fetchedCustomer));
		assertEquals("The Vertices with Edge Relationship Annotation are not handled.", thrownException.getMessage());
	}

	/**
	 * Tests vertex entity is dirty when Array field is changed.
	 */
	@Test
	void testVertexEntityForModifiedArrayField() {
		final Employee employee = new Employee(StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY);
		employee.setSecretEmpCode(new byte[] {0, 11, 14, 17, 18});
		final Employee fetchedEmployee = saveEntity(employee, employeeRepository);
		/* Modify array field. */
		fetchedEmployee.setSecretEmpCode(new byte[] {0, 11, 14, 17, 19});
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedEmployee));
	}

	/**
	 * Tests vertex entity is not dirty when array field is not changed.
	 */
	@Test
	void testVertexEntityForUnmodifiedArrayField() {
		final Employee employee = new Employee(StringUtils.EMPTY, StringUtils.EMPTY, StringUtils.EMPTY);
		employee.setSecretEmpCode(new byte[] {0, 11, 14, 17, 18});
		final Employee fetchedEmployee = saveEntity(employee, employeeRepository);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedEmployee));
	}

	/**
	 * Tests edge entity is dirty when a primitive field is changed.
	 */
	@Test
	void testEdgeEntityForModifiedPrimitiveField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		reviewed.setDescription(PRODUCT_REVIEW_DESCRIPTION);		
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final List<OEdge> edges = getEdgeFromVertex(((IEntityProxy) fromEntity).__getElement().asVertex().get());
		assertEquals(Integer.valueOf(1), Integer.valueOf(edges.size()));
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		assertEquals(fetchedReview.getRid(), edges.get(0).getIdentity().toString());
		/* Modify the edge description. */
		fetchedReview.setDescription(PRODUCT_REVIEW_MODIFIED_DESCRIPTION);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));		
	}

	/**
	 * Tests edge entity is not dirty when entity is not changed.
	 */
	@Test
	void testEdgeEntityForUnmodifiedPrimitiveField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		reviewed.setDescription(PRODUCT_REVIEW_DESCRIPTION);	
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));		
	}

	/**
	 * Tests edge entity is dirty when a linked field is changed.
	 */
	@Test
	void testEdgeEntityForModifiedLinkedField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		/* Modify the edge linked field. */
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		final Product product2 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		final Product savedProduct2 = saveEntity(product2, productRepository);
		fetchedReview.setIn(savedProduct2);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));
	}

	/**
	 * Tests edge entity is dirty when a linked field is changed.
	 */
	@Test
	void testEdgeEntityForModifiedLinkedField2() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		/* Modify the edge linked field. */
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		final Product product2 = new Product(ONE_PLUS, ONEPLUS_PRODUCT_CODE);
		fetchedReview.setIn(product2);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));
	}

	/**
	 * Tests edge entity is dirty when a property of linked field is changed.
	 */
	@Test
	void testEdgeEntityForModifiedLinkedFieldProperty() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		/* Modify the edge linked field property. */
		assertNotNull(fetchedReview.getIn()).setProductName(ONE_PLUS);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));		
	}

	/**
	 * Tests edge entity is not dirty when entity is not changed.
	 */
	@Test
	void testEdgeEntityForUnmodifiedLinkedField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));		
	}

	/**
	 * Tests edge entity is dirty when embedded field is changed.
	 */
	@Test
	void testEdgeEntityForModifiedEmbeddedField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		reviewed.setSellerAddress(new Address("12", "MG Road", "Bangalore", "560037"));
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		/* Modify the Embedded field i.e. Address to a new address. */
		final Address newAddress = new Address("221B", "Baker Street", "New York", "560037");
		fetchedReview.setSellerAddress(newAddress);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));	
	}

	/**
	 * Tests edge entity is dirty when property of embedded field is changed.
	 */
	@Test
	void testEdgeEntityForModifiedEmbeddedFieldProperty() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		reviewed.setSellerAddress(new Address("12", "MG Road", "Bangalore", "560037"));
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		/* Modify the Embedded field's property. */
		assertNotNull(fetchedReview.getSellerAddress()).setCity("New York");
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));		
	}

	/**
	 * Tests edge entity is not dirty when entity is not changed.
	 */
	@Test
	void testEdgeEntityForUnmodifiedEmbeddedField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		reviewed.setSellerAddress(new Address("12", "MG Road", "Bangalore", "560037"));
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));		
	}

	/**
	 * Tests edge entity is dirty when enum field is changed.
	 */
	@Test
	void testEdgeEntityForModifiedEnumField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product); 
		reviewed.setRating(Arrays.asList(RatingEnum.ONE, RatingEnum.TWO));
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		/* Modify the link list field rating. */
		fetchedReview.setRating(Arrays.asList(RatingEnum.ONE, RatingEnum.THREE));
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));		
	}

	/**
	 * Tests edge entity is not dirty when entity is not changed.
	 */
	@Test
	void testEdgeEntityForUnmodifiedEnumField() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final Reviewed reviewed = new Reviewed(customer, product);
		reviewed.setRating(Arrays.asList(RatingEnum.ONE, RatingEnum.TWO));
		customer.setReviewed(Arrays.asList(reviewed));
		final Customer fromEntity = saveEntity(customer, customerRepository);
		final Reviewed fetchedReview = assertNotNull(fromEntity.getReviewed()).get(0);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedReview));		
	}

	/**
	 * Tests vertex entity is skipped for dirty check when already traversed field is checked for isDirty.
	 */
	@Test
	void testVertexEntityIsNotDirtyForTraversedEntities() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);
		/* Modify the primitive field. */
		fetchedCustomer.setName(CUSTOMER_NAME_2);
		final Set<ORID> traversedEntitiesSet = new HashSet<>();
		traversedEntitiesSet.add(((IEntityProxy) fetchedCustomer).__getRid());
		final EntityDirtyCheck entityDirtyCheck = new EntityDirtyCheck(traversedEntitiesSet);
		assertFalse(entityDirtyCheck.isDirty((IEntityProxy) fetchedCustomer));
	}
	
	/**
	 * Tests custom properties is skipped for dirty check during update operation.
	 */
	@Test
	void testCustomPropertiesIsNotDirtyForUpdateOperation() {
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Customer fetchedCustomer = saveEntity(customer, customerRepository);

		final Product product = new Product(IPHONE, IPHONE_PRODUCT_CODE);
		final CustomProperty customPropertyProduct = new CustomProperty(PRODUCT_CUSTOM_PROPERTY_FIELD_STRING, "Saved Value", CustomPropertyDataType.STRING);
		product.addCustomProperty(PRODUCT_CUSTOM_PROPERTY_CLASS, customPropertyProduct);
		customer.setProducts(Arrays.asList(product));
		fetchedCustomer.setName(CUSTOMER_NAME_2);
		assertTrue(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) fetchedCustomer));

		final Customer customerUpdated = saveEntity(customer, customerRepository);
		final Product productUpdated = assertNotNull(customerUpdated.getProducts()).get(0);
		final CustomProperty newCustomPropertyProduct = new CustomProperty(PRODUCT_CUSTOM_PROPERTY_FIELD_STRING, "Im dirty", CustomPropertyDataType.STRING);
		productUpdated.addCustomProperty(PRODUCT_CUSTOM_PROPERTY_CLASS, newCustomPropertyProduct);
		assertFalse(assertNotNull(entityDirtyCheck).isDirty((IEntityProxy) customerUpdated), "Customer property changes should not be dirty");
	}

	private <T> T saveEntity(final T entity, final OrientRepository<T> repository) {
		final T savedEntity = repository.save(entity);
		((IEntityProxy) savedEntity).__injectRid();
		return savedEntity;		
	}
}
