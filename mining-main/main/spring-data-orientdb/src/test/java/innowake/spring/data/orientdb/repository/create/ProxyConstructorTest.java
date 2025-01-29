package innowake.spring.data.orientdb.repository.create;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.collections4.map.HashedMap;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Account;
import innowake.spring.data.orientdb.integration.repository.domain.Address;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.EntityWithCollectionLink;
import innowake.spring.data.orientdb.integration.repository.domain.EntityWithCollectionParams;
import innowake.spring.data.orientdb.integration.repository.domain.EntityWithDefaultConstructor;
import innowake.spring.data.orientdb.integration.repository.domain.EntityWithEmbeddedParam;
import innowake.spring.data.orientdb.integration.repository.domain.EntityWithEnumParams;
import innowake.spring.data.orientdb.integration.repository.domain.EntityWithEnumParams.EnumData;
import innowake.spring.data.orientdb.integration.repository.domain.EntityWithLinkParam;
import innowake.spring.data.orientdb.integration.repository.domain.EntityWithPrimitiveValues;
import innowake.spring.data.orientdb.repository.OrientRepository;

public class ProxyConstructorTest extends AbstractIntegrationTests {
	
	@Autowired
	private AccountRepository accountRepository;
	
	@Autowired
	private DefaultConstructorRepository defaultConstructorRepository;
	
	@Autowired
	private PrimitiveValuesConstructorRepository primitiveValuesConstructorRepository;
	
	@Autowired
	private EnumValuesConstructorRepository enumConstructorRepository;
	
	@Autowired
	private PrimitiveCollectionConstructorRepository primitiveCollectionConstructorRepository;
	
	@Autowired
	private EmbeddedValueConstructorRepository embeddedValueConstructorRepository;
	
	@Autowired
	private LinkFieldConstructorRepository linkFieldConstructorRepository;
	
	@Autowired
	private CollectionLinkFieldConstructorRepository collectionLinkFieldConstructorRepository;
	
	/**
	 * Clears data saved in oreintDB.
	 */
	@Before
	public void init() {
		clearData("Account", "EntityWithCollectionParams", "EntityWithDefaultConstructor",
				"EntityWithCollectionLink", "EntityWithEmbeddedParam", "EntityWithEnumParams", "EntityWithLinkParam", "EntityWithPrimitiveValues");
	}
	
	/**
	 * Tests the Save, Read and Update operations using constructor
	 */
	@Test
	public void testConstructorCreationInSaveReadUpdate() {
		final Account account = new Account("10248203048", "MasterCard", new Date());
		accountRepository.save(account);
		
		final List<Account> fetchedAccounts = (List<Account>) accountRepository.findAll();
		assertEquals(1, fetchedAccounts.size());
		final Account accountToBeUpdated = fetchedAccounts.get(0);
		assertEquals("MasterCard", accountToBeUpdated.getCardName());
		assertEquals("10248203048", accountToBeUpdated.getCardNumber());
		
		accountToBeUpdated.setCardName("Visa");
		accountRepository.save(accountToBeUpdated);
		final Optional<Account> updatedAccount = accountRepository.findById(accountToBeUpdated.getRid());
		assertTrue(updatedAccount.isPresent());
		assertEquals("Visa", updatedAccount.get().getCardName());
	}

	/**
	 * Tests the Save, Read and Update operations using default constructor for {@linkplain EntityWithDefaultConstructor}
	 */
	@Test
	public void testDefaultConstructor() {
		final EntityWithDefaultConstructor defaultConstructor = new EntityWithDefaultConstructor();
		defaultConstructor.setBooleanVal(true);
		defaultConstructor.setId(Long.valueOf(1));
		defaultConstructorRepository.save(defaultConstructor);
		
		final List<EntityWithDefaultConstructor> retrievedEntity = (List<EntityWithDefaultConstructor>) defaultConstructorRepository.findAll();
		assertEquals(1, retrievedEntity.size());
		final EntityWithDefaultConstructor entityToBeUpdated = retrievedEntity.get(0);
		assertEquals(defaultConstructor.getId(), entityToBeUpdated.getId());
		assertTrue(entityToBeUpdated.isBooleanVal());
		
		entityToBeUpdated.setBooleanVal(false);
		defaultConstructorRepository.save(entityToBeUpdated);
		final List<EntityWithDefaultConstructor> updatedEntity = (List<EntityWithDefaultConstructor>) defaultConstructorRepository.findAll();
		assertEquals(1, updatedEntity.size());
		assertFalse(updatedEntity.get(0).isBooleanVal());
	}
	
	/**
	 * Tests the Save, Read and Update operations using primitive data types for {@linkplain EntityWithPrimitiveValues}
	 */
	@Test
	public void testConstructorWithPrimitiveValues() {
		final boolean booleanValue = true;
		final byte bytes = 10;
		final Boolean wrappedBooleanValue = false;
		final Byte wrappedByteValue = Byte.valueOf("10");
		final Double doubleValue = 1.00000005;
		final Float floatValue = 1000.5f;
		final Integer intValue = 1000;
		final Long longValue = 1000l;
		final Short shortValue = 1000;
		final BigDecimal decimalValue = new BigDecimal(45000.4508);
		final EntityWithPrimitiveValues constructorWithPrimitiveValues = new EntityWithPrimitiveValues(booleanValue, bytes, wrappedBooleanValue,
				wrappedByteValue, doubleValue, floatValue, intValue, longValue, shortValue, decimalValue);
		primitiveValuesConstructorRepository.save(constructorWithPrimitiveValues);
		
		final List<EntityWithPrimitiveValues> retrievedEntityList = (List<EntityWithPrimitiveValues>) primitiveValuesConstructorRepository.findAll();
		assertEquals(1, retrievedEntityList.size());
		
		final EntityWithPrimitiveValues entityToBeUpdated = retrievedEntityList.get(0);
		assertTrue(entityToBeUpdated.isBooleanValue());
		assertEquals(constructorWithPrimitiveValues.getBytes(), entityToBeUpdated.getBytes());
		assertFalse(entityToBeUpdated.getWrappedBooleanValue());
		assertEquals(constructorWithPrimitiveValues.getWrappedByteValue(), entityToBeUpdated.getWrappedByteValue());
		assertEquals(constructorWithPrimitiveValues.getDoubleValue(), entityToBeUpdated.getDoubleValue());
		assertEquals(constructorWithPrimitiveValues.getFloatValue(), entityToBeUpdated.getFloatValue());
		assertEquals(constructorWithPrimitiveValues.getIntValue(), entityToBeUpdated.getIntValue());
		assertEquals(constructorWithPrimitiveValues.getLongValue(), entityToBeUpdated.getLongValue());
		assertEquals(constructorWithPrimitiveValues.getShortValue(), entityToBeUpdated.getShortValue());
		assertEquals(constructorWithPrimitiveValues.getDecimalValue(), entityToBeUpdated.getDecimalValue());
		
		entityToBeUpdated.setShortValue((short) 2000);
		entityToBeUpdated.setLongValue(5000L);
		primitiveValuesConstructorRepository.save(entityToBeUpdated);
		final List<EntityWithPrimitiveValues> updatedEntity = (List<EntityWithPrimitiveValues>) primitiveValuesConstructorRepository.findAll();
		assertEquals(1, updatedEntity.size());
		assertEquals(entityToBeUpdated.getShortValue(), updatedEntity.get(0).getShortValue());
		assertEquals(entityToBeUpdated.getLongValue(), updatedEntity.get(0).getLongValue());
	}
	
	/**
	 * Tests the Save, Read and Update operations using enum fields for {@linkplain EntityWithEnumParams}
	 */
	@Test
	public void testConstructorWithEnumField() {
		final EntityWithEnumParams constructorWithEnumValues = new EntityWithEnumParams(EnumData.ENUM1);
		enumConstructorRepository.save(constructorWithEnumValues);
		
		final List<EntityWithEnumParams> retrievedEntity = (List<EntityWithEnumParams>) enumConstructorRepository.findAll();
		assertEquals(1, retrievedEntity.size());
		final EntityWithEnumParams entityToBeUpdated = retrievedEntity.get(0);
		assertEquals(constructorWithEnumValues.getEnumData(), entityToBeUpdated.getEnumData());
		
		entityToBeUpdated.setEnumData(EnumData.ENUM2);
		enumConstructorRepository.save(entityToBeUpdated);
		final List<EntityWithEnumParams> updatedEntity = (List<EntityWithEnumParams>) enumConstructorRepository.findAll();
		assertEquals(1, updatedEntity.size());
		assertEquals(entityToBeUpdated.getEnumData(), updatedEntity.get(0).getEnumData());
	}
	
	/**
	 * Tests the Save, Read and Update operations using primitive collection fields for {@linkplain EntityWithCollectionParams}
	 */
	@Test
	public void testConstructorWithPrimitiveCollectionFields() {
		final List<String> stringList = new ArrayList<>(Arrays.asList("List Data 1", "List Data 2", "List Data 3"));
		final Set<Long> longSet = new HashSet<>();
		longSet.add(Long.valueOf(123));
		longSet.add(Long.valueOf(346));
		final Map<String, String> stringMap = new HashedMap<String, String>();
		stringMap.put("key 1", "value 1");
		stringMap.put("key 2", "value 2");
		final EntityWithCollectionParams constructorWithCollectionValues = new EntityWithCollectionParams(stringList, longSet, stringMap);
		primitiveCollectionConstructorRepository.save(constructorWithCollectionValues);
		
		final List<EntityWithCollectionParams> retrievedEntityList = (List<EntityWithCollectionParams>) primitiveCollectionConstructorRepository.findAll();
		assertEquals(1, retrievedEntityList.size());
		
		final EntityWithCollectionParams entityToBeUpdated = retrievedEntityList.get(0);
		assertEquals(entityToBeUpdated.getStringList(), constructorWithCollectionValues.getStringList());
		assertEquals(entityToBeUpdated.getLongSet(), constructorWithCollectionValues.getLongSet());
		assertEquals(constructorWithCollectionValues.getStringMap().size(), entityToBeUpdated.getStringMap().size());
		assertTrue(entityToBeUpdated.getStringMap().entrySet().containsAll(constructorWithCollectionValues.getStringMap().entrySet()));
		
		stringMap.put("key 3", "value 3");
		stringList.add("753l");
		entityToBeUpdated.setStringList(stringList);
		entityToBeUpdated.setStringMap(stringMap);
		primitiveCollectionConstructorRepository.save(entityToBeUpdated);
		
		final List<EntityWithCollectionParams> updatedEntityList = (List<EntityWithCollectionParams>) primitiveCollectionConstructorRepository.findAll();
		assertEquals(1, updatedEntityList.size());
		final EntityWithCollectionParams updatedEntity = updatedEntityList.get(0);
		assertEquals(entityToBeUpdated.getStringList(), updatedEntity.getStringList());
		assertEquals(entityToBeUpdated.getStringMap().size(), updatedEntity.getStringMap().size());
		assertTrue(updatedEntity.getStringMap().entrySet().containsAll(entityToBeUpdated.getStringMap().entrySet()));
	}
	
	/**
	 * Tests the Save, Read and Update operations using enum fields for {@linkplain EntityWithEmbeddedParam}
	 */
	@Test
	public void testConstructorWithEmbeddedFields() {
		final Address address = new Address("#1", "Street", "City", "1234");
		final EntityWithEmbeddedParam constructorWithEmbeddedValues = new EntityWithEmbeddedParam(address);
		embeddedValueConstructorRepository.save(constructorWithEmbeddedValues);
		
		final List<EntityWithEmbeddedParam> retrievedEntity = (List<EntityWithEmbeddedParam>) embeddedValueConstructorRepository.findAll();
		assertEquals(1, retrievedEntity.size());
		final EntityWithEmbeddedParam entityToBeUpdated = retrievedEntity.get(0);
		final Address fetchedAddress = entityToBeUpdated.getAddress();
		assertEquals(address.getCity(), fetchedAddress.getCity());
		assertEquals(address.getDoorNo(), fetchedAddress.getDoorNo());
		assertEquals(address.getPincode(), fetchedAddress.getPincode());
		
		fetchedAddress.setPincode("7000");
		embeddedValueConstructorRepository.save(entityToBeUpdated);
		final List<EntityWithEmbeddedParam> updatedEntity = (List<EntityWithEmbeddedParam>) embeddedValueConstructorRepository.findAll();
		assertEquals(1, updatedEntity.size());
		assertEquals("7000", updatedEntity.get(0).getAddress().getPincode());
	}
	
	/**
	 * Tests the Save, Read and Update operations using link fields for {@linkplain EntityWithLinkParam}
	 */
	@Test
	public void testConstructorWithLinkFields() {
		final Employee employee = new Employee("EmpFN 1", "EmpLN 1", "EmpEM 1");
		final EntityWithLinkParam constructorWithLinkField = new EntityWithLinkParam(employee);
		linkFieldConstructorRepository.save(constructorWithLinkField);
		
		final List<EntityWithLinkParam> retrievedEntity = (List<EntityWithLinkParam>) linkFieldConstructorRepository.findAll();
		assertEquals(1, retrievedEntity.size());
		final EntityWithLinkParam entityToBeUpdated = retrievedEntity.get(0);
		final Employee fetchedEmployee = entityToBeUpdated.getEmployee();
		assertEquals(employee.getFirst(), fetchedEmployee.getFirst());
		assertEquals(employee.getLast(), fetchedEmployee.getLast());
		assertEquals(employee.getEmail(), fetchedEmployee.getEmail());
		
		final Employee newEmployee = new Employee("EmpFN 2", "EmpLN 2", "EmpEM 2");
		entityToBeUpdated.setEmployee(newEmployee);
		linkFieldConstructorRepository.save(entityToBeUpdated);
		final List<EntityWithLinkParam> updatedEntity = (List<EntityWithLinkParam>) linkFieldConstructorRepository.findAll();
		assertEquals(1, updatedEntity.size());
		final Employee updatedEmployee = updatedEntity.get(0).getEmployee();
		assertEquals(newEmployee.getFirst(), updatedEmployee.getFirst());
		assertEquals(newEmployee.getLast(), updatedEmployee.getLast());
		assertEquals(newEmployee.getEmail(), updatedEmployee.getEmail());
	}
	
	/**
	 * Tests the Save, Read and Update operations using link fields with collection for {@linkplain EntityWithCollectionLink}
	 */
	@Test
	public void testConstructorWithCollectionLinkFields() {
		final Employee employee1 = new Employee("EmpFN 1", "EmpLN 1", "EmpEM 1");
		final Employee employee2 = new Employee("EmpFN 2", "EmpLN 2", "EmpEM 2");
		final EntityWithCollectionLink constructorWithLinkField = new EntityWithCollectionLink(Arrays.asList(employee1, employee2));
		collectionLinkFieldConstructorRepository.save(constructorWithLinkField);
		
		final List<EntityWithCollectionLink> retrievedEntity = (List<EntityWithCollectionLink>) collectionLinkFieldConstructorRepository.findAll();
		assertEquals(1, retrievedEntity.size());
		final EntityWithCollectionLink entityToBeUpdated = retrievedEntity.get(0);
		final List<Employee> fetchedValue = entityToBeUpdated.getEmployees();
		assertEquals(2, fetchedValue.size());
		constructorWithLinkField.getEmployees().stream().forEach((e) -> assertTrue(assertEmployeeInList(e, fetchedValue)));
		
		final Employee newEmployee = new Employee("EmpFN 3", "EmpLN 3", "EmpEM 3");
		fetchedValue.add(newEmployee);
		collectionLinkFieldConstructorRepository.save(entityToBeUpdated);
		final List<EntityWithCollectionLink> updatedEntity = (List<EntityWithCollectionLink>) collectionLinkFieldConstructorRepository.findAll();
		assertEquals(1, updatedEntity.size());
		final List<Employee> updatedEmployees = updatedEntity.get(0).getEmployees();
		assertEquals(3, updatedEmployees.size());
		fetchedValue.stream().forEach((e) -> assertTrue(assertEmployeeInList(e, updatedEmployees)));
	}
	
	private boolean assertEmployeeInList(final Employee employee, final List<Employee> employeeList) {
		for (final Employee e : employeeList) {
			if (assertNotNull(e.getFirst()).equals(employee.getFirst()) && assertNotNull(e.getEmail()).equals(employee.getEmail())
					&& assertNotNull(e.getLast()).equals(employee.getLast())) {
				return true;
			}
		}
		return false;
	}
}

interface AccountRepository extends OrientRepository<Account> {}

interface DefaultConstructorRepository extends OrientRepository<EntityWithDefaultConstructor> {}

interface PrimitiveValuesConstructorRepository extends OrientRepository<EntityWithPrimitiveValues> {}

interface EnumValuesConstructorRepository extends OrientRepository<EntityWithEnumParams> {}

interface PrimitiveCollectionConstructorRepository extends OrientRepository<EntityWithCollectionParams> {}

interface EmbeddedValueConstructorRepository extends OrientRepository<EntityWithEmbeddedParam> {}

interface LinkFieldConstructorRepository extends OrientRepository<EntityWithLinkParam> {}

interface CollectionLinkFieldConstructorRepository extends OrientRepository<EntityWithCollectionLink> {}