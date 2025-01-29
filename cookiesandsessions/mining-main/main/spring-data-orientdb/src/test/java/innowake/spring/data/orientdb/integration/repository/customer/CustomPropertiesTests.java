/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.customer;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import com.orientechnologies.orient.core.exception.OSchemaException;
import com.orientechnologies.orient.core.exception.OValidationException;
import innowake.mining.shared.model.CustomPropertyDataType;
import innowake.mining.shared.model.Entity;
import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.integration.repository.domain.Customer;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomProperty;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;
import innowake.spring.data.orientdb.repository.CustomerRepository;

/**
 * Test case for custom properties.
 */

public class CustomPropertiesTests extends AbstractIntegrationTests {

	@Autowired
	private CustomerRepository customerRepository;

	private static final String CUSTOMER_CUSTOM_PROPERTY_CLASS = "CustomerCustomProperties";
	private static final String CUSTOMER_CUSTOM_PROPERTY_FIELD_INTEGER = "customCustomerIntegerProperty";
	private static final String CUSTOMER_NAME = "customer 1";
	
	private static final String PRODUCT_CUSTOM_PROPERTY_CLASS = "ProductCustomProperties";
	private static final String PRODUCT_CUSTOM_PROPERTY_FIELD_STRING = "customProductStringProperty";
	private static final String PRODUCT_NAME = "IPhone";
	private static final Long PRODUCT_CODE = 12L;

	/**
	 * Clears data saved in oreintDB.
	 */
	@Before
	public void init() {
		clearData("Customer", "Product", CUSTOMER_CUSTOM_PROPERTY_CLASS, PRODUCT_CUSTOM_PROPERTY_CLASS);
	}

	/**
	 * Test case to validate save read and update of custom property.
	 */
	@Test
	public void testCustomPropertiesSaveReadUpdate() {
		final CustomProperty customPropertyToBeSaved = new CustomProperty(CUSTOMER_CUSTOM_PROPERTY_FIELD_INTEGER, "333", CustomPropertyDataType.INTEGER);
		final Customer customer = new Customer(CUSTOMER_NAME);
		customer.addCustomCustomerProperty(CUSTOMER_CUSTOM_PROPERTY_CLASS, customPropertyToBeSaved);
		/* save custom property*/
		customerRepository.save(customer);
		final Customer fetchedCustomer = customerRepository.findByName(CUSTOMER_NAME);
		final Map<String, List<CustomProperty>> fetchedCustomProperty = assertNotNull(fetchedCustomer.getCustomCustomerProperties());
		assertFalse("CustomProperty should not be empty", fetchedCustomProperty.isEmpty());
		final CustomProperty customPropertyToBeUpdated = getCustomPropertyByName(CUSTOMER_CUSTOM_PROPERTY_CLASS, CUSTOMER_CUSTOM_PROPERTY_FIELD_INTEGER, fetchedCustomer)
				.get();
		assertEquals("333", customPropertyToBeUpdated.getValue());
		customPropertyToBeUpdated.setValue("444");
		/* update custom property*/
		customerRepository.save(fetchedCustomer);
		final Customer updatedCustomer = customerRepository.findByName(CUSTOMER_NAME);
		final CustomProperty updatedCustomProperty = getCustomPropertyByName(CUSTOMER_CUSTOM_PROPERTY_CLASS, CUSTOMER_CUSTOM_PROPERTY_FIELD_INTEGER, updatedCustomer).get();
		assertEquals("444", updatedCustomProperty.getValue());
	}

	/**
	 * Test case to validate save read and update of custom properties in nested entities.
	 */
	@Test
	public void testCustomPropertiesSaveReadUpdateOfNestedEntities() {
		final String valueToBeSaved="Value Saved";
		final String valueToBeUpdated="Value Updated";
		
		/* save customer entity containing product class with custom properties*/
		final Customer customer = new Customer(CUSTOMER_NAME);
		final Product product = new Product(PRODUCT_NAME, PRODUCT_CODE);
		final CustomProperty customPropertyProduct = new CustomProperty(PRODUCT_CUSTOM_PROPERTY_FIELD_STRING, valueToBeSaved, CustomPropertyDataType.STRING);
		product.addCustomProperty(PRODUCT_CUSTOM_PROPERTY_CLASS, customPropertyProduct);
		customer.setProducts(Arrays.asList(product));
		customerRepository.save(customer);
		
		/* read customer entity containing product class with custom properties*/
		final Customer customerSaved = customerRepository.findByName(CUSTOMER_NAME);
		final Product productSaved = assertNotNull(customerSaved.getProducts()).get(0);
		final CustomProperty customPropertyProductSaved = productSaved.getCustomProperties().get(PRODUCT_CUSTOM_PROPERTY_CLASS).get(0);
		assertEquals(valueToBeSaved, customPropertyProductSaved.getValue());
		assertEquals(PRODUCT_CUSTOM_PROPERTY_FIELD_STRING, customPropertyProductSaved.getName());
		assertEquals(CustomPropertyDataType.STRING, customPropertyProductSaved.getDataType());
		
		/* update customer entity containing product class with custom properties*/
		customPropertyProductSaved.setValue(valueToBeUpdated);
		customerRepository.save(customerSaved);
		final Customer customerUpdated = customerRepository.findByName(CUSTOMER_NAME);
		final Product productUpdated = assertNotNull(customerUpdated.getProducts()).get(0);
		final CustomProperty customPropertyUpdated = productUpdated.getCustomProperties().get(PRODUCT_CUSTOM_PROPERTY_CLASS).get(0);
		assertEquals(valueToBeUpdated, customPropertyUpdated.getValue());
		assertEquals(PRODUCT_CUSTOM_PROPERTY_FIELD_STRING, customPropertyUpdated.getName());
		assertEquals(CustomPropertyDataType.STRING, customPropertyUpdated.getDataType());
	}

	/**
	 * Test case to validate custom property schema is not modified with in application.
	 */
	@Test
	public void testCustomPropertiesSchemaShouldNotModifiedUponSave() {
		final String propertyName = "customCustomerMandatoryProperty";
		final Customer customer = new Customer(CUSTOMER_NAME);
		final CustomProperty customProperty = new CustomProperty(propertyName, Boolean.TRUE, CustomPropertyDataType.BOOLEAN);
		customer.addCustomCustomerProperty(CUSTOMER_CUSTOM_PROPERTY_CLASS, customProperty);
		customerRepository.save(customer);

		final Customer fetchedCustomer = customerRepository.findByName(CUSTOMER_NAME);
		final Optional<CustomProperty> fetchedCustomProperty = getCustomPropertyByName(CUSTOMER_CUSTOM_PROPERTY_CLASS, propertyName, fetchedCustomer);
		assertEquals(propertyName, fetchedCustomProperty.get().getName());
		assertEquals("true", fetchedCustomProperty.get().getValue());
		assertEquals(CustomPropertyDataType.STRING, fetchedCustomProperty.get().getDataType());
	}

	/**
	 * Test case to validate list as custom property .
	 */
	@Test
	public void testCustomPropertiesList() {
		final String propertyName = "customCustomerListPropertyWithMax";
		final Customer customer1 = new Customer(CUSTOMER_NAME);
		final Customer customer2 = new Customer("customer 2");
		final Customer customer3 = new Customer("customer 3");
		final Customer savedCustomer2 = customerRepository.save(customer2);
		final Customer savedCustomer3 = customerRepository.save(customer3);
		final String[] rids = {
				((IEntityProxy) savedCustomer2).__getRid().toString(), ((IEntityProxy) savedCustomer3).__getRid().toString()
		};
		final CustomProperty list = new CustomProperty(propertyName, Arrays.asList(rids), CustomPropertyDataType.LINKLIST);
		customer1.addCustomCustomerProperty(CUSTOMER_CUSTOM_PROPERTY_CLASS, list);
		customerRepository.save(customer1);

		final Customer fetchedCustomer = customerRepository.findByName(CUSTOMER_NAME);
		final Optional<CustomProperty> fetchedCustomProperty = getCustomPropertyByName(CUSTOMER_CUSTOM_PROPERTY_CLASS, propertyName, fetchedCustomer);
		assertEquals(propertyName, fetchedCustomProperty.get().getName());
		assertEquals("[" + Arrays.asList(rids).stream().map(rid -> "\"" + rid + "\"").collect(Collectors.joining(",")) + "]",
				fetchedCustomProperty.get().getValue());
		assertEquals(CustomPropertyDataType.LINKLIST, fetchedCustomProperty.get().getDataType());
	}

	/**
	 * Test case to validate date custom property.
	 */
	@Test
	public void testCustomPropertiesDate() {
		validateSaveReadUpdateCustomProperties("customCustomerDateProperty", "2019-07-09", CustomPropertyDataType.DATE, "2020-08-10");
	}

	/**
	 * Test case to validate date-time custom property.
	 */
	@Test
	public void testCustomPropertiesDateTime() {
		validateSaveReadUpdateCustomProperties("customCustomerDatetimeProperty", "2019-07-09 01:23:45", CustomPropertyDataType.DATETIME, "2020-08-10 01:45:23");
	}

	/**
	 * Test case to validate boolean custom property.
	 */
	@Test
	public void testCustomPropertiesBoolean() {
		validateSaveReadUpdateCustomProperties("customCustomerBooleanProperty", "false", CustomPropertyDataType.BOOLEAN, "true");
	}

	/**
	 * Test case to validate integer custom property.
	 */
	@Test
	public void testCustomPropertiesInteger() {
		validateSaveReadUpdateCustomProperties("customCustomerIntegerProperty", "1", CustomPropertyDataType.INTEGER, "9");
	}

	/**
	 * Test case to validate short custom property.
	 */
	@Test
	public void testCustomPropertiesShort() {
		validateSaveReadUpdateCustomProperties("customCustomerShortProperty", "1", CustomPropertyDataType.SHORT, "9");
	}

	/**
	 * Test case to validate long custom property.
	 */
	@Test
	public void testCustomPropertiesLong() {
		validateSaveReadUpdateCustomProperties("customCustomerLongProperty", "1", CustomPropertyDataType.LONG, "9");
	}

	/**
	 * Test case to validate float custom property.
	 */
	@Test
	public void testCustomPropertiesFloat() {
		validateSaveReadUpdateCustomProperties("customCustomerFloatProperty", "1.1", CustomPropertyDataType.FLOAT, "9.9");
	}

	/**
	 * Test case to validate double custom property.
	 */
	@Test
	public void testCustomPropertiesDouble() {
		validateSaveReadUpdateCustomProperties("customCustomerDoubleProperty", "1.1", CustomPropertyDataType.DOUBLE, "9.9");
	}

	/**
	 * Test case to validate byte custom property.
	 */
	@Test
	public void testCustomPropertiesByte() {
		validateSaveReadUpdateCustomProperties("customCustomerByteProperty", "1", CustomPropertyDataType.BYTE, "9");
	}

	/**
	 * Test case to validate decimal custom property.
	 */
	@Test
	public void testCustomPropertiesDecimal() {
		validateSaveReadUpdateCustomProperties("customCustomerDecimalProperty", "1", CustomPropertyDataType.DECIMAL, "9");
	}
	
	/**
	 * Test case to validate string custom property.
	 */
	@Test
	public void testCustomPropertiesString() {
		validateSaveReadUpdateCustomProperties("customCustomerMandatoryProperty", "Value To Save", CustomPropertyDataType.STRING, "Value To Update");
	}

	/**
	 * Test case to validate reference custom property.
	 */
	@Test
	public void testCustomPropertiesReference() {
		final Customer customer = new Customer("customer test");
		final Customer savedCustomer = customerRepository.save(customer);
		final String ridToSave = ((IEntityProxy) savedCustomer).__getRid().getIdentity().toString();

		final Customer customer2 = new Customer("customer test 2");
		final Customer savedCustomer2 = customerRepository.save(customer2);
		final String ridToUpdate = ((IEntityProxy) savedCustomer2).__getRid().getIdentity().toString();

		validateSaveReadUpdateCustomProperties("customCustomerReferenceProperty", ridToSave, CustomPropertyDataType.REFERENCE, ridToUpdate);
	}

	/**
	 * Test case to validate save with invalid value type throws {@link OValidationException}.
	 */
	@Test
	public void testCustomPropertiesSaveWithInvalidValueType() {
		final CustomProperty customPropertyToBeSaved = new CustomProperty(CUSTOMER_CUSTOM_PROPERTY_FIELD_INTEGER, "AAA", CustomPropertyDataType.INTEGER);
		final Customer customer = new Customer(CUSTOMER_NAME);
		customer.addCustomCustomerProperty(CUSTOMER_CUSTOM_PROPERTY_CLASS, customPropertyToBeSaved);
		final OValidationException exception = assertThrows(OValidationException.class, () -> customerRepository.save(customer));
		assertThat(exception.getMessage(), containsString("Invalid value AAA for custom property : customCustomerIntegerProperty"));
	}

	/**
	 * Test case to validate save on non existing custom property field throws {@link OValidationException}.
	 */
	@Test
	public void testCustomPropertiesSaveOnNonExistingPropertyField() {
		final CustomProperty customPropertyToBeSaved = new CustomProperty("iDontExistAsACustomPropertyField", "111", CustomPropertyDataType.INTEGER);
		final Customer customer = new Customer(CUSTOMER_NAME);
		customer.addCustomCustomerProperty(CUSTOMER_CUSTOM_PROPERTY_CLASS, customPropertyToBeSaved);
		final OValidationException exception = assertThrows(OValidationException.class, () -> customerRepository.save(customer));
		assertThat(exception.getMessage(), containsString("Custom property iDontExistAsACustomPropertyField doesn't exist in CustomerCustomProperties class"));
	}

	/**
	 * Test case to validate save on non existing custom property class throws {@link OSchemaException}.
	 */
	@Test
	public void testCustomPropertiesSaveOnNonExistingClass() {
		final CustomProperty customPropertyToBeSaved = new CustomProperty(CUSTOMER_CUSTOM_PROPERTY_FIELD_INTEGER, "222", CustomPropertyDataType.INTEGER);
		final Customer customer = new Customer(CUSTOMER_NAME);
		customer.addCustomCustomerProperty("IDontExistAsACustomPropertyClass", customPropertyToBeSaved);
		final OValidationException exception = assertThrows(OValidationException.class, () -> customerRepository.save(customer));
		assertThat(exception.getMessage(), containsString("Custom property class IDontExistAsACustomPropertyClass doesn't exist in database"));
	}

	private void validateSaveReadUpdateCustomProperties(final String propertyName, final String propertyValueToSave,
			final CustomPropertyDataType customPropertyDataType, final String propertyValueToUpdate) {
		/* save custom property*/
		final Customer customer = new Customer(CUSTOMER_NAME);
		final CustomProperty customProperty = new CustomProperty(propertyName, propertyValueToSave, customPropertyDataType);
		customer.addCustomCustomerProperty(CUSTOMER_CUSTOM_PROPERTY_CLASS, customProperty);
		customerRepository.save(customer);

		/* read custom property*/
		final Customer savedCustomer = customerRepository.findByName(CUSTOMER_NAME);
		final Optional<CustomProperty> savedCustomProperty = getCustomPropertyByName(CUSTOMER_CUSTOM_PROPERTY_CLASS, propertyName, savedCustomer);
		assertCustomProperty(savedCustomProperty, propertyName, propertyValueToSave, customPropertyDataType);

		/* update custom property*/
		savedCustomProperty.get().setValue(propertyValueToUpdate);
		customerRepository.save(savedCustomer);
		final Customer updatedCustomer = customerRepository.findByName(CUSTOMER_NAME);
		final Optional<CustomProperty> updatedCustomProperty = getCustomPropertyByName(CUSTOMER_CUSTOM_PROPERTY_CLASS, propertyName, updatedCustomer);
		assertCustomProperty(updatedCustomProperty, propertyName, propertyValueToUpdate, customPropertyDataType);
	}
	
	private void assertCustomProperty(final Optional<CustomProperty> customProperty, final String propertyName, final String propertyValue,
			final CustomPropertyDataType customPropertyDataType) {
		assertTrue(String.format("custom property '%s' should present in class '%s'", propertyName, CUSTOMER_CUSTOM_PROPERTY_CLASS),
				customProperty.isPresent());
		assertEquals(propertyName, customProperty.get().getName());
		assertEquals(propertyValue, customProperty.get().getValue());
		assertEquals(customPropertyDataType, customProperty.get().getDataType());
	}

	private <T extends Entity> Optional<CustomProperty> getCustomPropertyByName(final String className, final String propertyName, final Customer entity) {
		if (entity.getCustomCustomerProperties().containsKey(className)) {
			final List<CustomProperty> customProperties = entity.getCustomCustomerProperties().get(className);
			return customProperties.stream().filter(p -> assertNotNull(p.getName()).equals(propertyName)).findFirst();
		}
		return Optional.empty();
	}
}
