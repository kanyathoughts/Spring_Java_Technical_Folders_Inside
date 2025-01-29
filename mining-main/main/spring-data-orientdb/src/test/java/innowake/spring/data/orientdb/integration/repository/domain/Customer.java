/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.repository.domain;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.EdgeDirection;
import innowake.mining.shared.springdata.annotations.CustomProperties;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;
import innowake.mining.shared.springdata.annotations.Property;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.mining.shared.springdata.annotations.Relationship;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomProperty;

/**
 * A vertex class mapped in orientDB.
 */
@Entity
public class Customer implements Comparable<Customer> {

	@RId
	@Nullable
	private String rid;	
	@Id(sequence = "Customer_Sequence")
	@Nullable
	private Long customerId;	
	@Property("customerName")
	@Nullable
	private String name;	
	@Nullable
	@JsonIgnoreProperties("customerReviewed")
	private List<Product> products; /* LinkList */	
	@Property("address")
	@Nullable
	private Address customerAddress; /* Embedded */	
	@Property("account")
	@Nullable
	private Account customerAccount; /* Link */	
	@Nullable
	private Set<String> embeddedSet;	
	@Nullable
	private Map<String, String> embeddedMap;	
	@Nullable
	@Relationship(name = "reviewed", direction = EdgeDirection.OUT, sequence = "Reviewed_Sequence")
	@JsonIgnoreProperties("out")
	private List<Reviewed> reviewed; /* edge relation */	
	@Nullable
	@Relationship(name = "reviewed", direction = EdgeDirection.OUT, sequence = "Reviewed_Sequence")
	@JsonIgnoreProperties("customerReviewed")
	private List<Product> productsReviewed; /* edge relation */	
	
	@Property( "subscribed")
	@Nullable
	private Product customerSubscribed; /* Link */	
	
	@Property("customProperties")
	@CustomProperties
	private Map<String, List<CustomProperty>> customCustomerProperties = new HashMap<>();	
	
	@Relationship(name = "Calls", direction = EdgeDirection.IN, sequence = "Reference_Sequence")
	@Nullable
	private Calls inCalls;	
	
	@Relationship(name = "Calls", direction = EdgeDirection.OUT, sequence = "Reference_Sequence")
	@Nullable
	private Calls outCalls;	
	
	@Relationship(name = "Calls", direction = EdgeDirection.OUT, sequence = "Reference_Sequence")
	@Nullable
	private List<Customer> callsCustomers;	
	
	@Property("embeddedMapSet")
	@Nullable
	private Map<String, Set<String>> customerEmbeddedMapSet;

	/**
	 * Default constructor required to create customer proxy instance.
	 */
	public Customer() {
	}

	/**
	 * Instantiates customer object.
	 * 
	 * @param customerName name of the customer
	 */
	public Customer(final String customerName) {
		super();
		this.name = customerName;
	}

	/**
	 * Instantiates customer object.
	 * 
	 * @param customerName name of the customer
	 * @param address customer address
	 */
	public Customer(final String customerName, final Address address) {
		super();
		this.name = customerName;
		this.customerAddress = address;
	}

	/**
	 * Instantiates customer object.
	 * 
	 * @param customerName name of the customer
	 * @param products id's of the product
	 */
	public Customer(final String customerName, final List<Product> products) {
		super();
		this.name = customerName;
		this.products = products;
	}
	
	/**
	 * Sets the record id.
	 * 
	 * @param rid the record id
	 */
	public void setRid(final String rid) {
		this.rid =  rid;
	}
	
	/**
	 * Returns the record id.
	 * 
	 * @return record id.
	 */
	public String getRid() {
		return assertNotNull(rid);
	}

	/**
	 * Returns customer id. 
	 * Customer id is null only for accessor invocation by spring data.
	 *
	 * @return customer id
	 */
	@Nullable
	public Long getCustomerId() {
		return customerId;
	}

	/**
	 * Sets customer id value.
	 * 
	 * @param customerId the id value
	 */
	public void setCustomerId(final Long customerId) {
		this.customerId = customerId;
	}

	/**
	 * Returns customer name.
	 *
	 * @return customer name
	 */
	public String getName() {
		return assertNotNull(name);
	}

	/**
	 * Sets customer name.
	 *
	 * @param customerName the customer name
	 */
	public void setName(final String customerName) {
		this.name = customerName;
	}

	/**
	 * Gets the products of the linked {@link Product}.
	 *
	 * @return the products of the linked {@link Product}
	 */
	@Nullable
	public List<Product> getProducts() {
		return products;
	}

	/**
	 * Sets the products of the linked class {@link Project}.
	 *
	 * @param products the products of the linked {@link Product}
	 */
	public void setProducts(final List<Product> products) {
		this.products = products;
	}

	/**
	 * Gets the address of the type {@link Address}.
	 *
	 * @return the address of the linked {@link Address}
	 */
	@Nullable
	public Address getCustomerAddress() {
		return customerAddress;
	}

	/**
	 * Sets the address of the linked class {@link Address}.
	 *
	 * @param address the address of the linked class {@link Address}
	 */
	public void setCustomerAddress(final Address address) {
		this.customerAddress = address;
	}

	/**
	 * Gets the object of the embedded type {@link Account}.
	 *
	 * @return the object of the embedded type {@link Account}.
	 */
	@Nullable
	public Account getCustomerAccount() {
		return customerAccount;
	}

	/**
	 * Sets the embedded type {@link Account}.
	 *
	 * @param account instance of type {@link Account}
	 */
	public void setCustomerAccount(final Account account) {
		this.customerAccount = account;
	}

	/**
	 * Gets the embedded set of {@link String} type.
	 *
	 * @return the embedded set of {@link String} type
	 */
	@Nullable
	public Set<String> getEmbeddedSet() {
		return embeddedSet;
	}

	/**
	 * Sets the embedded set of {@link String} type.
	 *
	 * @param embeddedSet instance of {@link Set} of type {@link String}
	 */
	public void setEmbeddedSet(final Set<String> embeddedSet) {
		this.embeddedSet = embeddedSet;
	}

	/**
	 * Gets the embedded map of {@link String} type.
	 *
	 * @return the embedded map of {@link String} type
	 */
	@Nullable
	public Map<String, String> getEmbeddedMap() {
		return embeddedMap;
	}

	/**
	 * Sets the embedded map of {@link String} type.
	 *
	 * @param embeddedMap map of {@link String} type
	 */
	public void setEmbeddedMap(final Map<String, String> embeddedMap) {
		this.embeddedMap = embeddedMap;
	}

	/**
	 * Gets a list of products reviewed.
	 *
	 * @return list of {@link Reviewed}
	 */
	@Nullable
	public List<Reviewed> getReviewed() {
		return reviewed;
	}

	/**
	 * Sets a list of {@link Reviewed} objects.
	 *
	 * @param reviewed list of products reviewed
	 */
	public void setReviewed(final List<Reviewed> reviewed) {
		this.reviewed = reviewed;
	}
	
	/**
	 * Gets a list of products.
	 *
	 * @return list of {@link Product}
	 */
	@Nullable
	public List<Product> getProductsReviewed() {
		return productsReviewed;
	}

	/**
	 * Sets a list of {@link Product} objects.
	 *
	 * @param productsReviewed list of products
	 */
	public void setProductsReviewed(final List<Product> productsReviewed) {
		this.productsReviewed = productsReviewed;
	}

	/**
	 * Gets a project object.
	 *
	 * @return a project object
	 */
	@Nullable
	public Product getCustomerSubscribed() {
		return customerSubscribed;
	}

	/**
	 * Sets a project object.
	 *
	 * @param subscribed a product  object
	 */
	public void setCustomerSubscribed(final Product subscribed) {
		this.customerSubscribed = subscribed;
	}

	/**
	 * Returns the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 *
	 * @return the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 */
	public Map<String, List<CustomProperty>> getCustomCustomerProperties() {
		return customCustomerProperties;
	}

	/**
	 * Sets the {@link Map} containing {@link List} of {@link CustomProperty}'s
	 *
	 * @param customProperties the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 */
	public void setCustomCustomerProperties(final Map<String, List<CustomProperty>> customProperties) {
		this.customCustomerProperties = customProperties;
	}
	
	/**
	 * Adds a {@link CustomProperty} to the class name provided.
	 *
	 * @param className name of the custom property class
	 * @param customProperty the {@link CustomProperty} to add
	 */
	public void addCustomCustomerProperty(final String className, final CustomProperty customProperty) {
		customCustomerProperties.computeIfAbsent(className, key -> new ArrayList<>()).add(customProperty);
	}
	
	/**
	 * Returns the {@link Calls} edge.
	 *
	 * @return the {@link Calls} edge
	 */
	@Nullable
	public Calls getInCalls() {
		return inCalls;
	}
	
	/**
	 * Sets the {@link Calls}.
	 *
	 * @param inCalls the {@link Calls}
	 */
	public void setInCalls(final Calls inCalls) {
		this.inCalls = inCalls;
	}

	
	/**
	 * Returns the called {@link Customer}.
	 *
	 * @return the called {@link Customer}
	 */
	@Nullable
	public Calls getOutCalls() {
		return outCalls;
	}

	
	/**
	 * Sets the called {@link Customer}.
	 *
	 * @param outCalls the called {@link Customer}
	 */
	public void setOutCalls(final Calls outCalls) {
		this.outCalls = outCalls;
	}
	
	/**
	 * Returns the called {@link Customer}.
	 *
	 * @return the called {@link Customer}.
	 */
	@Nullable
	public List<Customer> getCallsCustomers() {
		return callsCustomers;
	}
	
	/**
	 *  Sets the called {@link Customer}.
	 *
	 * @param callsCustomers the called {@link Customer}
	 */
	public void setCallsCustomers(final List<Customer> callsCustomers) {
		this.callsCustomers = callsCustomers;
	}
	
	/**
	 * Returns the Map containing value of type Set
	 *
	 * @return the embedded map containing value set
	 */
	@Nullable
	public Map<String, Set<String>> getCustomerEmbeddedMapSet() {
		return customerEmbeddedMapSet;
	}

	/**
	 *Sets the Map containing value of type Set
	 *
	 * @param embeddedMapSet the embedded map containing value set
	 */
	public void setCustomerEmbeddedMapSet(final Map<String, Set<String>> embeddedMapSet) {
		this.customerEmbeddedMapSet = embeddedMapSet;
	}

	@Override
	public int compareTo(@Nullable final Customer customer) {
		if (name != null && customer != null) {
			return name.compareTo(customer.name);
		}
		return 0;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		return super.equals(obj);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public String toString() {
		return "Customer [rid=" + rid + ", customerId=" + customerId + ", customerName=" + name + ", products=" + products + ", address=" + customerAddress
				+ ", account=" + customerAccount + ", embeddedSet=" + embeddedSet + ", embeddedMap=" + embeddedMap + ", productsReviewed=" + reviewed
				+ ", subscribed=" + customerSubscribed + "]";
	}

}
