/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.repository.domain;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.EdgeDirection;
import innowake.mining.shared.springdata.annotations.CustomProperties;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.mining.shared.springdata.annotations.Relationship;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomProperty;

/**
 * A vertex node which is used as a linkList in {@link Customer} class.
 */
@Entity
public class Product {

	@RId
	@Nullable
	private String rid;
	@Id(sequence = "Product_Sequence")
	@Nullable
	private Long productId;
	@Nullable
	private String productName;
	@Nullable
	private Long productCode;
	@Relationship(name = "reviewed", direction = EdgeDirection.IN, sequence = "Reviewed_Sequence")
	@JsonIgnore
	@Nullable
	private List<Reviewed> customerReviewed;
	@Relationship(name = "orderedBy", direction = EdgeDirection.IN)
	@Nullable
	private OrderedBy customer;
	@Nullable
	private byte[] productImage;
	@CustomProperties
	private Map<String, List<CustomProperty>> customProperties = new HashMap<>();

	/**
	 * Default constructor required to create product proxy instance.
	 */
	public Product() {
	}

	/**
	 * Instantiate a product.
	 * 
	 * @param productName name of the product
	 * @param productCode code of the product
	 */
	public Product(final String productName, final Long productCode) {
		super();
		this.productName = productName;
		this.productCode = productCode;
	}

	/**
	 * Instantiate a product.
	 * 
	 * @param productId id of the product
	 * @param productName name of the product
	 * @param productCode code of the product
	 */
	public Product(final Long productId, final String productName, final Long productCode) {
		super();
		this.productId = productId;
		this.productName = productName;
		this.productCode = productCode;
	}

	/**
	 * Sets the record id.
	 * 
	 * @param rid the record id
	 */
	public void setRid(final String rid) {
		this.rid = rid;
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
	 * Returns product id.
	 *
	 * @return product id
	 */
	@Nullable
	public Long getProductId() {
		return productId;
	}

	/**
	 * Sets product id value.
	 * 
	 * @param productId the id value
	 */
	public void setProductId(final Long productId) {
		this.productId = productId;
	}

	/**
	 * Returns product name.
	 *
	 * @return product name
	 */
	public String getProductName() {
		return assertNotNull(productName);
	}

	/**
	 * Sets product name.
	 *
	 * @param productName the product name
	 */
	public void setProductName(final String productName) {
		this.productName = productName;
	}

	/**
	 * Returns product code.
	 *
	 * @return product code
	 */
	@Nullable
	public Long getProductCode() {
		return productCode;
	}

	/**
	 * Sets the product code.
	 *
	 * @param productCode the product code
	 */
	public void setProductCode(final Long productCode) {
		this.productCode = productCode;
	}

	/**
	 * Returns the {@link List} of {@link Reviewed} objects.
	 *
	 * @return the {@link List} of {@link Reviewed} objects
	 */
	@Nullable
	public List<Reviewed> getCustomerReviewed() {
		return customerReviewed;
	}

	/**
	 * Sets the {@link List} of {@link Reviewed} objects.
	 *
	 * @param customerReviewed Returns the {@link List} of {@link Reviewed} objects
	 */
	public void setCustomerReviewed(final List<Reviewed> customerReviewed) {
		this.customerReviewed = customerReviewed;
	}

	/**
	 * Returns the orderedBy object
	 *
	 * @return customer object
	 */
	@Nullable
	public OrderedBy getCustomer() {
		return customer;
	}

	/**
	 * Sets the orderedBy object
	 *
	 * @param customer set the orderedBy object
	 */
	public void setCustomer(final OrderedBy customer) {
		this.customer = customer;
	}

	/**
	 * Get's the image as byte array.
	 *
	 * @return returns the image as byte array.
	 */
	@Nullable
	public byte[] getProductImage() {
		return productImage;
	}

	/**
	 * Sets the image as byte array.
	 *
	 * @param productImage Sets the image as byte array.
	 */
	public void setProductImage(final byte[] productImage) {
		this.productImage = productImage;
	}
	
	/**
	 * Returns the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 *
	 * @return the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 */
	public Map<String, List<CustomProperty>> getCustomProperties() {
		return customProperties;
	}

	/**
	 * Sets the {@link Map} containing {@link List} of {@link CustomProperty}'s
	 *
	 * @param customProperties the {@link Map} containing {@link List} of {@link CustomProperty}'s.
	 */
	public void setCustomProperties(final Map<String, List<CustomProperty>> customProperties) {
		this.customProperties = customProperties;
	}
	
	/**
	 * Adds a {@link CustomProperty} to the class name provided.
	 *
	 * @param className name of the custom property class
	 * @param customProperty the {@link CustomProperty} to add
	 */
	public void addCustomProperty(final String className, final CustomProperty customProperty) {
		customProperties.computeIfAbsent(className, key -> new ArrayList<>()).add(customProperty);
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
		return "Product [rid=" + rid + ", productId=" + productId + ", productName=" + productName + ", productCode=" + productCode + "]";
	}

}
