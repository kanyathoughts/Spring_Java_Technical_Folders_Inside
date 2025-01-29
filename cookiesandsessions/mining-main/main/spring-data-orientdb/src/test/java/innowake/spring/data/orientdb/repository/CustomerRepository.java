/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository;

import java.util.List;

import innowake.mining.shared.springdata.annotations.EdgeQuery;
import innowake.mining.shared.springdata.annotations.Query;
import innowake.spring.data.orientdb.integration.repository.domain.Customer;

/**
 * Repository to handle {@link Customer} entity.
 */
public interface CustomerRepository extends OrientRepository<Customer> {
	
	/**
	 * Returns customer whose link field address has a given city value.
	 *
	 * @param city string city name
	 * @return a {@link Customer} object
	 */
	@Query("select * from customer where address.city = ?")
	Customer findCustomerByAddressCity(final String city);
	
	
	/**
	 * Returns customers whose linkList field products has a given productName value.
	 *
	 * @param productName name of the product
	 * @return a list of {@link Customer} objects
	 */
	@Query("select * from customer where products contains (productName = ?)")
	List<Customer> findCustomerByProductsProductName(final String productName);
	
	/**
	 * Returns customers with given card name.
	 *
	 * @param accountName the cardName
	 * @return a list of {@link Customer} objects
	 */
	@Query("select * from customer where account.cardName = ?")
	List<Customer> findCustomerByAccountCardName(final String accountName);
	
	/**
	 * Returns customers with given value in embedded set.
	 *
	 * @param value the value to be matched
	 * @return a list of {@link Customer} objects
	 * 
	 * @Query("select * from Customer where ? in embeddedSet")
	 */
	List<Customer> findCustomerByEmbeddedSet(final String value);
	
	/**
	 * Returns customers with given value in embedded map.
	 * 
	 * @param key the key in the map
	 * @param value the value to be matched
	 * @return a list of {@link Customer} objects
	 * 
	 * @Query("select * from Customer where embeddedMap[?] = ?")
	 */
	List<Customer> findCustomerByEmbeddedMap(final String key, final String value);
	
	/**
	 * Returns customer whose link field address contains the given pincode value.
	 *
	 * @param pincode the pincode
	 * @return a list of {@link Customer} objects
	 */
	List<Customer> findCustomerByCustomerAddressPincode(final String pincode);
	
	/**
	 * Returns customer whose embedded field account contains the given cardNumber value.
	 *
	 * @param cardNumber the card number
	 * @return a list of {@link Customer} objects
	 */
	List<Customer> findCustomerByCustomerAccountCardNumberLike(final String cardNumber);
	
	/**
	 * Returns customer whose link list products contains the given productCode value.
	 *
	 * @param productCode the product code value
	 * @return a list of {@link Customer} objects
	 */
	List<Customer> findCustomerByProductsProductCode(final Long productCode);

	/**
	 * Returns customer related to products via edges productsReviewed containing the given productName value.
	 *
	 * @param productName the product name value
	 * @return a list of {@link Customer} objects
	 */
	@EdgeQuery
	List<Customer> findCustomerByReviewedInProductName(final String productName);
	
	/**
	 * Returns customer related to products via edges productsReviewed containing the given productName value.
	 *
	 * @param productName the product name value
	 * @return a list of {@link Customer} objects
	 */
	@EdgeQuery
	List<Customer> findCustomerByProductsReviewedProductName(final String productName);
	
	/**
	 * Return customer by customer name.
	 *
	 * @param customerName the name of the customer
	 * @return customer matching the name passed
	 */
	Customer findByName(final String customerName);
}
