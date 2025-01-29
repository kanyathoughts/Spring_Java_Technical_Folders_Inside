/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.repository.domain;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Date;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.RId;
import org.springframework.data.annotation.PersistenceCreator;

/**
 * A link class attached to {@link Customer} class in database.
 */
@Entity
public class Account {

	@RId
	@Nullable
	private String rid;
	@Nullable
	private String cardNumber;
	@Nullable
	private String cardName;
	@Nullable
	private Date expiryDate;

	/**
	 * Instantiates account object.
	 * 
	 * @param cardNumber the card number
	 * @param cardName the card name
	 * @param expiryDate the expiry date of the card
	 */
	@PersistenceCreator
	public Account(final String cardNumber, final String cardName, final Date expiryDate) {
		super();
		this.cardNumber = cardNumber;
		this.cardName = cardName;
		this.expiryDate = expiryDate;
	}
	
	/**
	 * Instantiates account object.
	 * 
	 * @param cardNumber the card number
	 * @param cardName the card name
	 */
	public Account(final String cardNumber, final String cardName) {
		super();
		this.cardNumber = cardNumber;
		this.cardName = cardName;
	}
	
	/**
	 * Default constructor
	 */
	public Account() {}

	/**
	 * Returns the record id.
	 * 
	 * @return record id.
	 */
	public String getRid() {
		return assertNotNull(rid);
	}

	/**
	 * Returns card number.
	 *
	 * @return card number
	 */
	@Nullable
	public String getCardNumber() {
		return cardNumber;
	}

	/**
	 * Sets card number value.
	 * 
	 * @param cardNumber the card number value
	 */
	public void setCardNumber(final String cardNumber) {
		this.cardNumber = cardNumber;
	}

	/**
	 * Returns card name.
	 *
	 * @return card name
	 */
	@Nullable
	public String getCardName() {
		return cardName;
	}

	/**
	 * Sets card name value.
	 * 
	 * @param cardName the card name value
	 */
	public void setCardName(final String cardName) {
		this.cardName = cardName;
	}

	/**
	 * Returns expiry date.
	 *
	 * @return expiry date
	 */
	@Nullable
	public Date getExpiryDate() {
		return expiryDate;
	}

	/**
	 * Sets expiry date value.
	 * 
	 * @param expiryDate the expiry date value
	 */
	public void setExpiryDate(final Date expiryDate) {
		this.expiryDate = expiryDate;
	}

	@Override
	public String toString() {
		return "Account [rid=" + rid + ", cardNumber=" + cardNumber + ", cardName=" + cardName + ", expiryDate=" + expiryDate + "]";
	}

}
