/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.repository.domain;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Embedded;

/**
 * A embedded class in {@link Customer} class.
 */
@Embedded
public class Address {

	@Nullable private String doorNo;
	@Nullable private String streetName;
	@Nullable private String city;
	@Nullable private String pincode;

	/**
	 * Default constructor required to create address proxy instance.
	 */
	public Address() {
	}

	/**
	 * Instantiate an address.
	 * 
	 * @param doorNo the door number
	 * @param streetName the street name
	 * @param city the place city
	 * @param pincode the pin code
	 */
	public Address(final String doorNo, final String streetName, final String city, final String pincode) {
		super();
		this.doorNo = doorNo;
		this.streetName = streetName;
		this.city = city;
		this.pincode = pincode;
	}

	/**
	 * Returns the door number.
	 *
	 * @return the door number
	 */
	public String getDoorNo() {
		return assertNotNull(doorNo);
	}

	/**
	 * Sets the door number.
	 * 
	 * @param doorNo the door number
	 */
	public void setDoorNo(final String doorNo) {
		this.doorNo = doorNo;
	}

	/**
	 * Returns the street name.
	 *
	 * @return the street name
	 */
	public String getStreetName() {
		return assertNotNull(streetName);
	}

	/**
	 * Sets the street name.
	 * 
	 * @param streetName the street name
	 */
	public void setStreetName(final String streetName) {
		this.streetName = streetName;
	}

	/**
	 * Returns the city information.
	 *
	 * @return the city information
	 */
	@Nullable
	public String getCity() {
		return city;
	}

	/**
	 * Sets the city.
	 * 
	 * @param city the city
	 */
	public void setCity(final String city) {
		this.city = city;
	}

	/**
	 * Returns the pin code.
	 *
	 * @return the pin code
	 */
	public String getPincode() {
		return assertNotNull(pincode);
	}

	/**
	 * Sets the pin code.
	 * 
	 * @param pincode the pin code
	 */
	public void setPincode(final String pincode) {
		this.pincode = pincode;
	}

	@Override
	public String toString() {
		return "Address [doorNo=" + doorNo + ", streetName=" + streetName + ", city=" + city + ", pincode=" + pincode + "]";
	}

}
