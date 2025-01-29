/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.io;

import innowake.lib.core.lang.Nullable;

/**
 * License expiration related information.
 */
public class LicenseExpirationInfo {

	@Nullable
	private String expiryDate;
	@Nullable
	private Long days;
	private boolean neverExpires;
	
	/**
	 * Returns the date of license expiration.
	 *
	 * @return the date when license will expire.
	 */
	public @Nullable String getExpiryDate() {
		return expiryDate;
	}
	
	/**
	 * Set the date of license expiration.
	 *
	 * @param expiryDate the date when license will expire.
	 */
	public void setExpiryDate(final String expiryDate) {
		this.expiryDate = expiryDate;
	}
	
	/**
	 * Returns the number of Days until license expires.
	 *
	 * @return the number of Days until license expires.
	 */
	public @Nullable Long getDays() {
		return days;
	}
	
	/**
	 * Set the number of Days until license expires.
	 *
	 * @param days the number of Days until license expires.
	 */
	public void setDays(final Long days) {
		this.days = days;
	}
	
	/**
	 * Returns whether there is an expiry date in the license or not.
	 *
	 * @return boolean indicating whether there is an expiry date in the license or not.
	 */
	public boolean isNeverExpires() {
		return neverExpires;
	}
	
	/**
	 * Set a boolean indicating whether there is an expiry date in the license or not.
	 *
	 * @param neverExpires a boolean indicating whether there is an expiry date in the license or not.
	 */
	public void setNeverExpires(final boolean neverExpires) {
		this.neverExpires = neverExpires;
	}
}
