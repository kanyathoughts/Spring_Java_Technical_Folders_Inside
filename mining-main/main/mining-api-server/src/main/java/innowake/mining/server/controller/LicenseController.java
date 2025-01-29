/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import innowake.base.license.iw.MaxensoLicenseManager;
import innowake.lib.license.LicenseException;
import innowake.lib.license.core.LicensePropertyGroup;
import innowake.lib.license.spi.IWDescriptors;
import innowake.mining.shared.io.LicenseExpirationInfo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Rest controller for returning License informations.
 */
@MiningUnsecuredRestController
@RequestMapping(value="${routes.api}", produces=MediaType.APPLICATION_JSON_VALUE)
public class LicenseController {

	/**
	 * URL pattern for getting license expiry information
	 */
	public static final String LICENSE_EXPIRY_INFO = "/license-expiry-info";

	/**
	 * Returns the information regarding license expiration.
	 *
	 * @return the information regarding license expiration.
	 * @throws LicenseException when the license check failed.
	 */
	@GetMapping(value = LICENSE_EXPIRY_INFO)
	@Operation(summary = "license expiry information", operationId = "getLicenseExpireInfo")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	@MiningUnsecuredRestEndpoint 
	public LicenseExpirationInfo getLicenseExpireInfo() throws LicenseException {
		final Date expiryDate =
				MaxensoLicenseManager.get().getLicensePropertyValue(LicensePropertyGroup.GLOBAL_GROUP.getId(), IWDescriptors.DESC_EXPIRES_ON, Date.class);
		final LicenseExpirationInfo licenseExpirationInfo = new LicenseExpirationInfo();
		if (expiryDate == null) {
			licenseExpirationInfo.setNeverExpires(true);
		} else {
			licenseExpirationInfo.setExpiryDate(new SimpleDateFormat("MM-dd-yyyy").format(expiryDate));
			licenseExpirationInfo.setDays(TimeUnit.DAYS.convert(expiryDate.getTime() - new Date().getTime(), TimeUnit.MILLISECONDS));
			licenseExpirationInfo.setNeverExpires(false);
		}
		return licenseExpirationInfo;
	}
}
