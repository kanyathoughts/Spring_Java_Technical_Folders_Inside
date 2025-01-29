/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.util;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.file.Paths;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import innowake.base.license.iw.MaxensoLicenseManager;
import innowake.base.license.iw.NotLicensedException;
import innowake.lib.license.LicenseException;

/**
 * Tests for {@link LicenseChecker}
 */
public class LicenseCheckerTest {

	private static final String PATH = "/test-resources/innowake/mining/server/util/";
	private static final LicenseChecker licenseChecker = new LicenseChecker();
	
	@BeforeEach
	void beforeEach() {
		/* Reset the license before each test to clear the internal singleton instance */
		MaxensoLicenseManager.reset();
	}
	
	@Test
	void unexpiredLicenseDoesNotThrow() throws LicenseException {		
		loadLicense("mining_license_check_noExp.lic");
		licenseChecker.verifyLicense();
	}
	
	@Test
	void expiredMiningCoreLicenseThrows() {
		assertThrows(NotLicensedException.class, () -> {
			loadLicense("mining_license_check_mining_exp.lic");
			licenseChecker.verifyLicense();
		});
	}
	
	@Test
	void expiredGlobalLicenseThrows() {
		assertThrows(LicenseException.class, () -> {
			loadLicense("mining_license_check_global_exp.lic");
			licenseChecker.verifyLicense();
		});
	}
	
	private void loadLicense(final String licenseFile) throws LicenseException {
		MaxensoLicenseManager.get(getPath(licenseFile));	
	}
	
	private String getPath(final String file) {
		return Paths.get(System.getProperty("user.dir"), PATH, file).toString();
	}
}