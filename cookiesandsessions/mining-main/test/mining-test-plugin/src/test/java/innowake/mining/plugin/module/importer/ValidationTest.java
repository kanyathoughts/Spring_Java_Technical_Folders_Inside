/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import org.junit.Test;
import innowake.mining.plugin.base.ValidationException;

/**
 * Tests regarding the validation of Excel files before the actual import occurs.
 */
public class ValidationTest {

	/**
	 * Test old format.
	 * 
	 * @throws ValidationException this is expected
	 */
	@Test(expected = ValidationException.class)
	public void testOldFormat() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-02_15-28-32.xlsx";
		callDiscoveryExcelValidator(path);
	}

	/**
	 * Test wrong property format.
	 * 
	 * @throws ValidationException this is expected
	 */
	@Test(expected = ValidationException.class)
	public void testWrongPropertyValue() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16-wrong-property-value.xlsx";
		callDiscoveryExcelValidator(path);
	}
	
	/**
	 * Test missing required column.
	 * 
	 * @throws ValidationException this is expected
	 */
	@Test(expected = ValidationException.class)
	public void testMissingRequiredColumn() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16-missing-column.xlsx";
		callDiscoveryExcelValidator(path);
	}

	/**
	 * Test missing required columns.
	 * 
	 * @throws ValidationException this is expected
	 */
	@Test(expected = ValidationException.class)
	public void testMissingRequiredColumns() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16-missing-columns.xlsx";
		callDiscoveryExcelValidator(path);
	}

	/**
	 * Test missing Modules sheet.
	 * 
	 * @throws ValidationException this is expected
	 */
	@Test(expected = ValidationException.class)
	public void testMissingModulesSheet() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16-missing-modules-sheet.xlsx";
		callDiscoveryExcelValidator(path);
	}
	
	/**
	 * Test missing Dependencies sheet.
	 * 
	 * @throws ValidationException this is expected
	 */
	@Test(expected = ValidationException.class)
	public void testMissingDependenciesSheet() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16-missing-dependencies-sheet.xlsx";
		callDiscoveryExcelValidator(path);
	}
	
	/**
	 * Test missing required Dependencies columns.
	 * 
	 * @throws ValidationException this is expected
	 */
	@Test(expected = ValidationException.class)
	public void testMissingRequiredDependenciesColumns() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16-missing-dependencies-columns.xlsx";
		callDiscoveryExcelValidator(path);
	}
	
	/**
	 * Test missing required Modules Module metrics columns.
	 * 
	 * @throws ValidationException this is expected
	 */
	@Test(expected = ValidationException.class)
	public void testMissingRequiredModuleMetricsColumns() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/wmin-129-missing-module-metrics-columns.xlsx";
		callDiscoveryExcelValidator(path);
	}
	
	/**
	 * Test successful import.
	 * 
	 * @throws ValidationException this is not expected
	 */
	@Test
	public void testSuccess() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2020-02-10_10-59-34.xlsx";
		callDiscoveryExcelValidator(path);
	}
	
	/**
	 * Test compatibility: missing sheet and column.
	 * 
	 * @throws ValidationException this is not expected
	 */
	@Test
	public void testCompatibilityMissingSheetAndColoumn() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-10-12_02-22-52_compatibility.xlsx";
		callDiscoveryExcelValidator(path);
	}
	
	/**
	 * Test missing locations in Modules and Dependencies sheet.
	 * 
	 * @throws ValidationException this is not expected
	 */
	@Test
	public void testMissingLocationsInModulesAndDependenciesSheet() throws ValidationException {
		final String path = "src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-2673.xlsx";
		callDiscoveryExcelValidator(path);
	}

	private void callDiscoveryExcelValidator(final String path) throws ValidationException {
		DiscoveryExcelValidator.validateAndReturnModulePaths(path);

	}
	
}
