/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.MetamodelController.CLASS_METADATA_URL;
import static innowake.mining.server.controller.MetamodelController.PROPERTY_METADATA_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpStatus.NOT_FOUND;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.MetamodelController;

/**
 * Authorization tests for the {@link MetamodelController}.
 */
class MetamodelControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Retrieve class metadata */
	/* Expecting NOT_FOUND in case of successful authorization as the given class is an example and there are no class with test-entity */
	@Test void test0001() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0002() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0004() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0005() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0006() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0007() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0008() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0009() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0010() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND); }
	@Test void test0011() throws Exception { test(GET, url(CLASS_METADATA_URL), uriVars("test-entity"), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
	/* Retrieve class metadata of the given custom property*/
	/* Expecting NOT_FOUND in case of successful authorization as the given class/property is an example and there are no class/property with test-entity/test-property */
	@Test void test0100() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0101() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0103() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0104() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0105() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0106() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0107() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0108() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0109() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND); }
	@Test void test0110() throws Exception { test(GET, url(PROPERTY_METADATA_URL), uriVars("test-entity", "test-property"), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
}
