/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.DataSchemaController.SCHEMAS_PER_PROJECT;
import static innowake.mining.server.controller.DataSchemaController.SCHEMA_FIELDS_URL;
import static innowake.mining.server.controller.DataSchemaController.SCHEMA_IMPORT_URL;
import static innowake.mining.server.controller.DataSchemaController.UPDATE_SCHEMA_FIELD_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.DataSchemaController;

/**
 * Authorization tests for the {@link DataSchemaController}.
 */
class DataSchemaControllerAuthorizationTest extends RestAuthorizationTests {
	
	/* Retrieve all schemas for the specified Project 
	 * Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0003() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0004() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0006() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0008() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0009() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0010() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0012() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(SCHEMAS_PER_PROJECT), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Import database schemas for the specified Project. */
	@Test void test0100() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0101() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0102() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0103() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0104() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0105() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0106() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0107() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0108() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0109() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0110() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0111() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(POST, url(SCHEMA_IMPORT_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieve field definitions for a Module. */
	@Test void test0200() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0202() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0203() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0204() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0205() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0206() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0207() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0208() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0209() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0211() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0212() throws Exception { test(GET, url(SCHEMA_FIELDS_URL), uriVars("1","2003"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Update the comment for FieldInfo based on moduleId and ordinal */
	@Test void test0300() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0301() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0302() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0303() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0304() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0305() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0306() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0307() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0308() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0310() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0311() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0312() throws Exception { test(PUT, url(UPDATE_SCHEMA_FIELD_URL), uriVars("1","2003","1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
