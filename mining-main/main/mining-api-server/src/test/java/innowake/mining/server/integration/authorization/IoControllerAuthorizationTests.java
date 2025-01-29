/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.util.tuple.Tuple2.of;
import static innowake.mining.server.controller.IoController.EFFORT_SUMMARY_URL;
import static innowake.mining.server.controller.IoController.EXPORT_FORMAT_BY_TYPE_URL;
import static innowake.mining.server.controller.IoController.EXPORT_FORMAT_COLLECTIONS_URL;
import static innowake.mining.server.controller.IoController.FILE;
import static innowake.mining.server.controller.IoController.PROJECT_AS_CSV_URL;
import static innowake.mining.server.controller.IoController.PROJECT_AS_EXCEL_URL;
import static innowake.mining.server.controller.IoController.PROJECT_AS_FILE_URL;
import static innowake.mining.server.controller.IoController.PROJECT_AS_FILE_URL;
import static innowake.mining.server.controller.IoController.SOURCE_OBJECTS_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static java.util.Arrays.asList;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.NOT_IMPLEMENTED;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;

import innowake.lib.core.util.tuple.Tuple2;
import org.apache.http.entity.ContentType;
import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.IoController;

import java.util.List;

/**
 * Authorization Tests for {@link IoController}.
 */
class IoControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Export Excel file for given Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0001() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0003() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0006() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0007() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0008() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0009() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0010() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0011() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0012() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieves Source Object References for given Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0100() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(admin()), OK); }
	@Test void test0101() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("5"), params(asList(of("baseRevision", "1"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0103() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(miningManager()), OK); }
	@Test void test0105() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(miningEditor()), OK); }
	@Test void test0106() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(miningViewer()), OK); }
	@Test void test0107() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0108() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0109() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0110() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0111() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(GET, url(SOURCE_OBJECTS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Retrieves Source Object References for given Project. */
	/* Expecting OK in case of successful authorization. */
	@Test void test0200() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(admin()), OK); }
	@Test void test0201() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(admin()), OK); }
	@Test void test0202() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0203() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(miningManager()), OK); }
	@Test void test0204() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(miningEditor()), OK); }
	@Test void test0205() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(miningViewer()), OK); }
	@Test void test0206() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0207() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0208() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0209() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0210() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0211() throws Exception { test(GET, url(EXPORT_FORMAT_COLLECTIONS_URL), uriVars("1"), params(asList(of("baseRevision", "1"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Exports data from the given project into the requested format. */
	/* Expecting NOT_IMPLEMENTED in case of successful authorization as the format type is invalid. */
	@Test void test0300() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(admin()), NOT_IMPLEMENTED); }
	@Test void test0301() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(admin()), NOT_IMPLEMENTED); }
	@Test void test0302() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(clientAdmin()), NOT_IMPLEMENTED); }
	@Test void test0303() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0304() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(miningManager()), NOT_IMPLEMENTED); }
	@Test void test0305() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0306() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(miningEditor()), NOT_IMPLEMENTED); }
	@Test void test0307() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(miningViewer()), NOT_IMPLEMENTED); }
	@Test void test0308() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(discoveryManager()), NOT_IMPLEMENTED); }
	@Test void test0309() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(discoveryEditor()), NOT_IMPLEMENTED); }
	@Test void test0310() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(discoveryViewer()), NOT_IMPLEMENTED); }
	@Test void test0311() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0312() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0313() throws Exception { test(GET, url(EXPORT_FORMAT_BY_TYPE_URL), uriVars("1", "test-format"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Exports a Discovery Effort Summary Excel workbook from the given project. */
	/* Expecting NOT_FOUND in case of successful authorization as data is not available for given project. */
	@Test void test0400() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0401() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0402() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0403() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0404() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0405() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0406() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0407() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0408() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0409() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0410() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0411() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0412() throws Exception { test(GET, url(EFFORT_SUMMARY_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Import CSV file into given Project. */
	/* Expecting NO_CONTENT in case of successful authorization. */
	@Test void test0500() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(admin()), NO_CONTENT, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0501() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("5"), noParams(), roles(admin()), NOT_FOUND, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0502() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(clientAdmin()), NO_CONTENT, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0503() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(clientAdmin(2)), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0504() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(miningManager()), NO_CONTENT, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0505() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(miningManager(1, 2)), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0506() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(miningEditor()), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0507() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(miningViewer()), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0508() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(discoveryManager()), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0509() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(discoveryEditor()), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0510() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(discoveryViewer()), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0511() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(discoveryLightManager()), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0512() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(discoveryLightEditor()), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0513() throws Exception { multiPartTests(POST, url(PROJECT_AS_CSV_URL), uriVars("1"), noParams(), roles(discoveryLightViewer()), FORBIDDEN, FILE, "foo.csv", ContentType.MULTIPART_FORM_DATA); }
	
	/* Import Excel file into given Project. */
	/* Expecting BAD_REQUEST in case of successful authorization as the excel file provided does not exist. */
	@Test void test0600() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(admin()), BAD_REQUEST, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0601() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("5"), noParams(), roles(admin()), NOT_FOUND, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0602() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(clientAdmin()), BAD_REQUEST, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0603() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(clientAdmin(2)), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0604() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(miningManager()), BAD_REQUEST, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0605() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(miningManager(1, 2)), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0606() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(miningEditor()), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0607() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(miningViewer()), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0608() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(discoveryManager()), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0609() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(discoveryEditor()), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0610() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(discoveryViewer()), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0611() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(discoveryLightManager()), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0612() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(discoveryLightEditor()), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0613() throws Exception { multiPartTests(POST, url(PROJECT_AS_EXCEL_URL), uriVars("1"), noParams(), roles(discoveryLightViewer()), FORBIDDEN, FILE, "foo.xlsx", ContentType.MULTIPART_FORM_DATA); }
	
	/* Import Source Objects into given Project. */
	/* Expecting BAD_REQUEST in case of successful authorization as the zip file provided does not exist. */
	@Test void test0700() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("5"), noParams(), roles(admin()), NOT_FOUND, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0701() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(clientAdmin(2)), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0702() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(miningEditor()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0703() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(miningViewer()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0704() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(discoveryManager(1, 2)), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0705() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(discoveryEditor()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0706() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(discoveryViewer()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0707() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(discoveryLightManager()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0708() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(discoveryLightEditor()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0709() throws Exception { multiPartTests(POST, url(SOURCE_OBJECTS_URL), uriVars("1"), noParams(), roles(discoveryLightViewer()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }

	/* Upload a new file */
	/* Expecting OK in case of successful authorization */
	@Test void test0801() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(admin()), OK, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0802() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("3"), noParams(), roles(admin()), OK, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0803() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(clientAdmin()), OK, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0804() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(clientAdmin(2)), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0805() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(miningManager()), OK, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0806() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(miningManager(1, 2)), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0807() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(miningEditor()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0808() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(miningEditor(1, 2)), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0809() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(miningViewer()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0810() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(miningViewer(1, 2)), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0811() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("5"), noParams(), roles(miningViewer(1, 5)), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0812() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("5"), noParams(), roles(miningViewer(1, 4)), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0813() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(discoveryManager()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0814() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(discoveryEditor()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0815() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(discoveryViewer()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0816() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(discoveryLightManager()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0817() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(discoveryLightEditor()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0818() throws Exception { multiPartTests(POST, url(PROJECT_AS_FILE_URL), uriVars("1"), noParams(), roles(discoveryLightViewer()), FORBIDDEN, FILE, "foo.zip", ContentType.MULTIPART_FORM_DATA); }

	/* DELETE a file */
	/* Expecting NOT_FOUND in case of successful authorization */
	@Test void test0901() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0902() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("3"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0903() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0904() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0905() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0906() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0907() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0908() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0909() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0910() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0911() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("5"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0912() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("5"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test0913() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0914() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0915() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0916() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0917() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0918() throws Exception { test(DELETE, url(PROJECT_AS_FILE_URL), uriVars("1"), params(List.of(Tuple2.of("fileId", "550e8400-e29b-41d4-a716-446655440000"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
