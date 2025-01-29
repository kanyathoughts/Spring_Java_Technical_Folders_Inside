/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.util.tuple.Tuple2.of;
import static innowake.mining.server.controller.DataDictionaryController.AGGREGATIONS_URL;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_BY_ID_URL;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_COLLECTION_URL;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_ID_URL;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_OTHER_SCOPES_URL;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_SEARCH_URL;
import static innowake.mining.server.controller.DataDictionaryController.DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL;
import static innowake.mining.server.controller.DataDictionaryController.SEARCH_PARAM_DATA_ELEMENT_NAME;
import static innowake.mining.server.controller.DataDictionaryController.SEARCH_PARAM_DESCRIPTION;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.LIST;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static java.util.Arrays.asList;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.DataDictionaryController;

/**
 * Authorization tests for the {@link DataDictionaryController}.
 */
class DataDictionaryControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* GETs all the data dictionary entries for given module id */
	/* Expecting OK in case of successful authorization */
	@Test void test0001() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2324"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0002() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0003() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0004() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0006() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningManager(1, 2)), OK); }
	@Test void test0007() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0008() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningEditor(1, 2)), OK); }
	@Test void test0009() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0010() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0011() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0012() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0013() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0014() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0015() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0016() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0017() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0018() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0019() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0020() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0021() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0022() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0023() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* GETs all the data dictionary entries for given module id */
	/* Expecting BAD_REQUEST in case of successful authorization as the empty JSON object does not validate */
	@Test void test0100() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2324"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0101() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0102() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0103() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0104() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0105() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningManager(1, 2)), BAD_REQUEST); }
	@Test void test0106() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0107() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningEditor(1, 2)), BAD_REQUEST); }
	@Test void test0108() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0109() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0110() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0111() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0112() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0113() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0114() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0115() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0116() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0117() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0118() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0119() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0120() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0121() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0122() throws Exception { test(POST, url(DATA_DICTIONARY_COLLECTION_URL), uriVars("2","2003"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* Updates the data dictionary entry of the given entry id */
	/* Expecting OK in case of successful authorization */
	@Test void test0200() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2324","10"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0202() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0203() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0204() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0205() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(miningManager(1, 2)), OK); }
	@Test void test0206() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0207() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(miningEditor(1, 2)), OK); }
	@Test void test0208() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0209() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0210() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0211() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0212() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0213() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0214() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0215() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0216() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0217() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0218() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0219() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0220() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0221() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0222() throws Exception { test(PUT, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","1"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* Deletes the data dictionary entry of the given entry id */
	/* Expecting NO_CONTENT in case of successful authorization */
	@Test void test0300() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2324","10"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0301() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(admin()), NO_CONTENT); }
	@Test void test0302() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(clientAdmin()), NO_CONTENT); }
	@Test void test0303() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0304() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0305() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(miningManager(1, 2)), NO_CONTENT); }
	@Test void test0306() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0307() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(miningEditor(1, 2)), NO_CONTENT); }
	@Test void test0308() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0309() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0310() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0311() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0312() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0313() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0314() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0315() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0316() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0317() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0318() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0319() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0320() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0321() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0322() throws Exception { test(DELETE, url(DATA_DICTIONARY_BY_ID_URL), uriVars("2","2003","77"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* GETs the data dictionary entry for the given record id */
	/* Expecting NOT_FOUND in case of successful authorization as there is no data dictionary entry with such a record ID. */
	@Test void test0400() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2324","0"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0401() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0402() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0403() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0404() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0405() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningManager(1, 2)), NOT_FOUND); }
	@Test void test0406() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0407() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningEditor(1, 2)), NOT_FOUND); }
	@Test void test0408() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0409() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningViewer(1, 2)), NOT_FOUND); }
	@Test void test0410() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0411() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0412() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0413() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0414() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0415() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0416() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0417() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0418() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0419() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0420() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0421() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0422() throws Exception { test(GET, url(DATA_DICTIONARY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* GETs all the data dictionary entries that matches the provided project id, description, dataElementName */
	/* Expecting OK in case of successful authorization */
	@Test void test0500() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("5"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "This is an english description of the data element name MY"),of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX-ORIGIN"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0501() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(admin()), OK); }
	@Test void test0502() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0503() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0504() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0505() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(miningManager(1, 2)), OK); }
	@Test void test0506() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0507() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(miningEditor(1, 2)), OK); }
	@Test void test0508() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0509() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0510() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0511() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0512() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0513() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0514() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0515() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0516() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0517() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0518() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0519() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0520() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0521() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0522() throws Exception { test(GET, url(DATA_DICTIONARY_SEARCH_URL), uriVars("2"), params(asList(of(SEARCH_PARAM_DESCRIPTION, "Dictionary Description"), of(SEARCH_PARAM_DATA_ELEMENT_NAME, "MY-HEX"))), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* Lists all the available other scopes for the given project id */
	/* Expecting OK in case of successful authorization */
	@Test void test0600() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("5"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0601() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0602() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0603() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0604() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0605() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(miningManager(1, 2)), OK); }
	@Test void test0606() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0607() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(miningEditor(1, 2)), OK); }
	@Test void test0608() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0609() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0610() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0611() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0612() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0613() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0614() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0615() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0616() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0617() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0618() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0619() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0620() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0621() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0622() throws Exception { test(GET, url(DATA_DICTIONARY_OTHER_SCOPES_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* Validates data dictionary entry */
	/* Expecting NOT_FOUND in case of successful authorization, because Ast is not available */
	@Test void test0800() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("5", "2003", "45"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0801() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0802() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0803() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0804() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0805() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(miningManager(1, 2)), NOT_FOUND); }
	@Test void test0806() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0807() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(miningEditor(1, 2)), NOT_FOUND); }
	@Test void test0808() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0809() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0810() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0811() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0812() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0813() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0814() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0815() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0816() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0817() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0818() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0819() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0820() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0821() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0822() throws Exception { test(GET, url(DATA_DICTIONARY_VALIDATE_BY_OFFSET_URL), uriVars("2", "2003", "45"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* GETs aggregated values for data dictionaries that matches the provided project id */
	/* Expecting BAD_REQUEST in case of successful authorization as the request body s not valid*/
	@Test void test0900() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(admin()), BAD_REQUEST); }
	@Test void test0901() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0902() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0903() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test0904() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(miningManager(1, 2)), BAD_REQUEST); }
	@Test void test0905() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test0906() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0907() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 2)), BAD_REQUEST); }
	@Test void test0908() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test0909() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0910() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test0911() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0912() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0913() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0914() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0915() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0916() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test0917() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0918() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test0919() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test0920() throws Exception { test(POST, url(AGGREGATIONS_URL), uriVars("2"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* GETs the finding linked Business Rules for a Data Dictionary for given record id.*/
	/* Expecting NOT_FOUND in case of successful authorization as there is no data dictionary entry with such a record ID. */
	@Test void test1000() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2324","0"), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test1001() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test1002() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test1003() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1004() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningManager()), FORBIDDEN); }
	@Test void test1005() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningManager(1, 2)), OK); }
	@Test void test1006() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningEditor()), FORBIDDEN); }
	@Test void test1007() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningEditor(1, 2)), OK); }
	@Test void test1008() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test1009() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test1010() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(miningViewer(1, 5)), FORBIDDEN); }
	@Test void test1011() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1012() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryManager(1, 2)), FORBIDDEN); }
	@Test void test1013() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1014() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test1015() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1016() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test1017() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1018() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightManager(1, 2)), FORBIDDEN); }
	@Test void test1019() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1020() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightEditor(1, 2)), FORBIDDEN); }
	@Test void test1021() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	@Test void test1022() throws Exception { test(GET, url(DATA_DICTIONARY_LINKED_BUSINESS_RULE_BY_ID_URL), uriVars("2","2003","0"), noParams(), OBJECT, roles(discoveryLightViewer(1, 2)), FORBIDDEN); }
	
	/* Retrieve list of DataDictionary By module id Based on Offset*/
	@Test void test1100() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1101() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("5", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1102() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1103() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1104() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(miningManager()), NOT_FOUND); }
	@Test void test1105() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1106() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningEditor()), NOT_FOUND); }
	@Test void test1107() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1108() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningViewer()), NOT_FOUND); }
	@Test void test1109() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1110() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("5", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1111() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("5", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1112() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1113() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1114() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1115() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1116() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"))), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1117() throws Exception { test(GET, url(DATA_DICTIONARY_COLLECTIONS_BASED_ON_OFFSET), uriVars("1", "1"), params(asList(of("startOffset", "0"), of("endOffset", "500"))), LIST, roles(discoveryLightViewer()), FORBIDDEN); }

	/* Retrieve list of DataDictionary UUIDs By data flow ids */
	@Test void test1118() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1119() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("5", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(admin()), NOT_FOUND); }
	@Test void test1120() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(clientAdmin()), NOT_FOUND); }
	@Test void test1121() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test1122() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(miningManager()), NOT_FOUND); }
	@Test void test1123() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test1124() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(miningEditor()), NOT_FOUND); }
	@Test void test1125() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test1126() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(miningViewer()), NOT_FOUND); }
	@Test void test1127() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test1128() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("5", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(miningViewer(1, 5)), NOT_FOUND); }
	@Test void test1129() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("5", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(miningViewer(1, 4)), FORBIDDEN); }
	@Test void test1130() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test1131() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test1132() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test1133() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test1134() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test1135() throws Exception { test(GET, url(DATA_DICTIONARY_IDS_BASED_ON_DATAFLOW_IDS), uriVars("1", "1"), params(asList(of("dataFlowIds", "module-3828-field-6717"))), LIST, roles(discoveryLightViewer()), FORBIDDEN); }

}
