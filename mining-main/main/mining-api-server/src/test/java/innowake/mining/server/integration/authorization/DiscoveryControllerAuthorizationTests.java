/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.util.tuple.Tuple2.of;
import static innowake.mining.server.controller.discovery.DiscoveryController.DISCOVERY_CONFIGURATION_URL;
import static innowake.mining.server.controller.discovery.DiscoveryController.DISCOVER_CODE_URL;
import static innowake.mining.server.controller.discovery.DiscoveryController.DISCOVER_DNA_URL;
import static innowake.mining.server.controller.discovery.DiscoveryController.DISCOVER_METRICS_URL;
import static innowake.mining.server.controller.discovery.DiscoveryController.DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static java.util.Arrays.asList;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.ACCEPTED;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import java.util.UUID;

import org.apache.http.entity.ContentType;
import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.discovery.DiscoveryController;

/**
 * Authorization tests for the {@link DiscoveryController}.
 */
class DiscoveryControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* Discover code */
	/* Expecting ACCEPTED in case of successful authorization */
	@Test void test0001() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(admin()),                 ACCEPTED); }
	@Test void test0002() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()),           ACCEPTED); }
	@Test void test0003() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)),          FORBIDDEN); }
	@Test void test0004() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()),         ACCEPTED); }
	@Test void test0005() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)),     FORBIDDEN); }
	@Test void test0006() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()),          FORBIDDEN); }
	@Test void test0007() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()),          FORBIDDEN); }
	@Test void test0008() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()),      ACCEPTED); }
	@Test void test0009() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)),  FORBIDDEN); }
	@Test void test0010() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()),       FORBIDDEN); }
	@Test void test0011() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()),       FORBIDDEN); }
	@Test void test0012() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)),   FORBIDDEN); }
	@Test void test0013() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()),  FORBIDDEN); }
	@Test void test0015() throws Exception { test(POST, url(DISCOVER_CODE_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()),  FORBIDDEN); }
	
	/* Discover metrics */
	/* Expecting ACCEPTED in case of successful authorization */
	@Test void test0100() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(admin()),                 ACCEPTED); }
	@Test void test0101() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()),           ACCEPTED); }
	@Test void test0102() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)),          FORBIDDEN); }
	@Test void test0103() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()),         ACCEPTED); }
	@Test void test0104() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)),     FORBIDDEN); }
	@Test void test0105() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()),          FORBIDDEN); }
	@Test void test0106() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()),          FORBIDDEN); }
	@Test void test0107() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()),      ACCEPTED); }
	@Test void test0108() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)),  FORBIDDEN); }
	@Test void test0109() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()),       FORBIDDEN); }
	@Test void test0110() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()),       FORBIDDEN); }
	@Test void test0111() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)),   FORBIDDEN); }
	@Test void test0112() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0113() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()),  FORBIDDEN); }
	@Test void test0114() throws Exception { test(POST, url(DISCOVER_METRICS_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()),  FORBIDDEN); }
	
	/* Discover DNA */
	/* Expecting ACCEPTED in case of successful authorization */
	@Test void test0200() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(admin()),                 ACCEPTED); }
	@Test void test0201() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()),           ACCEPTED); }
	@Test void test0202() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)),          FORBIDDEN); }
	@Test void test0203() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()),         ACCEPTED); }
	@Test void test0204() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)),     FORBIDDEN); }
	@Test void test0205() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()),          FORBIDDEN); }
	@Test void test0206() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()),          FORBIDDEN); }
	@Test void test0207() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()),      ACCEPTED); }
	@Test void test0208() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)),  FORBIDDEN); }
	@Test void test0209() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()),       FORBIDDEN); }
	@Test void test0210() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()),       FORBIDDEN); }
	@Test void test0211() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)),   FORBIDDEN); }
	@Test void test0212() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0213() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()),  FORBIDDEN); }
	@Test void test0214() throws Exception { test(POST, url(DISCOVER_DNA_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()),  FORBIDDEN); }
	
	/* Download discovery configuration */
	/* Expecting OK in case of successful authorization */
	@Test void test0300() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(admin()),                 OK); }
	@Test void test0301() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin()),           OK); }
	@Test void test0302() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(clientAdmin(2)),          FORBIDDEN); }
	@Test void test0303() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager()),         OK); }
	@Test void test0304() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningManager(1, 2)),     FORBIDDEN); }
	@Test void test0305() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor()),          OK); }
	@Test void test0306() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningEditor(1, 2)),      FORBIDDEN); }
	@Test void test0307() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer()),          OK); }
	@Test void test0308() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(miningViewer(1, 2)),      FORBIDDEN); }
	@Test void test0309() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager()),      OK); }
	@Test void test0310() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryManager(1, 2)),  FORBIDDEN); }
	@Test void test0311() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor()),       OK); }
	@Test void test0312() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryEditor(1, 2)),   FORBIDDEN); }
	@Test void test0313() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer()),       OK); }
	@Test void test0314() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryViewer(1, 2)),   FORBIDDEN); }
	@Test void test0315() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0316() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightEditor()),  FORBIDDEN); }
	@Test void test0317() throws Exception { test(GET, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), OBJECT, roles(discoveryLightViewer()),  FORBIDDEN); }
	
	/* Update DnaCommunity Title and Description*/
	/* Expecting BAD_REQUEST in case of successful authorization as the no parameter does not validate */
	@Test void test0400() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0401() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), noParams(), OBJECT, roles(clientAdmin()), BAD_REQUEST); }
	@Test void test0402() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(clientAdmin(2)), FORBIDDEN); }
	@Test void test0403() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0404() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(miningManager(1, 2)), FORBIDDEN); }
	@Test void test0405() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0406() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(miningEditor(1, 2)), FORBIDDEN); }
	@Test void test0407() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(miningViewer()), FORBIDDEN); }
	@Test void test0408() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0409() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0410() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryManager(1,2)), FORBIDDEN); }
	@Test void test0411() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0412() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryEditor(1, 2)), FORBIDDEN); }
	@Test void test0413() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0414() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryViewer(1, 2)), FORBIDDEN); }
	@Test void test0415() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0416() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0417() throws Exception { test(PUT, url(DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL), uriVars("1", UUID.randomUUID().toString()), params(asList(of("title", "Dna title"))), OBJECT, roles(discoveryLightViewer()), FORBIDDEN); }
	
	/* Upload discovery configuration */
	@Test void test0500() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(admin()),                 OK,        "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0501() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(clientAdmin()),           OK,        "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0502() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(clientAdmin(2)),          FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0503() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(miningManager()),         OK,        "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0504() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(miningManager(1, 2)),     FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0505() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(miningEditor()),          FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0506() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(miningViewer()),          FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0507() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(discoveryManager()),      OK,        "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0508() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(discoveryManager(1, 2)),  FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0509() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(discoveryEditor()),       FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0510() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(discoveryViewer()),       FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0511() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(discoveryViewer(1, 2)),   FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0512() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(discoveryLightManager()), FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0513() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(discoveryLightEditor()),  FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	@Test void test0514() throws Exception { multiPartTests(POST, url(DISCOVERY_CONFIGURATION_URL), uriVars("1"), noParams(), roles(discoveryLightViewer()),  FORBIDDEN, "file", "foo.zip", ContentType.MULTIPART_FORM_DATA); }
	
}
