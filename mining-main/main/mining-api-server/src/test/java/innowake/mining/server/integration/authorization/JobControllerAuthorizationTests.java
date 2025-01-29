/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.controller.job.JobController.JOB_CANCEL_URL;
import static innowake.mining.server.controller.job.JobController.JOB_COLLECTION_URL;
import static innowake.mining.server.controller.job.JobController.JOB_INFO_URL;
import static innowake.mining.server.controller.job.JobController.JOB_LOG_URL;
import static innowake.mining.server.controller.job.JobController.JOB_RESULT_URL;
import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.OBJECT;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.http.HttpStatus.OK;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import innowake.mining.server.controller.job.JobController;

/**
 * Authorization tests for the {@link JobController}.
 */
class JobControllerAuthorizationTests extends RestAuthorizationTests {
	
	/* GET all jobs */
	/* Expecting OK for all requests */
	@Test void test0001() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(clientAdmin()), OK); }
	@Test void test0003() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager()), OK); }
	@Test void test0004() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningManager(2, 2)), OK); }
	@Test void test0005() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor()), OK); }
	@Test void test0006() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningEditor(2, 2)), OK); }
	@Test void test0007() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer()), OK); }
	@Test void test0008() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(miningViewer(1, 2)), OK); }
	@Test void test0009() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryManager()), OK); }
	@Test void test0010() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryEditor()), OK); }
	@Test void test0011() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryViewer()), OK); }
	@Test void test0012() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightManager()), OK); }
	@Test void test0013() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightEditor()), OK); }
	@Test void test0014() throws Exception { test(GET, url(JOB_COLLECTION_URL), noUriVars(), noParams(), OBJECT, roles(discoveryLightViewer()), OK); }
	
	/* Gets the Job information*/
	/* Expecting NOT_FOUND in case of successful authorization as the given job could be validated only after authorization*/
	@Test void test0100() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0101() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0102() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0103() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningManager(2, 2)), NOT_FOUND); }
	@Test void test0104() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0105() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningEditor(2, 2)), NOT_FOUND); }
	@Test void test0106() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0107() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningViewer(1, 2)), NOT_FOUND); }
	@Test void test0108() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0109() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0110() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0111() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0112() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND); }
	@Test void test0113() throws Exception { test(GET, url(JOB_INFO_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
	/* Gets the Job log*/
	/* Expecting INTERNAL_SERVER_ERROR for Admin as he directly query the jobs and NOT_FOUND for other users 
	 * as it's access is validated before querying. In both cases the Job validation occurs only after the successful authorization */
	@Test void test0200() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0201() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0202() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0203() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningManager(2, 2)), NOT_FOUND); }
	@Test void test0204() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0205() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningEditor(2, 2)), NOT_FOUND); }
	@Test void test0206() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0207() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningViewer(1, 2)), NOT_FOUND); }
	@Test void test0208() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0209() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0210() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0211() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0212() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND); }
	@Test void test0213() throws Exception { test(GET, url(JOB_LOG_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
	/* Cancels the Job */
	/* Expecting NOT_FOUND in case of successful authorization as the given job could be validated only after authorization*/
	@Test void test0300() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0301() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0302() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0303() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningManager(2, 2)), NOT_FOUND); }
	@Test void test0304() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0305() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningEditor(2, 2)), NOT_FOUND); }
	@Test void test0306() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0307() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningViewer(1, 2)), NOT_FOUND); }
	@Test void test0308() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0309() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0310() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0311() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0312() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND); }
	@Test void test0313() throws Exception { test(PUT, url(JOB_CANCEL_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
	/* Gets Job result */
	/* Expecting NOT_FOUND in case of successful authorization as the given job could be validated only after authorization*/
	@Test void test0400() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(admin()), NOT_FOUND); }
	@Test void test0401() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(clientAdmin()), NOT_FOUND); }
	@Test void test0402() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningManager()), NOT_FOUND); }
	@Test void test0403() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningManager(2, 2)), NOT_FOUND); }
	@Test void test0404() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningEditor()), NOT_FOUND); }
	@Test void test0405() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningEditor(2, 2)), NOT_FOUND); }
	@Test void test0406() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningViewer()), NOT_FOUND); }
	@Test void test0407() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(miningViewer(1, 2)), NOT_FOUND); }
	@Test void test0408() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryManager()), NOT_FOUND); }
	@Test void test0409() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryEditor()), NOT_FOUND); }
	@Test void test0410() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryViewer()), NOT_FOUND); }
	@Test void test0411() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightManager()), NOT_FOUND); }
	@Test void test0412() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightEditor()), NOT_FOUND); }
	@Test void test0413() throws Exception { test(GET, url(JOB_RESULT_URL), uriVars(UUID.randomUUID()), noParams(), OBJECT, roles(discoveryLightViewer()), NOT_FOUND); }
	
}
