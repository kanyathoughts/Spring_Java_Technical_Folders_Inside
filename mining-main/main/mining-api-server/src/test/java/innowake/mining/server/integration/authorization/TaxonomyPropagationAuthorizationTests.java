/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.mining.server.integration.authorization.RestAuthorizationTests.Content.LIST;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.FORBIDDEN;
import static org.springframework.http.HttpStatus.OK;

import org.junit.jupiter.api.Test;

import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagation;

/**
 * Authorization Tests for {@link TaxonomyPropagation}.
 */
class TaxonomyPropagationAuthorizationTests extends RestAuthorizationTests {
	
	private static final String TAXONOMY_PROPAGATION_URL = "/v1/projects/{projectId}/job-extensions/taxonomyPropagation";
	
	/* POST all jobs */
	/* Expecting FORBIDDEN for all requests except Admin and ClientAdmin */
	@Test void test0001() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(admin()), OK); }
	@Test void test0002() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(clientAdmin()), OK); }
	@Test void test0003() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(miningManager()), FORBIDDEN); }
	@Test void test0004() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(miningManager(2, 2)), FORBIDDEN); }
	@Test void test0005() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(miningEditor()), FORBIDDEN); }
	@Test void test0006() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(miningEditor(2, 2)), FORBIDDEN); }
	@Test void test0007() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(miningViewer()), FORBIDDEN); }
	@Test void test0008() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(miningViewer(1, 2)), FORBIDDEN); }
	@Test void test0009() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(discoveryManager()), FORBIDDEN); }
	@Test void test0010() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(discoveryEditor()), FORBIDDEN); }
	@Test void test0011() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(discoveryViewer()), FORBIDDEN); }
	@Test void test0012() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightManager()), FORBIDDEN); }
	@Test void test0013() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightEditor()), FORBIDDEN); }
	@Test void test0014() throws Exception { test(POST, url(TAXONOMY_PROPAGATION_URL), uriVars("1"), noParams(), LIST, roles(discoveryLightViewer()), FORBIDDEN); }
	
}
