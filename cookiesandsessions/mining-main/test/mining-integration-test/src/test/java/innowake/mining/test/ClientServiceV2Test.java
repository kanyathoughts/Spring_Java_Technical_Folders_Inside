/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Base64;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.test.util.RestTemplateUtil;

/**
 * Integration tests for the {@link ClientService} V2 service.
 */
class ClientServiceV2Test extends IntegrationTest {

	private static final String ALL_CLIENT_URI = RouteConfiguration.API_BASE + "/v2/clients";
	private static final String CLIENT_LOGO_URI = ALL_CLIENT_URI + "/{clientId}/logo";
	private static final String LOGO_FILE_NAME = "logo.png";

	private final RestTemplate restTemplate = new RestTemplate();
	private final ConnectionInfo info = getConnectionInfo();
	
	private final static Long ONE = Long.valueOf(1);
	private final static Long TWO = Long.valueOf(2);

	@Test
	void testFindAllClients() {
		final ParameterizedTypeReference<RestResponsePage<ClientPojo>> responseType = new ParameterizedTypeReference<RestResponsePage<ClientPojo>>() { };
		final HttpEntity<String> request = new HttpEntity<>(RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<RestResponsePage<ClientPojo>> responseEntity = restTemplate.exchange(
				info.getUrl() + ALL_CLIENT_URI,
				HttpMethod.GET,
				request,
				responseType);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.OK.value(), responseEntity.getStatusCodeValue());
		final Page<ClientPojo> clientsPage = responseEntity.getBody();
		assertNotNull(clientsPage);
		final List<ClientPojo> clients = clientsPage.getContent();
		assertEquals(2, clients.size());
	}

	@Test
	void testFindAllClientsOnlySorting() {
		final UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromHttpUrl(info.getUrl() + ALL_CLIENT_URI)
				.queryParam("sortBy", new Object[] {"name;ASC"});
		final ParameterizedTypeReference<RestResponsePage<ClientPojo>> responseType = new ParameterizedTypeReference<RestResponsePage<ClientPojo>>() { };
		final HttpEntity<String> request = new HttpEntity<>(RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<RestResponsePage<ClientPojo>> responseEntity = restTemplate.exchange(
				uriBuilder.build().toUri(),
				HttpMethod.GET,
				request,
				responseType);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.OK.value(), responseEntity.getStatusCodeValue());
		final Page<ClientPojo> clientsPage = responseEntity.getBody();
		assertNotNull(clientsPage);
		final List<ClientPojo> clients = clientsPage.getContent();
		assertEquals(2, clients.size());
		assertEquals("Demo Client 1", clients.get(0).getName());
		assertEquals("Demo Client 2", clients.get(1).getName());
	}

	@Test
	void testFindAllClientsWithPaginationAndSorting() {
		final UriComponentsBuilder uriBuilder = UriComponentsBuilder.fromHttpUrl(info.getUrl() + ALL_CLIENT_URI)
				.queryParam("page", "0")
				.queryParam("size", "2")
				.queryParam("sortBy", new Object[] {"name;ASC"});
		final ParameterizedTypeReference<RestResponsePage<ClientPojo>> responseType = new ParameterizedTypeReference<RestResponsePage<ClientPojo>>() { };
		final HttpEntity<String> request = new HttpEntity<>(RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<RestResponsePage<ClientPojo>> responseEntity = restTemplate.exchange(
				uriBuilder.build().toUri(),
				HttpMethod.GET,
				request,
				responseType);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.OK.value(), responseEntity.getStatusCodeValue());
		final Page<ClientPojo> clientsPage = responseEntity.getBody();
		assertNotNull(clientsPage);
		final List<ClientPojo> clients = clientsPage.getContent();
		/* We have 2 clients due to pagination, last one is excluded. */
		assertEquals(2, clients.size());
		assertEquals("Demo Client 1", clients.get(0).getName());
		assertEquals("Demo Client 2", clients.get(1).getName());
	}
	
	@Test
	void testCreateLogo() {
		/* Client should not have logo initially  */
		try {
			getLogoForClient(ONE);
			throw new AssertionError();
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getStatusCode());
		}
		
		final ResponseEntity<Void> responseEntity = createLogoForClient(ONE, LOGO_FILE_NAME);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.CREATED.value(), responseEntity.getStatusCodeValue());

		/* Client should have a logo now. */
		assertEquals(HttpStatus.OK, getLogoForClient(ONE).getStatusCode());
	}
	
	@Test
	void testCreateLogoShouldOverwriteOldLogo() {
		final ResponseEntity<Void> responseEntity = createLogoForClient(ONE, LOGO_FILE_NAME);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.CREATED.value(), responseEntity.getStatusCodeValue());

		final ResponseEntity<char[]> originalContentResponse = getLogoForClient(ONE);
		assertNotNull(originalContentResponse);
		assertEquals(HttpStatus.OK.value(), originalContentResponse.getStatusCodeValue());
		final String originalContent = String.valueOf(originalContentResponse.getBody());

		final ResponseEntity<Void> responseEntityUpdateLogo = createLogoForClient(ONE, "mining.png");
		assertNotNull(responseEntityUpdateLogo);
		assertEquals(HttpStatus.CREATED.value(), responseEntityUpdateLogo.getStatusCodeValue());

		final ResponseEntity<char[]> updatedContentResponse = getLogoForClient(ONE);
		assertNotNull(updatedContentResponse);
		assertEquals(HttpStatus.OK.value(), updatedContentResponse.getStatusCodeValue());
		final String updatedContent = String.valueOf(updatedContentResponse.getBody());

		assertNotEquals(originalContent, updatedContent);
	}

	@Test
	void testCreateClientLogoInvalidMimeType() {
		/* Should not save the logo for Client as the MIME Type is invalid. */
		try {
			createLogoForClient(TWO, "dummy.txt");
			throw new AssertionError();
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.BAD_REQUEST, e.getStatusCode());
		}
	}
	
	@Test
	void testGetClientLogo() throws IOException {
		/* Client should not have a logo initially. */
		try {
			getLogoForClient(TWO);
			throw new AssertionError();
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getStatusCode());
		}

		/* Save the logo for Client */
		final ResponseEntity<Void> saveLogoResponseEntity = createLogoForClient(TWO, LOGO_FILE_NAME);
		assertNotNull(saveLogoResponseEntity);
		assertEquals(HttpStatus.CREATED.value(), saveLogoResponseEntity.getStatusCodeValue());

		/* Should get the content for Client's logo */
		final ResponseEntity<char[]> responseEntityWithContent = getLogoForClient(TWO);
		assertNotNull(responseEntityWithContent);
		assertEquals(HttpStatus.OK.value(), responseEntityWithContent.getStatusCodeValue());
		final char[] content = responseEntityWithContent.getBody();
		assertTrue(content != null && content.length > 0);
		assertEquals(getContentForLogo(LOGO_FILE_NAME), new String(content));
	}
	
	@Test
	void testDeleteClientLogo() {
		/* Client should not have a logo initially. */
		try {
			getLogoForClient(TWO);
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getStatusCode());
		}

		final ResponseEntity<Void> saveLogoResponseEntity = createLogoForClient(TWO, LOGO_FILE_NAME);
		assertNotNull(saveLogoResponseEntity);
		assertEquals(HttpStatus.CREATED.value(), saveLogoResponseEntity.getStatusCodeValue());
		final ResponseEntity<char[]> responseEntityWithContent = getLogoForClient(TWO);
		assertNotNull(responseEntityWithContent);
		assertEquals(HttpStatus.OK.value(), responseEntityWithContent.getStatusCodeValue());
		final char[] content = responseEntityWithContent.getBody();
		assertTrue(content != null && content.length > 0);

		final HttpEntity<String> request = new HttpEntity<>(RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<Void> deleteLogoResponseEntity = restTemplate.exchange(
				info.getUrl() + CLIENT_LOGO_URI,
				HttpMethod.DELETE,
				request,
				Void.class,
				TWO);
		assertNotNull(deleteLogoResponseEntity);
		assertEquals(HttpStatus.OK.value(), deleteLogoResponseEntity.getStatusCodeValue());

		try {
			getLogoForClient(TWO);
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getStatusCode());
		}
	}

	@Test
	void testUpdateClientLogo() {
		/* Create logo for Client */
		final ResponseEntity<Void> createResponse = createLogoForClient(TWO, LOGO_FILE_NAME);
		assertNotNull(createResponse);
		assertEquals(HttpStatus.CREATED.value(), createResponse.getStatusCodeValue());
		final ResponseEntity<char[]> originalContentResponse = getLogoForClient(TWO);
		assertNotNull(originalContentResponse);
		assertEquals(HttpStatus.OK.value(), originalContentResponse.getStatusCodeValue());
		final String originalContent = String.valueOf(originalContentResponse.getBody());
		assertFalse(StringUtils.isEmpty(originalContent));

		/* Update logo for the client */
		final Resource fileResource = new ClassPathResource("mining.png");
		final HttpHeaders headers = RestTemplateUtil.getHttpHeaders(info);
		headers.setContentType(MediaType.MULTIPART_FORM_DATA);
		final MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
		body.add("file", fileResource);
		final HttpEntity<MultiValueMap<String, Object>> saveLogoRequest = new HttpEntity<MultiValueMap<String, Object>>(body, headers);
		final ResponseEntity<Void> updateResponse = restTemplate.exchange(
				info.getUrl() + CLIENT_LOGO_URI,
				HttpMethod.PUT,
				saveLogoRequest,
				Void.class,
				TWO);
		assertNotNull(updateResponse);
		assertEquals(HttpStatus.NO_CONTENT.value(), updateResponse.getStatusCodeValue());

		/* Get updated logo content for client*/
		final ResponseEntity<char[]> updatedContentResponse = getLogoForClient(TWO);
		assertNotNull(updatedContentResponse);
		assertEquals(HttpStatus.OK.value(), updatedContentResponse.getStatusCodeValue());
		final String updatedContent = String.valueOf(updatedContentResponse.getBody());
		assertFalse(StringUtils.isEmpty(updatedContent));

		/* Original and updated content should be different */
		assertNotEquals(originalContent, updatedContent);
	}

	private ResponseEntity<Void> createLogoForClient(final Long clientId, final String fileName) {
		/* Save the logo for specified Client */
		final Resource fileResource = new ClassPathResource(fileName);
		final HttpHeaders headers = RestTemplateUtil.getHttpHeaders(info);
		headers.setContentType(MediaType.MULTIPART_FORM_DATA);
		final MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
		body.add("file", fileResource);
		final HttpEntity<MultiValueMap<String, Object>> saveLogoRequest = new HttpEntity<MultiValueMap<String, Object>>(body, headers);
		return restTemplate.exchange(
				info.getUrl() + CLIENT_LOGO_URI,
				HttpMethod.POST,
				saveLogoRequest,
				Void.class,
				clientId);
	}

	private ResponseEntity<char[]> getLogoForClient(final Long clientId) {
		final HttpEntity<String> request = new HttpEntity<>(RestTemplateUtil.getHttpHeaders(info));
		return restTemplate.exchange(
				info.getUrl() + CLIENT_LOGO_URI,
				HttpMethod.GET,
				request,
				char[].class,
				clientId);
	}

	private String getContentForLogo(final String fileName) throws IOException {
		final Resource fileResource = new ClassPathResource(fileName);
		final StringBuilder builder = new StringBuilder("data:image/png;base64,");
		builder.append(Base64.getEncoder().encodeToString(IOUtils.toByteArray(fileResource.getInputStream())));
		return builder.toString();
	}
}
