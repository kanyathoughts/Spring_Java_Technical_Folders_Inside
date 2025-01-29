/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.lib.core.lang.Assert.assertNotNull;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Stream;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.mining.data.io.DiscoveryCsvExportService;
import innowake.mining.data.io.DiscoveryExcelExportService;
import innowake.mining.data.io.ExcelImportService;
import innowake.mining.data.io.sourceobject.SourceObjectExportService;
import innowake.mining.data.io.sourceobject.SourceObjectImportService;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.extensions.TestExportExtension;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.importer.csv.CSVImportService;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.CookieIdVerifier;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.security.RoleType;

/**
 * Mocked tests verifying the behavior of {@link IoController}.
 */
@WebMvcTest(IoController.class)
@Import({ CookieIdVerifier.class, BuildProperties.class })
@ActiveProfiles(value = Profiles.AUTH_TEST)
@WithMockUser
@Tag("mocked")
class IoControllerTests extends MockedBaseTest {
	
	private static final Long ONE = Long.valueOf(1);

	@Autowired
	private MockMvc mvc;
	
	@MockBean
	private CSVImportService csvImportService;
	@MockBean
	private ExcelImportService excelImportService;
	@MockBean
	private DiscoveryExcelExportService excelExportService;
	@MockBean
	private DiscoveryCsvExportService csvExportService;
	@MockBean
	private SourceObjectImportService sourceCodeImportService;
	@MockBean
	private SourceObjectExportService sourceCodeExportService;
	@MockBean
	private List<MiningJobExtension<?>> jobExtensions;
	@MockBean
	private List<MiningExportExtension> exportExtensions;
	@MockBean
	private JobManager jobManager;
	@MockBean
	private JobConfigurationProperties jobConfigurationProperties;
	@MockBean
	private GenericConfigProperties genericConfigProperties;
	@MockBean
	private DiscoveryPersistenceImpl discoveryPersistence;

	
	/**
	 * Tests {@link IoController#getExportFormats(HttpServletRequest, HttpServletResponse, Long)}.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testGetExportFormats() throws Exception {
		testDataSetup(ONE, RoleType.EDITOR, true);
		mvc.perform(get("/api" + IoController.EXPORT_FORMAT_COLLECTIONS_URL, ONE).contentType(MediaType.APPLICATION_JSON_VALUE))
				.andDo(print())
				.andExpect(status().isOk())
				.andExpect(jsonPath("$.[0].id").value("txt"))
				.andExpect(jsonPath("$.[0].description").value("Sample Test extension"))
				.andExpect(jsonPath("$.[0].extensionType").value("EXPORT_EXTENSION"))
				.andExpect(jsonPath("$.[0].requiredRole").value("EDITOR"))
				.andExpect(jsonPath("$.[0].requiredNature").value("MINING"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].name").value("sample param 1"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].description").value("sample test extension parameter 1"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].type").value("BOOLEAN"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].required").value("true"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].defaultValue").value("false"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].allowMultiple").value("false"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].example").value("sample"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].allowableValues[0]").value("true"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[0].allowableValues[1]").value("false"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].name").value("sample param 2"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].description").value("sample test extension parameter 2"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].type").value("STRING"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].required").value("false"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].defaultValue").value("ABC"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].allowMultiple").value("false"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].example").value("sample"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].allowableValues[0]").value("ABC"))
				.andExpect(jsonPath("$.[0].parameterDescriptions[1].allowableValues[1]").value("DEF"))
				.andExpect(jsonPath("$.[0].showOnExportPage.show").value("true"))
				.andExpect(jsonPath("$.[0].showOnExportPage.category").value("test category"))
				.andExpect(jsonPath("$.[0].showOnExportPage.label").value("sample extension"))
				.andExpect(jsonPath("$.[0].uploadDescription.name").value(""))
				.andExpect(jsonPath("$.[0].uploadDescription.description").value(""))
				.andExpect(jsonPath("$.[0].uploadDescription.required").value(false))
				.andExpect(jsonPath("$.[0].uploadDescription.accept").isEmpty())
				.andExpect(jsonPath("$.[0].uploadDescription.supported").value(false));
	}
	
	@Test
	void testGetEmptyExporttFormattWhenNoAccessForRole() throws Exception {
		testDataSetup(ONE, RoleType.VIEWER, true);
		mvc.perform(get("/api" + IoController.EXPORT_FORMAT_COLLECTIONS_URL, ONE).contentType(MediaType.APPLICATION_JSON_VALUE))
				.andDo(print())
				.andExpect(status().isOk())
				.andExpect(content().string("[]"));
	}
	
	@Test
	void testGetEmptyExportFormattWhenRoleAsNoAccessToProject() throws Exception {
		testDataSetup(ONE, RoleType.EDITOR, false);
		mvc.perform(get("/api" + IoController.EXPORT_FORMAT_COLLECTIONS_URL, ONE).contentType(MediaType.APPLICATION_JSON_VALUE))
				.andDo(print())
				.andExpect(status().isOk())
				.andExpect(content().string("[]"));
	}

	@SuppressWarnings("unchecked")
	private void testDataSetup(final Long id, final RoleType role, final boolean hasProjectRole) {
		final ClientPojo client = new ClientPojo(UUID.randomUUID(), CustomPropertiesMap.empty(), 1l, "Client 1", false, false);
		final ProjectPojo project = new ProjectPojo(
				UUID.randomUUID(), CustomPropertiesMap.empty(), EntityId.of(id), null, null , 1l, "Project 1", false,
				null, null, "", null, Collections.emptyList(), null, null, Collections.emptyMap(), null);
		when(assertNotNull(clientService).get(EntityId.of(id), true)).thenReturn(client);
		when(assertNotNull(projectService).get(EntityId.of(id))).thenReturn(project);
		when(assertNotNull(projectService).getNid(EntityId.of(id))).thenReturn(id);
		when(assertNotNull(userRoleService).hasRequiredRole(id, role)).thenReturn(hasProjectRole);
		when(assertNotNull(projectService).isValid(any(EntityId.class))).thenReturn(Boolean.TRUE);
		final Iterator<MiningExportExtension> mockIterator = mock(Iterator.class);
		when(exportExtensions.stream()).thenReturn(Stream.of(new TestExportExtension()));
		doCallRealMethod().when(exportExtensions).forEach(any(Consumer.class));
		when(exportExtensions.iterator()).thenReturn((mockIterator));
		when(mockIterator.hasNext()).thenReturn(true, false);
		when(mockIterator.next()).thenReturn(new TestExportExtension());
	}
}
