/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.hazelcast.shaded.org.json.JSONArray;
import com.hazelcast.shaded.org.json.JSONObject;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojoPrototype;

/**
 * Test for {@link DataPointController}
 */
@AutoConfigureMockMvc
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
public class DataPointControllerTest extends DatabaseRelatedTest {
	
	@Autowired
	private MockMvc mvc;

	private EntityId projectId = EntityId.VOID;

	@BeforeAll
	void initialize() {
		projectId = loadProjectAndClient(EntityId.of(1L));
	}

	@Test
	@Order(1)
	void testdefaultColumnIndexForDataDictionaryTable() throws Exception {
		final MvcResult result = mvc.perform(get("/api" + DataPointController.DATA_POINTS_FOR_TYPE_URL + "?usages=miningUi.dataDictionaryTable",
				projectId.getNid(), "PAGED_DataDictionaryEntry").contentType("application/json"))
				.andDo(print()).andExpect(status().isOk()).andReturn();

		final String content = result.getResponse().getContentAsString();
		final JSONArray jsonArray = new JSONArray(content);

		final Map<String, Integer> expectedIndices = new HashMap<>();
		expectedIndices.put("Field Name", 0);
		expectedIndices.put("Field Description", 1);
		expectedIndices.put("Translated Field Value", 2);
		expectedIndices.put("Field Type", 3);
		expectedIndices.put("Field Format", 4);
		expectedIndices.put("Length", 5);
		expectedIndices.put("Group Field", 6);
		expectedIndices.put("Field Level", 7);
		expectedIndices.put("Module Name", 8);
		expectedIndices.put("Created By", 9);

		int assertionCount = 0;

		for (int i = 0; i < jsonArray.length(); i++) {
			final JSONObject jsonObject = jsonArray.getJSONObject(i);
			final String displayName = jsonObject.getString("displayName");
			if (expectedIndices.containsKey(displayName) && jsonObject.has("usageAttributes")) {
				final JSONObject usageAttributes = jsonObject.getJSONObject("usageAttributes");
				if (usageAttributes.has("miningUi.dataDictionaryTable")) {
					final JSONObject miningUiDataDictionaryTable = usageAttributes.getJSONObject("miningUi.dataDictionaryTable");
					if (miningUiDataDictionaryTable.has("defaultColumnIndex")) {
						final int defaultColumnIndex = miningUiDataDictionaryTable.getInt("defaultColumnIndex");
						assertEquals(expectedIndices.get(displayName), defaultColumnIndex, "Index check failed for attribute: " + displayName);
						assertionCount++;
					}
				}
			}
		}

		assertEquals(expectedIndices.size(), assertionCount, "All expected assertions were not executed");
	}

	private EntityId loadProjectAndClient(final EntityId clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Mock Project 2")
				.setClient(clientId)
				.setNatures(new HashSet<>(Collections.emptySet()))
		).identity();
	}
}
