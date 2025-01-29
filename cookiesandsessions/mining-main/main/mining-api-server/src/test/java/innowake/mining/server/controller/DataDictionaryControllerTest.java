/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.OK;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.request;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DataDictionaryFieldName;
import innowake.mining.shared.model.DataDictionaryVariableAttribute;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

/**
 * Test class for the update of the english translation field of annotations if the translated value was updated in a related DDE
 * -> Annotation english description: ... bla FIELD1 bla ... 
 * -> DDE: name = FIELD1; translated_value = NEWVALUE
 * -> Annotation english description: ... bla NEWVALUE bla ... 
 */
@AutoConfigureMockMvc
@TestMethodOrder(OrderAnnotation.class)
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class DataDictionaryControllerTest extends DatabaseRelatedTest {

	private static final EntityId PROJECT_ID = EntityId.of(1L);

	@Autowired
	private MockMvc mockMvc;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private AnnotationService annotationService;

	private final String replacedValue = "REPLACED VALUE";
	private final String secondReplacedValue = "SECOND REPLACED VALUE";

	@Test
	void testAnnotationTranslationReplacement() {
		final EntityId moduleId = createModule();
		final DataDictionaryPojo dde = createDataDictionaryEntry("TESTDDE", moduleId, new ModuleLocation(100, 100), null);
		final EntityId anno = createAnnotation(moduleId);
		dataDictionaryService.linkAnnotations(dde.identity(), anno);

		try {
			final String jsonForUpdate = PojoMapper.jsonWriter().writeValueAsString(createDataDictionaryEntry("Test", moduleId, new ModuleLocation(1000, 100), null)
					.convertToPrototype()
					.setTranslatedFieldValue(replacedValue)
					.withId(dde.identity()));

			/* update translated_value of DDE */
			mockMvc.perform(
					request(PUT, "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{dataDictionaryEntryId}",
							PROJECT_ID.getNid(), moduleId.getNid(), dde.getId())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForUpdate)
					)
			.andDo(print())
			.andExpect(status().is(OK.value()))
			.andReturn();

			final Optional<AnnotationPojo> updatedAnno = annotationService.findAny(q -> q.byId(anno));

			updatedAnno.ifPresent(annotationPojo -> assertTrue(annotationPojo.getEnglishTranslation().get().contains(replacedValue)));

			final String jsonForSecondUpdate = PojoMapper.jsonWriter().writeValueAsString(createDataDictionaryEntry("Test", moduleId, new ModuleLocation(10000, 100), null)
					.convertToPrototype()
					.setTranslatedFieldValue(secondReplacedValue)
					.withId(dde.identity()));

			/* second update translated_value of DDE */
			mockMvc.perform(
					request(PUT, "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{dataDictionaryEntryId}",
							PROJECT_ID.getNid(), moduleId.getNid(), dde.getId())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForSecondUpdate)
					)
			.andDo(print())
			.andExpect(status().is(OK.value()))
			.andReturn();

			final Optional<AnnotationPojo> secondUpdatedAnno = annotationService.findAny(q -> q.byId(anno));

			secondUpdatedAnno.ifPresent(annotationPojo -> assertTrue(annotationPojo.getEnglishTranslation().get().contains(secondReplacedValue)));

		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	@Test
	void testAggregatedParameterAccessType() {
		final EntityId moduleId = createModule();
		createDataDictionaryEntry("TESTINPUT1", moduleId, new ModuleLocation(100, 0), "INPUT");
		createDataDictionaryEntry("TESTINPUT2", moduleId, new ModuleLocation(101, 0), "INPUT");
		createDataDictionaryEntry("TESTOUTPUT1", moduleId, new ModuleLocation(102, 0), "OUTPUT");
		createDataDictionaryEntry("TESTINPUTOUTPUT", moduleId, new ModuleLocation(103, 0), "INPUT,OUTPUT");

		final AggregationRequest<DataDictionaryFieldName> aggr1 = new AggregationRequest<>();
		aggr1.setFilterObject(Map.of(DataDictionaryFieldName.SCOPE_ATTRIBUTES, Map.of("eq", "INPUT")));
		aggr1.setFields(Map.of(DataDictionaryFieldName.DATA_ELEMENT_NAME, AggregationOperator.LIST));
		List<AggregationResult<DataDictionaryFieldName>> results1 = dataDictionaryService.getAggregations(PROJECT_ID, aggr1);

		/* The aggregation request should only contain TESTINPUT1, TESTINPUT2 and TESTINPUTOUTPUT since the Aggregationrequest looks for INPUT*/
		assertEquals(results1.get(0).getFields().get(DataDictionaryFieldName.DATA_ELEMENT_NAME), List.of("TESTINPUT1", "TESTINPUT2", "TESTINPUTOUTPUT"));

		final AggregationRequest<DataDictionaryFieldName> aggr2 = new AggregationRequest<>();
		aggr2.setFilterObject(Map.of(DataDictionaryFieldName.SCOPE_ATTRIBUTES, Map.of("in", List.of("INPUT", "OUTPUT"))));
		aggr2.setFields(Map.of(DataDictionaryFieldName.DATA_ELEMENT_NAME, AggregationOperator.LIST));
		List<AggregationResult<DataDictionaryFieldName>> results2 = dataDictionaryService.getAggregations(PROJECT_ID, aggr2);
		assertEquals(results2.get(0).getFields().get(DataDictionaryFieldName.DATA_ELEMENT_NAME), List.of("TESTINPUT1", "TESTINPUT2", "TESTINPUTOUTPUT", "TESTOUTPUT1"));
	}

	private EntityId createModule() {
		final ModulePojoPrototype proto = new ModulePojoPrototype()
				.setName("Test Module")
				.setProject(DataDictionaryControllerTest.PROJECT_ID)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API);
		return moduleService.create(proto);
	}

	private DataDictionaryPojo createDataDictionaryEntry(final String dataElementName, final EntityId moduleId, final ModuleLocation moduleLocation, @Nullable final String accessType) {
		final Map<DataDictionaryVariableScope, Map<String, String>> scopes = new HashMap<>();
		scopes.put(DataDictionaryVariableScope.SQL_DATABASE, null);
		scopes.put(DataDictionaryVariableScope.FILE, Collections.singletonMap(DataDictionaryVariableAttribute.FILE_DATASET.name(), "My data set name"));
		scopes.put(DataDictionaryVariableScope.CICS_UI, null);
		scopes.put(DataDictionaryVariableScope.OTHER, Collections.singletonMap("SCOPE_1", "TEST SOURCE"));

		if (accessType != null) {
			scopes.put(DataDictionaryVariableScope.PARAMETER, Map.of("accessType", accessType));
		}

		return dataDictionaryService.create(new DataDictionaryPojoPrototype()
				.setModule(moduleId)
				.setLocation(moduleLocation)
				.setName(dataElementName)
				.setScopes(scopes)
				.setDescription("MY description")
				.setFormat("PICX")
				.setLength(58L)
				.setParentGroup("XTAX-PRD")
				.setCreatedByUserId("admin")
				.setPicClause("TEST PIC CLAUSE")
				.setDefinedLocation(DefinedLocation.PROGRAM)
				.setIsBusiness(Boolean.TRUE)
				.setState(WorkingState.CANDIDATE)
				.setFieldTransformation("TEST TRANSFORMATION")
				.setSourceInput("TEST INPUT")
				.setTargetOutput("TEST OUTPUT")
				.setUsage("COMP1")
				.setFieldLevel(56L));
	}

	private EntityId createAnnotation(final EntityId moduleId) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setType(AnnotationType.RULE);
		annotation.setName("TESTANNOTATION");
		annotation.setState(WorkingState.IN_ANALYSIS);
		annotation.setCreatedByUserId("asdf");
		annotation.setModule(moduleId);
		annotation.setLocation(new ModuleLocation(0, 1));
		annotation.setEnglishTranslation("THIS SHOULD BE REPLACED -> Test");

		return annotationService.create(annotation);
	}

}
