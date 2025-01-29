/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;

import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.metamodel.FindMetamodel;
import innowake.mining.client.service.metamodel.FindPropertyMetadata;
import innowake.mining.client.service.metamodel.MetamodelServiceProvider;
import innowake.mining.shared.model.CustomPropertyDataType;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Integration tests for the metamodel services.
 * 
 * @see MetamodelServiceProvider
 */
class MetamodelServiceTest extends IntegrationTest {

	private static final int NUMBER_OF_CUSTOM_PROPERTIES_ON_ANNOTATION_ENTITY = 5;
	private MetamodelServiceProvider metaModelServiceProvider2 = MiningApiClient.metaModelService(getConnectionInfo());
	
	@Test
	void testFindMetamodelReturnsCustomPropertyInformation() throws IOException {
		final FindMetamodel findMetamodel = metaModelServiceProvider2.findMetamodel();
		final Result<CustomPropertyMetadata[]> result = findMetamodel.setClassName(CustomPropertyClass.AnnotationCustomProperties.name()).execute();
		
		assertEquals(HttpStatus.SC_OK, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final CustomPropertyMetadata[] metamodel = result.getValue().get();
		assertEquals(NUMBER_OF_CUSTOM_PROPERTIES_ON_ANNOTATION_ENTITY, metamodel.length);
	}

	@Test
	void testFindMetamodelReturns404ForUnknownClass() throws IOException {
		final FindMetamodel findMetamodel = metaModelServiceProvider2.findMetamodel();
		final Result<CustomPropertyMetadata[]> result = findMetamodel.setClassName("NONEXISTINGCLASSNAME").execute();
		
		assertEquals(HttpStatus.SC_NOT_FOUND, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testFindPropertyMetadataReturns404ForUnknownClass() throws IOException {
		final FindPropertyMetadata findPropertyMetadata = metaModelServiceProvider2.findPropertyMetadata(); 
		final Result<CustomPropertyMetadata> result = findPropertyMetadata.setClassName("NONEXISTING").setPropertyName("NOTRELEVANT").execute();

		assertEquals(HttpStatus.SC_NOT_FOUND, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testFindPropertyMetadataReturnsAllPropertyMetadata() throws IOException {
		final FindPropertyMetadata findPropertyMetadata = metaModelServiceProvider2.findPropertyMetadata(); 
		final Result<CustomPropertyMetadata> customMetaResult = findPropertyMetadata.setClassName(CustomPropertyClass.AnnotationCustomProperties.name()).setPropertyName("customMetaInfo").execute();

		assertEquals(HttpStatus.SC_OK, customMetaResult.getStatusCode());
		assertTrue(customMetaResult.getValue().isPresent());

		final CustomPropertyMetadata customPropertyMeta = customMetaResult.getValue().get();
		assertEquals("customMetaInfo", customPropertyMeta.getName());
		assertEquals("Some custom meta information", customPropertyMeta.getLabel());
		assertEquals(5, (int) customPropertyMeta.getMin());
		assertEquals(30, (int) customPropertyMeta.getMax());
		assertEquals("Custom datasource URL", customPropertyMeta.getDataSource());
		assertEquals("This is some more custom meta information", customPropertyMeta.getDescription());
		assertFalse(customPropertyMeta.isPluginVisible().booleanValue());
		assertFalse(customPropertyMeta.isMandatory().booleanValue());
		assertEquals(CustomPropertyDataType.STRING, customPropertyMeta.getDataType());
		final Map<String, Object> expectedShowWhen = new HashMap<>();
		expectedShowWhen.put("annotationCategoryId", 42);
		assertEquals(expectedShowWhen, customPropertyMeta.getShowWhen());
		assertEquals(Arrays.asList("First Custom View", "Second Custom View"), customPropertyMeta.getCustomViewNames());
		assertEquals(Integer.valueOf(21), customPropertyMeta.getCustomViewIndex());
		assertEquals("[a-z]*", customPropertyMeta.getValidationRegex());
		assertEquals("Incorrect Format", customPropertyMeta.getValidationErrorMessage());
	}
	
	@Test
	void testFindPropertyMetadataReturns404ForUnknownProperty() throws IOException {
		final FindPropertyMetadata findPropertyMetadata = metaModelServiceProvider2.findPropertyMetadata(); 
		final Result<CustomPropertyMetadata> result = findPropertyMetadata.setClassName(CustomPropertyClass.AnnotationCustomProperties.name()).setPropertyName("NOTRELEVANT").execute();

		assertEquals(HttpStatus.SC_NOT_FOUND, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}

	@Test
	void testFindPropertyMetadataReturns404ForDefaultProperty() throws IOException {
		final FindPropertyMetadata findPropertyMetadata = metaModelServiceProvider2.findPropertyMetadata(); 
		final Result<CustomPropertyMetadata> result = findPropertyMetadata.setClassName(CustomPropertyClass.AnnotationCustomProperties.name()).setPropertyName("name").execute();

		assertEquals(HttpStatus.SC_NOT_FOUND, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
}
