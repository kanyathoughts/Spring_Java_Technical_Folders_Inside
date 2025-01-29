/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.service;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.config.Profiles;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.entities.testing.AstNodePojoDummy;
import innowake.mining.shared.entities.testing.AstRelationshipPojoDummy;
import innowake.mining.shared.entities.testing.TestAstMap;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.DataFieldFormat;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.ast.model.statement.Format;
/**
 * Contains tests for AstNodeService
 */
@Import({ AstNodeService.class })
@ActiveProfiles(Profiles.AUTH_TEST)
@WithMockUser
@Tag("mocked")
class AstNodeServiceMockedTest extends MockedBaseTest {

    private static final Integer OFFSET = 5;
    private static final Integer LENGTH = 15;
	
	private static final String FIELD_DEFINITION = "FieldDefinition";
	private static final String FIELD_REFERENCE = "FieldReference";
	private static final String TEST_SOURCE_CONTENT = "01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS71D1:'.";
	
	private static final EntityId PROJECT_ID = EntityId.of(1l);

	@Autowired
	private AstNodeService astNodeService;
	
	private static ModuleLightweightPojo moduleLightweight;
	private static DataDictionaryPojo ddEntry;
	static {
		moduleLightweight = new ModuleLightweightPojo(UUID.randomUUID(), 1L, PROJECT_ID, null, null,
				"test_module", "test_module", Technology.COBOL, Type.PROGRAM, "", null, true, null, null, null, null);
		ddEntry = new DataDictionaryPojo(UUID.randomUUID(), 5000L, moduleLightweight.identity(), null, null,
				new ModuleLocation(OFFSET, LENGTH), "MY-PROGRAM-NAME", "test description",
				FIELD_DEFINITION, Map.of(), LENGTH.longValue(), 
				"system_user", null, null, 
				null, null, null, 
				null, null, null, 
				null, null, false, 
				null, null, null, 
				null, null, null, Collections.emptyList(), CustomPropertiesMap.empty());
	}

	/**
	 * Tests whether the DataFieldFormat returned by {@link AstNodeService#getFormatIfSelectionIsValid} 
	 * has the correct DataDictionaryEntryId assigned to it. Uses "FieldDefinition" as an AstNode supeType.
	 */
	@Test
	void testGetFormatIfSelectionIsValidFieldDefinition() {
		testGetFormatIfSelectionIsValid(FIELD_DEFINITION);
	}
	
	/**
	 * Tests whether the DataFieldFormat returned by {@link AstNodeService#getFormatIfSelectionIsValid} 
	 * has the correct DataDictionaryEntryId assigned to it. Uses "FieldReference" as an AstNode supeType.
	 */
	@Test
	void testGetFormatIfSelectionIsValidFieldReference() {
		testGetFormatIfSelectionIsValid(FIELD_REFERENCE);
	}
	
	/**
	 * Tests whether {@link AstNodeService#getFormatIfSelectionIsValid} throws an EntityNotFoundException 
	 * when there are no AstNodes available in a given module at a given offset.
	 */
	@Test
	void testGetFormatIfSelectionIsValidNoAstNode() {
		when(Assert.assertNotNull(moduleService).findAnyModuleLightweight(any())).thenReturn(Optional.of(moduleLightweight));
		when(Assert.assertNotNull(astService).find(any())).thenReturn(Collections.emptyList());
		final var moduleId = moduleLightweight.identity();
		final MiningEntityNotFoundException exception = assertThrows(MiningEntityNotFoundException.class,
				() -> astNodeService.getFormatIfSelectionIsValid(PROJECT_ID, moduleId, OFFSET, false));
		assertTrue(exception.getMessage().contains(String.format("at module %s, offset %d.", moduleLightweight.identity().toString(), OFFSET)));
	}
	
	/**
	 * Creates an {@link AstNodeV2} and makes the mock return it.
	 * Tests whether the DataFieldFormat returned by {@link AstNodeService#getFormatIfSelectionIsValid} 
	 * has the correct DataDictionaryEntryId assigned to it.
	 *
	 * @param superType this gets added to superTypes of the created {@link AstNodeV2}
	 */
	private void testGetFormatIfSelectionIsValid(final String superType) {
		final TestAstMap testAST = new TestAstMap();
		final Set<String> superTypes = new HashSet<>();
		superTypes.add(superType);
		final AstNodePojoDummy newAstNode = prepareTestAstNode(OFFSET, LENGTH.intValue(), superTypes);
		
		final List<AstRelationshipPojo> refersToList = new ArrayList<>();
		refersToList.add(new AstRelationshipPojoDummy().prepare(r -> r
				.setSrc(newAstNode.id.getNonNull())
				.setDst(testAST.addNode(createTestAstNode(0, 0, null)).getId())
				.setType(AstRelationshipType.REFERS))
			.build(testAST));
		refersToList.add(new AstRelationshipPojoDummy().prepare(r -> r
				.setSrc(testAST.addNode(createTestAstNode(0, 0, null)).getId())
				.setDst(newAstNode.id.getNonNull())
				.setType(AstRelationshipType.REFERS))
			.build(testAST));
		
		final AstNodePojo astNode = testAST.addNode(newAstNode.build(refersToList));
		
		when(Assert.assertNotNull(moduleService).findAnyModuleLightweight(any())).thenReturn(Optional.of(moduleLightweight));
		when(Assert.assertNotNull(moduleService).getContentSubstring(any(), any(), any())).thenReturn(TEST_SOURCE_CONTENT);
		when(Assert.assertNotNull(astService).find(any())).thenReturn(Arrays.asList(astNode));
		when(Assert.assertNotNull(dataDictionaryService).find(any())).thenReturn(Collections.singletonList(ddEntry));
		final DataFieldFormat dfFormat = astNodeService.getFormatIfSelectionIsValid(PROJECT_ID, moduleLightweight.identity(), 10, false);
		assertEquals(ddEntry.getId(), dfFormat.getDataDictionaryEntryId());
	}
	
	/**
	 * Creates an {@link AstNodeV2} for test purposes using the provided data.
	 *
	 * @param offset offset in module
	 * @param length length of the DataField
	 * @param superTypes list of superTypes to be added to created {@link AstNodeV2}
	 * @return the created {@link AstNodeV2} instance
	 */
	private AstNodePojo createTestAstNode(final Integer offset, final Integer length, @Nullable final Set<String> superTypes) {
		return prepareTestAstNode(offset, length, superTypes).build();
	}
	
	private AstNodePojoDummy prepareTestAstNode(final Integer offset, final Integer length, @Nullable final Set<String> superTypes) {
		final AstNodePojoDummy astNode = new AstNodePojoDummy();
		astNode.setId(UUID.randomUUID());
		astNode.setLocation(new AstNodeLocation(offset, length));
		astNode.setLabel(TEST_SOURCE_CONTENT);
		astNode.setProperties(new NestedMap()
				.set(Format.PROPERTY_LANGUAGE_TYPE, "PICX")
				.set(Format.PROPERTY_BYTE_LENGTH, 10)
				.set(FieldDefinition.PROPERTY_NAME, "MY-PROGRAM-NAME"));
		if(superTypes != null) {
			astNode.setSuperTypes(superTypes);
		}
		return astNode;
	}

}
