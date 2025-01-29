/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.generation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult.Operation;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.entities.testing.AstNodePojoDummy;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.ast.model.statement.Format;

/**
 * Tests for {@link ModuleBlockGeneration}.
 */
class ModuleBlockGenerationTest extends MockedBaseTest {

	private static final String TEST_SOURCE_CONTENT = "01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS71D1:'.";
	private static final String TEST_SOURCE_CONTENT1 = "       IDENTIFICATION DIVISION.                                         \r\n"
			+ "       PROGRAM-ID.  SIMPCOND.                                           \r\n"
			+ "       DATA DIVISION.\r\n"
			+ "       WORKING-STORAGE SECTION.\r\n"
			+ "       PROCEDURE DIVISION.                                              \r\n"
			+ "                                                                        \r\n"
			+ "       MAIN0100-CONTROL.                                                \r\n"
			+ "                                                                        \r\n"
			+ "           IF WS-NO-MORE-RECORDS = 'Y'                                  \r\n"
			+ "              CONTINUE                                                  \r\n"
			+ "           ELSE                                                         \r\n"
			+ "              PERFORM PROC0100-PROCESS THRU PROC0100-EXIT               \r\n"
			+ "                   UNTIL WS-NO-MORE-RECORDS = 'Y'.                      \r\n"
			+ "                                                                        \r\n"
			+ "       MAIN0100-EXIT.                                                   \r\n"
			+ "           GOBACK.                                                      \r\n"
			+ "                                                                        ";
	private static final String TEST_SOURCE_CONTENT2 = "       IDENTIFICATION DIVISION.                                         \r\n"
			+ "       PROGRAM-ID.  SIMPCOND.                                           \r\n"
			+ "       DATA DIVISION.\r\n"
			+ "       WORKING-STORAGE SECTION.\r\n"
			+ "       PROCEDURE DIVISION.                                              \r\n"
			+ "                                                                        \r\n"
			+ "       MAIN0100-CONTROL.                                                \r\n"
			+ "                                                                        \r\n"
			+ "           IF WS-NO-MORE-RECORDS = 'Y'                                  \r\n"
			+ "              CONTINUE                                                  \r\n"
			+ "           END-IF.\r\n"
			+ "                                                                        \r\n"
			+ "       MAIN0100-EXIT.                                                   \r\n"
			+ "           GOBACK.                                                      \r\n"
			+ "                                                                        ";
	private static final EntityId ID_ONE = EntityId.of(1L);

	private ModulePojoDummy moduleProto = new ModulePojoDummy().prepare(m -> m
			.setName("Mod 1")
			.setTechnology(Technology.COBOL)
			.setType(Type.PROGRAM)
			.setProject(ID_ONE)
			.setPath("/src/MOD 1")
			.setStorage(Storage.DATABASE)
			.setIdentification(Identification.IDENTIFIED)
			.setOrigin(Origin.CUSTOM)
			.setCreator(Creator.DISCOVERY)
			.setNid(1L)
			.setLinkHash("abcdefghijklmnop")
			.setDescription("Description")
	);
	private ModulePojo module = moduleProto.build();
	private final Set<String> superTypes = Set.of(AstNodeUtils.CFG_COLLAPSIBLE_NODE, AstNodeUtils.INVOCABLE);
	
	private ModuleBlockGeneration functionalBlockGenerator;
	private FunctionalBlockService functionalBlockService;
	private AstService astService;
	private FunctionalBlockPojo mockFunctionalBlockPojo;
	
	@BeforeAll
	static void init() {
	}

	@BeforeEach
	public void setUp() {
		functionalBlockService = mock(FunctionalBlockService.class);
		astService = mock(AstService.class);
		functionalBlockGenerator = new ModuleBlockGeneration(functionalBlockService, moduleService, annotationService, astService);
		mockFunctionalBlockPojo = mock(FunctionalBlockPojo.class);
	}

	@Test
	void testGenerateModuleBlockWithNonNullModuleId() {
		final EntityId moduleId = EntityId.of(1L);
		when(Assert.assertNotNull(moduleService).findAnyModule(any())).thenReturn(Optional.of(module));
		when(Assert.assertNotNull(annotationService).find(any())).thenReturn(Collections.emptyList());
		when(Assert.assertNotNull(astService).find(any())).thenReturn(Collections.emptyList());
		
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(moduleId);
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> results = new ArrayList<>(functionalBlockGenerator.generate(context, moduleId));
		assertEquals(1, results.size());
		final Map<String, Object> flags = results.get(0).getFunctionalBlock().flags.get();
		assertEquals(Set.of(FunctionalBlockType.MODULE, FunctionalBlockType.STRUCTURAL), Assert.assertNotNull(flags).get("TYPE"));
		assertEquals(ModuleBlockGeneration.MODULE_BLOCK_GENERATION_ID, Assert.assertNotNull(flags).get(FunctionalBlockFlag.GENERATED_BY.name()));
		assertEquals(Boolean.TRUE, Assert.assertNotNull(flags).get(FunctionalBlockFlag.READ_ONLY.name()));
		assertEquals(Operation.CREATE, results.get(0).getOperation());
	}
	
	@Test
	void updateModuleAndAnotationDescriptions() {
		final EntityId moduleId = EntityId.of(1L);
		mockModuleDetails();
		final List<UUID> children = new ArrayList<>();
		final String newModuleDescription = "Changed Module Description";
		final String newAnnotationDescription = "New Annotation";
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockCount = new ArrayList<>();
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> annotationBlock = new ArrayList<>();
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(moduleId);
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> results = functionalBlocksAssertions(moduleId, children, moduleBlockCount,
				new ArrayList<>(), new ArrayList<>(), annotationBlock, context);
		final List<AnnotationPojo> annotations = new ArrayList<>();
		annotations.add(createAnnotation(newAnnotationDescription, 10, 3, 1));
		final UUID moduleBlockUid = moduleBlockCount.get(0).getFunctionalBlock().uid.getNonNull();
		module = new ModulePojoDummy().prepare(m -> m
				.setName("Mod 1")
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setProject(ID_ONE)
				.setPath("/src/MOD 1")
				.setStorage(Storage.DATABASE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
				.setDescription(newModuleDescription)
				.setNid(1L)
			).build();

		final FunctionalBlockPojo existingFunctionalBlock = mockForUpdateModuleFunctionalBlock(children, results, annotations, moduleBlockUid);
		
		mockForUpdateAnnotationFunctionalBlock(children, annotationBlock, existingFunctionalBlock);

		final List<FunctionalBlockGenerationResult<GeneratedFrom>> results2 = new ArrayList<>(functionalBlockGenerator.generate(context, moduleId));
		/* Filtering the functional blocks which has Operation as UPDATE there should only be 2blocks */
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> updatedFunctionalBlocks = results2.stream().filter(fb -> fb.getOperation()
				.equals(Operation.UPDATE)).collect(Collectors.toList());
		assertEquals(2, updatedFunctionalBlocks.size());
		
		/* Making sure that correct functional blocks(Module and Annotation) got updated by comparing the description */
		final boolean containsNewModuleAndNewAnn = updatedFunctionalBlocks.stream().map(item -> item.getFunctionalBlock().description.getNonNull())
				.allMatch(description -> description.equals(newModuleDescription) || description.equals(newAnnotationDescription));

		Assert.assertTrue(containsNewModuleAndNewAnn, "Both" + newModuleDescription + " and " + newAnnotationDescription + "should be present");
	}
	
	@Test
	void updateModuleDescriptionAndAddNewAnotation() {
		final EntityId moduleId = EntityId.of(1L);
		final String newModuleDescription = "Changed Module Description";
		final String newAnnotationDescription = "New Annotation";
		mockModuleDetails();
		final List<UUID> children = new ArrayList<>();
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockCount = new ArrayList<>();
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> annotationBlock = new ArrayList<>();
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(moduleId);
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> results = functionalBlocksAssertions(moduleId, children, moduleBlockCount,
				new ArrayList<>(), new ArrayList<>(), annotationBlock, context);
		final List<AnnotationPojo> annotations = new ArrayList<>();
		annotations.add(createAnnotationList().get(0));
		final AnnotationPojo annotation = createAnnotation(newAnnotationDescription, 20, 21, 5);
		annotations.add(annotation);
		final UUID moduleBlockUid = moduleBlockCount.get(0).getFunctionalBlock().uid.getNonNull();
		module = ModulePojoDummy.build(moduleProto.setDescription(newModuleDescription));

		final FunctionalBlockPojo existingFunctionalBlock = mockForUpdateModuleFunctionalBlock(children, results, annotations, moduleBlockUid);
		
		mockForUpdateAnnotationFunctionalBlock(children, annotationBlock, existingFunctionalBlock);

		final List<FunctionalBlockGenerationResult<GeneratedFrom>> results2 = new ArrayList<>(functionalBlockGenerator.generate(context, moduleId));
		/* Filtering the functional blocks which has Operation as UPDATE there should only be 2blocks */
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> updatedFunctionalBlocks = results2.stream().filter(fb -> fb.getOperation()
				.equals(Operation.UPDATE)).collect(Collectors.toList());
		updatedFunctionalBlocks.get(0).getFunctionalBlock().description.getNonNull();
		assertEquals(2, updatedFunctionalBlocks.size());
		/* Making sure that correct functional blocks(Module and Annotation) got updated by comparing the description */
		final boolean containsNewModuleAndNewAnn = updatedFunctionalBlocks.stream().map(item -> item.getFunctionalBlock().description.getNonNull())
				.allMatch(description -> description.equals(newModuleDescription) || description.equals(newAnnotationDescription));

		Assert.assertTrue(containsNewModuleAndNewAnn, "Both" + newModuleDescription + " and " + newAnnotationDescription + "should be present");
	}
	
	@Test
	void testGenerateParagraphBlockWithNonNullModuleId() {
		final EntityId moduleId = EntityId.of(1L);
		when(Assert.assertNotNull(moduleService).findAnyModule(any())).thenReturn(Optional.of(module));
		when(Assert.assertNotNull(annotationService).find(any())).thenReturn(Collections.emptyList());
		when(Assert.assertNotNull(astService).find(any())).thenReturn(Arrays.asList(
				createTestAstNode(1, 3, superTypes, TEST_SOURCE_CONTENT), createTestAstNode(5, 10, superTypes, TEST_SOURCE_CONTENT1), 
				createTestAstNode(11, 15, superTypes, TEST_SOURCE_CONTENT2)));
		
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(moduleId);
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> results = new ArrayList<>(functionalBlockGenerator.generate(context, moduleId));
		assertEquals(4, results.size());
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockCount = new ArrayList<>();
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> paragraphBlockCount = new ArrayList<>();
		
		results.stream().forEach(result -> {
			final FunctionalBlockPojoPrototype functionalBlock = Assert.assertNotNull(result.getFunctionalBlock());
			final Map<String, Object> flags = Assert.assertNotNull(Assert.assertNotNull(functionalBlock.flags).get());

			if (flags.get("TYPE").equals(Set.of(FunctionalBlockType.MODULE, FunctionalBlockType.STRUCTURAL))) {
				assertEquals(3, Assert.assertNotNull(Assert.assertNotNull(functionalBlock.children).get()).size());
				assertEquals(ModuleBlockGeneration.MODULE_BLOCK_GENERATION_ID, Assert.assertNotNull(flags).get(FunctionalBlockFlag.GENERATED_BY.name()));
				assertEquals(Boolean.TRUE, Assert.assertNotNull(flags).get(FunctionalBlockFlag.READ_ONLY.name()));
				moduleBlockCount.add(result);
			} else if (flags.get("TYPE").equals(Set.of(FunctionalBlockType.STRUCTURAL))) {
				assertEquals(ModuleBlockGeneration.PARAGRAPH_BLOCK_GENERATION_ID, Assert.assertNotNull(flags).get(FunctionalBlockFlag.GENERATED_BY.name()));
				assertEquals(Boolean.TRUE, Assert.assertNotNull(flags).get(FunctionalBlockFlag.READ_ONLY.name()));
				paragraphBlockCount.add(result);
			}
		});
		assertEquals(1, moduleBlockCount.size());
		assertEquals(3, paragraphBlockCount.size());
	}

	@Test
	void testGenerateAnnotationBlockWithNonNullModuleId() {
		final EntityId moduleId = EntityId.of(1L);
		mockModuleDetails();
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(moduleId);
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> results = new ArrayList<>(functionalBlockGenerator.generate(context, moduleId));
		assertEquals(9, results.size());
		 
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockCount = new ArrayList<>();
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> annotationBlockCount = new ArrayList<>();
		
		functionalBlocksAssertions(moduleId, new ArrayList<>(),
				moduleBlockCount, new ArrayList<>(), new ArrayList<>(), annotationBlockCount, context); 
	}

	@Test
	void testGenerateWithNullModuleId() {
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(null);
		/* asserting that method throws an exception when module is null */
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> functionalBlockGenerator.generate(context, null));
		assertEquals("Cannot execute module block generation because the 'moduleId' is null", exception.getMessage());
	}

	@Test
	void testGenerateWithNullModule() {
		final EntityId moduleId = EntityId.of(1L);
		when(Assert.assertNotNull(moduleService).findAnyModule(any())).thenReturn(Optional.empty());
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(moduleId);
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> functionalBlockGenerator.generate(context, moduleId));
		assertEquals("Cannot execute module block generation because the 'module' is null", exception.getMessage());
	}

	private List<AnnotationPojo> createAnnotationList() {
		final List<AnnotationPojo> annotationList = new ArrayList<>();
		final var annotation = createAnnotation("MINIMAL ANNOTATION", 10, 3, 1);
		final var annotation1 = createAnnotation("MINIMAL ANNOTATION1 Test ANNOTATION1 Test ANNOTATION1", 12, 2, 2);
		final var annotation2 = createAnnotation("MINIMAL ANNOTATION2 Test ANNOTATION2", 3, 6, 3);
		final var annotation3 = createAnnotation("MINIMAL ANNOTATION2 Test ANNOTATION2", 2, 4, 3);
		annotationList.add(annotation);
		annotationList.add(annotation1);
		annotationList.add(annotation2);
		annotationList.add(annotation3);
		return annotationList;
	}

	private AnnotationPojo createAnnotation(final String name, final int offset, final int length, final long id) {
		return new AnnotationPojoDummy().prepare(a -> a
				.setLocation(new ModuleLocation(offset, length))
				.setName(name)
				.setState(WorkingState.IN_ANALYSIS)
				.setType(AnnotationType.RULE)
				.setNid(id)
			).build();
	}

	/**
	 * Creates an {@link AstNodeV2} for test purposes using the provided data.
	 *
	 * @param offset offset in module
	 * @param length length of the DataField
	 * @param superTypes list of superTypes to be added to created {@link AstNodeV2}
	 * @param testSourceContent Source content
	 * @return the created {@link AstNodeV2} instance
	 */
	private AstNodePojo createTestAstNode(final Integer offset, final Integer length, @Nullable final Set<String> superTypes, final String testSourceContent) {
		final AstNodePojoDummy astNode = new AstNodePojoDummy();
		astNode.setLocation(new AstNodeLocation(offset, length));
		astNode.setLabel(testSourceContent);
		astNode.setProperties(new NestedMap()
				.set(Format.PROPERTY_LANGUAGE_TYPE, "PICX")
				.set(Format.PROPERTY_BYTE_LENGTH, 10)
				.set(FieldDefinition.PROPERTY_NAME, "MY-PROGRAM-NAME"));
		if(superTypes != null) {
			astNode.setSuperTypes(superTypes);
		}
		return astNode.build();
	}
	
	private void mockModuleDetails() {
		when(Assert.assertNotNull(moduleService).findAnyModule(any())).thenReturn(Optional.of(module));
		when(Assert.assertNotNull(astService).find(any())).thenReturn(Arrays.asList(
				createTestAstNode(5, 10, superTypes, TEST_SOURCE_CONTENT), createTestAstNode(6, 15, superTypes, TEST_SOURCE_CONTENT2),
				createTestAstNode(14, 20, superTypes, TEST_SOURCE_CONTENT), createTestAstNode(20, 30, superTypes, TEST_SOURCE_CONTENT1)));
		when(Assert.assertNotNull(annotationService).find(any())).thenReturn(createAnnotationList());
	}
	
	@SuppressWarnings("unchecked")
	private void mockForUpdateAnnotationFunctionalBlock(final List<UUID> children, final List<FunctionalBlockGenerationResult<GeneratedFrom>> annotationBlock,
			final FunctionalBlockPojo existingFunctionalBlock) {
		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(Arrays.asList(existingFunctionalBlock));
		
		when(mockFunctionalBlockPojo.getUid()).thenReturn(annotationBlock.get(0).getFunctionalBlock().uid.getNonNull());
		when(mockFunctionalBlockPojo.getProject()).thenReturn(annotationBlock.get(0).getFunctionalBlock().project.getNonNull());
		when(mockFunctionalBlockPojo.getModuleParts()).thenReturn(annotationBlock.get(0).getFunctionalBlock().moduleParts.getNonNull());
		when(mockFunctionalBlockPojo.getName()).thenReturn(annotationBlock.get(0).getFunctionalBlock().name.getNonNull());
		when(mockFunctionalBlockPojo.getDescription()).thenReturn(annotationBlock.get(0).getFunctionalBlock().description.getNonNull());
		when(mockFunctionalBlockPojo.getFlags()).thenReturn(annotationBlock.get(0).getFunctionalBlock().flags.getNonNull());
		when(mockFunctionalBlockPojo.getChildren()).thenReturn(null);
		
		final FunctionalBlockPojo existingAnnotationFunctionalBlock = new FunctionalBlockPojo(children.get(0), null, mockFunctionalBlockPojo.getProject(),
				mockFunctionalBlockPojo.getModuleParts(), null, null, mockFunctionalBlockPojo.getName(), mockFunctionalBlockPojo.getDescription(),
				mockFunctionalBlockPojo.getFlags(),  null);
		
		when(functionalBlockService.find(any(BuildingConsumer.class))).thenReturn(Arrays.asList(existingAnnotationFunctionalBlock));
	}

	private FunctionalBlockPojo mockForUpdateModuleFunctionalBlock(final List<UUID> children,
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> results, final List<AnnotationPojo> annotations, final UUID moduleBlockUid) {
		when(Assert.assertNotNull(moduleService).findAnyModule(any())).thenReturn(Optional.of(module));
		when(Assert.assertNotNull(astService).find(any())).thenReturn(Arrays.asList(
				createTestAstNode(5, 10, superTypes, TEST_SOURCE_CONTENT), createTestAstNode(6, 15, superTypes, TEST_SOURCE_CONTENT2),
				createTestAstNode(14, 20, superTypes, TEST_SOURCE_CONTENT), createTestAstNode(20, 30, superTypes, TEST_SOURCE_CONTENT1)));
		when(Assert.assertNotNull(annotationService).find(any())).thenReturn(annotations);

		when(mockFunctionalBlockPojo.getUid()).thenReturn(moduleBlockUid);
		when(mockFunctionalBlockPojo.getProject()).thenReturn(results.get(0).getFunctionalBlock().project.getNonNull());
		when(mockFunctionalBlockPojo.getModuleParts()).thenReturn(results.get(0).getFunctionalBlock().moduleParts.getNonNull());
		when(mockFunctionalBlockPojo.getName()).thenReturn(results.get(0).getFunctionalBlock().name.getNonNull());
		when(mockFunctionalBlockPojo.getDescription()).thenReturn(results.get(0).getFunctionalBlock().description.getNonNull());
		when(mockFunctionalBlockPojo.getFlags()).thenReturn(results.get(0).getFunctionalBlock().flags.getNonNull());
		when(mockFunctionalBlockPojo.getChildren()).thenReturn(children);
		final FunctionalBlockPojo existingFunctionalBlock = new FunctionalBlockPojo(moduleBlockUid, null, mockFunctionalBlockPojo.getProject(),
				mockFunctionalBlockPojo.getModuleParts(), null, mockFunctionalBlockPojo.getChildren(), mockFunctionalBlockPojo.getName(),
				mockFunctionalBlockPojo.getDescription(), mockFunctionalBlockPojo.getFlags(),  null);
		return existingFunctionalBlock;
	}

	public List<FunctionalBlockGenerationResult<GeneratedFrom>> functionalBlocksAssertions(final EntityId moduleId, final List<UUID> children,
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockCount,
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> paragraphBlockCount,
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> paragraphChildren,
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> annotationBlockCount, final FunctionalBlockGenerationContext context) {
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> results = new ArrayList<>(functionalBlockGenerator.generate(context, moduleId));
		assertEquals(9, results.size());
		results.stream().forEach(result -> {
			final FunctionalBlockPojoPrototype functionalBlock = Assert.assertNotNull(result.getFunctionalBlock());
			final Map<String, Object> flags = Assert.assertNotNull(Assert.assertNotNull(functionalBlock.flags).get());
			if (flags.get("TYPE").equals(Set.of(FunctionalBlockType.MODULE, FunctionalBlockType.STRUCTURAL))) {
				/* Module block contains 4 paragraph blocks and 2 annotation blocks*/
				assertEquals(6, Assert.assertNotNull(Assert.assertNotNull(functionalBlock.children).get()).size());
				assertEquals(ModuleBlockGeneration.MODULE_BLOCK_GENERATION_ID, Assert.assertNotNull(flags).get(FunctionalBlockFlag.GENERATED_BY.name()));
				assertEquals(Boolean.TRUE, Assert.assertNotNull(flags).get(FunctionalBlockFlag.READ_ONLY.name()));
				moduleBlockCount.add(result);
			} else if (flags.get("TYPE").equals(Set.of( FunctionalBlockType.STRUCTURAL))) {
				paragraphBlockCount.add(result);
				assertEquals(ModuleBlockGeneration.PARAGRAPH_BLOCK_GENERATION_ID, Assert.assertNotNull(flags).get(FunctionalBlockFlag.GENERATED_BY.name()));
				assertEquals(Boolean.TRUE, Assert.assertNotNull(flags).get(FunctionalBlockFlag.READ_ONLY.name()));
				children.add(result.getFunctionalBlock().uid.getNonNull());
				if (Assert.assertNotNull(Assert.assertNotNull(functionalBlock.children).get()).size() > 0) {
					paragraphChildren.add(result);
				}
			} else if (flags.get("TYPE").equals(Set.of(FunctionalBlockType.FUNCTIONAL_UNIT))) {
				assertEquals(ModuleBlockGeneration.ANNOTATION_BLOCK_GENERATION_ID, Assert.assertNotNull(flags).get(FunctionalBlockFlag.GENERATED_BY.name()));
				assertEquals(Boolean.TRUE, Assert.assertNotNull(flags).get(FunctionalBlockFlag.READ_ONLY.name()));
				children.add(result.getFunctionalBlock().uid.getNonNull());
				annotationBlockCount.add(result);

			}
		});
		assertEquals(1, moduleBlockCount.size());
		assertEquals(4, paragraphBlockCount.size());
		assertEquals(4, annotationBlockCount.size());
		/* Three annotation blocks added to matching paragraph blocks */
		assertEquals(2, paragraphChildren.size());
		return results;
	}

}
