/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.annotationcategory.AnnotationCategoryServiceProvider;
import innowake.mining.data.access.postgres.AnnotationPgDao;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationType;

/**
 * Integration unit tests for the {@link AnnotationCategory} service.
 */
class AnnotationCategoryServiceTest extends IntegrationTest {
	
	private final AnnotationCategoryServiceProvider annotationCategoryServiceProvider = MiningApiClient.annotationCategoryService(getConnectionInfo());
	private static final AnnotationCategory TEST_ANNOTATION_CATEGORY_1 = new AnnotationCategory();
	private static final AnnotationCategory TEST_ANNOTATION_CATEGORY_2 = new AnnotationCategory();
	private static final AnnotationCategory TEST_ANNOTATION_CATEGORY_NO_TYPES = new AnnotationCategory();
	
	private static final EntityId NON_EXISTING_ID = EntityId.of(Long.MAX_VALUE);
	private static final EntityId ONE = EntityId.of(1l);
	private static final EntityId TWO = EntityId.of(2l);
	
	@BeforeAll
	public static void init() {
		TEST_ANNOTATION_CATEGORY_1.setName("TEST PROJECT 1");
		TEST_ANNOTATION_CATEGORY_1.setProjectId(ONE);
		TEST_ANNOTATION_CATEGORY_2.setName("TEST PROJECT 2");
		TEST_ANNOTATION_CATEGORY_2.setProjectId(TWO);
		TEST_ANNOTATION_CATEGORY_NO_TYPES.setName("TEST NO TYPES");
		TEST_ANNOTATION_CATEGORY_NO_TYPES.setProjectId(ONE);
	}
	
	@Test
	void testFindAllAnnotationCategories() throws IOException {
		final Result<AnnotationCategory[]> resultFindAll1 = annotationCategoryServiceProvider.findAllAnnotationCategories().setProjectId(ONE).execute();
		createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		createAnnotationCategory(TWO, TEST_ANNOTATION_CATEGORY_2);
		final Result<AnnotationCategory[]> resultFindAll2 = annotationCategoryServiceProvider.findAllAnnotationCategories().setProjectId(ONE).execute();
		assertEquals(200, resultFindAll2.getStatusCode());
		final AnnotationCategory[] annotationCategoriesBeforeCreate = resultFindAll1.getValue().get();
		final AnnotationCategory[] allAnnotationCategories = resultFindAll2.getValue().get();
		assertEquals(annotationCategoriesBeforeCreate.length + 1, allAnnotationCategories.length);
		assertFalse(resultContains(annotationCategoriesBeforeCreate, TEST_ANNOTATION_CATEGORY_1.getName()));
		assertFalse(resultContains(annotationCategoriesBeforeCreate, TEST_ANNOTATION_CATEGORY_2.getName()));
		assertTrue(resultContains(allAnnotationCategories, TEST_ANNOTATION_CATEGORY_1.getName()));
		assertFalse(resultContains(allAnnotationCategories, TEST_ANNOTATION_CATEGORY_2.getName()));
	}

	@Test
	void testFindAllAnnotationCategoriesWithDatabase() throws IOException {
		createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		createAnnotationCategory(TWO, TEST_ANNOTATION_CATEGORY_2);
		final Result<AnnotationCategory[]> resultFindAll = annotationCategoryServiceProvider.findAllAnnotationCategories().setProjectId(ONE).execute();
		assertEquals(200, resultFindAll.getStatusCode());
		verifyFindAll(resultFindAll.getValue().get(), findeAllByJDBC());
	}
	
	@Test
	void testFindById() throws IOException {
		final Long id = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1).getId();
		final Result<AnnotationCategory> resultFind = annotationCategoryServiceProvider.findAnnotationCategoryById().setProjectId(ONE).setAnnotationCategoryId(id).execute();
		assertEquals(200, resultFind.getStatusCode());
		final AnnotationCategory annotationCategory = resultFind.getValue().get();
		verifyAnnotationCategoryWithoutIdAndRid(TEST_ANNOTATION_CATEGORY_1, annotationCategory);
		//verifyNumberOfCustomProperties(annotationCategory, 1);
	}
	
	@Test
	void testFindByIdWithWrongProject() throws IOException {
		final Long id = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1).getId();
		final Result<AnnotationCategory> resultFind = annotationCategoryServiceProvider.findAnnotationCategoryById().setProjectId(TWO).setAnnotationCategoryId(id).execute();
		assertEquals(404, resultFind.getStatusCode());
		assertFalse(resultFind.getValue().isPresent());
	}

	@Test
	void testFindByIdNotFound() throws IOException {
		final Result<AnnotationCategory> resultFind = annotationCategoryServiceProvider
				.findAnnotationCategoryById().setProjectId(ONE).setAnnotationCategoryId(Long.MAX_VALUE).execute();
		assertEquals(404, resultFind.getStatusCode());
		assertFalse(resultFind.getValue().isPresent());
	}
	
	@Test
	void testCreateAnnotationCategory() throws IOException {
		verifyAnnotationCategoryWithoutIdAndRid(TEST_ANNOTATION_CATEGORY_1, createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1));
	}
	
	@Test
	void testCreateAnnotationCategoryWithWrongProjectId() throws IOException {
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(TWO).setAnnotationCategory(TEST_ANNOTATION_CATEGORY_1).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testCreateAnnotationCategoryDuplicate() throws IOException {
		verifyAnnotationCategoryWithoutIdAndRid(TEST_ANNOTATION_CATEGORY_1, createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1));
		final Result<AnnotationCategory> resultFail = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(ONE).setAnnotationCategory(TEST_ANNOTATION_CATEGORY_1).execute();
		assertEquals(400, resultFail.getStatusCode());
		assertFalse(resultFail.getValue().isPresent());
	}
	
	@Test
	void testCreateAnnotationCategoryWithoutNameAndClient() throws IOException {
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(ONE).setAnnotationCategory(new AnnotationCategory()).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testCreateAnnotationCategoryWithoutName() throws IOException {
		final AnnotationCategory incompleteAnnotationCategory = new AnnotationCategory();
		incompleteAnnotationCategory.setProjectId(ONE);
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(ONE).setAnnotationCategory(incompleteAnnotationCategory).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testCreateAnnotationCategoryWithoutExistingProject() throws IOException {
		final AnnotationCategory incompleteAnnotationCategory = new AnnotationCategory();
		incompleteAnnotationCategory.setName("I HAVE A NAME");
		incompleteAnnotationCategory.setProjectId(NON_EXISTING_ID);
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(ONE).setAnnotationCategory(incompleteAnnotationCategory).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	@Test
	void testCreateAnnotationCategoryWithMultipleTypes() throws IOException {
		final AnnotationCategory category = new AnnotationCategory();
		final List<AnnotationType> typeList = Arrays.asList(AnnotationType.DATABASE, AnnotationType.RULE);
		category.setName("CategoryWithTypes");
		category.setProjectId(ONE);
		category.setTypes(typeList);
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(ONE).setAnnotationCategory(category).execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final List<AnnotationType> types = result.getValue().get().getTypes();
		assertNotNull(types);
		assertTrue(types.contains(AnnotationType.DATABASE));
		assertTrue(types.contains(AnnotationType.RULE));
	}
	
	@Test
	void testCreateAnnotationCategoryWithEmptyTypes() throws IOException {
		final AnnotationCategory category = new AnnotationCategory();
		final List<AnnotationType> typeList = new ArrayList<AnnotationType>();
		category.setName("CategoryWithTypes");
		category.setProjectId(ONE);
		category.setTypes(typeList);
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(ONE).setAnnotationCategory(category).execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final List<AnnotationType> types = result.getValue().get().getTypes();
		assertNotNull(types);
		assertTrue(types.isEmpty());
	}
	
	@Test
	void testCreateAnnotationCategoryWithOneType() throws IOException {
		final AnnotationCategory category = new AnnotationCategory();
		final List<AnnotationType> typeList = Arrays.asList(AnnotationType.DATABASE);
		category.setName("CategoryWithTypes");
		category.setProjectId(ONE);
		category.setTypes(typeList);
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(ONE).setAnnotationCategory(category).execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final List<AnnotationType> types = result.getValue().get().getTypes();
		assertNotNull(types);
		assertTrue(types.contains(AnnotationType.DATABASE));
		assertEquals(1, types.size());
	}
	
	@Test
	void testUpdate() throws IOException {
		final AnnotationCategory resultAnnotationCategory = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		final AnnotationCategory annotationCategoryExpected = getUpdateAnnotationCategory(resultAnnotationCategory.getId(), resultAnnotationCategory.getProjectId());
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		final AnnotationCategory updatedAnnotationCategory = resultUpdate.getValue().get();
		verifyAnnotationCategoryWithId(annotationCategoryExpected, updatedAnnotationCategory);
	}

	@Test
	void testUpdateWithWrongProjectId() throws IOException {
		final AnnotationCategory resultAnnotationCategory = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		final AnnotationCategory annotationCategoryExpected = getUpdateAnnotationCategory(resultAnnotationCategory.getId(), resultAnnotationCategory.getProjectId());
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(TWO).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(400, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateWithoutProjectId() throws IOException {
		final AnnotationCategory resultAnnotationCategory = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		final AnnotationCategory annotationCategoryExpected = getUpdateAnnotationCategory(resultAnnotationCategory.getId(), null);
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
	}

	@Test
	void testUpdateAnnotationCategoryNotFound() throws IOException {
		final AnnotationCategory annotationCategoryExpected = getUpdateAnnotationCategory(Long.MAX_VALUE, ONE);
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(404, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateUniqueName() throws IOException {
		final AnnotationCategory resultAnnotationCategory1 = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		final AnnotationCategory annotationCategoryExpected = getUpdateAnnotationCategory(resultAnnotationCategory1.getId(), resultAnnotationCategory1.getProjectId());
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		final Result<AnnotationCategory> resultCreate2 = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(ONE).setAnnotationCategory(TEST_ANNOTATION_CATEGORY_1).execute();
		assertEquals(201, resultCreate2.getStatusCode());
		final AnnotationCategory resultAnnotationCategory2 = resultCreate2.getValue().get();
		final AnnotationCategory annotationCategoryExpected2 = getUpdateAnnotationCategory(resultAnnotationCategory2.getId(), resultAnnotationCategory2.getProjectId());
		final Result<AnnotationCategory> resultUpdate3 = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected2).execute();
		assertEquals(400, resultUpdate3.getStatusCode());
		verifyAnnotationCategoryWithId(annotationCategoryExpected, resultUpdate.getValue().get());
		assertFalse(resultUpdate3.getValue().isPresent());
	}

	@Test
	void testUpdateChangeProjectId() throws IOException {
		final AnnotationCategory resultAnnotationCategory1 = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		final AnnotationCategory annotationCategoryExpected = getUpdateAnnotationCategory(resultAnnotationCategory1.getId(), TWO);
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(400, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateUniqueNameDifferentClient() throws IOException {
		final AnnotationCategory resultAnnotationCategory1 = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		final AnnotationCategory resultAnnotationCategory2 = createAnnotationCategory(TWO, TEST_ANNOTATION_CATEGORY_2);
		final AnnotationCategory annotationCategoryExpected = getUpdateAnnotationCategory(resultAnnotationCategory1.getId(), resultAnnotationCategory1.getProjectId());
		final AnnotationCategory annotationCategoryExpected2 = getUpdateAnnotationCategory(resultAnnotationCategory2.getId(), resultAnnotationCategory2.getProjectId());
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		final Result<AnnotationCategory> resultUpdate3 = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(TWO).setAnnotationCategory(annotationCategoryExpected2).execute();
		assertEquals(200, resultUpdate3.getStatusCode());
		verifyAnnotationCategoryWithId(annotationCategoryExpected, resultUpdate.getValue().get());
		verifyAnnotationCategoryWithId(annotationCategoryExpected2, resultUpdate3.getValue().get());
	}

	@Test
	void testUpdateWithSameName() throws IOException {
		final AnnotationCategory resultAnnotationCategory1 = createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1);
		final AnnotationCategory annotationCategoryExpected = getUpdateAnnotationCategory(resultAnnotationCategory1.getId(), resultAnnotationCategory1.getProjectId());
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		final Result<AnnotationCategory> resultUpdate2 = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(200, resultUpdate2.getStatusCode());
		verifyAnnotationCategoryWithId(annotationCategoryExpected, resultUpdate2.getValue().get());
	}

	@Test
	void testUpdateWithoutName() throws IOException {
		final AnnotationCategory annotationCategoryExpected = new AnnotationCategory();
		annotationCategoryExpected.setId(createAnnotationCategory(ONE, TEST_ANNOTATION_CATEGORY_1).getId());
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(400, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateNonExistingWithoutName() throws IOException {
		final AnnotationCategory annotationCategoryExpected = new AnnotationCategory();
		annotationCategoryExpected.setId(Long.MAX_VALUE);
		final Result<AnnotationCategory> resultUpdate = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(annotationCategoryExpected).execute();
		assertEquals(400, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}
	
	@Test
	void testUpdateWithMultipleTypes() throws IOException {
		assertTrue(CollectionUtils.isEmpty(TEST_ANNOTATION_CATEGORY_NO_TYPES.getTypes()));
		final AnnotationCategory existingCategory = createAnnotationCategoryNoCustomProperty(ONE, TEST_ANNOTATION_CATEGORY_NO_TYPES);
		final List<AnnotationType> typeList = Arrays.asList(AnnotationType.DATABASE, AnnotationType.RULE);
		existingCategory.setTypes(typeList);
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(existingCategory).execute();
		assertEquals(200, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final List<AnnotationType> types = result.getValue().get().getTypes();
		assertNotNull(types);
		assertTrue(types.contains(AnnotationType.DATABASE));
		assertTrue(types.contains(AnnotationType.RULE));
	}
	
	@Test
	void testUpdateWithEmptyTypes() throws IOException {
		assertTrue(CollectionUtils.isEmpty(TEST_ANNOTATION_CATEGORY_NO_TYPES.getTypes()));
		final AnnotationCategory existingCategory = createAnnotationCategoryNoCustomProperty(ONE, TEST_ANNOTATION_CATEGORY_NO_TYPES);
		final List<AnnotationType> typeList = new ArrayList<AnnotationType>();
		existingCategory.setTypes(typeList);
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(existingCategory).execute();
		assertEquals(200, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final List<AnnotationType> types = result.getValue().get().getTypes();
		assertNotNull(types);
		assertTrue(types.isEmpty());
	}
	
	@Test
	void testUpdateWithOneType() throws IOException {
		assertTrue(CollectionUtils.isEmpty(TEST_ANNOTATION_CATEGORY_NO_TYPES.getTypes()));
		final AnnotationCategory existingCategory = createAnnotationCategoryNoCustomProperty(ONE, TEST_ANNOTATION_CATEGORY_NO_TYPES);
		final List<AnnotationType> typeList = Arrays.asList(AnnotationType.DATABASE);
		existingCategory.setTypes(typeList);
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.updateAnnotationCategory().setProjectId(ONE).setAnnotationCategory(existingCategory).execute();
		assertEquals(200, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final List<AnnotationType> types = result.getValue().get().getTypes();
		assertNotNull(types);
		assertTrue(types.contains(AnnotationType.DATABASE));
		assertEquals(1, types.size());
	}
	
	private AnnotationCategory getUpdateAnnotationCategory(final Long id, @Nullable final EntityId projectId) {
		final AnnotationCategory annotationCategory = new AnnotationCategory();
		annotationCategory.setId(id);
		if (projectId != null) {
			annotationCategory.setProjectId(projectId);
		}
		annotationCategory.setName("UPDATE TEST CLIENT");
		return annotationCategory;
	}
	
	private boolean resultContains(final AnnotationCategory[] result, final String annotationCategoryName) {
		for (int i = 0; i < result.length; i++) {
			if (annotationCategoryName.equals(result[i].getName())) {
				return true;
			}
		}
		return false;
	}
	
	private void verifyAnnotationCategoryWithoutIdAndRid(final AnnotationCategory input, final AnnotationCategory result) {
		assertEquals(input.getName(), result.getName());
		assertEquals(input.getProjectId().getNid(), result.getProjectId().getNid());
	}
	
	private void verifyAnnotationCategoryWithId(final AnnotationCategory input, final AnnotationCategory result) {
		assertEquals(input.getId(), result.getId());
		assertEquals(input.getProjectId(), result.getProjectId());
		assertEquals(input.getName(), result.getName());
	}

	private Map<Long,AnnotationCategory> findeAllByJDBC() {
		final var annotationDao = new AnnotationPgDao(getDataSource());
		return annotationDao.findCategories(q -> q.ofProjectWithDefault(ONE))
			.stream().collect(Collectors.toMap(AnnotationCategory::getId, Function.identity()));
	}
	
	private void verifyFindAll(final AnnotationCategory[] annotationCategories, final Map<Long,AnnotationCategory> databaseResult) {
		for (final AnnotationCategory annotationCategory : annotationCategories) {
			final Long id = annotationCategory.getId();
			final AnnotationCategory expected = databaseResult.get(id);
			assertNotNull(expected);
			verifyAnnotationCategoryWithId(expected, annotationCategory);
			databaseResult.remove(id);
		}
		assertTrue(databaseResult.isEmpty());
	}
	
	private AnnotationCategory createAnnotationCategoryNoCustomProperty(final EntityId projectId, final AnnotationCategory annotationCategory) throws IOException {
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider
				.createAnnotationCategory().setProjectId(projectId).setAnnotationCategory(annotationCategory).execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final AnnotationCategory createdAnnotationCategory = result.getValue().get();
		return createdAnnotationCategory;
	}
	
	private AnnotationCategory createAnnotationCategory(final EntityId projectId, final AnnotationCategory annotationCategory) throws IOException {
		final Result<AnnotationCategory> result = annotationCategoryServiceProvider.createAnnotationCategory().setProjectId(projectId.getNid())
				.setAnnotationCategory(annotationCategory).execute();
		assertEquals(result.getExtendedStatusMessage(), 201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final AnnotationCategory createdAnnotationCategory = result.getValue().get();
		return createdAnnotationCategory;
	}
}
