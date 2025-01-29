/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.data.io.sourceobject;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import innowake.mining.shared.io.SecuredZipInputStream;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.gson.Gson;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.io.sourceobject.SourceObjectImportService;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.io.MiningFileIndex.File;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.springframework.test.context.TestPropertySource;

/**
 * Test cases for SourcePojo import.
 */
@TestPropertySource(properties = {
		"configuration.discovery-source-upload-maximum-size=-1", /* Override maximum size as -1 to disable checks */
		"configuration.discovery-source-upload-maximum-ratio=10",        /* Override maximum ratio */
		"configuration.discovery-source-upload-maximum-entries=5"     /* Override maximum entries */
})
class SourceObjectImportServiceTest extends DatabaseRelatedTest {
	
	private static final NullProgressMonitor PROGRESS_MONITOR = new NullProgressMonitor();

	@Autowired
	private SourceObjectImportService sourceObjectImportService;

	@Autowired
	private SourceService sourceService;

	private final Gson gson = new Gson();

	private static final String UPLOAD2 = "test/upload2.cbl";
	private static final String UPLOAD1_MODIFIED = "test/upload1-to-be-modified-in-upload2.cbl";
	private static final String UPLOAD1_REMOVED = "test/upload1-to-be-deleted-in-upload2.cbl";
	private static final String UPLOAD1_PRESERVED = "test/upload1.cbl";
	private static final EntityId PROJECT_ONE = EntityId.of(1L);
	private static final String STREAM_ID = "test";
	private static final String UPLOAD_MAP_FILE1 = "upload1.map";
	private static final String UPLOAD_MAP_FILE2 = "test/upload1.map";

	@Test
	void testImportSourceObjects() throws IOException {
		final long currentRevision = getCurrentSourceCodeRevision(PROJECT_ONE);
		final long currentMetricsBase = getCurrentMetricsBaseRevision(PROJECT_ONE);

		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));

		/* do the upload */
		final byte[] fileData = prepareFileUpload(Long.valueOf(21), "/", null, "test/foo.cbl", "test/bar.cbl", "test-foobar/baz.job");
		final MiningFileIndex fileIndex = sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, 
				new ByteArrayInputStream(fileData));

		/* it should have imported all objects into the database and returned an updated file index */

		/* check database */
		final List<SourcePojo> sourceObjects = sourceService.find(q -> q.ofProject(PROJECT_ONE));
		final Set<String> pathSet = sourceObjects.stream().map(sourceObject -> sourceObject.getPath()).collect(Collectors.toSet());
		assertEquals(3, sourceObjects.size());
		assertTrue(pathSet.contains("test/foo.cbl"));
		assertTrue(pathSet.contains("test/bar.cbl"));
		assertTrue(pathSet.contains("test-foobar/baz.job"));

		/* check generated index file */
		final Set<String> indexPathSet = fileIndex.getFiles().stream().map(file -> file.getPath()).collect(Collectors.toSet());
		assertEquals(Long.valueOf(currentRevision + 1), fileIndex.getSourceCodeRevision());
		assertEquals(3, fileIndex.getFiles().size());
		assertEquals("/", fileIndex.getScope());
		/* all files must have id in the updated index  */
		assertFalse(fileIndex.getFiles().stream().anyMatch(file -> file.getId() == null));
		assertTrue(indexPathSet.contains("test/foo.cbl"));
		assertTrue(indexPathSet.contains("test/bar.cbl"));
		assertTrue(indexPathSet.contains("test-foobar/baz.job"));
		assertEquals(currentMetricsBase, getCurrentMetricsBaseRevision(PROJECT_ONE));
		assertTrue(currentMetricsBase < getCurrentSourceCodeRevision(PROJECT_ONE),
				"Project.metricsBaseRevision should be smaller than Project.sourceCodeRevision after import of source objects");
	}
	
	@Test
	void testImportSourceObjectWithNoScope() throws IOException {
		final long currentRevision = getCurrentSourceCodeRevision(PROJECT_ONE);
		final long currentMetricsBase = getCurrentMetricsBaseRevision(PROJECT_ONE);

		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));

		/* do the upload */
		final byte[] fileData = prepareFileUpload(Long.valueOf(21), StringUtils.EMPTY, null, "test/foo.cbl", "test/bar.cbl", "test-foobar/baz.job");
		final MiningFileIndex fileIndex = sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID,
				new ByteArrayInputStream(fileData));

		/* it should have imported all objects into the database and returned an updated file index */

		/* check database */
		final List<SourcePojo> sourceObjects = sourceService.find(q -> q.ofProject(PROJECT_ONE));
		final Set<String> pathSet = sourceObjects.stream().map(sourceObject -> sourceObject.getPath()).collect(Collectors.toSet());
		assertEquals(3, sourceObjects.size());
		assertTrue(pathSet.contains("test/foo.cbl"));
		assertTrue(pathSet.contains("test/bar.cbl"));
		assertTrue(pathSet.contains("test-foobar/baz.job"));

		/* check generated index file */
		final Set<String> indexPathSet = fileIndex.getFiles().stream().map(file -> file.getPath()).collect(Collectors.toSet());
		assertEquals(Long.valueOf(currentRevision + 1), fileIndex.getSourceCodeRevision());
		assertEquals(3, fileIndex.getFiles().size());
		assertEquals("/", fileIndex.getScope());
		/* all files must have id in the updated index  */
		assertFalse(fileIndex.getFiles().stream().anyMatch(file -> file.getId() == null));
		assertTrue(indexPathSet.contains("test/foo.cbl"));
		assertTrue(indexPathSet.contains("test/bar.cbl"));
		assertTrue(indexPathSet.contains("test-foobar/baz.job"));
		assertEquals(currentMetricsBase, getCurrentMetricsBaseRevision(PROJECT_ONE));
		assertTrue(currentMetricsBase < getCurrentSourceCodeRevision(PROJECT_ONE),
				"Project.metricsBaseRevision should be smaller than Project.sourceCodeRevision after import of source objects");
	}

	@Test
	void testReplaceExistingSourceObjects() throws IOException {
		final long currentRevision = getCurrentSourceCodeRevision(PROJECT_ONE);
		final long currentMetricsBase = getCurrentMetricsBaseRevision(PROJECT_ONE);

		/* create some source objects first */
		testImportSourceObjects();

		/* upload new stuff */
		final byte[] fileData = prepareFileUpload(Long.valueOf(22), STREAM_ID, null, "test/qux.cbl");
		final MiningFileIndex fileIndex = sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, 
				new ByteArrayInputStream(fileData));

		/* it should have deleted the existing files in "test" folder due to the scope setting and uploaded the new file
		 * it should have left the existing file in "test-foobar" as is */

		/* check database */
		final List<SourcePojo> sourceObjects = sourceService.find(q -> q.ofProject(PROJECT_ONE));
		final Set<String> pathSet = sourceObjects.stream().map(sourceObject -> sourceObject.getPath()).collect(Collectors.toSet());
		assertEquals(2, sourceObjects.size());
		assertTrue(pathSet.contains("test/qux.cbl"));
		assertTrue(pathSet.contains("test-foobar/baz.job"));

		/* check generated index file - it should return the updated contents of the "test" folder */
		final Set<String> indexPathSet = fileIndex.getFiles().stream().map(file -> file.getPath()).collect(Collectors.toSet());
		assertEquals(Long.valueOf(currentRevision + 2), fileIndex.getSourceCodeRevision());
		assertEquals(2, fileIndex.getFiles().size());
		assertEquals("/", fileIndex.getScope());
		assertFalse(fileIndex.getFiles().stream().anyMatch(file -> file.getId() == null));
		assertTrue(indexPathSet.contains("test/qux.cbl"));
		assertEquals(currentMetricsBase, getCurrentMetricsBaseRevision(PROJECT_ONE));
		assertTrue(currentMetricsBase < getCurrentSourceCodeRevision(PROJECT_ONE),
				"Project.metricsBaseRevision should be smaller than Project.sourceCodeRevision after import of source objects");
	}

	@Test
	void testReImportSourceObjects() throws IOException {
		final long currentMetricsBase = getCurrentMetricsBaseRevision(PROJECT_ONE);
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));

		/* do the upload 1 */
		final byte[] upload1Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD1_PRESERVED, UPLOAD1_REMOVED, UPLOAD1_MODIFIED);
		final MiningFileIndex fileIndex1 = sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, 
				new ByteArrayInputStream(upload1Data));
		final Map<String, MiningFileIndex.File> upload1Info = fetchSourceObjectIdFromMiningFileIndex(fileIndex1);

		/* do the upload 2 */
		final byte[] upload2Data = prepareFileUpload(Long.valueOf(56), "/", Arrays.asList(upload1Info.get(UPLOAD1_PRESERVED)), UPLOAD2, UPLOAD1_MODIFIED);
		final MiningFileIndex fileIndex2 = sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, 
				new ByteArrayInputStream(upload2Data));
		final Map<String, MiningFileIndex.File> upload2Info = fetchSourceObjectIdFromMiningFileIndex(fileIndex2);

		final List<String> sourceObjects = sourceService.find(q -> q.ofProject(PROJECT_ONE)).stream().map(SourcePojo::getPath).collect(Collectors.toList());
		assertTrue(sourceObjects.contains(UPLOAD1_PRESERVED));
		assertFalse(sourceObjects.contains(UPLOAD1_REMOVED));
		assertTrue(sourceObjects.contains(UPLOAD2));
		assertTrue(sourceObjects.contains(UPLOAD1_MODIFIED));
		assertNotEquals(upload1Info.get(UPLOAD1_MODIFIED).getId(), upload2Info.get(UPLOAD1_MODIFIED).getId());
		assertEquals(currentMetricsBase, getCurrentMetricsBaseRevision(PROJECT_ONE));
		assertTrue(currentMetricsBase < getCurrentSourceCodeRevision(PROJECT_ONE),
				"Project.metricsBaseRevision should be smaller than Project.sourceCodeRevision after import of source objects");
	}
	
	/**
	 * Test to check that the Technology/Type not updated on re-upload of SourcePojo without any properties changed
	 *
	 * @throws IOException On file upload
	 */
	@Test
	void testTechnologyTypeOnSrObjReImport() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));

		/* First Upload of the file upload1.map */
		final byte[] upload1Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD_MAP_FILE1);
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload1Data));
		final SourcePojo firstUploadsourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE1));
		/* Checking the technology/type is set as per the File Extension on first upload*/
		assertEquals(Technology.CICS, firstUploadsourceObject.getTechnology());
		assertEquals(Type.BMS_MAPSET, firstUploadsourceObject.getType());
		
		/* Mocking the Discovery Code analysis by updating the technology & type to BASIC & PROGRAM respectively for .map file */
		sourceService.update(new SourcePojoPrototype().withId(firstUploadsourceObject.identity())
				.setTechnology(Technology.BASIC)
				.setType(Type.PROGRAM));
		
		final MiningFileIndex.File file = new File(firstUploadsourceObject.getId(), firstUploadsourceObject.getPath(),
				Technology.UNKNOWN, Type.UNKNOWN,
				firstUploadsourceObject.getMetaDataRevision().longValue(), firstUploadsourceObject.getContentRevision().longValue(), false);
		
		/* Second Upload of the same file upload1.map with same properties */
		final byte[] upload2Data = prepareFileUpload(Long.valueOf(56), "/", Arrays.asList(file));
		
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload2Data));
		final SourcePojo nextUploadSourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE1));
		
		/* Checking that the technology/type is not updated on re-upload as the file properties remain same */
		assertEquals(Technology.BASIC, nextUploadSourceObject.getTechnology());
	 	assertEquals(Type.PROGRAM, nextUploadSourceObject.getType());
	}
	
	/**
	 * Test to check that the Technology/Type updates/changes on re-upload of SourcePojo of changed property
	 *
	 * @throws IOException On file upload
	 */
	@Test
	void testTechnologyTypeOnSrObjReImportWithPathChange() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));

		/* First Upload of the file upload1.map */
		final byte[] upload1Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD_MAP_FILE1);
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload1Data));
		
		/* Mocking the Discovery Code analysis by updating the technology & type to BASIC & PROGRAM respectively for .map file */
		final SourcePojo firstUploadsourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE1));
		assertNotNull(firstUploadsourceObject);
		assertNotNull(firstUploadsourceObject);
		sourceService.update(new SourcePojoPrototype().withId(firstUploadsourceObject.identity())
				.setTechnology(Technology.BASIC)
				.setType(Type.PROGRAM));
		
		/* Second Upload of the file upload1.map with change in properties like path change */
		final byte[] upload2Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD_MAP_FILE2);
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload2Data));
		final SourcePojo nextUploadSourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE2));
		final Technology nextUploadTech =  assertNotNull(nextUploadSourceObject).getTechnology();
		final Type nextUploadType =  assertNotNull(nextUploadSourceObject).getType();
		
		final List<String> sourceObjects = sourceService.find(q -> q.ofProject(PROJECT_ONE)).stream().map(SourcePojo::getPath).collect(Collectors.toList());
		/* Checking the deletion of old file and new file creation */
		assertFalse(sourceObjects.contains(UPLOAD_MAP_FILE1));
		assertTrue(sourceObjects.contains(UPLOAD_MAP_FILE2));
		/* Asserting the updated technology/type as per File extension on re-upload of path changed file  */
		assertEquals(Technology.CICS, nextUploadTech);
		assertEquals(Type.BMS_MAPSET, nextUploadType);
	}
	
	/**
	 * Test to check that the MetadataRevision/ContentRevision does not change on re-upload of SourcePojo
	 *
	 * @throws IOException On file upload
	 */
	@Test
	void testUnchangedMetadataAndContentRevisionsForSourceObjects() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));
		
		/* First Upload of the file upload1.map */
		final byte[] upload1Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD_MAP_FILE1);
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload1Data));
		final SourcePojo firstUploadsourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE1));
		final Long contentRevisionAfterFirstUpload = assertNotNull(firstUploadsourceObject).getContentRevision();
		final Long metaDataRevisionAfterFirstUpload = assertNotNull(firstUploadsourceObject).getMetaDataRevision();
		
		/* Second Upload of the same file without content change */
		final MiningFileIndex.File file = MiningFileIndex.File.setFromSourceObject(assertNotNull(firstUploadsourceObject));
		final byte[] upload2Data = prepareFileUpload(Long.valueOf(56), "/", Arrays.asList(file));
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload2Data));
		final SourcePojo secondUploadsourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE1));
		final Long contentRevisionAfterSecondUpload = assertNotNull(secondUploadsourceObject).getContentRevision();
		final Long metaDataRevisionAfterSecondUpload = assertNotNull(secondUploadsourceObject).getMetaDataRevision();
		
		assertEquals(contentRevisionAfterFirstUpload, contentRevisionAfterSecondUpload);
		assertEquals(metaDataRevisionAfterFirstUpload, metaDataRevisionAfterSecondUpload);
	}
	
	/**
	 * Test to check that the MetaDataRevision is increased when path is changed on re-upload of SourcePojo
	 *
	 * @throws IOException On file upload
	 */
	@Test
	void testChangedMetaDataRevisionsForSourceObjects() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));
		
		/* First Upload of the file upload1.map */
		final byte[] upload1Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD_MAP_FILE1);
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload1Data));
		final SourcePojo firstUploadSourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE1));
		final Long contentRevisionAfterFirstUpload = assertNotNull(firstUploadSourceObject).getContentRevision();
		final Long metaDataRevisionAfterFirstUpload = assertNotNull(firstUploadSourceObject).getMetaDataRevision();
		final String pathAfterFirstUpload = assertNotNull(firstUploadSourceObject).getPath();
		
		/* Second Upload of the same file with path change */
		final MiningFileIndex.File file = MiningFileIndex.File.setFromSourceObject(assertNotNull(firstUploadSourceObject));
		file.setPath(UPLOAD_MAP_FILE2);
		final byte[] upload2Data = prepareFileUpload(Long.valueOf(56), "/", Arrays.asList(file));
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload2Data));
		final List<SourcePojo> secondUploadSourceObjects = sourceService.find(q -> q.ofProject(PROJECT_ONE));
		/* check that no new source object was created - number of source objects is still 1 */
		assertEquals(1, secondUploadSourceObjects.size());
		final SourcePojo secondUploadSourceObject = secondUploadSourceObjects.get(0);
		final Long contentRevisionAfterSecondUpload = assertNotNull(secondUploadSourceObject).getContentRevision();
		final Long metaDataRevisionAfterSecondUpload = assertNotNull(secondUploadSourceObject).getMetaDataRevision();
		final String pathAfterSecondUpload = assertNotNull(secondUploadSourceObject).getPath();
		
		/* check that meta data revision was incremented (because path changed) but not content revision (because we did not send content) */
		assertEquals(contentRevisionAfterFirstUpload, contentRevisionAfterSecondUpload);
		assertEquals(Long.valueOf(metaDataRevisionAfterFirstUpload.longValue() + 1), metaDataRevisionAfterSecondUpload);
		
		/* Check for path to be updated */
		assertNotEquals(pathAfterFirstUpload, pathAfterSecondUpload);
		assertEquals(UPLOAD_MAP_FILE2, pathAfterSecondUpload);
	}
	
	/**
	 * Test to check that the ContentRevision is increased when content is changed on re-upload of SourcePojo
	 *
	 * @throws IOException On file upload
	 */
	@Test
	void testChangedContentRevisionsForSourceObjects2() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));
		
		/* First Upload of the file upload1.map */
		final byte[] upload1Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD1_PRESERVED);
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload1Data));
		
		final SourcePojo firstUploadsourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD1_PRESERVED));
		final int contentRevisionAfterFirstUpload = assertNotNull(firstUploadsourceObject).getContentRevision().intValue();
		
		/* Second Upload of the same file with content change */
		final byte[] upload2Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD1_MODIFIED);
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload2Data));
		final SourcePojo secondUploadsourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD1_MODIFIED));
		final int contentRevisionAfterSecondUpload = assertNotNull(secondUploadsourceObject).getContentRevision().intValue();
		
		assertEquals(contentRevisionAfterFirstUpload + 1, contentRevisionAfterSecondUpload);
	}

	/**
	 * Test to check that the ContentRevision is increased when content is changed on re-upload of SourcePojo
	 *
	 * @throws IOException On file upload
	 */
	@Test
	void testInvalidZipFile() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));
		
		try {
			final byte[] strBytes = "just a string".getBytes(StandardCharsets.UTF_8);
			sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(strBytes));
			fail("SourceImport must fail for an invalid zip file");
		} catch (final IllegalArgumentException e) {
			/* expected */
		}
	}
	
	/**
	 * Test to check that the Technology/Type are updated on re-upload of SourcePojo
	 *
	 * @throws IOException On file upload
	 */
	@Test
	void testTypeAndTechnologyUpdate() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));

		/* First Upload of the file upload1.map */
		final byte[] upload1Data = prepareFileUpload(Long.valueOf(56), "/", null, UPLOAD_MAP_FILE1);
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload1Data));
		final SourcePojo firstUploadsourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE1));
		/* Checking the technology/type is set as per the File Extension on first upload*/
		assertEquals(Technology.CICS, firstUploadsourceObject.getTechnology());
	 	assertEquals(Type.BMS_MAPSET, firstUploadsourceObject.getType());
		
		final MiningFileIndex.File file = MiningFileIndex.File.setFromSourceObject(assertNotNull(firstUploadsourceObject));
		file.setTechnology(Technology.BASIC);
		file.setType(Type.PROGRAM);
		
		/* Second Upload of the same file upload1.map with different properties */
		final byte[] upload2Data = prepareFileUpload(Long.valueOf(56), "/", Arrays.asList(file));
		
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(upload2Data));
		final SourcePojo nextUploadSourceObject = sourceService.get(q -> q.ofProject(PROJECT_ONE).withPath(UPLOAD_MAP_FILE1));
		
		/* Checking that the technology/type to match the updated values on re-upload */
		assertEquals(Technology.BASIC, nextUploadSourceObject.getTechnology());
	 	assertEquals(Type.PROGRAM, nextUploadSourceObject.getType());
	}

	@Test
	void testImportLargeSourceObject() throws IOException {
		final long currentRevision = getCurrentSourceCodeRevision(PROJECT_ONE);
		final long currentMetricsBase = getCurrentMetricsBaseRevision(PROJECT_ONE);

		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));

		/* do the upload */
		final byte[] fileData = prepareFileUpload(
				Long.valueOf(21), "/", null, new byte[][] {
						new byte[1], 
						new byte[(int) SourceService.CONTENT_SIZE_LIMIT + 1],
						new byte[(int) SourceService.CONTENT_SIZE_LIMIT - 1],
					}, "test/foo.cbl", "test/big.cbl", "test/baz.job");
		final MiningFileIndex fileIndex = sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, 
				new ByteArrayInputStream(fileData));

		final List<SourcePojo> sourceObjects = sourceService.find(q -> q.ofProject(PROJECT_ONE));
		final Set<String> pathSet = sourceObjects.stream().map(sourceObject -> sourceObject.getPath()).collect(Collectors.toSet());
		assertEquals(2, sourceObjects.size());
		assertTrue(pathSet.contains("test/foo.cbl"));
		assertTrue(pathSet.contains("test/baz.job"));
		assertFalse(pathSet.contains("test/big.cbl"));
		
		/* check generated index file */
		final Set<String> indexPathSet = fileIndex.getFiles().stream().map(file -> file.getPath()).collect(Collectors.toSet());
		assertEquals(Long.valueOf(currentRevision + 1), fileIndex.getSourceCodeRevision());
		assertEquals(2, fileIndex.getFiles().size());
		assertEquals("/", fileIndex.getScope());
		/* all files must have id in the updated index */
		assertFalse(fileIndex.getFiles().stream().anyMatch(file -> file.getId() == null));
		assertTrue(indexPathSet.contains("test/foo.cbl"));
		assertTrue(indexPathSet.contains("test/baz.job"));
		assertFalse(indexPathSet.contains("test/big.cbl"));
		assertFalse(indexPathSet.contains("test/large.cbl"));
		assertEquals(currentMetricsBase, getCurrentMetricsBaseRevision(PROJECT_ONE));
		assertTrue(currentMetricsBase < getCurrentSourceCodeRevision(PROJECT_ONE),
				"Project.metricsBaseRevision should be smaller than Project.sourceCodeRevision after import of source objects");
	}

	@Test
	void testImportFailsWithMaxFiles() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));
		/* do the upload */
		final byte[] fileData = prepareFileUpload(Long.valueOf(21), "/", null,
				new byte[][] {
						new byte[1],
						new byte[1],
						new byte[1],
						new byte[1],
						new byte[1],
						new byte[1],
						new byte[1]
				}, "test/foo.cbl", "test/foo2.cbl", "test/big.cbl", "test/baz.job", "test/large.pdf", "test/large2.pdf","test/file.job");
		final var securityException = assertThrows(Exception.class,
				() -> sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(fileData)));
		assertTrue(securityException instanceof SecuredZipInputStream.SecurityException);
		assertEquals("Entry limit exceeded: 6>5", securityException.getMessage());
	}

	@Test
	void testImportWithNegativeMaxSize() throws IOException {
		/* empty database */
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));

		/* do the upload */
		final byte[] fileData = prepareFileUpload(Long.valueOf(21), "/", null,
				new byte[][] {
						new byte[450000000],
				}, "test/foo.cbl");
		assertDoesNotThrow(() -> sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(fileData)));
	}

	@Test
	void testImportWithNullBytes() throws IOException {
		sourceService.removeAll(q -> q.ofProject(PROJECT_ONE).withPath(STREAM_ID + "%"));
		final var contentWithNulByte =  "Hello\0World";
		final var bytes = new byte[][] {
				contentWithNulByte.getBytes(StandardCharsets.UTF_8),
		};
		final byte[] fileData = prepareFileUpload(Long.valueOf(21), "/", null, bytes, "test/foo.cbl");
		sourceObjectImportService.importSourceObjects(PROGRESS_MONITOR, PROJECT_ONE, STREAM_ID, new ByteArrayInputStream(fileData));
		sourceService.findContent(q -> q.ofProject(PROJECT_ONE).withPath("test/foo.cbl")).forEach(content -> {
			assertTrue(content.getContent().toString().contains("Hello"));
			assertFalse(content.getContent().toString().contains("\0"));
			assertTrue(content.getContent().toString().contains("World"));
			assertEquals(10, content.getContent().toString().length());
		});
	}
	
	private Map<String, MiningFileIndex.File> fetchSourceObjectIdFromMiningFileIndex(final MiningFileIndex fileIndex) {
		return fileIndex.getFiles().stream().collect(Collectors.toMap(MiningFileIndex.File::getPath, f -> f));
	}

	private byte[] prepareFileUpload(final Long revision, final String scope, @Nullable final List<MiningFileIndex.File> additionalFileIndex,
			final byte[][] filesBytes,	final String... fileNames) throws IOException {
		assertEquals(filesBytes.length, fileNames.length);
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();

		final List<MiningFileIndex.File> files = Arrays.stream(fileNames)
				.map(fileName -> new MiningFileIndex.File(null, fileName, null, null, revision.longValue(), revision.longValue(), true))
				.collect(Collectors.toList());

		if (additionalFileIndex != null) {
			files.addAll(additionalFileIndex);
		}
		final MiningFileIndex fileIndex = new MiningFileIndex();
		fileIndex.setVersion(1);
		fileIndex.setScope(scope);
		fileIndex.setSourceCodeRevision(revision);
		fileIndex.setFiles(files);

		try (final ZipOutputStream zipOut = new ZipOutputStream(baos)) {
			zipOut.putNextEntry(new ZipEntry(MiningFileIndex.NAME));
			zipOut.write(gson.toJson(fileIndex).getBytes(StandardCharsets.UTF_8));

			int i = 0;
			for (final String fileName : fileNames) {
				zipOut.putNextEntry(new ZipEntry(fileName));
				zipOut.write(filesBytes[i++]);
			}
		}

		return baos.toByteArray();	
	
	}

	private byte[] prepareFileUpload(final Long revision, final String scope, @Nullable final List<MiningFileIndex.File> additionalFileIndex,
			final String... fileNames) throws IOException {
		/* Populates the filesSize parameter with an arbitrary value */
		final byte[][] filesBytes = new byte[fileNames.length][];
		for (int i = 0; i < fileNames.length; i++) {
			filesBytes[i] = new byte[1];
		}
		return prepareFileUpload(revision, scope, additionalFileIndex, filesBytes, fileNames);
	}

	private long getCurrentSourceCodeRevision(final EntityId projectId) {
		final ProjectPojo project = projectService.get(projectId);
		final Long sourceCodeRevision = project.getSourceCodeRevision();
		return sourceCodeRevision == null ? 0 : sourceCodeRevision.longValue();
	}

	private long getCurrentMetricsBaseRevision(final EntityId projectId) {
		final ProjectPojo project = projectService.get(projectId);
		final Long metricsBaseRevision = project.getMetricsBaseRevision();
		return metricsBaseRevision == null ? 0 : metricsBaseRevision.longValue();
	}
}
