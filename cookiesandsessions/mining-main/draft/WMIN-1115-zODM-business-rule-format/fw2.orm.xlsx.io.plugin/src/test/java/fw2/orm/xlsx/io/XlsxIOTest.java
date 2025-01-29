package fw2.orm.xlsx.io;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.Mapping;
import fw2.orm.xlsx.XlsxPackage;
import fw2.orm.xlsx.io.mapper.Mapper;
import fw2.orm.xlsx.io.mapper.RepositoryLookupStrategy;

public class XlsxIOTest {

	MappingLoader mappingLoader;

	@Before
	public void init() {

		this.mappingLoader = new MappingLoader();

	}

	@Test
	public void testLoadMapping() throws Exception {

		final Mapping mapping = this.mappingLoader.load("src/main/resources/model/ClassMapMapping.xmi");
		Assert.assertTrue(mapping != null);
	}

	@Test
	public void testReadClassMappingXlsx() throws Exception {

		final Mapping mapping = this.mappingLoader.load("src/main/resources/model/ClassMapMapping.xmi");
		Assert.assertTrue(mapping != null);
		final String inFileName = URI.createURI("src/test/resources/ClassMapMapping.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final Mapper mapper = new Mapper(new RepositoryImpl(mapping, XlsxPackage.eINSTANCE),new RepositoryLookupStrategy());
		final ClassMapXlsxReader reader = new ClassMapXlsxReader(inputStream, mapper);
		final List<ClassMap> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());

	}

	@Test
	public void testReadModel2ClassMappingXlsx() throws Exception {

		final Mapping mapping = this.mappingLoader.load("src/main/resources/model/ClassMapMapping.xmi");
		Assert.assertTrue(mapping != null);
		final String inFileName = URI.createURI("src/test/resources/model2ClassMaps.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final ClassMapXlsxReader reader = new ClassMapXlsxReader(inputStream, new Mapper(new RepositoryImpl(mapping, XlsxPackage.eINSTANCE),new RepositoryLookupStrategy()));
		final List<ClassMap> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());

	}

	@Test
	public void testWriteApplicationClassMappingXmi() throws Exception {

		final Mapping mapping = this.mappingLoader.load("src/main/resources/model/ClassMapMapping.xmi");
		Assert.assertTrue(mapping != null);
		final String inFileName = URI.createURI("src/test/resources/ApplicationClassMaps.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final ClassMapXlsxReader reader = new ClassMapXlsxReader(inputStream, new Mapper(new RepositoryImpl(mapping, XlsxPackage.eINSTANCE),new RepositoryLookupStrategy()));
		final List<ClassMap> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());

		final Mapping model2Mapping = XlsxPackage.eINSTANCE.getXlsxFactory().createMapping();

		model2Mapping.setName("ApplicationClassMaps");
		model2Mapping.getClassMaps().addAll(classMaps);
		final String uri = "src/test/resources/ApplicationClassMaps.xmi";

		this.mappingLoader.save(model2Mapping, uri);

	}

	@Test
	public void testWriteApplicationMappingDetailsClassMappingXmi() throws Exception {

		final Mapping mapping = this.mappingLoader.load("src/main/resources/model/ClassMapMapping.xmi");
		Assert.assertTrue(mapping != null);
		final String inFileName = URI.createURI("src/test/resources/Application Mapping Details ClassMaps.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final ClassMapXlsxReader reader = new ClassMapXlsxReader(inputStream, new Mapper(new RepositoryImpl(mapping, XlsxPackage.eINSTANCE),new RepositoryLookupStrategy()));
		final List<ClassMap> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());

		final Mapping model2Mapping = XlsxPackage.eINSTANCE.getXlsxFactory().createMapping();

		model2Mapping.setName("Application Mapping Details ClassMaps");
		model2Mapping.getClassMaps().addAll(classMaps);
		final String uri = "src/test/resources/Application Mapping Details ClassMaps.xmi";

		this.mappingLoader.save(model2Mapping, uri);

	}

	@Test
	public void testWriteClassMappingXlsx() throws Exception {

		final Mapping mapping = this.mappingLoader.load("src/main/resources/model/ClassMapMapping.xmi");
		Assert.assertTrue(mapping != null);
		final String outFileName = URI.createURI("src/test/resources/ClassMapMapping.xlsx").toFileString();
		final Mapper mapper = new Mapper(new RepositoryImpl(mapping, XlsxPackage.eINSTANCE),new RepositoryLookupStrategy());
		final ClassMapXlsxWriter writer = new ClassMapXlsxWriter(outFileName, mapper);
		writer.writeObjects(mapping.getClassMaps());
	}

	@Test
	public void testWriteModel2ClassMappingXmi() throws Exception {

		final Mapping mapping = this.mappingLoader.load("src/main/resources/model/ClassMapMapping.xmi");
		Assert.assertTrue(mapping != null);
		final String inFileName = URI.createURI("src/test/resources/model2ClassMaps.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final ClassMapXlsxReader reader = new ClassMapXlsxReader(inputStream, new Mapper(new RepositoryImpl(mapping, XlsxPackage.eINSTANCE),new RepositoryLookupStrategy()));
		final List<ClassMap> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());

		final Mapping model2Mapping = XlsxPackage.eINSTANCE.getXlsxFactory().createMapping();

		model2Mapping.setName("Model2ClassMapping");
		model2Mapping.getClassMaps().addAll(classMaps);
		final String uri = "src/test/resources/Model2ClassMapping.xmi";

		this.mappingLoader.save(model2Mapping, uri);

	}
}
