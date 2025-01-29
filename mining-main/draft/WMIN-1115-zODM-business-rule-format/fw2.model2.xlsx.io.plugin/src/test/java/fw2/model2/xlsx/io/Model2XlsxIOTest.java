package fw2.model2.xlsx.io;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import fw2.model2.Application;
import fw2.model2.ClassOrm;
import fw2.model2.Model;
import fw2.model2.Model2Package;
import fw2.model2.ModelElement;
import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.Mapping;
import fw2.orm.xlsx.XlsxPackage;
import fw2.orm.xlsx.io.ClassMapXlsxReader;
import fw2.orm.xlsx.io.MappingLoader;
import fw2.orm.xlsx.io.RepositoryImpl;
import fw2.orm.xlsx.io.mapper.Mapper;
import fw2.orm.xlsx.io.mapper.RepositoryLookupStrategy;

public class Model2XlsxIOTest {

	MappingLoader mappingLoader;

	ModelLoader modelLoader;

	@Before
	public void init() {

		this.mappingLoader = new MappingLoader();
		this.modelLoader = new ModelLoader();
	}
	
	@Test
	public void testConvertACNVClassMappingXlsx() throws Exception {

		final Mapping model2Mapping = this.mappingLoader.load("src/main/resources/model/Model2ClassMapping.xmi");
		Assert.assertTrue(model2Mapping != null);

		final String inFileName = URI.createURI("src/test/resources/ACNVClassMaps.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final Mapper mapper = new Mapper(new RepositoryImpl(model2Mapping, Model2Package.eINSTANCE),new RepositoryLookupStrategy());
		final ClassOrmXlsxReader reader = new ClassOrmXlsxReader(inputStream, mapper);
		final List<ClassOrm> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());

		final Model nomadsDataModel = Model2Package.eINSTANCE.getModel2Factory().createModel();

		nomadsDataModel.setName("NOMADS Data Model");
		final Map<String, List<?>> lookupMap = mapper.getRepository().getLookupMap();

		for (final String root : lookupMap.keySet()) {
			final Model model = Model2Package.eINSTANCE.getModel2Factory().createModel();
			model.setName(root + "Model");
			model.getElements().addAll((Collection<? extends ModelElement>) lookupMap.get(root));
			nomadsDataModel.getElements().add(model);
		}

		final String uri = "src/test/resources/ACNVClassMaps.xmi";

		this.modelLoader.save(nomadsDataModel, uri);

	}


	@Test
	public void testConvertNOAMDSClassMappingXlsx() throws Exception {

		final Mapping model2Mapping = this.mappingLoader.load("src/main/resources/model/Model2ClassMapping.xmi");
		Assert.assertTrue(model2Mapping != null);

		final String inFileName = URI.createURI("src/test/resources/NOMADSClassMaps v2.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final Mapper mapper = new Mapper(new RepositoryImpl(model2Mapping, Model2Package.eINSTANCE),new RepositoryLookupStrategy());
		final ClassOrmXlsxReader reader = new ClassOrmXlsxReader(inputStream, mapper);
		final List<ClassOrm> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());

		final Model nomadsDataModel = Model2Package.eINSTANCE.getModel2Factory().createModel();

		nomadsDataModel.setName("NOMADS Data Model");
		final Map<String, List<?>> lookupMap = mapper.getRepository().getLookupMap();

		for (final String root : lookupMap.keySet()) {
			final Model model = Model2Package.eINSTANCE.getModel2Factory().createModel();
			model.setName(root + "Model");
			model.getElements().addAll((Collection<? extends ModelElement>) lookupMap.get(root));
			nomadsDataModel.getElements().add(model);
		}

		final String uri = "src/test/resources/NOMADSClassMaps.xmi";

		this.modelLoader.save(nomadsDataModel, uri);

	}

	@Test
	public void testReadApplicationClassMappingXlsx() throws Exception {

		final Mapping model2Mapping = this.mappingLoader.load("src/main/resources/model/ApplicationClassMaps.xmi");
		Assert.assertTrue(model2Mapping != null);

		final String inFileName = URI.createURI("src/test/resources/Application.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final Mapper mapper = new Mapper(new RepositoryImpl(model2Mapping, Model2Package.eINSTANCE),new RepositoryLookupStrategy());
		final ApplicationXlsxXReader reader = new ApplicationXlsxXReader(inputStream, mapper);
		final List<Application> applications = reader.readObjects();
		Assert.assertTrue(applications != null);
		Assert.assertTrue(!applications.isEmpty());

		final Model applicationModel = Model2Package.eINSTANCE.getModel2Factory().createModel();

		applicationModel.setName("Application Model");
		applicationModel.getElements().addAll(applications);
		final String uri = "src/test/resources/Application.xmi";

		this.modelLoader.save(applicationModel, uri);

	}

	@Test
	public void testReadApplicationMappingDetailClassMappingXlsx() throws Exception {

		final Mapping model2Mapping = this.mappingLoader.load("src/main/resources/model/Application Mapping Details ClassMaps.xmi");
		Assert.assertTrue(model2Mapping != null);

		final String inFileName = URI.createURI("src/test/resources/Application Mapping Details.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final Mapper mapper = new Mapper(new RepositoryImpl(model2Mapping, Model2Package.eINSTANCE),new RepositoryLookupStrategy());
		final ApplicationXlsxXReader reader = new ApplicationXlsxXReader(inputStream, mapper);
		final List<Application> applications = reader.readObjects();
		Assert.assertTrue(applications != null);
		Assert.assertTrue(!applications.isEmpty());

		final Model applicationModel = Model2Package.eINSTANCE.getModel2Factory().createModel();

		applicationModel.setName("Application Model");
		applicationModel.getElements().addAll(applications);

		final String uri = "src/test/resources/Application Mapping Details.xmi";
		this.modelLoader.save(applicationModel, uri);

	}

	@Test
	public void testReadModel2ClassMappingXlsx() throws Exception {

		final Mapping mapping = this.mappingLoader.load("src/main/resources/model/ClassMapMapping.xmi");
		Assert.assertTrue(mapping != null);
		final String inFileName = URI.createURI("src/main/resources/model/model2ClassMaps.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final Mapper mapper = new Mapper(new RepositoryImpl(mapping, XlsxPackage.eINSTANCE),new RepositoryLookupStrategy());
		final ClassMapXlsxReader reader = new ClassMapXlsxReader(inputStream, mapper);
		final List<ClassMap> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());
	}

	@Test
	public void testReadNOAMDSClassMappingXlsx() throws Exception {

		final Mapping model2Mapping = this.mappingLoader.load("src/main/resources/model/Model2ClassMapping.xmi");
		Assert.assertTrue(model2Mapping != null);

		final String inFileName = URI.createURI("src/test/resources/NOMADSClassMaps v2.xlsx").toFileString();
		final InputStream inputStream = new FileInputStream(inFileName);
		final Mapper mapper = new Mapper(new RepositoryImpl(model2Mapping, Model2Package.eINSTANCE),new RepositoryLookupStrategy());
		final ClassOrmXlsxReader reader = new ClassOrmXlsxReader(inputStream, mapper);
		final List<ClassOrm> classMaps = reader.readObjects();
		Assert.assertTrue(classMaps != null);
		Assert.assertTrue(!classMaps.isEmpty());

		final Model nomadsDataModel = Model2Package.eINSTANCE.getModel2Factory().createModel();

		nomadsDataModel.setName("NOMADS Data Model");
		final Map<String, List<?>> lookupMap = mapper.getRepository().getLookupMap();

		for (final String root : lookupMap.keySet()) {
			final Model model = Model2Package.eINSTANCE.getModel2Factory().createModel();
			model.setName(root + "Model");
			model.getElements().addAll((Collection<? extends ModelElement>) lookupMap.get(root));
			nomadsDataModel.getElements().add(model);
		}
		Assert.assertTrue(!nomadsDataModel.getElements().isEmpty());

	}
}
