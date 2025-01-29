package innowake.mining.test;

import static org.junit.Assert.assertEquals;

import java.util.UUID;

import org.junit.Test;

import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test for the {@link ModulePojo} Technology and Type.
 */
public class ModuleTechnologyTypeTest {

	/**
	 * Tests for the correct mapping of C Modules.
	 */
	@Test
	public void cMapping() {
		assertMapping("C", "C_PROGRAM", ExpectedValues.of(Technology.C, Type.PROGRAM, Storage.FILE, RelationshipType.CALLS));
		assertMapping("C", "C_HEADER", ExpectedValues.of(Technology.C, Type.HEADER, Storage.FILE, RelationshipType.INCLUDES));
		assertMapping("C", "C_FUNCTION", ExpectedValues.of(Technology.C, Type.FUNCTION, Storage.FILE_SECTION, RelationshipType.CALLS));
	}
	
	/**
	 * Tests for the correct mapping of Resource Modules.
	 */
	@Test
	public void resourceMapping() {
		assertMapping("RESOURCE", "FILE", ExpectedValues.of(Technology.RESOURCE, Type.FILE, Storage.FILE, RelationshipType.ACCESSES));
		assertMapping("RESOURCE", "LISTCAT", ExpectedValues.of(Technology.RESOURCE, Type.LISTCAT, Storage.FILE, RelationshipType.ACCESSES));
		assertMapping("RESOURCE", "RESOURCE_GDG_FILE", ExpectedValues.of(Technology.RESOURCE, Type.GDG_FILE, Storage.FILE, RelationshipType.ACCESSES));
		assertMapping("RESOURCE", "TPFDF_DATASET", ExpectedValues.of(Technology.RESOURCE, Type.TPFDF_DATASET, Storage.FILE, RelationshipType.ACCESSES));
	}
	
	/**
	 * Tests for the correct mapping of IMS Modules.
	 */
	@Test
	public void imsMapping() {
		assertMapping("IMS", "TDFXTRCT", ExpectedValues.of(Technology.IMS, Type.TDFXTRCT, Storage.FILE, RelationshipType.REFERENCES));
	}
	
	/**
	 * Tests for the correct mapping of Oracle Modules.
	 */
	@Test
	public void oracleMapping() {
		assertMapping("ORACLE", "CDO_FILE", ExpectedValues.of(Technology.ORACLE, Type.CDO_FILE, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("ORACLE", "CDO_RECORD", ExpectedValues.of(Technology.ORACLE, Type.CDO_RECORD, Storage.FILE_SECTION, RelationshipType.REFERENCES));
		assertMapping("ORACLE", "SQLMOD", ExpectedValues.of(Technology.ORACLE, Type.SQLMOD, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("ORACLE", "SQLMOD_PROC", ExpectedValues.of(Technology.ORACLE, Type.SQLMOD_PROCEDURE, Storage.FILE_SECTION, RelationshipType.REFERENCES));
	}
	
	/**
	 * Tests for the correct mapping of VMS Modules.
	 */
	@Test
	public void vmsMapping() {
		assertMapping("VMS", "DCL", ExpectedValues.of(Technology.VMS, Type.DCL, Storage.FILE, RelationshipType.CALLS));
		assertMapping("VMS", "FMS_FORM", ExpectedValues.of(Technology.VMS, Type.FMS_FORM, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("VMS", "IFDL_FORM", ExpectedValues.of(Technology.VMS, Type.IFDL_FORM, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("VMS", "VAX_MACRO", ExpectedValues.of(Technology.VMS, Type.VAX_MACRO, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("VMS", "VAX_MACRO_ENTRY", ExpectedValues.of(Technology.VMS, Type.VAX_MACRO_ENTRY, Storage.FILE_SECTION, RelationshipType.CALLS));
	}
	
	/**
	 * Tests for the correct mapping of BASIC Modules.
	 */
	@Test
	public void basicMapping() {
		assertMapping("BASIC", "BASIC_OBJECT", ExpectedValues.of(Technology.BASIC, Type.OBJECT, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("BASIC", "BASIC_PROGRAM", ExpectedValues.of(Technology.BASIC, Type.PROGRAM, Storage.FILE_SECTION, RelationshipType.CALLS));
		assertMapping("BASIC", "BASIC_FUNCTION", ExpectedValues.of(Technology.BASIC, Type.FUNCTION, Storage.FILE_SECTION, RelationshipType.CALLS));
		assertMapping("BASIC", "BASIC_SUBROUTINE", ExpectedValues.of(Technology.BASIC, Type.SUBROUTINE, Storage.FILE_SECTION, RelationshipType.CALLS));
	}
	
	/**
	 * Tests for the correct mapping of SQL Scripts.
	 */
	@Test
	public void sqlMapping() {
		assertMapping("SQL", "SQL_SCRIPT", ExpectedValues.of(Technology.SQL, Type.SCRIPT, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("SQL", "SQL_STORED_PROCEDURE", ExpectedValues.of(Technology.SQL, Type.STORED_PROCEDURE, Storage.FILE_SECTION, RelationshipType.CALLS));	
		assertMapping("SQL", "SQL_TABLE", ExpectedValues.of(Technology.SQL, Type.TABLE, Storage.DATABASE, RelationshipType.ACCESSES));
		assertMapping("SQL", "SQL_VIEW", ExpectedValues.of(Technology.SQL, Type.VIEW, Storage.DATABASE, RelationshipType.ACCESSES));
	}
	
	@Test
	public void javaMapping() {
		assertMapping("JAVA", "COMPILATION_UNIT", ExpectedValues.of(Technology.JAVA, Type.COMPILATION_UNIT, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("JAVA", "PACKAGE", ExpectedValues.of(Technology.JAVA, Type.PACKAGE, Storage.FILE_SECTION, RelationshipType.REFERENCES));
		assertMapping("JAVA", "ENUM", ExpectedValues.of(Technology.JAVA, Type.ENUM, Storage.FILE_SECTION, RelationshipType.REFERENCES));
		assertMapping("JAVA", "TYPE", ExpectedValues.of(Technology.JAVA, Type.TYPE, Storage.FILE_SECTION, RelationshipType.REFERENCES));
		assertMapping("JAVA", "ANNOTATION", ExpectedValues.of(Technology.JAVA, Type.ANNOTATION, Storage.FILE_SECTION, RelationshipType.REFERENCES));
		assertMapping("JAVA", "INTERFACE", ExpectedValues.of(Technology.JAVA, Type.INTERFACE, Storage.FILE_SECTION, RelationshipType.REFERENCES));
	}

	@Test
	public void cicsMapping() {
		assertMapping("CICS", "CICS_TDQ", ExpectedValues.of(Technology.CICS, Type.TDQ, Storage.QUEUE, RelationshipType.ACCESSES));
		assertMapping("CICS", "CICS_TSQ", ExpectedValues.of(Technology.CICS, Type.TSQ, Storage.QUEUE, RelationshipType.ACCESSES));
	}
	
	/**
	 * Tests for the correct mapping of C++ Modules.
	 */
	@Test
	public void cppMapping() {
		assertMapping("CPP", "CPP_PROGRAM", ExpectedValues.of(Technology.CPP, Type.PROGRAM, Storage.FILE, RelationshipType.CALLS));
		assertMapping("CPP", "CPP_HEADER", ExpectedValues.of(Technology.CPP, Type.HEADER, Storage.FILE, RelationshipType.INCLUDES));
	}

	/**
	 * Tests for the correct mapping of C# Modules.
	 */
	@Test
	public void cSharpMapping() {
		assertMapping("CSHARP", "CSHARP_COMPILATION_UNIT", ExpectedValues.of(Technology.CSHARP, Type.COMPILATION_UNIT, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("CSHARP", "CSHARP_PROJECT", ExpectedValues.of(Technology.CSHARP, Type.PROJECT, Storage.FILE, RelationshipType.REFERENCES));
		assertMapping("CSHARP", "CSHARP_SOLUTION", ExpectedValues.of(Technology.CSHARP, Type.SOLUTION, Storage.FILE, RelationshipType.REFERENCES));
	}
	
	private void assertMapping(final String discoveryLanguage, final String discoveryType, final ExpectedValues expectedValues) {
		final ModulePojo module = new ModulePojo(
				UUID.randomUUID(),
				Long.valueOf(1),
				CustomPropertiesMap.empty(),
				EntityId.of(Long.valueOf(1)), null, null,
				"name",
				"path",
				Technology.fromName(discoveryLanguage),
				Type.fromName(discoveryType),
				Storage.from(Technology.fromName(discoveryLanguage), Type.fromName(discoveryType)),
				Origin.CUSTOM,
				Creator.API,
				Identification.IDENTIFIED,
				null,
				"description",
				null,
				null,
				"link Hash",
				null,
				null,
				false,
				null,
				null,
				null,
				null,
				0,
				0,
				0,
				false,
				null, null, null,
				null, null);

		assertEquals(expectedValues.technology, module.getTechnology());
		assertEquals(expectedValues.type, module.getType());
		assertEquals(expectedValues.storage, module.getStorage());
		assertEquals(expectedValues.referenceType, RelationshipType.from(module.getTechnology(), module.getType()));
	
	}
	
	private static class ExpectedValues {
		private final Technology technology;
		private final Type type;
		private final Storage storage;
		private final RelationshipType referenceType;
		
		private ExpectedValues(final Technology technology, final Type type, final Storage storage, final RelationshipType referenceType) {
			this.technology = technology;
			this.type = type;
			this.storage = storage;
			this.referenceType = referenceType;
		}
		
		private static ExpectedValues of(final Technology technology, final Type type, final Storage storage, final RelationshipType referenceType) {
			return new ExpectedValues(technology, type, storage, referenceType);
		}
	}
}
