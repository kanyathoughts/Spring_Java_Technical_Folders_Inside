package innowake.mining.server.discovery.categorize.sql;

import static innowake.mining.shared.discovery.categorize.Identification.ID.NO;
import static innowake.mining.shared.model.Technology.UNKNOWN;
import static innowake.mining.shared.model.discovery.ResolveTarget.NONE;
import static org.apache.commons.lang3.math.NumberUtils.LONG_ONE;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * JUnit test for Sql categorization
 */
public class SqlScriptCategorizeTest {

	private static final Path BASE_FOLDER = Paths.get("./test-resources/innowake/mining/server/discovery");
	protected static final Path SOURCE_FOLDER = BASE_FOLDER.resolve("source");
	
	private void compare(final Path path) throws IOException {
		final BinaryString content = new BinaryString(Files.readAllBytes(path));
		final ModuleType moduleType = FileExtension.resolve(path.toString());
		final SourcePojo sourceObject = SourcePojoDummy.build(o -> o
				.setNid(LONG_ONE)
				.setProject(EntityId.of(LONG_ONE))
				.setName(path.getFileName().toString())
				.setPath(path.toString())
				.setTechnology(moduleType.getTechnology())
				.setType(moduleType.getType())
				.setContent(content));
		final SQLScriptCategorizer categorizer = new SQLScriptCategorizer();
		final Identification identification = categorizer.identifyByContent(sourceObject);

		assertNotNull("Identification must not be null", identification);
		assertEquals(ResolveTarget.SQL_SCRIPT, identification.getTarget(), "File must be evaluated as SQL script");
	}

	@Test
	public void testFilteredSource() {
		final BinaryString content = new BinaryString(new StringBuilder()
				.append("/* ********************************************************************* */")
				.append("/* Nat-Name : BXCGN-DP")
				.append(" * Funktion : BAG, allgemein: Define Printer")
				.append(" * Erstellt : 01.01.2006 Heckmann")
				.append(" * Copyright: GiP mbH - Frankfurt / Main")
				.append(" * ")
				.append(" * Änderungen:")
				.append(" * ----------")
				.append(" * 01 tt.mm.jjjj xx AVS00000 Grund")
				.append(" * *********************************************************************")
				.append(" * ")
				.append(" * *$* IGNORE /* * &COCO-NAT 1 gbl GenJavaCode")
				.append(" * ")
				.append(" * ENDE BXCGN-DP */")
				.toString());

		final SourcePojo so = SourcePojoDummy.build(o -> o
				.setNid(LONG_ONE)
				.setProject(EntityId.of(LONG_ONE))
				.setName("BXCGN_CE.jcopy")
				.setPath("p")
				.setTechnology(UNKNOWN)
				.setType(Type.UNKNOWN)
				.setContent(content));
		final SQLScriptCategorizer categorizer = new SQLScriptCategorizer();
		
		try {
			final Identification identification = categorizer.identifyByContent(so);
			assertNotNull("Identification must not be null", identification);
			assertEquals(NONE, identification.getTarget(), "Target of identification must be NONE when evaluating a JCopy file with PLSQL parser.");
			assertEquals(NO, identification.getId(), "Id of identification must be NO when evaluating a JCopy file with PLSQL parser.");
		} catch (final ArrayIndexOutOfBoundsException exception) {
			exception.printStackTrace();
			fail("PLSQL content analysis for JCopy must not throw an ArrayIndexOutOfBoundsException", exception);
		}
	}
	
	@TestFactory
	public Collection<DynamicTest> patternTest() {
		try (final Stream<Path> paths = Files.walk(SOURCE_FOLDER.resolve("WDIS501"), 1)) {
			final List<Path> filePaths = paths.filter(Files::isRegularFile).collect(Collectors.toList());
			final Collection<DynamicTest> dynamicTests = new ArrayList<>();
			for (final Path path : filePaths) {
				final DynamicTest dt = DynamicTest.dynamicTest(path.getFileName().toString(),()-> compare(path));
				dynamicTests.add(dt);
			}
			paths.close();
			return dynamicTests;
		} catch (final IOException e) {
			e.printStackTrace();
			return fail("Fail to identify the dynamic tests. Exception occured : " + e.getMessage(), e);
		}
	}
}
