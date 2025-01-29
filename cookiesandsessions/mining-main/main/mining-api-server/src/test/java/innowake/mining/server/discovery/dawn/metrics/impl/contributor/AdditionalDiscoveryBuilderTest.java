/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.contributor;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.mining.shared.model.AstNodeLocation;
import org.junit.jupiter.api.Test;

import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Additional Tests for {@link DiscoveryBuilderImpl}.
 */
class AdditionalDiscoveryBuilderTest {

	private static final String FAKE_CLASS_NAME = "innowake.mining.server.discovery.dawn.metrics.contributors.TestContributor";
	private final DiscoveryBuilderImpl builder = new DiscoveryBuilderImpl(FAKE_CLASS_NAME);
	private final DiscoveryBuilder.ModuleBuilder moduleBuilder = builder.declareExternalModule("TESTMODULE", ModuleType.COBOL_PROGRAM);
	
	/**
	 * Tests to construct contributor results by declaring statements on the module.
	 */
	@Test
    void testDeclareStatement() {        
        moduleBuilder.declareStatement(StatementType.CALL).setText("CALL FOO");
        final ContributorResult result = buildContributorResult();
        final List<StatementPojoPrototype> statements = result.getStatements().stream().collect(Collectors.toList());
        assertEquals(1, statements.size());
        assertEquals(StatementType.CALL, statements.get(0).type.orElse(null));
        assertEquals("CALL FOO", statements.get(0).text.orElse(null));
    }
	
	/**
	 * Tests to construct contributor results by declaring outgoing dependency on the module.
	 */
	@Test
	void testDeclareDependency() {  
        /* using ModuleFilter as target */
        moduleBuilder.declareDependency(RelationshipType.INCLUDES, new ModuleFilter().setNames("CALL FOO").setTypes(ModuleType.COBOL_PROGRAM));      
        /* using ModuleBuilder as target */
        moduleBuilder.declareDependency(RelationshipType.CALLS, moduleBuilder);      
        final ContributorResult result = buildContributorResult();
        final List<DependencyDefinitionPojoPrototype> dependencies = result.getDependencies().stream().collect(Collectors.toList());
        assertEquals(2, dependencies.size());        
        assertEquals(RelationshipType.INCLUDES, dependencies.get(0).type.get());
        assertEquals(Collections.singleton("CALL FOO"), dependencies.get(0).moduleFilters.getNonNull().get(0).getNames());
        assertEquals(Collections.singleton(ModuleType.COBOL_PROGRAM), dependencies.get(0).moduleFilters.getNonNull().get(0).getTypes());
        assertEquals(RelationshipType.CALLS, dependencies.get(1).type.get());

	}
	
	/**
	 * Tests to construct contributor results by defer an action that is run on the module.
	 */
	@Test
	void testDeferAction() {
        moduleBuilder.deferAction("CALL FOO");        
        final ContributorResult result = buildContributorResult();
        final List<DeferredActionDefinition> deferredActions = result.getDeferrredActions().stream().collect(Collectors.toList());
        assertEquals(1, deferredActions.size());       
        final DeferredActionDefinition deferredAction = deferredActions.get(0);
        assertEquals("CALL FOO", deferredAction.getName());
	}
	
	/**
	 * Tests to construct contributor results by marking a region of the module as "dead code".
	 */
	@Test
	void testAddDeadCode() {     
        moduleBuilder.addDeadCode("CALL FOO", 5, 10);      
        final ContributorResult result = buildContributorResult();
        final List<ModuleDeadCodePojoPrototype> deadCodes = result.getDeadCodes().stream().collect(Collectors.toList());
        assertEquals(1, deadCodes.size());       
        assertEquals("CALL FOO", deadCodes.get(0).deadCode.get());
        assertEquals(Integer.valueOf(5), deadCodes.get(0).startingLine.get());
        assertEquals(Integer.valueOf(10), deadCodes.get(0).numberOfLines.get());
	}
	
	/**
	 * Tests to construct contributor results by adding an error marker to the module.
	 */
	@Test
	void testAddError() {      
        moduleBuilder.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "ERROR WHILE PARSING", new AstNodeLocation(5, 10,5, 10, 5, 10, -1, -1));
        final Throwable throwable = new Throwable("400 EMPTY FILE");
        moduleBuilder.addError(Severity.ERROR, ErrorKey.EMPTY_FILE, "EMPTY FILE" , throwable);      
        final ContributorResult result = buildContributorResult();
        final List<ErrorMarker> errors = result.getErrors().stream().collect(Collectors.toList());
        assertEquals(2, errors.size());
        errors.stream().forEach(error -> assertEquals(Severity.ERROR, error.getSeverity()));               
        assertEquals(ErrorKey.PARSE_ERROR, errors.get(0).getKey());
        assertEquals("ERROR WHILE PARSING", errors.get(0).getCause());
		assertTrue(assertNotNull(errors.get(0).getModuleLocation()).getAssembledOffset().isPresent());
        assertEquals(5, assertNotNull(errors.get(0).getModuleLocation()).getAssembledOffset().get());
		assertTrue(assertNotNull(errors.get(0).getModuleLocation()).getAssembledLength().isPresent());
        assertEquals(10, assertNotNull(errors.get(0).getModuleLocation()).getAssembledLength().get());
        assertEquals("EMPTY FILE", errors.get(1).getCause());
        assertEquals(ErrorKey.EMPTY_FILE, errors.get(1).getKey());
	}
	
	/**
	 * Tests to construct contributor results by attaching additional information to the module.
	 */
	@Test
	void testAddAdditionalInfo() {       
		/* addAdditionalInfo on ModuleBuilder */
		final SourceMetrics moduleBuilderAdditionalInfo = new SourceMetrics();
		moduleBuilderAdditionalInfo.setModuleId(UUID.randomUUID());
		moduleBuilder.addAdditionalInfo(moduleBuilderAdditionalInfo);

		final ContributorResult result = buildContributorResult();
		final var moduleBuilderAdditionalInfos = result.getModuleDefinition().sourceMetrics.getNonNull();
		assertEquals(moduleBuilderAdditionalInfo.getModuleId(), moduleBuilderAdditionalInfos.module.getNonNull().getUid());
		
	}
	
	private ContributorResult buildContributorResult() {
		final List<ContributorResult> results = builder.buildResults();
        assertEquals(1, results.size());
        checkExternalModule(results.get(0));
        return results.get(0);
	}
	
	private void checkExternalModule(final ContributorResult result) {
        assertEquals(ContributorResult.Type.EXTERNAL_MODULE, result.getType());
        assertEquals(Technology.COBOL, result.getModuleDefinition().technology.get());
        assertEquals(Type.PROGRAM, result.getModuleDefinition().type.get());
        assertEquals("TESTMODULE", result.getModuleDefinition().name.get());
	}

}
