/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.job.identification.AbstractIdentificationTest;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.service.UserRoleService;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Origin.CUSTOM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.BDDMockito.given;

/**
 * Test class for FunctionalBlockServiceImpl with multiple projects.
 */
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false)
@WithMockUser
class FunctionalBlockServiceImplWithMultipleProjectTest extends AbstractIdentificationTest {

	@Autowired
	private AnnotationToFunctionalBlockService annotationToFunctionalBlockService;

	@Autowired
	private FunctionalBlockService functionalBlockService;

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private JobManager jobManager;

	@Nullable
	@MockBean
	private UserRoleService userRoleService;

	private ModulePojo moduleAOnProj1;
	private ModulePojo moduleBOnProj1;
	private ModulePojo moduleCOnProj1;
	private ModulePojo moduleAOnProj2;
	private ModulePojo moduleBOnProj2;
	private ModulePojo moduleCOnProj2;

	@BeforeAll
	void init() {
		moduleAOnProj1 = createModule(PROJECT_ID_1, "MMRS7101", "MMRS7101.cbl");
		moduleBOnProj1 = createModule(PROJECT_ID_1, "CBACT04C", "CBACT04C.cbl");
		moduleCOnProj1 = createModule(PROJECT_ID_1, "MMRS7102", "MMRS7102.cbl");
		moduleAOnProj2 = createModule(PROJECT_ID_2, "CBACT04C", "CBACT04C.cbl");
		moduleBOnProj2 = createModule(PROJECT_ID_2, "MMRS7101", "MMRS7101.cbl");
		moduleCOnProj2 = createModule(PROJECT_ID_2, "MMRS7102", "MMRS7102.cbl");
		setupProjectAccesses(Arrays.asList(PROJECT_ID_1.getNid(), PROJECT_ID_2.getNid()), Arrays.asList(PROJECT_ID_1.getNid(), PROJECT_ID_2.getNid()));

		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "CBACT04C.cbl");
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7101.cbl");
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7102.cbl");
		submitIdentifyCandidatesJob(PROJECT_ID_2, RESOURCE_PATH + "CBACT04C.cbl");
		submitIdentifyCandidatesJob(PROJECT_ID_2, RESOURCE_PATH + "MMRS7101.cbl");
		submitIdentifyCandidatesJob(PROJECT_ID_2, RESOURCE_PATH + "MMRS7102.cbl");
	}

	@Test
	void testCreateFbOnMultipleProjects() {
		/* Create Functional Group on annotations of moduleAOnProj1 of PROJECT_ID_1 */
		final List<AnnotationPojo> annotations1 = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleAOnProj1.identity()));
		final List<AnnotationPojo> validationRules1 = annotations1.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Validation Rule")).toList();
		final List<EntityId> validationRuleIds1 = validationRules1.stream().map(AnnotationPojo :: identity).toList();
		final Map<Long, UUID> validations1 = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1, validationRuleIds1);

		FunctionalBlockPojoPrototype functionalBlockPojoPrototypeA = createFunctionalBlockPojoPrototype("FB1", "FB1", validations1.values().stream().toList(),
				PROJECT_ID_1, moduleAOnProj1);
		UUID createdFBonProj1 = functionalBlockService.create(functionalBlockPojoPrototypeA);
		assertNotNull(createdFBonProj1);

		/* create Functional Group on annotations of moduleAOnProj2 of PROJECT_ID_2 */
		final List<AnnotationPojo> annotations2 = annotationService.find(q -> q.ofProject(PROJECT_ID_2).ofModule(moduleAOnProj2.identity()));
		final List<AnnotationPojo> validationRules2 = annotations2.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Validation Rule")).toList();
		final List<EntityId> validationRuleIds2 = validationRules2.stream().map(AnnotationPojo :: identity).toList();
		final Map<Long, UUID> validations2 = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_2, validationRuleIds2);

		FunctionalBlockPojoPrototype functionalBlockPojoPrototypeB = createFunctionalBlockPojoPrototype("FB1", "FB1", validations2.values().stream().toList(),
				PROJECT_ID_2, moduleAOnProj2);
		UUID createdFBonProj2 = functionalBlockService.create(functionalBlockPojoPrototypeB);
		assertNotNull(createdFBonProj2);

		/* Verify the created Functional Groups */
		FunctionalBlockPojo functionalBlockPojoA = functionalBlockService.get(Collections.singletonList(createdFBonProj1)).get(0);
		FunctionalBlockPojo functionalBlockPojoB = functionalBlockService.get(Collections.singletonList(createdFBonProj2)).get(0);

		assertEquals(functionalBlockPojoPrototypeA.name.get(), functionalBlockPojoA.getName());
		assertEquals(functionalBlockPojoPrototypeA.description.get(), functionalBlockPojoA.getDescription());

		assertEquals(functionalBlockPojoPrototypeB.name.get(), functionalBlockPojoB.getName());
		assertEquals(functionalBlockPojoPrototypeB.description.get(), functionalBlockPojoB.getDescription());
	}

	@Test
	void testUpdateFbOnMultipleProjects() {
		/* create Functional Group on annotations of moduleBOnProj1 of PROJECT_ID_1 */
		final List<AnnotationPojo> annotationPojos1 = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleBOnProj1.identity()));
		final List<EntityId> annotationIds1 = annotationPojos1.stream().map(AnnotationPojo :: identity).toList();
		final Map<Long, UUID> annotations1 = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1, annotationIds1);

		FunctionalBlockPojoPrototype functionalBlockPojoPrototypeA = createFunctionalBlockPojoPrototype("FB2", "FB2", annotations1.values().stream().toList(),
				PROJECT_ID_1, moduleBOnProj1);
		UUID createdFBonProj1 = functionalBlockService.create(functionalBlockPojoPrototypeA);
		assertNotNull(createdFBonProj1);

		/* create Functional Group on annotations of moduleBOnProj2 of PROJECT_ID_2 */
		final List<AnnotationPojo> annotationPojos2 = annotationService.find(q -> q.ofProject(PROJECT_ID_2).ofModule(moduleBOnProj2.identity()));
		final List<EntityId> annotationIds2 = annotationPojos2.stream().map(AnnotationPojo :: identity).toList();
		final Map<Long, UUID> annotations2 = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_2, annotationIds2);

		FunctionalBlockPojoPrototype functionalBlockPojoPrototypeB = createFunctionalBlockPojoPrototype("FB2", "FB2", annotations2.values().stream().toList(),
				PROJECT_ID_2, moduleBOnProj2);
		UUID createdFBonProj2 = functionalBlockService.create(functionalBlockPojoPrototypeB);
		assertNotNull(createdFBonProj2);

		/* Update the createdFBonProj2*/
		functionalBlockPojoPrototypeB = createFunctionalBlockPojoPrototype("FB2", "FB2-Updated", annotations2.values().stream().toList(), PROJECT_ID_2,
				moduleBOnProj2);
		functionalBlockPojoPrototypeB.setUid(createdFBonProj2);
		functionalBlockService.update(functionalBlockPojoPrototypeB);

		/* Verify the updated Functional Group */
		FunctionalBlockPojo functionalBlockPojoB = functionalBlockService.get(Collections.singletonList(createdFBonProj2)).get(0);
		assertEquals(functionalBlockPojoPrototypeB.name.get(), functionalBlockPojoB.getName());
		assertEquals(functionalBlockPojoPrototypeB.description.get(), functionalBlockPojoB.getDescription());

		/* Update the createdFBonProj1*/
		functionalBlockPojoPrototypeA = createFunctionalBlockPojoPrototype("FB2", "FB2-Updated", annotations1.values().stream().toList(), PROJECT_ID_1,
				moduleBOnProj1);
		functionalBlockPojoPrototypeA.setUid(createdFBonProj1);
		functionalBlockService.update(functionalBlockPojoPrototypeA);

		/* Verify the updated Functional Group */
		FunctionalBlockPojo functionalBlockPojoA = functionalBlockService.get(Collections.singletonList(createdFBonProj1)).get(0);
		assertEquals(functionalBlockPojoPrototypeA.name.get(), functionalBlockPojoA.getName());
		assertEquals(functionalBlockPojoPrototypeA.description.get(), functionalBlockPojoA.getDescription());
	}

	@Test
	void testDeleteFbOnMultipleProjects() {
		/* create Functional Group on annotations of moduleCOnProj1 of PROJECT_ID_1 */
		final List<AnnotationPojo> annotationPojos1 = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleCOnProj1.identity()));
		final List<EntityId> annotationIds1 = annotationPojos1.stream().map(AnnotationPojo :: identity).toList();
		final Map<Long, UUID> annotations1 = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1, annotationIds1);

		FunctionalBlockPojoPrototype functionalBlockPojoPrototypeA = createFunctionalBlockPojoPrototype("FB3", "FB3", annotations1.values().stream().toList(),
				PROJECT_ID_1, moduleCOnProj1);
		UUID createdFBonProj1 = functionalBlockService.create(functionalBlockPojoPrototypeA);
		assertNotNull(createdFBonProj1);

		/* create Functional Group on annotations of moduleCOnProj2 of PROJECT_ID_2 */
		final List<AnnotationPojo> annotationPojos2 = annotationService.find(q -> q.ofProject(PROJECT_ID_2).ofModule(moduleCOnProj2.identity()));
		final List<EntityId> annotationIds2 = annotationPojos2.stream().map(AnnotationPojo :: identity).toList();
		final Map<Long, UUID> annotations2 = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_2, annotationIds2);

		FunctionalBlockPojoPrototype functionalBlockPojoPrototypeB = createFunctionalBlockPojoPrototype("FB3", "FB3", annotations2.values().stream().toList(),
				PROJECT_ID_2, moduleCOnProj2);
		UUID createdFBonProj2 = functionalBlockService.create(functionalBlockPojoPrototypeB);
		assertNotNull(createdFBonProj2);

		/* Delete the createdFBonProj2*/
		functionalBlockService.delete(Collections.singletonList(createdFBonProj2));

		/* Verify the deleted Functional Group */
		List<FunctionalBlockPojo> functionalBlockPojos = functionalBlockService.get(Collections.singletonList(createdFBonProj2));
		assertEquals(0, functionalBlockPojos.size());

		/* Delete the createdFBonProj1*/
		functionalBlockService.delete(Collections.singletonList(createdFBonProj1));

		/* Verify the deleted Functional Group */
		functionalBlockPojos = functionalBlockService.get(Collections.singletonList(createdFBonProj1));
		assertEquals(0, functionalBlockPojos.size());
	}

	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final String name, final String desc, final List<UUID> childs,
			final EntityId project, final ModulePojo module) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(project);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(desc);
		final List<ModulePart> moduleParts = new ArrayList<>();
		final ModulePart functionalBlockModulePart = new ModulePart(module.getLinkHash(), null);
		moduleParts.add(functionalBlockModulePart);
		functionalBlockPojoPrototype.setModuleParts(moduleParts);
		if ( childs != null ) {
			functionalBlockPojoPrototype.setChildren(childs);
		}
		return functionalBlockPojoPrototype;
	}

	private ModulePojo createModule(final EntityId projectId, final String name, final String file) {
		final String content = getContent(file, AbstractIdentificationTest.RESOURCE_PATH);
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(name);
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(IDENTIFIED);
		module.setOrigin(CUSTOM);
		module.setPath(AbstractIdentificationTest.RESOURCE_PATH + file);
		module.setContent(content);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.getModule(moduleService.create(module));
	}

	private void submitIdentifyCandidatesJob(final EntityId projectId, final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Collections.singletonList(path))));
	}

	private void setupProjectAccesses(final List<Long> authProjectIds, final List<Long> userRoleProjectIds) {
		final UserRoleService userRoleService = Assert.assertNotNull(this.userRoleService);
		given(userRoleService.getProjectIds()).willReturn(userRoleProjectIds);

		final List<SimpleGrantedAuthority> authorities = new ArrayList<>(authProjectIds.size() * 2);
		authProjectIds.forEach(projectId -> {
			authorities.add(new SimpleGrantedAuthority(String.format("client-1-project-%d-viewer", projectId)));
			authorities.add(new SimpleGrantedAuthority(String.format("client-1-project-%d-mining", projectId)));
		});

		final Authentication auth = new UsernamePasswordAuthenticationToken("", "", authorities);
		final SecurityContext context = SecurityContextHolder.createEmptyContext();
		context.setAuthentication(auth);
		SecurityContextHolder.setContext(context);
	}
}

