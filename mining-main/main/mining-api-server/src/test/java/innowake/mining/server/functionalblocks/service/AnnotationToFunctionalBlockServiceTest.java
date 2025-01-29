package innowake.mining.server.functionalblocks.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.functionalblocks.job.FunctionalBlockComputationJob;
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
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.service.UserRoleService;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Origin.CUSTOM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.BDDMockito.given;

/**
 * Tests for the {@link AnnotationToFunctionalBlockService}.
 */
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false)
@WithMockUser
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class AnnotationToFunctionalBlockServiceTest extends AbstractIdentificationTest {

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

	@BeforeAll
	void init() {
		moduleAOnProj1 = createModule(PROJECT_ID_1, "MMRS7101", "MMRS7101.cbl");
		setupProjectAccesses(Arrays.asList(PROJECT_ID_1.getNid(), PROJECT_ID_2.getNid()), Arrays.asList(PROJECT_ID_1.getNid(), PROJECT_ID_2.getNid()));
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7101.cbl");
	}

	@Test
	@Order(1)
	void testGetFunctionalUnitsForAnnotations() {
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleAOnProj1.identity()));
		final List<AnnotationPojo> validationRules = annotations.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Validation Rule")).toList();
		assertEquals(2, validationRules.size());
		final List<EntityId> validationIds = validationRules.stream().map(AnnotationPojo :: identity).toList();
		assertEquals(2, validationIds.size());

		final Map<Long, UUID> functionalUnits = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1, validationIds);
		assertEquals(2, functionalUnits.size());

		validationRules.forEach(annotation -> {
			final UUID functionalUnitId = functionalUnits.get(annotation.getId());
			assertNotNull(functionalUnitId);
			final FunctionalBlockPojo functionalBlock = functionalBlockService.find(q -> q.ofProject(PROJECT_ID_1).byUid(functionalUnitId)).get(0);
			assertNotNull(functionalBlock);
			assertEquals(annotation.getName(), functionalBlock.getName());
			assertEquals(annotation.getName(), functionalBlock.getDescription());
		});
	}

	@Test
	@Order(2)
	void testGetFunctionalGroupsForAnnotations() {
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleAOnProj1.identity()));
		final List<AnnotationPojo> validationRules = annotations.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Validation Rule")).toList();
		assertEquals(2, validationRules.size());
		final List<EntityId> validationIds = validationRules.stream().map(AnnotationPojo :: identity).toList();
		assertEquals(2, validationIds.size());

		final Map<Long, UUID> functionalUnits = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1, validationIds);
		assertEquals(2, functionalUnits.size());

		final List<UUID> functionalUnitIds = new ArrayList<>(functionalUnits.values());
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] { "FUNCTIONAL_GROUP"
		});
		FunctionalBlockPojoPrototype functionalBlockPojoPrototypeA = createFunctionalBlockPojoPrototype("FB1", "FB1", functionalUnitIds, moduleAOnProj1,
				jsonMap);
		UUID createdFBonProj1 = functionalBlockService.create(functionalBlockPojoPrototypeA);
		assertNotNull(createdFBonProj1);

		submitFunctionalBlockComputation(new HashSet<>(Collections.singleton(createdFBonProj1)));

		final Map<Long, List<FunctionalBlockPojo>> functionalGroups = annotationToFunctionalBlockService.getFunctionalGroupsForAnnotations(PROJECT_ID_1,
				validationIds);

		assertEquals(2, functionalGroups.size());
		validationRules.forEach(annotation -> {
			final List<FunctionalBlockPojo> functionalGroup = functionalGroups.get(annotation.getId());
			assertNotNull(functionalGroup);
			assertEquals(1, functionalGroup.size());
			assertEquals("FB1", functionalGroup.get(0).getName());
			assertEquals("FB1", functionalGroup.get(0).getDescription());
		});
	}

	@Test
	@Order(3)
	void testGetFunctionalBlockNamesByAnnotationId() {
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleAOnProj1.identity()));
		final List<AnnotationPojo> validationRules = annotations.stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Field Computation Rule")).toList();
		assertEquals(1, validationRules.size());
		final List<EntityId> validationIds = validationRules.stream().map(AnnotationPojo :: identity).toList();
		assertEquals(1, validationIds.size());

		final Map<Long, UUID> functionalUnits = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(PROJECT_ID_1, validationIds);
		assertEquals(1, functionalUnits.size());

		final List<UUID> functionalUnitIds = new ArrayList<>(functionalUnits.values());
		final Map<String, Object> jsonMap = new HashMap<>();
		jsonMap.put("TYPE", new String[] { "FUNCTIONAL_GROUP"
		});
		FunctionalBlockPojoPrototype functionalBlockPojoPrototypeA = createFunctionalBlockPojoPrototype("FB1", "FB1", functionalUnitIds, moduleAOnProj1,
				jsonMap);
		UUID createdFBonProj1 = functionalBlockService.create(functionalBlockPojoPrototypeA);
		assertNotNull(createdFBonProj1);

		submitFunctionalBlockComputation(new HashSet<>(Collections.singleton(createdFBonProj1)));

		final Set<String> functionalBlockNames = annotationToFunctionalBlockService.getFunctionalBlockNamesByAnnotationId(validationRules.get(0).getId());
		assertEquals(1, functionalBlockNames.size());
		assertTrue(functionalBlockNames.contains("FB1"));
	}

	@Test
	@Order(4)
	void testGetAnnotationForFunctionalUnits() {
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleAOnProj1.identity()));

		List<FunctionalBlockPojo> blocks = functionalBlockService.find(q -> q.ofProject(PROJECT_ID_1).withType(FunctionalBlockType.FUNCTIONAL_UNIT));

		List<UUID> functionalUnitIds = blocks.stream().map(FunctionalBlockPojo :: getUid).toList();

		final Map<UUID, AnnotationPojo> annotationsForFunctionalUnits = annotationToFunctionalBlockService.getAnnotationForFunctionalUnits(PROJECT_ID_1,
				functionalUnitIds);
		assertEquals(3, annotationsForFunctionalUnits.size());

		List<AnnotationPojo> validationAnnotations = annotationsForFunctionalUnits.values().stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Validation Rule")).toList();
		assertEquals(2, validationAnnotations.size());

		List<AnnotationPojo> fieldComputationAnnotations = annotationsForFunctionalUnits.values().stream()
				.filter(annotation -> annotation.getCategoryName().orElse("").equals("Field Computation Rule")).toList();
		assertEquals(1, fieldComputationAnnotations.size());

		Map<UUID, AnnotationPojo> annotationUidAndAnnotationMap = annotationsForFunctionalUnits.values().stream()
				.collect(Collectors.toMap(AnnotationPojo :: getUid, Function.identity()));

		annotations.forEach(annotation -> {
			AnnotationPojo annotationPojo = annotationUidAndAnnotationMap.get(annotation.getUid());
			assertNotNull(annotationPojo);
			assertEquals(annotation.getName(), annotationPojo.getName());
			assertEquals(annotation.getProject(), annotationPojo.getProject());
			assertEquals(annotation.getType(), annotationPojo.getType());
		});
	}

	private FunctionalBlockPojoPrototype createFunctionalBlockPojoPrototype(final String name, final String desc, final List<UUID> children,
			final ModulePojo module, final Map<String, Object> flags) {
		final FunctionalBlockPojoPrototype functionalBlockPojoPrototype = new FunctionalBlockPojoPrototype();
		functionalBlockPojoPrototype.setProject(AbstractIdentificationTest.PROJECT_ID_1);
		functionalBlockPojoPrototype.setName(name);
		functionalBlockPojoPrototype.setDescription(desc);
		functionalBlockPojoPrototype.setFlags(flags);
		final List<ModulePart> moduleParts = new ArrayList<>();
		final ModulePart functionalBlockModulePart = new ModulePart(module.getLinkHash(), null);
		moduleParts.add(functionalBlockModulePart);
		functionalBlockPojoPrototype.setModuleParts(moduleParts);
		if ( children != null ) {
			functionalBlockPojoPrototype.setChildren(children);
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

	private void submitFunctionalBlockComputation(Set<UUID> uuids) {
		submitJob(jobManager, new FunctionalBlockComputationJob(uuids));
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
