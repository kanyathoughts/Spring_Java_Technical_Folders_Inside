/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.mining.data.error.ConstraintViolationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.lang.Assert;
import innowake.mining.shared.PatternConverter;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.TaxonomyReport;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;

/**
 * Connects the Taxonomies with other Entities
 */
@Service
public class TaxonomyModelService {

	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private TaxonomyService taxonomyService;

	public List<TaxonomyReport> findReports(final BuildingConsumer<TaxonomyService.TaxonomyInquiryBuilder> builder) {
		final Map<EntityId, List<TaxonomyPojo>> mappings = taxonomyService.findTaxonomiesPerModule(builder);
		final List<TaxonomyReport> result = new ArrayList<>(mappings.size());
		
		final List<ModulePojo> modules = moduleService.findModules(q -> q.byIds(mappings.keySet()));
		
		for (final ModulePojo module : modules) {
			final List<TaxonomyPojo> taxonomies = mappings.get(module.identity());
			result.add(new TaxonomyReport(module, taxonomies));
		}
		
		return result;
	}

	@SuppressWarnings({ "null", "unused" })
	public void updateAssignments(final EntityId projectId, final TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest) {
		final var moduleMatcher = taxonomyAssignmentsSetRequest.getModules();
		final List<EntityId> assignmentsToRemove = new LinkedList<>();
		final List<EntityId> assignmentsToAdd = new LinkedList<>();
		for (final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignment : taxonomyAssignmentsSetRequest.getTaxonomies()) {
			final AssignmentState state = assignment.getState();
			/* taxonomy id can be null if not contained in service request object */
			if (assignment.getTaxonomyId() == null) {
				throw new IllegalArgumentException("TaxonomyId must not be null in TaxonomyAssignmentsSetRequest.TaxonomySetAssignment");
			}
			
			if (state == AssignmentState.ALL) {
				assignmentsToAdd.add(assignment.getTaxonomyId());
			} else if (state == AssignmentState.NONE) {
				assignmentsToRemove.add(assignment.getTaxonomyId());
			} else if (state == AssignmentState.SOME) {
				throw new ConstraintViolationException(assignment, "Invalid assignment state : " + state);
			}
		}

		final var pathPatterns = moduleMatcher.getPathPatterns().stream()
				.map(PatternConverter::convertAntToRegexPattern).collect(Collectors.toList());
		final List<EntityId> matchingModuleIds = new ArrayList<>();
		matchingModuleIds.addAll(moduleService.findModuleIds(q -> q.ofProject(projectId)
				.withPathPatternsSelfOrContaining(projectId, pathPatterns)));
		matchingModuleIds.addAll(moduleService.findModuleIds(q -> q.ofProject(projectId)
				.byIds(moduleMatcher.getIds())));

		taxonomyService.deleteModuleLinks(q -> q.ofModules(matchingModuleIds).byIds(assignmentsToRemove).ofProject(projectId));
		taxonomyService.createModuleLinks(matchingModuleIds, assignmentsToAdd);
	}

	/** overwrite Project and category ID when using this list! */
	private static final Map<TechnicalTaxonomies.TypeName, List<TechnicalTaxonomies.Name>> TECHNICAL_TAXONOMY_TYPES = new HashMap<>();
	static {
		TECHNICAL_TAXONOMY_TYPES.put(TypeName.PROGRAM_TYPE,
				Arrays.asList(TechnicalTaxonomies.Name.BATCH, TechnicalTaxonomies.Name.LIBRARY, TechnicalTaxonomies.Name.UI, TechnicalTaxonomies.Name.MQ));
		TECHNICAL_TAXONOMY_TYPES.put(TypeName.FILE_ACCESS, Arrays.asList(TechnicalTaxonomies.Name.READ, TechnicalTaxonomies.Name.WRITE));
		TECHNICAL_TAXONOMY_TYPES.put(TypeName.DB_ACCESS,
				Arrays.asList(TechnicalTaxonomies.Name.READ, TechnicalTaxonomies.Name.DELETE, TechnicalTaxonomies.Name.STORE, TechnicalTaxonomies.Name.UPDATE));
		TECHNICAL_TAXONOMY_TYPES.put(TypeName.UTILITY, 
				Arrays.asList(TechnicalTaxonomies.Name.COMMON_INTERFACE, TechnicalTaxonomies.Name.COMMON_LOGGING, TechnicalTaxonomies.Name.COMMON_TRANSACTION));

	}

	public void createTechnicalTaxonomies(final ProjectPojo project) {
		//create technical taxonomy default types
		
		for (final Entry<TypeName, List<Name>> entry : TECHNICAL_TAXONOMY_TYPES.entrySet()) {
			final EntityId projectId = project.identity();
			final TaxonomyTypePojoPrototype typeProto = new TaxonomyTypePojoPrototype()
				.setName(entry.getKey().getDisplayName())
				.setCategoryId(Assert.assertNotNull(project.getTechnicalTaxonomyCategoryId()))
				.setProject(projectId);
			final UUID protoUid = taxonomyService.createType(typeProto);
			
			for (final TechnicalTaxonomies.Name name : entry.getValue()) {
				final TaxonomyPojoPrototype taxProto = new TaxonomyPojoPrototype()
							.setType(protoUid)
							.setName(name.getDisplayName())
							.setProject(projectId);
				taxonomyService.create(taxProto);
			}
		}
	}
	
	
}
