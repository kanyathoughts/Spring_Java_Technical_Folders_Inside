/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.TreeMap;
import java.util.UUID;

import innowake.mining.shared.access.SortDirection;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.stereotype.Controller;

import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyTypePojo;

/**
 * Controller for the "taxonomyCategories" query. 
 */
@Controller
public class TaxonomyCategoriesGraphQlController implements MiningDataPointSource {

	public static class AggregationResult {
		private final TreeMap<Long, CategoryAggregate> categories;
		
		private AggregationResult() {
			categories = new TreeMap<>();
		}

		/**
		 * This method has to be called for each category before the other put methods,
		 * as the other put's won't find the category otherwise!
		 * @param category to create a CategoryAggregate for
		 */
		public void put(final TaxonomyCategoryPojo category) {
			categories.put(category.getId(), new CategoryAggregate(category.getId(), category.getName()));
		}
		
		public void put(final TaxonomyTypePojo type) {
			categories.get(type.getCategory().getId()).put(type);
		}
		
		public void put(final TaxonomyPojo taxonomy, final long assignments) {
			categories.get(taxonomy.getType().getCategory().getId()).put(taxonomy, assignments);
		}
		
		public void putCategory(final Long categoryId, final long assignments) {
			categories.get(categoryId).setDistinctAssignments(assignments);
		}
		
		public void putType(final TaxonomyTypePojo type, final long assignments) {
			categories.get(type.getCategory().getId()).putAssignments(type, assignments);
		}
		
		public Collection<CategoryAggregate> getCategories() {
			return categories.values();
		}
	}
	
	public static class CategoryAggregate {
		private final Long id;
		private final String name;
		private final TreeMap<String, TypeAggregate> types;
		private long distinctAssignments;
		
		private CategoryAggregate(final Long id, final String name) {
			this.id = id;
			this.name = name;
			types = new TreeMap<>();
		}
		
		public void put(final TaxonomyTypePojo type) {
			types.put(type.getName(), new TypeAggregate(type.getId(), type.getName()));
		}
		
		public void put(final TaxonomyPojo taxonomy, final long assignments) {
			types.computeIfAbsent(taxonomy.getType().getName(), name -> new TypeAggregate(taxonomy.getUid(), name)).put(taxonomy, assignments);
		}

		public void putAssignments(final TaxonomyTypePojo type, final long assignments) {
			types.get(type.getName()).setDistinctAssignments(assignments);
		}
		
		public Collection<TypeAggregate> getTypes() {
			return types.values();
		}
		
		public Long getId() {
			return id;
		}
		
		public String getName() {
			return name;
		}
		
		public void setDistinctAssignments(final long assignments) {
			this.distinctAssignments = assignments;
		}
		
		public long getDistinctAssignments() {
			return distinctAssignments;
		}
	}
	
	public static class TypeAggregate {
		private final UUID id;
		private final String name;
		private final List<TermAggregate> terms;
		private long distinctAssignments;
		
		private TypeAggregate(final UUID id, final String name) {
			this.id = id;
			this.name = name;
			this.terms = new ArrayList<>();
		}
		
		public void put(final TaxonomyPojo taxonomy, final long assignments) {
			terms.add(new TermAggregate(taxonomy.getId(), taxonomy.getName(), assignments));
		}
		
		public List<TermAggregate> getTerms() {
			return Collections.unmodifiableList(terms);
		}
		
		public String getName() {
			return name;
		}

		public UUID getId() {
			return id;
		}
		
		public void setDistinctAssignments(final long assignments) {
			this.distinctAssignments = assignments;
		}
		
		public long getDistinctAssignments() {
			return distinctAssignments;
		}
	}
	
	public static class TermAggregate {
		private final Long id;
		private final String name;
		private final long assignments;
		
		public TermAggregate(final Long id, final String name, final long assignments) {
			super();
			this.id = id;
			this.name = name;
			this.assignments = assignments;
		}
		
		public Long getId() {
			return id;
		}
		
		public String getName() {
			return name;
		}
		
		public long getAssignments() {
			return assignments;
		}
	}
	
	private final TaxonomyService taxonomyService;

	public TaxonomyCategoriesGraphQlController(@Autowired final TaxonomyService taxonomyService) {
		this.taxonomyService = taxonomyService;
	}
	
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
		public AggregationResult taxonomyCategories(@Argument final EntityId projectId) {
		final AggregationResult result = new AggregationResult();
		taxonomyService.findCategories(q -> q.ofProject(projectId)).forEach(result::put);
		taxonomyService.countModulesPerCategory(q -> q.ofProject(projectId)).forEach(result::putCategory);
		taxonomyService.findTypes(q -> q.ofProject(projectId).sortName(SortDirection.ASCENDING)).forEach(result::put);
		taxonomyService.countModulesPerType(q -> q.ofProject(projectId)).forEach(result::putType);
		taxonomyService.find(q -> q.ofProject(projectId)).forEach(t -> result.put(t, t.getTaxonomyReferenceCount()));
		return result;
	}
	
	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		builder.defineDataPointsFromSchemaMappingAnnotations(this);
	}

}
