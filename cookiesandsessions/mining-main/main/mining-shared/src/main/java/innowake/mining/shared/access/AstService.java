/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import javax.persistence.EntityNotFoundException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.CachingFunction;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.MapPropertyAccessor;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import innowake.ndt.core.parsing.ast.model.BranchStatement;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.ast.model.statement.FieldReference;

/**
 * Functions for accessing AST related database entities.
 */
public interface AstService {
	
	interface AstNodeInquiryBuilder {
		/**
		 * Filters {@code ast_node} entities by ID.
		 *
		 * @param id the id of the ast node
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder byId(UUID id);
		
		AstNodeInquiryBuilder byIds(Collection<UUID> ids);
		
		/**
		 * Retrieve AST Nodes by IDs in specific order.
		 * @param ids IDs of Nodes.
		 * @return Found Nodes in the same order as the specified IDs unless a different ordering is specified.
		 */
		AstNodeInquiryBuilder joiningIds(List<UUID> ids);
		
		AstNodeInquiryBuilder ofParent(@Nullable UUID id);
		
		/**
		 * Filters {@code ast_node} entities by {@code project} ID.
		 * 
		 * @param project the id of the project
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder ofProject(EntityId project);

		/**
		 * Filters {@code ast_node} entities by {@code module} ID.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder ofModule(EntityId module);

		/**
		 * Filters {@code ast_node} entities by {@code module} IDs.
		 * 
		 * @param modules the ids of the modules
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder ofModules(Collection<UUID> modules);

		AstNodeInquiryBuilder withRelationshipToModule(EntityId module, AstModuleRelationshipType type);
		
		AstNodeInquiryBuilder withRelationshipToModules(Collection<UUID> modules, Collection<AstModuleRelationshipType> types);

		/**
		 * Filters AST nodes by the ID of the Module they were included from.
		 * If the given ID is {@code null}, then the query will find only non-included nodes.
		 * 
		 * @param module the id of the module
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withIncludedModule(@Nullable EntityId module);

		/**
		 * Finds AST nodes with any of the specified types.
		 * @param types Eligible types.
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withTypes(Collection<String> types);
		
		default AstNodeInquiryBuilder withType(final String... types) {
			return withTypes(Arrays.asList(types));
		}
		
		/**
		 * Filters {@code ast_node} entities whose {@code super_type} contains any of the given {@code types}.
		 * 
		 * @param types the super types of the ast node
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withSuperTypes(Collection<String> types);
		
		default AstNodeInquiryBuilder withSuperTypes(final String... types) {
			return withSuperTypes(Arrays.asList(types));
		}

		AstNodeInquiryBuilder withLabel(String label);
		
		default AstNodeInquiryBuilder withLabelContaining(final String label) {
			return withLabel("*" + label + "*");
		}

		/**
		 * Filters {@code ast_node} entities by their {@link AstNodeLocation} retraced offset.
		 *
		 * @param comperator the filter operator
		 * @param offset the offset to filter
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withRetracedOffset(Comperator comperator, int offset);

		/**
		 * Filters {@code ast_node} entities by their {@link AstNodeLocation} retraced end offset: retraced offset + retraced length.
		 *
		 * @param comperator the filter operator
		 * @param offset the end offset to filter
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withRetracedEndOffset(Comperator comperator, int offset);

		/**
		 * Filters {@code ast_node} entities by their {@link AstNodeLocation} assembled offset.
		 *
		 * @param comperator the filter operator
		 * @param offset the offset to filter
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withAssembledOffset(Comperator comperator, int offset);

		/**
		 * Filters {@code ast_node} entities by their {@link AstNodeLocation} assembled end offset: assembled offset + assembled length.
		 *
		 * @param comperator the filter operator
		 * @param offset the end offset to filter
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withAssembledEndOffset(Comperator comperator, int offset);

		/**
		 * Includes AST having at least one of the specified relation types, or any if the respective collection is empty, or none if {@code null} is passed.
		 * @param astRelations Types of relations between AST nodes.
		 * @param moduleRelations Types of telations between AST nodes and modules.
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withRelationshipTypes(
				@Nullable Collection<AstRelationshipType> astRelations,
				@Nullable Collection<AstModuleRelationshipType> moduleRelations);
		
		/**
		 * Filters {@code ast_node} entities by the number of {@code ast_relationship} counts.
		 *
		 * @param type the type of the AST relationship to count
		 * @param direction direction of relationships to be counted
		 * @param comperator the filter operator
		 * @param count the count to filter for
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withRelationshipCount(AstRelationshipType type, RelationshipDirection direction, Comperator comperator, int count);
		
		/**
		 * Filters {@code ast_node} entities by the number of {@code ast_module_relationship} counts.
		 *
		 * @param types the types of the ast module relationships to count
		 * @param comperator the filter operator
		 * @param count the count to filter for
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withModuleRelationshipCount(Collection<AstModuleRelationshipType> types, Comperator comperator, int count);

		/**
		 * Filters {@code ast_node} entities by their parent {@code ast_node}.
		 *
		 * @param parentBuilder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria for the parent node
		 * @param matches whether the parent node should match the criteria or not
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder withParent(BuildingConsumer<AstNodeInquiryBuilder> parentBuilder, boolean matches);

		/**
		 * Specifies a cache for AST nodes.
		 * @param cache Cache implementation.
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder usingCache(CachingFunction<UUID, AstNodePojo> cache);

		/**
		 * Truncates labels on AST nodes if they exceed the specified length. 
		 * @param length Maximum lenth for a label.
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder limitLabelLength(int length);
		
		/**
		 * Sort AST nodes by their offset within a Module.
		 * @param order Sorting direction.
		 * @return this instance for method chaining
		 */
		AstNodeInquiryBuilder sortRetracedLocation();
	}

	interface AstRelationshipInquiryBuilder {
		/**
		 * Filters {@code ast_relationship} entities by id.
		 *
		 * @param id the id of the ast relationship
		 * @return this instance for method chaining
		 */
		AstRelationshipInquiryBuilder byId(UUID id);
		
		/**
		 * Filters {@code ast_relationship} entities by {@code src} id.
		 * 
		 * @param src the id of the source node
		 * @return this instance for method chaining
		 */
		AstRelationshipInquiryBuilder ofSource(UUID src);
		
		/**
		 * Filters {@code ast_relationship} entities by {@code dst} id.
		 * 
		 * @param dst the id of the destination node
		 * @return this instance for method chaining
		 */
		AstRelationshipInquiryBuilder ofDestination(UUID dst);
		
		/**
		 * Filters {@code ast_relationship} entities by {@code type}.
		 * 
		 * @param types the types of the relationship
		 * @return this instance for method chaining
		 */
		AstRelationshipInquiryBuilder withTypes(Collection<AstRelationshipType> types);
		
		default AstRelationshipInquiryBuilder withType(final AstRelationshipType... types) {
			return withTypes(Arrays.asList(types));
		}
		
		AstRelationshipInquiryBuilder withinModule(EntityId module);
	}
	
	interface AstModuleRelationshipInquiryBuilder {
		/**
		 * Filters {@code ast_relationship} entities by {@code src} id.
		 * 
		 * @param src the id of the source node
		 * @return this instance for method chaining
		 */
		AstModuleRelationshipInquiryBuilder ofModule(EntityId src);
		
		/**
		 * Filters {@code ast_relationship} entities by {@code dst} id.
		 * 
		 * @param dst the id of the destination node
		 * @return this instance for method chaining
		 */
		AstModuleRelationshipInquiryBuilder ofNode(UUID dst);
		
		/**
		 * Filters {@code ast_relationship} entities by {@code type}.
		 * 
		 * @param types the types of the relationship
		 * @return this instance for method chaining
		 */
		AstModuleRelationshipInquiryBuilder withTypes(Collection<AstModuleRelationshipType> types);
		
		default AstModuleRelationshipInquiryBuilder withType(final AstModuleRelationshipType... types) {
			return withTypes(Arrays.asList(types));
		}
	}

	public static class Properties {
		private Properties() { }
		
		/** Property for storing field names of {@link FieldDefinition}s */
		public static final MapPropertyAccessor<String> FIELD_NAME = new MapPropertyAccessor<>("name");
		/** Property for storing field names in {@link FieldReference}s */
		public static final MapPropertyAccessor<String> UNRESOLVED_FIELD_NAME = new MapPropertyAccessor<>("unresolvedFieldName");
		/** Property for storing paths of {@link BranchStatement}s to its conditions */
		public static final MapPropertyAccessor<String> CONDITION_PATHS = new MapPropertyAccessor<>("conditionPaths");
		public static final MapPropertyAccessor<String> LANGUAGE_TYPE = new MapPropertyAccessor<>("languageType");
		public static final MapPropertyAccessor<Integer> BYTE_LENGTH = new MapPropertyAccessor<>("byteLength");
		public static final MapPropertyAccessor<String> CONDITION_PATH = new MapPropertyAccessor<>("conditionPaths");
		/** List of numeric module IDs */
		public static final MapPropertyAccessor<List<Number>> FILE_IDS_IN = new MapPropertyAccessor<>("inputFileIds");
		/** List of numeric module IDs */
		public static final MapPropertyAccessor<List<Number>> FILE_IDS_OUT = new MapPropertyAccessor<>("outputFileIds");
	}

	/**
	 * Creates a new {@code ast_node} entity out of the given {@code prototype}.
	 *
	 * @param astNode the {@link AstNodePojoPrototype} to create
	 * @return the id of the created ast node
	 */
	UUID create(AstNodePojoPrototype astNode);

	/**
	 * Updates the {@code module} entity.
	 *
	 * @param astNode the {@link AstNodePojoPrototype} to update
	 * @return the {@link EntityId} of the updated modules
	 */
	UUID update(AstNodePojoPrototype astNode);

	/**
	 * Returns the {@code ast_node} entity for the specified ID.
	 * @param id Unique ID of the {@code ast_node} entity.
	 * @return The {@code ast_node} entity, if found.
	 * @throws EntityNotFoundException If no match was found.
	 */
	AstNodePojo get(UUID id);

	/**
	 * Returns all {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain AstNodePojo AstNodePojos}
	 */
	List<AstNodePojo> find(BuildingConsumer<AstNodeInquiryBuilder> builder);
	
	Optional<AstNodePojo> findOne(BuildingConsumer<AstNodeInquiryBuilder> builder);
	
	Optional<AstNodePojo> findAny(BuildingConsumer<AstNodeInquiryBuilder> builder);

	/**
	 * Returns the ids of all {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain AstNodePojo AstNodePojos}
	 */
	List<UUID> findIds(BuildingConsumer<AstNodeInquiryBuilder> builder);

	/**
	 * Returns the number of {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code ast_node} entities
	 */
	long count(BuildingConsumer<AstNodeInquiryBuilder> builder);

	/**
	 * Deletes all {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code ast_node} entities
	 */
	int delete(BuildingConsumer<AstNodeInquiryBuilder> builder);

	/**
	 * Returns all {@code ast_relationship} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstRelationshipInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain AstRelationshipPojo AstRelationshipPojos}
	 */
	List<AstRelationshipPojo> findRelationships(BuildingConsumer<AstRelationshipInquiryBuilder> builder);
	
	List<AstModuleRelationshipPojo> findModuleRelationships(BuildingConsumer<AstModuleRelationshipInquiryBuilder> builder);
	
	UUID createRelationship(UUID src, UUID dst, AstRelationshipType type);
	
	UUID createRelationship(AstRelationshipPojoPrototype relationship);
	
	void createModuleRelationship(AstModuleRelationshipPojoPrototype relation);
	
	int deleteRelationshipsByModule(EntityId moduleId, Collection<AstRelationshipType> types);
	
	int deleteModuleRelationships(EntityId moduleId, Collection<AstModuleRelationshipType> types);
	
	ControlFlowGraph getControlFlow(EntityId module, @Nullable Integer maxLabelLength);
	
}
