/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.pl1;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JobControl;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.parsing.parser.dependency.ast.pl1.AbstractPl1ProcedureNode;
import innowake.ndt.parsing.parser.dependency.ast.pl1.NamedVariableDeclaration;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.HashSetValuedHashMap;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Examines a target PL/1 program, finds DCL FILE statements and traces the file accesses back to the actual RESOURCE_FILE module.
 */
public class Pl1ReadWriteAccessAnalyzer {

	private static final Collection<Tuple2<Technology, Type>> RESOURCE_TYPES = List.of(Tuple2.of(Technology.RESOURCE, Type.FILE),
			Tuple2.of(Technology.RESOURCE, Type.VSAM_FILE), Tuple2.of(Technology.RESOURCE, Type.GDG_FILE));

	private final Pl1DependencyUtility<AbstractPl1ProcedureNode> pl1DependencyUtility;
	private final ModuleService moduleService;
	private final CallChainService callChainService;

	public Pl1ReadWriteAccessAnalyzer(final Pl1DependencyUtility<AbstractPl1ProcedureNode> pl1DependencyUtility, final ModuleService moduleService,
			final CallChainService callChainService) {
		this.pl1DependencyUtility = pl1DependencyUtility;
		this.moduleService = moduleService;
		this.callChainService = callChainService;
	}

	/**
	 * Creates transitive file dependencies.
	 *
	 * @param context the current discovery context
	 * @param module the lightweight module
	 * @param moduleBuilder the module builder to which dependencies should be added
	 * @param parseResult the ast model of the current source object
	 */
	public void createFileDependencies(final DiscoveryContext context, final ModuleLightweightPojo module,
			final ModuleBuilder moduleBuilder, final AstModel parseResult) {

		/* collect all DD assignments from all steps that somehow call this program (directly or transitively) */
		final var ddToDsnMap = getDDtoDSNMapForModule(module.identity(), context.getProjectId());
		final var subModules = moduleService.findRelationship(q -> q.ofModuleInDirection(module.getUid(), RelationshipDirection.OUT)
				.withType(RelationshipType.CONTAINS)).stream().map(ModuleRelationshipPojo::getDstModuleDetails).filter(Optional::isPresent)
				.map(moduleBasePojo -> moduleBasePojo.get().identity())
				.collect(Collectors.toList());
		subModules.forEach(id -> ddToDsnMap.putAll(getDDtoDSNMapForModule(id, context.getProjectId())));

		collectFileDeclarations(parseResult).stream()
				.flatMap(fileDeclaration -> {
					if ( ! ddToDsnMap.containsKey(fileDeclaration.getDataDefinitionName())) {
						/* DSN not resolved */
						return Stream.of(fileDeclaration);
					}
					return ddToDsnMap.get(fileDeclaration.getDataDefinitionName()).stream().map(dataSetName -> {
						/* there may be multiple DSN assignments for this file declaration - duplicate the file declaration object for each */
						final var copiedDeclaration = new FileDeclaration(fileDeclaration);
						copiedDeclaration.setDataSetName(dataSetName);
						return copiedDeclaration;
					});
				})
				/* Only create for FileDeclaration if we either read or write the file - this excludes "VARIABLE" file declarations */
				.filter(fileDeclaration -> fileDeclaration.isReads() || fileDeclaration.isWrites())
				.forEach(fileDeclaration -> declareFileDependencies(fileDeclaration, moduleBuilder));
	}

	private List<FileDeclaration> collectFileDeclarations(final AstModel parseResult) {
		final var root = parseResult.getRoot();
		return root.map(astNode -> astNode.getChildrenDeep(NamedVariableDeclaration.class,
				dcl -> Pl1ParseResultProvider.isNamedDeclarationOfType((NamedVariableDeclaration) dcl, "FILE")).stream().map(dcl -> {
			final var fileDeclaration = new FileDeclaration(dcl, dcl.getVariableName());
			fileDeclaration.setReads(dcl.getDataFormatTypes().contains("INPUT") || dcl.getDataFormatTypes().contains("UPDATE"));
			fileDeclaration.setWrites(dcl.getDataFormatTypes().contains("OUTPUT") || dcl.getDataFormatTypes().contains("UPDATE"));
			return fileDeclaration;
		}).collect(Collectors.toList())).orElse(Collections.emptyList());
	}

	private void declareFileDependencies(final FileDeclaration fileDeclaration, final ModuleBuilder moduleBuilder) {
			final var dataSetName = fileDeclaration.getDataSetName();
			if (dataSetName == null) {
				moduleBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY,
						"Unable to resolve file " + fileDeclaration.getDataDefinitionName() + " to actual data set.");
			} else {
				final var attributes = new ModelAttributeMap<>();
				final List<Object> accessTypes = new ArrayList<>();
				if (fileDeclaration.isReads()) {
					accessTypes.add(ModelAttributeValue.FileAccess.READ);
				}
				if (fileDeclaration.isWrites()) {
					accessTypes.add(ModelAttributeValue.FileAccess.WRITE);
				}
				attributes.put(ModelAttributeKey.FILE_ACCESS_TYPE, accessTypes);

				pl1DependencyUtility.createModelDependencies(dataSetName, fileDeclaration.getDcl(),
				findTopLevelParentProcedure(fileDeclaration.getDcl()), Binding.LATE, RelationshipType.ACCESSES, true, attributes, true,
						ModuleType.RESOURCE_FILE, ModuleType.RESOURCE_VSAM_FILE, ModuleType.RESOURCE_GDG_FILE);
			}
	}

	@Nullable
	AbstractPl1ProcedureNode findTopLevelParentProcedure(final AstNode node) {
		AbstractPl1ProcedureNode parentProcedure = node.getGenericType(AbstractPl1ProcedureNode.class);
		for (AstNode currentNode = node; currentNode != null; currentNode = currentNode.getNullableParent()) {
			final AbstractPl1ProcedureNode maybeProcedure = currentNode.getGenericType(AbstractPl1ProcedureNode.class);
			if (maybeProcedure != null) {
				parentProcedure = maybeProcedure;
			}
		}
		return parentProcedure;
	}

	private MultiValuedMap<String, String> getDDtoDSNMapForModule(final EntityId id, final EntityId projectId) {
		final Set<EntityId> callingSteps = new HashSet<>();

		final Optional<List<CallChainGraph>> callChains = callChainService.createCallChainGraphs(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(projectId)
				.setStartModuleIds(Collections.singletonList(id))
				.setCallTypes(Collections.singleton(RelationshipType.CALLS))
				.setDirections(Collections.singletonList(CallChain.CallChainDirection.IN))
				.setEndModuleTypes(Collections.singleton(Type.EXEC_PGM))
				.setParallel(1)
				.build());

		callChains.ifPresent(callChainGraphs ->
				callChainGraphs.stream()
						.flatMap(graph -> graph.getEndModules(endModule -> Type.EXEC_PGM == endModule.getType()).stream())
						.forEach(endModule -> callingSteps.add(endModule.identity())));

		final MultiValuedMap<String, String> ddToDsnMap = new HashSetValuedHashMap<>();

		callingSteps.stream()
				.flatMap(stepModuleId -> moduleService.findRelationship(q -> q.ofSource(stepModuleId)).stream())
				.forEach(dep -> {
					final Optional<ModuleLightweightPojo> called = moduleService.findAnyModuleLightweight(q -> q.byUid(dep.getDstModule())
							.withTechnologiesAndTypes(RESOURCE_TYPES));
					if (called.isPresent()) {
						final var properties = dep.getProperties();
						if (properties.isPresent()) {
							@SuppressWarnings("unchecked")
							final List<Map<String, String>> propertiesList = (List<Map<String, String>>) properties.get().get("PROPERTIES");
							ddToDsnMap.put(propertiesList.get(0).get(JobControl.ID_NAME), called.get().getName());
						}
					}
				});

		return ddToDsnMap;
	}

	/**
	 * Model class encapsulating the information related to file declarations.
	 */
	private static final class FileDeclaration {
		private final AstNode dcl;
		private final String dataDefinitionName;
		@Nullable
		private String dataSetName;
		private boolean reads;
		private boolean writes;

		public FileDeclaration(final AstNode dcl, final String dataDefinitionName) {
			this.dcl = dcl;
			this.dataDefinitionName = dataDefinitionName;
		}

		private FileDeclaration(final FileDeclaration other) {
			dcl = other.dcl;
			dataDefinitionName = other.dataDefinitionName;
			dataSetName = other.dataSetName;
			reads = other.reads;
			writes = other.writes;
		}

		/**
		 * Gets the resolved name of the actual data set.
		 *
		 * @return the resolved name of the actual data set
		 */
		@Nullable
		public String getDataSetName() {
			return dataSetName;
		}

		/**
		 * Sets the resolved name of the actual data set.
		 *
		 * @param dataSetName the resolved name of the actual data set
		 */
		public void setDataSetName(@Nullable final String dataSetName) {
			this.dataSetName = dataSetName;
		}

		/**
		 * Returns whether the program reads from the file.
		 *
		 * @return whether the program reads from the file
		 */
		public boolean isReads() {
			return reads;
		}

		/**
		 * Sets whether the program reads from the file.
		 *
		 * @param reads whether the program reads from the file
		 */
		public void setReads(final boolean reads) {
			this.reads = reads;
		}

		/**
		 * Returns whether the program writes to the file.
		 *
		 * @return whether the program writes to the file
		 */
		public boolean isWrites() {
			return writes;
		}

		/**
		 * Sets whether the program writes to the file.
		 *
		 * @param writes whether the program writes to the file
		 */
		public void setWrites(final boolean writes) {
			this.writes = writes;
		}

		/**
		 * Gets the AstNode containing the file declaration statement inside the program.
		 *
		 * @return the AstNode containing the file declaration statement inside the program
		 */
		public AstNode getDcl() {
			return dcl;
		}

		/**
		 * Gets the symbolic name of the file, used inside the program and in the DD statement in JCL.
		 *
		 * @return the symbolic name of the file, used inside the program and in the DD statement in JCL
		 */
		public String getDataDefinitionName() {
			return dataDefinitionName;
		}
	}
}
