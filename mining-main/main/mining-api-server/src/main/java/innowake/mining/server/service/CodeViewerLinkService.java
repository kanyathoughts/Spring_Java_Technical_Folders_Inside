/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.OParseResult;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.api.Model;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.codeviewer.CodeViewerLink;
import innowake.mining.shared.model.codeviewer.CodeViewerLinkModel;
import innowake.mining.shared.model.codeviewer.CodeViewerRange;
import innowake.mining.shared.model.codeviewer.LinkTargetType;
import innowake.mining.shared.model.codeviewer.LinkType;
import innowake.ndt.core.parsing.IDocument;
import innowake.ndt.core.parsing.ast.AdvancedLocationProvider;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.RetracedLocation;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Service for calculating the {@link CodeViewerLinkModel} for a given module.
 */
@Service
public class CodeViewerLinkService {

    private static final Logger LOG = LoggerFactory.getLogger(CodeViewerLinkService.class);

    @Autowired
    private AstService astService;

	@Autowired
    private MiningDataCoreService core;

	/**
	 * Calculate the {@link CodeViewerLinkModel} for a given module
	 * @param projectId the project id
	 * @param moduleId the module id
	 * @param assembled whether to generate the link model for the assembled content of the module
	 * @return the link model
	 */
	public CodeViewerLinkModel getLinkModel(final EntityId projectId, final EntityId moduleId, final boolean assembled) {
		final List<CodeViewerLink> links = new ArrayList<>();
        final Optional<AstModel> model =
                assembled ? getNdtAstModel(moduleId) : Optional.empty();
        final IDocument document = getDocument(projectId, moduleId, model);

		final Set<EntityId> moduleIds = new HashSet<>();
		moduleIds.add(moduleId);
		if (assembled) {
			moduleIds.addAll(core.moduleService.findModuleIds(q -> q.withSourceRelationshipsFrom(moduleId, RelationshipType.CONTAINS)));
		}

		for (final var currentModuleId : moduleIds) {
			links.addAll(getLinksFromAstBindings(currentModuleId, document, model));
			links.addAll(getLinksFromControlFlow(currentModuleId, document, model));
		}

		/* collect dependencies also from contained sub-modules. */
		moduleIds.addAll(core.moduleService.findModuleIds(q -> q.withSourceRelationshipsFrom(moduleId, RelationshipType.CONTAINS)));
		/* remove duplicates (same code location references same module multiple times) */
		final Set<Pair<Integer, Long>> offsetToModuleSet = new HashSet<>();
		for (final var currentModuleId : moduleIds) {
			links.addAll(getLinksFromDependencies(projectId, currentModuleId, document, model, offsetToModuleSet));
		}

		return new CodeViewerLinkModel(links);
	}

	/**
	 * Translates from an unassembled location to a assembled location
	 * 
	 * @param moduleId The id of the module
	 * @param location The location to translate
	 * @param model The AstModel
	 * @return Returns an Optional of the found translated location
	 */
	public Optional<ModuleLocation> getAssembledLocation(final EntityId moduleId, @Nullable final ModuleLocation location, final Optional<AstModel> model) {
		if (location == null) {
			return Optional.empty();
		}
		if (model.isEmpty()) {
			return Optional.of(location);
		}

		final Optional<AstNode> root = model.get().getRoot();
		if (root.isEmpty()) {
			return Optional.of(location);
		}
		final AdvancedLocationProvider<ModulePojo> advancedLocationProvider = model.get().getAdvancedLocationProvider(ModulePojo.class);
		if (advancedLocationProvider == null) {
			return Optional.of(location);
		}
		final Optional<AstNode> matchingNode = root.get().getChildrenDeep(AstNode.class, n -> {
			final int startOffset;
			try {
				startOffset = n.getStartOffset();
			} catch (final Exception e) {
				/* some "artificial" nodes (Cobol parser only, I think) throw an exception when calling getStartOffset() -.-
				 * we can ignore those nodes and the Exception */
				return false;
			}
			if (startOffset < 0) {
				/* whatever this is, it is not a real AST node, so ignore it */
				return false;
			}
			 RetracedLocation<ModulePojo> retracedLocation;
				try {
					retracedLocation = advancedLocationProvider.getRetracedLocation(n);
				} catch (final Exception e) {
					LOG.warn(() -> String.format("storeAst: Unable to get location for node %s. Original error was: %s", n, e));
					return false;
				}
				if (retracedLocation == null) {
					return false;
				}
			return location.getOffset().equals(Integer.valueOf(retracedLocation.getOffset()))
					&& retracedLocation.getPhyiscalModule().identity().equals(moduleId);
		}).stream().findFirst();
		return matchingNode.map(n -> new ModuleLocation(n.getStartOffset(), location.getLength().intValue()));
	}

	private IDocument getDocument(final EntityId projectId, final EntityId moduleId, final Optional<AstModel> model) {
		if (model.isPresent()) {
			return new Document(model.get().getSource());
		}
		final var content = core.sourceService.findAnyContent(q -> q.ofProject(projectId).withModule(moduleId)).orElseThrow();
		return new Document(assertNotNull(content.toString()));
	}

	/* currently, NDT AstModel is required in order to calculate assembled locations (until WMIN-5241 is implemented) */
	private Optional<AstModel> getNdtAstModel(final EntityId moduleId) {
		final Optional<OParseResult> parseResult = core.getParseResult(moduleId);
		return parseResult.flatMap(OParseResult::getModel).map(Model::getParseModel);
	}

	private List<CodeViewerLink> getLinksFromAstBindings(final EntityId moduleId, final IDocument document, final Optional<AstModel> model) {
		final List<CodeViewerLink> links = new ArrayList<>();

		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(moduleId));
		for (final AstNodePojo astNode : astNodes) {
			astNode.getOutgoingRelations().stream().filter(r -> r.getType() == AstRelationshipType.BINDING).forEach(astBinding -> {
				final EntityId sourceModuleId = getEffectiveModuleId(astNode);
				final AstNodePojo targetNode = astBinding.getDstNode();
				final EntityId targetModuleId = getEffectiveModuleId(targetNode);
				if (model.isEmpty() && ( ! sourceModuleId.equals(moduleId) || ! targetModuleId.equals(moduleId))) {
					/* when no assembling information is present (i.e. looking at the unassembled source code), then we ignore links
					 * from or to nodes that come from other Modules */
					return;
				}
				final Optional<ModuleLocation> location = getAssembledLocation(sourceModuleId, astNode.getLocation().convertToSharedModuleLocation(), model);
				final Optional<ModuleLocation> targetLocation = getAssembledLocation(targetModuleId,
						targetNode.getLocation().convertToSharedModuleLocation(),
						model);
				if (location.isEmpty() || targetLocation.isEmpty()) {
					LOG.warn(() -> String.format("Unable to calculate assembled location for CodeViewerLink on %s (AstBinding to %s) omitting link.",
							astNode.getLabel(), targetNode.getLabel()));
					return;
				}
				links.add(new CodeViewerLink(
						LinkType.AST_BINDING,
						LinkTargetType.LOCAL,
						StringUtils.trimToEmpty(astBinding.getLabel().orElse(null)),
						targetNode.getLabel(),
						moduleId,
						location.get(),
						convertEditorRange(document, location.get()),
						moduleId,
						targetLocation.get(),
						convertEditorRange(document, targetLocation.get())));
			});
		}

		return links;
	}

	private List<CodeViewerLink> getLinksFromControlFlow(final EntityId moduleId, final IDocument document, final Optional<AstModel> model) {
		final List<CodeViewerLink> links = new ArrayList<>();

		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(moduleId)
				.withSuperTypes(AstNodeUtils.BRANCH_STATEMENT, AstNodeUtils.JUMP_STATEMENT));
		for (final AstNodePojo astNode : astNodes) {
			astNode.getOutgoingRelations().stream().filter(r -> r.getType().equals(AstRelationshipType.FLOW)).forEach(flowsControl -> {
				final EntityId sourceModuleId = getEffectiveModuleId(astNode);
				final AstNodePojo targetNode = flowsControl.getDstNode();
				final EntityId targetModuleId = getEffectiveModuleId(targetNode);
				if (model.isEmpty() && ( ! sourceModuleId.equals(moduleId) || ! targetModuleId.equals(moduleId))) {
					/* when no assembling information is present (i.e. looking at the unassembled source code), then we ignore links
					 * from or to nodes that come from other Modules */
					return;
				}
				final Optional<ModuleLocation> location = getAssembledLocation(sourceModuleId,
						astNode.getLocation().convertToSharedModuleLocation(), model);
				final Optional<ModuleLocation> targetLocation = getAssembledLocation(targetModuleId,
						targetNode.getLocation().convertToSharedModuleLocation(), model);
				if (location.isEmpty() || targetLocation.isEmpty()) {
					LOG.warn(() -> String.format("Unable to calculate assembled location for CodeViewerLink on %s (Control Flow to %s) omitting link.",
							astNode.getLabel(), targetNode.getLabel()));
					return;
				}
				/* astNode.getModuleLocation.getLength() would return the length of the entire if-else block
				 * for an if-else statement. Therefore we use the length of the label of the branch statement instead
				 * to set the length for the code viewer link */
				final ModuleLocation adjustedLocation = new ModuleLocation(location.get().getOffset().intValue(), assertNotNull(astNode.getLabel()).length());
				links.add(new CodeViewerLink(
						LinkType.CONTROL_FLOW,
						LinkTargetType.LOCAL,
						"Branch: " + StringUtils.trimToEmpty(flowsControl.getLabel().orElse(null)),
						targetNode.getLabel(),
						moduleId,
						adjustedLocation,
						convertEditorRange(document, adjustedLocation),
						moduleId,
						targetLocation.get(),
						convertEditorRange(document, targetLocation.get())));
			});
		}

		return links;
	}

	private List<CodeViewerLink> getLinksFromDependencies(final EntityId projectId, final EntityId moduleId, final IDocument document, final Optional<AstModel> model,
															final Set<Pair<Integer, Long>> offsetToModuleSet) {
		final List<CodeViewerLink> links = new ArrayList<>();

		core.moduleService.findRelationship(q -> q
				.ofProject(projectId)
				.ofSource(moduleId)
				.withTypes(RelationshipType.DEPENDENCY_TYPES))
			.forEach(reference -> {
				if (!isDependencyRelationship(reference.getRelationship())) {
					return;
				}
				if (reference.getSrcLocation().isEmpty()) {
					/* can not add link if dependency has no location information */
					return;
				}
				final ModulePojo targetModule = core.moduleService.getModule(EntityId.of(reference.getDstModule()));
	
				final Optional<ModuleLocation> location = getAssembledLocation(EntityId.of(reference.getSrcModule()), reference.getSrcLocation().orElse(null), model);
				if (location.isEmpty()) {
					LOG.warn(() -> String.format("Unable to calculate assembled location for CodeViewerLink (external link to %s) omitting link.",
							reference.getDstModule()));
					return;
				}
				/* avoid creating more than one link to the same module at the same location */
				final Pair<Integer, Long> offsetToModule = Pair.of(location.get().getOffset(), targetModule.getId());
				if (offsetToModuleSet.contains(offsetToModule)) {
					return;
				}
				offsetToModuleSet.add(offsetToModule);
				
				links.add(new CodeViewerLink(
						LinkType.DEPENDENCY,
						LinkTargetType.EXTERNAL,
						reference.getRelationship().toString(),
						targetModule.getName(),
						moduleId,
						location.get(),
						convertEditorRange(document, location.get()),
						targetModule.identity(),
						reference.getDstLocation().orElse(null),
						convertEditorRange(document, reference.getDstLocation().orElse(null))));
			});

		return links;
	}

	@Nullable
	private CodeViewerRange convertEditorRange(final IDocument document, @Nullable final ModuleLocation moduleLocation) {
		if (moduleLocation == null) {
			return null;
		}
		final int startOffset = moduleLocation.getOffset();
		final int endOffset = moduleLocation.getOffset() + moduleLocation.getLength();
		/* converting to 1-based indexes here */
		return new CodeViewerRange(
				document.getLineNumber(startOffset) + 1,
				document.getColumnNumber(startOffset) + 1,
				document.getLineNumber(endOffset) + 1,
				document.getColumnNumber(endOffset) + 1);
	}

	private EntityId getEffectiveModuleId(final AstNodePojo astNode) {
		return astNode.getIncludedModule().orElseGet(astNode::getModule);
	}

	private boolean isDependencyRelationship(final RelationshipType relationship) {
		switch (relationship) {
			case CALLS:
			case INCLUDES:
			case REFERENCES:
			case ACCESSES:
				return true;
			default:
				return false;
		}
	}
}
