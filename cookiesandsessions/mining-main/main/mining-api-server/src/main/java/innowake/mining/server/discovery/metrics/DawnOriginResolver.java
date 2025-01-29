/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.Optional;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.AnchorToBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.retrace.Inclusion;
import innowake.ndt.core.assembling.retrace.Retracer;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.assembling.retrace.RetracingSourceOffset;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Resolves the actual object a statement is in. This is used to find the
 * copybook a statement is in, rather than the copying module.
 */
public class DawnOriginResolver {

	private final Retracing<SourcePojo> retracing;

	/**
	 * Creates a new instance and initializes the retracing.
	 * 
	 * @param assembling the assembling of the module
	 */
	public DawnOriginResolver(final IAssembling<SourcePojo> assembling) {
		this.retracing = new Retracer<SourcePojo>().retrace(assembling);
	}
	
	/**
	 * Resolves a given {@link AstNode} to the origin module. If no other origin is found
	 * the root is returned.
	 *
	 * @param node the node to resolve
	 * @param builder the {@link DiscoveryBuilder}
	 * @param rootModule the root {@link ModuleBuilder}
	 * @return the {@link ModuleBuilder} actually containing the given node
	 */
	public ModuleBuilder resolve(final AstNode node, final DiscoveryBuilder builder, final ModuleBuilder rootModule) {
		final Inclusion<SourcePojo> inclusion = retracing.findInclusion(node.getStartOffset());
		final Optional<AnchorToBuilder> module = Optional.ofNullable(inclusion)
				.map(Inclusion::getCallee)
				.map(sourceObject -> new ModuleFilter()
						.setPaths(sourceObject.getPath())
						.setPhysical(true))
				.map(moduleFilter -> builder.anchorTo(moduleFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE));
		
		return module.isPresent() ? module.get() : rootModule;
	}
	
	/**
	 * Resolves the location of an {@link AstNode} in the original source code. 
	 *
	 * @param node the node to resolve
	 * @return the location in the original source code.
	 */
	public ModuleLocation resolveLocation(final AstNode node) {
		final RetracingSourceOffset<SourcePojo> start = retracing.retrace(node.getStartOffset());
		if (start == null) {
			return new ModuleLocation(node.getStartOffset(), node.getLength());
		}
		final int startOffset = start.getOffset();
		final RetracingSourceOffset<SourcePojo> end = retracing.retrace(node.getEndOffset());
		if (end == null) {
			return new ModuleLocation(startOffset, node.getLength());
		}
		return new ModuleLocation(startOffset, end.getOffset() - startOffset + 1);
	}
	
	/**
	 * Resolves the source object of an {@link AstNode}.
	 *
	 * @param node the node to resolve
	 * @return the source object that the node is associated with.
	 */
	public Optional<SourcePojo> resolveSourceObject(final AstNode node) {
		final Inclusion<SourcePojo> inclusion = retracing.findInclusion(node.getStartOffset());
		SourcePojo result = null;
		if (inclusion != null) {
			result = inclusion.getCallee();
		}
		return Optional.ofNullable(result);
	}
}
