/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;


import java.util.List;
import java.util.Optional;

import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.retrace.Inclusion;
import innowake.ndt.core.assembling.retrace.Retracer;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.assembling.retrace.RetracingSourceOffset;
import innowake.ndt.core.parsing.ILocationWithPosition;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Resolves the actual object a statement is in. This is used to find the
 * copybook a statement is in, rather than the copying module.
 */
public class OriginResolver {
	private final Retracing<SourcePojo> retracing;

	/**
	 * Creates a new instance and initializes the retracing.
	 * 
	 * @param assembling the assembling of the module
	 */
	public OriginResolver(final IAssembling<SourcePojo> assembling) {
		this.retracing = new Retracer<SourcePojo>().retrace(assembling);
	}

	/**
	 * Resolves a given {@link AstNode} to the origin module. If no other origin is found
	 * the root is returned.
	 *
	 * @param node the node to resolve
	 * @param repo the module repository
	 * @param root the corresponding module entry for the module
	 * @return the module actually containing the given node
	 */
	public ModelArtifact resolve(final AstNode node, final IModuleRepository repo, final ModelArtifact root) {
		return resolve(node.getStartOffset(), repo, root);
	}

	/**
	 * Resolves a given offset to the origin module. If no other origin is found
	 * the root is returned.
	 *
	 * @param offset the offset to resolve
	 * @param repo the module repository
	 * @param root the corresponding module entry for the module
	 * @return the module actually containing the given node
	 */
	public ModelArtifact resolve(final int offset, final IModuleRepository repo, final ModelArtifact root) {
		final Inclusion<SourcePojo> inclusion = retracing.findInclusion(offset);
		
		return Optional.ofNullable(inclusion)
				.map(Inclusion::getCallee)
				.flatMap(f -> repo.getPhysicalEntry(f.getPath()) )
				.orElse(root);
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
	
	/**
	 * Calculates how many lines of a module are contained in a list of {@link ILocationWithPosition}.
	 *
	 * @param tokens the list of {@link ILocationWithPosition}
	 * @param repo the module repository
	 * @param artifact the module of which the number of lines are counted
	 * @param root the corresponding module entry for the module
	 * @return the number of lines represented by the tokens in the given module.
	 */
	public int calculateLinesOfArtifact(final List<? extends ILocationWithPosition> tokens, final IModuleRepository repo, final ModelArtifact artifact, final ModelArtifact root) {
		int result = 0;
		int currentLine = -1;
		for (final ILocationWithPosition token : tokens) {
			final int tokenLine = token.getLine();
			if (tokenLine > currentLine) {
				currentLine = tokenLine;
				if (artifact.equals(resolve(token.getOffset(), repo, root))) {
					result++;
				}
			}
		}
		return result;
	}
}
