/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.cobol.parser.CobolToken;
import innowake.ndt.cobol.parser.ast.model.CobolNode;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.retrace.Retracer;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.assembling.retrace.RetracingSourceOffset;
import innowake.ndt.core.parsing.spi.OneBasedDocument;

/**
 * Collect offset information for back track of a cobol parse result node to the 
 * effective CobolObject (maybe a Copybook) and CobolObject relative line.
 */
public final class CobolOffsetModel {

	private final Retracing<SourcePojo> retracing;
	private final OneBasedDocument assembledRoot;
	
	/**
	 * Create a new instance of the offset model based on a given Cobol assembling.
	 * @param assembly The parse assembling instance.
	 */
	public CobolOffsetModel(final IAssembling<SourcePojo> assembly) {
		retracing = new Retracer<SourcePojo>().retrace(assembly);
		assembledRoot = new OneBasedDocument(retracing.getAssembling().getAssembledContent());
	}

	/**
	 * Calculates the line number in the original source.
	 *
	 * @param token the given token
	 * @return the line number of the given token in the original source.
	 * @throws DiscoveryException Thrown if the retrace failed.
	 */
	public int getLine(final CobolToken token) throws DiscoveryException {
		return getLine(token.getLine());
	}

	/**
	 * Get the line number of absolute to CobolObject relative.

	 * @param assembledLine The absolute line number from the assembling.
	 * @return The relative line number or -1 if the retracing does not result in a valid location.
	 * @throws DiscoveryException If the retracing failed. See cause for details.
	 */
	public int getLine(final int assembledLine) throws DiscoveryException {
		final int assembledOffset = assembledRoot.getOffset(assembledLine);
		@Nullable final RetracingSourceOffset<SourcePojo> sourceOffset = retracing.retrace(assembledOffset);
		
		if (sourceOffset == null) {
			return -1;
		}
		
		final SourcePojo original = assertNotNull(sourceOffset.getOriginal());
		return new OneBasedDocument(original.getContent().toString()).getLineNumber(sourceOffset.getOffset());
	}
	
	/**
	 * Get the module CobolObject where this token is located.
	 *
	 * @param astNode The parser node to get the origin file for.
	 * @return The file instance or empty if it could not be located.
	 */
	public Optional<SourcePojo> getModule(final CobolNode astNode) {
		final int assembledOffset = assembledRoot.getOffset(astNode.getStartToken().getLine());
		final @Nullable RetracingSourceOffset<SourcePojo> sourceOffset = retracing.retrace(assembledOffset);
		if (sourceOffset == null || sourceOffset.getOriginal() == null) {
			return Optional.empty();
		}
		return Optional.of(sourceOffset.getOriginal());
	}
}
