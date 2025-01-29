/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core;

import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ModulePojo;
import innowake.ndt.core.assembling.AssemblerConfiguration;
import innowake.ndt.core.assembling.IAssembler;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.IAssemblingOrigin;
import innowake.ndt.core.assembling.IAssemblingProtocol;
import innowake.ndt.core.assembling.IAssemblingReplacementMap;
import innowake.ndt.core.assembling.IAssemblingSourceOffset;
import innowake.ndt.core.assembling.IAssemblingStep;
import innowake.ndt.core.parsing.ILocation;
import innowake.ndt.core.parsing.replacement.Replacements;

/**
 * Dummy implementation for the Assembling. This is required for the language which don't has a assembler
 */
public class DummyIAssembling implements IAssembling<ModulePojo> {

	private final ModulePojo sourceObject;

	private static final String EXCEPTION_MESSAGE = "This method should never be invoked";

	
	/**
	 * Constructor for DummyIAssembling
	 * @param sourceObject The concrete class for assembling
	 */
	public DummyIAssembling(final ModulePojo sourceObject) {
		this.sourceObject = sourceObject;
	}

	@Override
	public @NonNull ModulePojo getSourceObject() {
		return sourceObject;
	}

	@Override
	public int getLineBefore(final int after) {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public ILocation getLocationBeforeByOffset(final int offsetAfter) {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public IAssemblingSourceOffset<ModulePojo> retraceSourceOffset(final int offsetAfter) {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public String getContentBefore() {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public Replacements<IAssemblingOrigin<ModulePojo>> getReplacements() {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public Replacements<IAssemblingOrigin<ModulePojo>> getReplacements(final boolean after) {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public String getContent(final boolean after) {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public IAssemblingReplacementMap<ModulePojo> computeReplacementMap(final boolean after) {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public IAssembling<ModulePojo> getAssembling() {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public IAssemblingProtocol<ModulePojo> getProtocol() {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public String getAssembledContent() {
		return sourceObject.getContent().orElseThrow(() -> new IllegalStateException("Assembled content must not be null"));
	}

	@Override
	public int getNumberOfSteps() {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public IAssemblingStep<ModulePojo> getStepAt(final int index) {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public IAssemblingStep<ModulePojo> getRoot() {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public AssemblerConfiguration getConfiguration() {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public IAssembler<ModulePojo> getAssembler() {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}

	@Override
	public int getOffsetAssembled(final int offsetUnassembled, final @Nullable ModulePojo include) {
		throw new UnsupportedOperationException(EXCEPTION_MESSAGE);
	}
}
