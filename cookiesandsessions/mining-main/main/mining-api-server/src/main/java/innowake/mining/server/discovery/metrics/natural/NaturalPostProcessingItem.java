/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.assembling.natural.NaturalAssemblerConfiguration;
import innowake.ndt.core.parsing.replacement.ReplacementMode;


/**
 * Item for post-processing.
 * <p>
 * This class was widely copied from {@code innowake.ndt.natclipse.core.object.internal.PostProcessingItem}.
 */
class NaturalPostProcessingItem {

	static final int TASK_ASSEMBLE = 1 << 0;
	static final int TASK_PARSE_INCOMING = 1 << 1;
	static final int TASK_CHECK_MISSING_REFS = 1 << 2;
	
	private final SourcePojo object;
	
	private int types = 0;
	private int tasks = 0;
	
	NaturalPostProcessingItem(final SourcePojo object) {
		this.object = object;
	}
	
	void putAssemblingType(final int type) {
		this.types = this.types | type;
	}
	
	void putTask(final int task) {
		this.tasks = this.tasks | task;
	}
	
	SourcePojo getObject() {
		return object;
	}

	NaturalAssemblerConfiguration getConfiguration() {
		return new NaturalAssemblerConfiguration(ReplacementMode.FAST_LOCATIONS, false, types);
	}
	
	boolean hasToAssemble() {
		return (this.tasks & TASK_ASSEMBLE) != 0; 
	}
}
