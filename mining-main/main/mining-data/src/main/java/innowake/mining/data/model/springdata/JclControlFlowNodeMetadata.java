/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.model.springdata;

import com.google.common.base.MoreObjects;

import innowake.lib.core.api.lang.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Metadata related to JCL control flow graph nodes.
 */
public class JclControlFlowNodeMetadata {

	private List<Long> inputFileIds = new ArrayList<>();
	private List<Long> outputFileIds = new ArrayList<>();

	public List<Long> getInputFileIds() {
		return inputFileIds;
	}

	public void setInputFileIds(final List<Long> inputFileIds) {
		this.inputFileIds = inputFileIds;
	}

	public List<Long> getOutputFileIds() {
		return outputFileIds;
	}

	public void setOutputFileIds(final List<Long> outputFileIds) {
		this.outputFileIds = outputFileIds;
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("inputFileIds", inputFileIds)
				.add("outputFileIds", outputFileIds)
				.toString();
	}

	@Override
	public boolean equals(@Nullable final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		if ( ! super.equals(o)) {
			return false;
		}
		final JclControlFlowNodeMetadata that = (JclControlFlowNodeMetadata) o;
		return inputFileIds.equals(that.inputFileIds) && outputFileIds.equals(that.outputFileIds);
	}

	@Override
	public int hashCode() {
		return Objects.hash(super.hashCode(), inputFileIds, outputFileIds);
	}
}
