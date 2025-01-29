/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Source metrics for testing purposes.
 */
public class SourceMetricsPojoDummy extends SourceMetricsPojoPrototype {
	
	public SourceMetricsPojoDummy prepare(final BuildingConsumer<SourceMetricsPojoDummy> builder) {
		return builder.prepare(this);
	}
	
	public SourceMetricsPojo build() {
		return build(this);
	}
	
	public static SourceMetricsPojo build(final SourceMetricsPojoPrototype proto) {
		return new SourceMetricsPojo(
				proto.module.getNonNull(), null, null,
				proto.physicalLines.orElse(null),
				proto.codeLines.orElse(null),
				proto.commentLines.orElse(null),
				proto.complexityMcCabe.orElse(null),
				proto.deadCodeLines.orElse(null));
	}

	public static SourceMetricsPojoPrototype build(final SourceMetricsPojo sm) {
		final SourceMetricsPojoPrototype proto = new SourceMetricsPojoPrototype()
				.setModule(sm.getModule());
		
		Integer value = sm.getCodeLines();
		if (value != null) {
			proto.setCodeLines(value);
		}
		value = sm.getCommentLines();
		if (value != null) {
			proto.setCommentLines(value);
		}
		value = sm.getComplexityMcCabe();
		if (value != null) {
			proto.setComplexityMcCabe(value);
		}
		value = sm.getDeadCodeLines();
		if (value != null) {
			proto.setDeadCodeLines(value);
		}
		value = sm.getPhysicalLines();
		if (value != null) {
			proto.setPhysicalLines(value);
		}

		return proto;
	}
}
