/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.generic.loc;

import innowake.mining.server.discovery.metrics.generic.MetricResult;
import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.parsing.scanner.generic.LocHelper;

/**
 * Initializes LOC Metric based on language type
 */
public class CustomRegionsLocMetric extends LocMetric {

	private final String[] categoryCode;
	private final String categoryComment;

	/**
	 * Initializes LOC Metric based on language region category
	 * 
	 * @param inputType of the given LOC metric
	 * @param categoryComment based on language type
	 * @param categoryCode array of language categories
	 */
	public CustomRegionsLocMetric(final InputType<ITokenPartitioning> inputType, final String categoryComment, final String... categoryCode) {
		super(inputType);
		this.categoryCode = categoryCode;
		this.categoryComment = categoryComment;
	}

	@Override
	public MetricResult<ITokenPartitioning> execute(final ITokenPartitioning input) {
		final int countLinesOfCodeWithRegion = LocHelper.countLinesOfCodeWithRegion(input, categoryCode);
		return new LocMetricResult(countLinesOfCodeWithRegion, 
				LocHelper.countLinesOfCommentsWithRegion(input, categoryComment), LocHelper.countPhysicalLines(input));
	}
}
