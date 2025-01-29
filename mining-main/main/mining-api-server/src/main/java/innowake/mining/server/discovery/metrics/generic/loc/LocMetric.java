package innowake.mining.server.discovery.metrics.generic.loc;

import innowake.mining.server.discovery.metrics.generic.AbstractMetric;
import innowake.mining.server.discovery.metrics.generic.MetricResult;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.parsing.scanner.generic.LocHelper;

public class LocMetric extends AbstractMetric<ITokenPartitioning> {
	
	/**
	 * Initializes LOC Metric.
	 * 
	 * @param inputType of the given LOC metric
	 */
	public LocMetric(final InputType<ITokenPartitioning> inputType) {
		super(inputType);
	}

	@Override
	public MetricType<ITokenPartitioning> getMetricType() {
		return MetricType.LOC_LOC;
	}

	@Override
	public MetricResult<ITokenPartitioning> execute(final ITokenPartitioning input) {
		return new LocMetricResult(LocHelper.countLinesOfCode(input), LocHelper.countLinesOfComments(input), LocHelper.countPhysicalLines(input));
	}

}
