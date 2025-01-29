package innowake.mining.server.discovery.metrics.generic.loc;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.metrics.generic.MetricResult;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.ndt.core.parsing.ITokenPartitioning;

public class LocMetricResult implements MetricResult<ITokenPartitioning> {

	private final int code;
	private final int comment;
	private final int physicalLines;
	
	/**
	 * Initializes LOC Metric Result.
	 * 
	 * @param code number of lines of code
	 * @param comment number of lines of comment
	 * @param physicalLines total number of lines of code, lines of comment
	 */
	public LocMetricResult(final int code, final int comment, final int physicalLines) {
		this.code = code;
		this.comment = comment;
		this.physicalLines = physicalLines;
	}
	
	@Override
	public MetricType<ITokenPartitioning> getMetricType() {
		return MetricType.LOC_LOC;
	}

	@Override
	public void applyTo(final ModelArtifact artifact) {
		artifact.setLinesOfCode(code);
		artifact.setLinesOfComments(comment);
		assertNotNull(artifact.getSourceMetrics()).setPhysicalLines(Integer.valueOf(physicalLines));
	}

}