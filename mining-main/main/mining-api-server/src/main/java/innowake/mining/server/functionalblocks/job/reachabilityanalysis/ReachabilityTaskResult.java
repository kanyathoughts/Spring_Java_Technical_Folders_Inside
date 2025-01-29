package innowake.mining.server.functionalblocks.job.reachabilityanalysis;

import innowake.mining.server.job.base.TaskSummary;

import java.io.Serializable;
import java.util.Collection;
import java.util.UUID;

public class ReachabilityTaskResult implements Serializable {

	private final Collection<UUID> functionalBlockIds;
	private final TaskSummary taskSummary;

	public ReachabilityTaskResult(final Collection<UUID> functionalBlockIds, final TaskSummary taskSummary) {
		this.functionalBlockIds = functionalBlockIds;
		this.taskSummary = taskSummary;
	}

	public Collection<UUID> getFunctionalBlockIds() {
		return functionalBlockIds;
	}

	public TaskSummary getTaskSummary() {
		return taskSummary;
	}
}
