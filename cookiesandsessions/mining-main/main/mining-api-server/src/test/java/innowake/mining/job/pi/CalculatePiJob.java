/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.pi;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.IgnoreExceptionHandler;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskSource;

/**
 * Test job calculating pi with multiple tasks.
 */
public class CalculatePiJob extends Job<BigDecimal> {
	
	private final int digits;
	
	public CalculatePiJob(final int digits) {
		this.digits = digits;
	}
	
	@Override
	protected final Result<BigDecimal> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("My PI calculation with " + digits + " digits");
		
		final MathContext mc = new MathContext(digits + 1, RoundingMode.HALF_EVEN);

		final int iterationsPerTask = 25;
		final int taskCount = (digits / iterationsPerTask) + (digits % iterationsPerTask == 0 ? 0 : 1);
		
		progressMonitor.begin(taskCount);
		
		final TaskSource<BigDecimal> taskSource = new TaskSource<BigDecimal>() {

			private int remainingDigits = digits;
			private int fromIncluding = 0;
			
			@Override
			public boolean hasNextTask() {
				return remainingDigits > 0;
			}

			@Override
			public Task<BigDecimal> nextTask() {
				try {
					final ProgressMonitor subProgressMonitor = progressMonitor.subMonitor(1);
					return createPiTask(mc, fromIncluding, fromIncluding + Math.min(remainingDigits, iterationsPerTask), subProgressMonitor, jobId);
				} finally {
					fromIncluding += iterationsPerTask;
					remainingDigits -= iterationsPerTask;
				}
			}
			
		};
		
		final PiResultConsumer resultConsumer = new PiResultConsumer();
		forkTasks(createTaskProcessor(), taskSource, resultConsumer);
		
		return new Result<>(resultConsumer.getPi());
	}
	
	protected CalculatePiTask createPiTask(final MathContext mc, final int fromIncluding, final int toExcluding, final ProgressMonitor subProgressMonitor,
			final String jobId) {
		return new CalculatePiTask(mc, fromIncluding, toExcluding, subProgressMonitor, jobId);
	}
	
	private class PiResultConsumer extends ResultConsumer<BigDecimal> {
		
		BigDecimal pi = BigDecimal.ZERO;

		private PiResultConsumer() {
			super(new IgnoreExceptionHandler<>());
		}

		@Override
		protected void handleResult(final String taskId, final Result<BigDecimal> result) {
			pi = pi.add(result.value);
		}
		
		private BigDecimal getPi() {
			pi.setScale(digits, RoundingMode.DOWN);
			return pi;
		}
		
	}
	
}
