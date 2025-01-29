/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.pi;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.concurrent.TimeUnit;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.Task;

/**
 * Test task that calculates a specified range of pi.
 */
public class CalculatePiTask extends Task<BigDecimal> {

	private final MathContext mc;
	private final int fromIncluding;
	private final int toExcluding;
	
	protected CalculatePiTask(final MathContext mc, final int fromIncluding, final int toExcluding, final ProgressMonitor progressMonitor, final String jobId) {
		super(progressMonitor, jobId);
		this.mc = mc;
		this.fromIncluding = fromIncluding;
		this.toExcluding = toExcluding;
	}

	@Override
	protected Result<BigDecimal> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(toExcluding - fromIncluding);
		BigDecimal subPi = BigDecimal.ZERO;
		for (int i = fromIncluding; i < toExcluding; i++) {
			subPi = subPi.add(term(i));
			progressMonitor.worked(1);
			
			try {
				TimeUnit.MILLISECONDS.sleep(200);
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
		return new Result<>(Status.OK, subPi);
	}
	
	private static final BigDecimal bd(final int value) {
		return new BigDecimal(value);
	}

	private static final BigDecimal[] CONSTANTS = new BigDecimal[] {
			bd(4), bd(0), bd(0), bd(-2), bd(-1), bd(-1), bd(0), bd(0)
	};

	private static final BigDecimal K16 = bd(16);

	private final BigDecimal term(final int k) {
		BigDecimal result = BigDecimal.ZERO;
		for (int i = 0; i < 8; i++) {
			result = result.add(CONSTANTS[i].divide(bd(8 * k + i + 1), mc));
		}
		return result.multiply(K16.pow(-k, mc));
	}

}
