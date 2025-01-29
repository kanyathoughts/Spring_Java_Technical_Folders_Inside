/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * the class is responsible for counting the threads which is an atomic Integer value and increasing during parallelization
 */
public class CurrentThreadCounts {

	private final AtomicInteger count = new AtomicInteger(1);

	public int getAndIncrement() { return this.count.getAndIncrement(); }

}
