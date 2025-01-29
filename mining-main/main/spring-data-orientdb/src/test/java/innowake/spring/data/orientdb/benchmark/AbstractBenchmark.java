/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.benchmark;

import org.junit.Test;
import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;

/**
 * Configuration for bench mark tests.
 */
public abstract class AbstractBenchmark extends AbstractIntegrationTests {
	private final static Integer MEASUREMENT_ITERATIONS = Integer.valueOf(3);
    private final static Integer WARMUP_ITERATIONS = Integer.valueOf(3);

    /**
     * Executes the jmh runner to evaluate the performance.
     *
     * @throws RunnerException any runtime application error
     */
    @Test
    public void executeJmhRunner() throws RunnerException {
        final Options opt = new OptionsBuilder()
            .include("\\." + this.getClass().getSimpleName() + "\\.")
            .warmupIterations(WARMUP_ITERATIONS.intValue())
            .measurementIterations(MEASUREMENT_ITERATIONS.intValue())
            .forks(0)
            .threads(1)
            .shouldDoGC(true)
            .shouldFailOnError(true)
            .resultFormat(ResultFormatType.JSON)
            .shouldFailOnError(true)
            .jvmArgs("-server")
            .build();

        new Runner(opt).run();
    }
}
