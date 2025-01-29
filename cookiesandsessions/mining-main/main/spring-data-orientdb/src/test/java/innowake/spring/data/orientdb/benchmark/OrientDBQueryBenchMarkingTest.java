/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.benchmark;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.Ignore;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.spring.data.orientdb.integration.repository.domain.Customer;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.repository.CustomerRepository;

/**
 * Benchmarking tests for various spring data operations. To be enabled only to test the performance locally.
 * 
 * In an 16GB system, the performance was as below.
 * 	Benchmark                                           Mode  Cnt    Score    Error  Units
 *	OrientDBQueryBenchMarkingTest.testCustomerDataLoad  avgt    3   133.853 ± 119.161  ms/op
 *	OrientDBQueryBenchMarkingTest.testCustomerViewLoad  avgt    3  	167.492 ±  40.038  ms/op
 */

@State(Scope.Benchmark)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Ignore
public class OrientDBQueryBenchMarkingTest extends AbstractBenchmark {

	private static final Logger LOGGER = LoggerFactory.getLogger(OrientDBQueryBenchMarkingTest.class);

	@Nullable
	private static CustomerRepository customerRepository;

	/**
	 * Instantiate customer repository.
	 *
	 * @param customerRepository {@link CustomerRepository}
	 */
	@Autowired
	public void setCustomerRepositoy(final CustomerRepository customerRepository) {
		OrientDBQueryBenchMarkingTest.customerRepository = customerRepository;
	}

	/**
	 * Clear previously stored entities.
	 */
	@Before
	public void init() {
		clearData("Customer", "Product");
		for (int i = 0; i < 1000; i++) {
			assertNotNull(OrientDBQueryBenchMarkingTest.customerRepository).save(customer);
		}
		LOGGER.info(() -> "loaded the trial data");
	}

	private final Product product1 = new Product("IPhone", Long.valueOf(12));
	private final Product product2 = new Product("Samsung galaxy", Long.valueOf(15));
	private final Customer customer = new Customer("User 4", Arrays.asList(product1, product2));

	/**
	 * Bench mark test for fetching all the records.
	 *
	 * @throws IOException exception while writing json data
	 */
	@Benchmark
	public void testCustomerDataLoad() throws IOException {
		final List<Customer> findAll = (List<Customer>) assertNotNull(OrientDBQueryBenchMarkingTest.customerRepository).findAll();
		new ObjectMapper().writeValueAsString(findAll);
	}

}
