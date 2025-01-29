/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.benchmark;

import static innowake.lib.core.lang.Assert.assertEqual;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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
 * Testing the lazy loaded link performance. 
 * Performance on a 16 GB RAM system.
 * OrientDBDataBenchMarkingTests.testCustomerDataLazyLoad          avgt    3  172.581 ± 927.890  ms/op
 * OrientDBDataBenchMarkingTests.testCustomerDataLazyLoadEvenData  avgt    3  173.766 ± 113.871  ms/op
 * OrientDBDataBenchMarkingTests.testCustomerDataLoadAllData       avgt    3  253.408 ± 206.835  ms/op
 * 
 * Performance on a 32 GB RAM system.
 * OrientDBDataBenchMarkingTests.testCustomerDataLazyLoad          avgt    3  170.452 ± 136.447  ms/op
 * OrientDBDataBenchMarkingTests.testCustomerDataLazyLoadEvenData  avgt    3  169.067 ±  34.219  ms/op
 * OrientDBDataBenchMarkingTests.testCustomerDataLoadAllData       avgt    3  237.832 ± 407.611  ms/op
 */
@State(Scope.Benchmark)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Ignore
public class OrientDBDataBenchMarkingTests  extends AbstractBenchmark {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(OrientDBDataBenchMarkingTests.class);
	
	@Nullable
	private static CustomerRepository customerRepository;
	private final Product product = new Product("IPhone", Long.valueOf(12));
	private final Customer customer = new Customer("User 4");

	/**
	 * Instantiate customer repository.
	 *
	 * @param customerRepository {@link CustomerRepository}
	 */
	@Autowired
	public void setCustomerRepositoy(final CustomerRepository customerRepository) {
		OrientDBDataBenchMarkingTests.customerRepository = customerRepository;
	}

	/**
	 * Clear previously stored entities.
	 */
	@Before
	public void init() {
		clearData("Customer", "Product", "Account");
		for (int i = 0; i < 1000; i++) {
			customer.setCustomerSubscribed(product);
			assertNotNull(OrientDBDataBenchMarkingTests.customerRepository).save(customer);
		}
		LOGGER.info(() -> "loaded the trial data");
	}
	
	/**
	 * Bench mark test for fetching all the records lazily, not fetching any links.
	 *
	 * @throws IOException exception while writing json data
	 */
	@Benchmark
	public void testCustomerDataLazyLoad() throws IOException {
		final List<Customer> customers = (List<Customer>) assertNotNull(OrientDBDataBenchMarkingTests.customerRepository).findAll();
		assertEqual(Integer.valueOf(1000), Integer.valueOf(customers.size()));
	}
	
	/**
	 * Bench mark test for fetching all the records lazily, fetching only few links.
	 *
	 * @throws IOException exception while writing json data
	 */
	@Benchmark
	public void testCustomerDataLazyLoadEvenData() throws IOException {
		final List<Customer> customers = (List<Customer>) assertNotNull(OrientDBDataBenchMarkingTests.customerRepository).findAll();
		final List<Customer> evenCustomers = IntStream.range(0, customers.size())
			    .filter(n -> n % 2 == 0)
			    .mapToObj(customers::get)
			    .collect(Collectors.toList());
		evenCustomers.stream().forEach(customer -> customer.getCustomerSubscribed());
		assertEqual(Integer.valueOf(500), Integer.valueOf(evenCustomers.size()));
	}
	
	/**
	 * Bench mark test for fetching all the records with entire data including links.
	 *
	 * @throws IOException exception while writing json data
	 */
	@Benchmark
	public void testCustomerDataLoadAllData() throws IOException {
		final List<Customer> customers = (List<Customer>) assertNotNull(OrientDBDataBenchMarkingTests.customerRepository).findAll();
		new ObjectMapper().writeValueAsString(customers);
		assertEqual(Integer.valueOf(1000), Integer.valueOf(customers.size()));
	}


}
