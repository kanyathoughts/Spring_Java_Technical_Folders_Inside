/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.commons.lang3.concurrent.ConcurrentException;
import org.apache.commons.lang3.time.StopWatch;
import org.junit.Test;

import com.orientechnologies.orient.core.record.OVertex;
import com.orientechnologies.orient.core.sql.executor.OResultSet;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;

/**
 * Class to test CRUD operations when multiple threads access them.
 */
public class ConcurrencyTests extends AbstractEmployeeRepositoryIntegrationTests {
	
	private static final Logger LOG = LoggerFactory.getLogger(ConcurrencyTests.class);
	
	/**
	 * Tests the concurrency with a single thread.
	 * 
	 * @throws InterruptedException if an execution of a thread is interrupted mid run
	 */
	@Test
	public void testSimpleThread() throws InterruptedException {
		testSimpleConcurrency(1);
	}
	
	/**
	 * Tests the concurrency with save crud operation being accessed by multiple threads.
	 * 
	 * @throws InterruptedException if an execution of a thread is interrupted mid run
	 */
	@Test
	public void testConcurrencyWithThreads() throws InterruptedException {
		testSimpleConcurrency(50);
	}

	
	/**
	 * Tests the concurrency with save crud operation being accessed by single thread from Executor service.
	 * 
	 * @throws InterruptedException if an execution of a thread is interrupted mid run
	 */
	@Test
	public void testConcurrencyWithExecutorServiceSingleThreadExecutor() throws InterruptedException {
		final ExecutorService pool = Executors.newSingleThreadExecutor();
		testConcurrency(pool, 50);
	}
	
	/**
	 * Tests the concurrency with save crud operation being accessed by mutliple fixed threads from Executor service.
	 * 
	 * @throws InterruptedException if an execution of a thread is interrupted mid run
	 */
	@Test
	public void testConcurrencyWithExecutorServiceFixedThreadPool() throws InterruptedException {
		final ExecutorService pool = Executors.newFixedThreadPool(10);
		testConcurrency(pool, 50);
	}
	
	private void testConcurrency(final ExecutorService pool, final int numberOfEmployees) throws InterruptedException {
		final int[] numberOfErrors = new int[1];
		final CountDownLatch latch = new CountDownLatch(numberOfEmployees);
		final StopWatch watch = new StopWatch();
		watch.start();
		for (int i = 0; i < numberOfEmployees; i++) {
			pool.submit(() ->  {
				try {
					saveEmployee();
					saveEmployee();
				} catch (final Exception e) {
					numberOfErrors[0]++;
					throw new IllegalStateException("Error while saving employees : " , e);
				} finally {
					latch.countDown();
				}
			});
		}
		latch.await();
		watch.stop();
		LOG.debug(() -> String.format("%s took %s", "saving " + numberOfEmployees , watch.toString()));
		assertTrue("number of errors" + numberOfErrors[0] , numberOfErrors[0] == 0);
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved  = getVertices(resultSet);
		assertEquals(numberOfEmployees * 2, verticesSaved.size(), "count is mismatched");
		assertTrue(getAllEdges(verticesSaved).isEmpty());
		pool.shutdownNow();
	}
	
	private void testSimpleConcurrency(int numberOfEmployees) throws InterruptedException {
		final int[] numberOfErrors = new int[1];
		final StopWatch watch = new StopWatch();
		watch.start();
		for (int i = 0; i < numberOfEmployees; i++) {
			final Thread thread = new Thread(() ->  {
				try {
					saveEmployee();
				} catch (final Exception e) {
					numberOfErrors[0]++;
					throw new IllegalStateException("Error while saving employee : ", e);
				}
			});
			thread.start();
			thread.join();
		}
		watch.stop();
		LOG.info(() -> String.format("%s took %s", "saving " + numberOfEmployees , watch.toString()));
		assertTrue("number of errors" + numberOfErrors[0] , numberOfErrors[0] == 0);
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved  = getVertices(resultSet);
		assertEquals(numberOfEmployees, verticesSaved.size(), "count is mismatched");
		assertTrue(getAllEdges(verticesSaved).isEmpty());
	}
	
	private void saveEmployee() throws ConcurrentException {
		try {
			final Employee employee = new Employee("user_1" , "admin_1", "user_1@deloitte.com");
			employeeRepository.save(employee);
		} catch (final Exception e) {
			throw new ConcurrentException("Error while performing concurent save", e);
		}
	}
}
