/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import com.orientechnologies.orient.core.record.OVertex;
import com.orientechnologies.orient.core.sql.executor.OResultSet;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Project;
import innowake.spring.data.orientdb.repository.cache.OrientRequestCacheManager;
import org.junit.Test;
import org.springframework.cache.Cache;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletWebRequest;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Test case for request-scoped entity proxy cache.
 */
public class OrientRequestCacheTest extends AbstractEmployeeRepositoryIntegrationTests {

	@Test
	public void checkIfCacheIsBeingUsed() {
		MockHttpServletRequest request = new MockHttpServletRequest("GET", "/dummy");
		MockHttpServletResponse response = new MockHttpServletResponse();
		ServletWebRequest servletWebRequest = new ServletWebRequest(request, response);
		RequestContextHolder.setRequestAttributes(servletWebRequest);

		final Project[] projects = {new Project("project_1"), new Project("project_2")};
		final Employee employee = new Employee("Bob", "Marley", "bob_marley@deloitte.com", Arrays.asList(projects));
		final Employee savedemployee = assertNotNull(employeeRepository.save(employee));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String employeeId = getVertexFromUserName(savedemployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final Employee employeeAlice = assertNotNull(employeeRepository.findById(employeeId).orElse(null));
		// getting projects to invoke lazy
		assertNotNull(employeeAlice.getProjects()).stream().map(employeeProjects -> employeeProjects.getIn()).collect(Collectors.toList());
		final RequestAttributes requestAttributes = assertNotNull(RequestContextHolder.getRequestAttributes());
		Cache cache = (Cache) requestAttributes.getAttribute("orientRequestCache:" + OrientRequestCacheManager.VERTEX_CACHE_NAME, RequestAttributes.SCOPE_REQUEST);
		assertNotNull(cache, "cache was not created");
		// calling this to get more coverage, so that destroy method is called, cannot verify anything as cache itself is closed.
		servletWebRequest.requestCompleted();
	}

}
