/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server;

import static org.junit.Assert.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.sql.DataSource;

import innowake.mining.server.util.BranchStatementUtility;
import org.ff4j.FF4j;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.internal.StaleJobCleaner;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.server.config.security.AuthenticationFacade;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JclJobContributor;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.filter.ValidationFilter;
import innowake.mining.server.service.ConverterService;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.server.service.UserNameService;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EffortSummaryService;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.service.UserRoleService;

/**
 * Base class for mocked Spring tests, e.g. @WebMvcTest.
 * <p>
 * This base class makes sure that the required components are mocked properly:
 * <ul>
 *  <li>the database data source
 *  <li>the validation filter
 *  <li>the Keycloak configuration
 */
@ExtendWith(SpringExtension.class)
@Import({ MiningDataCoreService.class, UserNameUtil.class })
public abstract class MockedBaseTest {

	@MockBean
	@Nullable
	private ConverterService converterService;

	@MockBean
	@Nullable
	private DataSource dataSource;

	@MockBean
	@Nullable
	private ValidationFilter validationFilter;

	/* Mocking all DAOs which are used in the BaseController */
	@MockBean
	@Nullable
	protected ClientService clientService;
	@MockBean
	@Nullable
	protected ProjectService projectService;
	@MockBean
	@Nullable
	protected CustomPropertiesService customPropertiesService;
	@MockBean
	@Nullable
	protected AnnotationService annotationService;
	@MockBean
	@Nullable
	protected ModuleService moduleService;
	@MockBean
	@Nullable
	protected TaxonomyService taxonomyService;
	@MockBean
	@Nullable
	protected StaleJobCleaner staleJobCleaner;
	@MockBean
	@Nullable
	protected SourceCachingService sourceCachingService;
	@MockBean
	@Nullable
	protected ParserProviderService parserProviderService;
	@MockBean
	@Nullable
	protected AuthenticationFacade authentication;
	@MockBean
	@Nullable
	protected CallChainService callChainService; 
	@MockBean
	@Nullable
	protected UserNameService userNameService;
	@MockBean
	@Nullable
	protected UserRoleService userRoleService;
	@MockBean
	@Nullable
	protected FieldInfoService dataSchemaService;
	@MockBean
	@Nullable
	protected DiscoveryJobCache discoveryJobCache;
	@Nullable
	@MockBean
	protected DataDictionaryService dataDictionaryService;
	@Nullable
	@MockBean
	protected AstService astService;
	@Nullable
	@MockBean
	protected BranchStatementUtility branchStatementUtility;
	@Nullable
	@MockBean
	protected FF4j ff4j;
	@MockBean
	@Nullable
	private JclJobContributor jclContributor;
	@MockBean
	@Nullable
	private EffortSummaryService effortSummaryService;
	
	/**
	 * Prepares the mocks for proper usage.
	 */
	@BeforeEach
	public void setup() {
		/* Even though the tests are mocked, the ValidationFilter still gets called.
		 * As we are not interested in any kind of validation, we just 'overwrite' the doFilter
		 * method, directly calling the next filter in the chain in the mock. */
		try {
			doAnswer(invocation -> {
				if (invocation != null) {
					final ServletRequest request = invocation.getArgument(0);
					final ServletResponse response = invocation.getArgument(1);
					final FilterChain chain = invocation.getArgument(2);
					try {
						chain.doFilter(request, response);
					} catch (final IOException | ServletException e) {
						throw new IllegalStateException(e);
					}
				}
				return null;
			}).when(validationFilter).doFilter(any(ServletRequest.class), any(ServletResponse.class), any(FilterChain.class));
		} catch (final Exception e) {
			fail("The mocked 'doFilter' threw an exception. This should not have happened.");
		}
	}
}
