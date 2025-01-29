/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration.authorization;

import org.junit.jupiter.api.Disabled;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;

import innowake.mining.server.config.Profiles;

/**
 * Tests validating the behavior of csrf with different user-agent
 */
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@AutoConfigureMockMvc
@WithMockUser
@Disabled("CSRF protection temporarily disabled - WMIN-5382 will re-enable")
class CsrfAuthorizationWithNoAuthTest extends CsrfAuthorizationTest {

}
