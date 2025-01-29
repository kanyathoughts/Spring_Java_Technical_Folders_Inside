package innowake.mining.server.config.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.access.AccessDecisionManager;
import org.springframework.security.access.AccessDecisionVoter;
import org.springframework.security.access.vote.AbstractAccessDecisionManager;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.method.configuration.GlobalMethodSecurityConfiguration;

import innowake.mining.server.config.Profiles;

/**
 * Configuration for setting up the method security with the appropriate {@link AccessDecisionVoter}.
 */
@Configuration
@EnableGlobalMethodSecurity(securedEnabled = true, prePostEnabled = true)
@Profile(Profiles.IAM)
public class MiningAccessDecisionConfiguration extends GlobalMethodSecurityConfiguration {

	@Autowired
	private MiningVoter voter;

	@Bean
	@Override
	protected AccessDecisionManager accessDecisionManager() {
		final AbstractAccessDecisionManager superManager = (AbstractAccessDecisionManager) super.accessDecisionManager();
		superManager.getDecisionVoters().add(voter);
		return superManager;
	}
}