package innowake.mining.draft.idm.configuration;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.access.AccessDecisionManager;
import org.springframework.security.access.vote.AbstractAccessDecisionManager;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.method.configuration.GlobalMethodSecurityConfiguration;

@Configuration
@EnableGlobalMethodSecurity(securedEnabled = true, prePostEnabled = true)
public class AccessDecisionConfiguration extends GlobalMethodSecurityConfiguration {

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