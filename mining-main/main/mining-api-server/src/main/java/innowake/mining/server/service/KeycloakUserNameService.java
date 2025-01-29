/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.service;

import static innowake.mining.server.service.UserNameService.LookupResult.Status.MISSING;
import static innowake.mining.server.service.UserNameService.LookupResult.Status.OK;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import javax.annotation.PostConstruct;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Profile;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.cache.MiningCacheConfig;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.shared.model.Member;

/**
 * Service to look up user names with keycloak authentication.
 */
@Service
@Profile(Profiles.IAM)
public class KeycloakUserNameService implements UserNameService {
	
	private static final Logger LOG = LoggerFactory.getLogger(KeycloakUserNameService.class);

	@Autowired
	private RestTemplate keycloakRestTemplate;

	@Autowired
	private KeycloakApplicationConfiguration keycloakConfig;

	@Autowired
	private CacheManager cacheManager;

	@Nullable
	private String userNameURI;

	@Nullable
	private Cache lookUpCache;

	private static final LookupResult UNKNOWN_USER = new LookupResult("", MISSING);

	/**
	 * PostConstruct to initialize Keycloak configuration, used to interact with the extended Keycloak endpoint for fetching user name.
	 */
	@PostConstruct
	public void createKeycloakURI() {
		final String realmURI = keycloakConfig.getAuthServerUrl() + "/realms/" + keycloakConfig.getRealm();
		userNameURI = realmURI + "/admin/users/%s";
		lookUpCache = cacheManager.getCache(MiningCacheConfig.KEYCLOAK_USER_NAME_LOOKUP);
	}

	@Override
	public LookupResult lookupUserName(final String userId) {
		if (StringUtils.isBlank(userId)) {
			return new LookupResult(userId, OK);
		} else if (userId.equals(SYSTEM_USER_ID)) {
			return new LookupResult(SYSTEM_USER_NAME, OK);
		}

		final LookupResult cachedValue = Objects.requireNonNull(lookUpCache).get(userId, LookupResult.class);
		if (cachedValue != null) {
			return cachedValue;
		}

		try {
			final String url = String.format(userNameURI, userId);
			final ResponseEntity<Member> response = keycloakRestTemplate.getForEntity(url, Member.class);
			final Member member = response.getBody();
			if (member != null) {
				final LookupResult result = new LookupResult(getUserName(member.getFirstName(), member.getLastName()), OK);
				Objects.requireNonNull(lookUpCache).put(userId, result);
				return result;
			} else {
				return new LookupResult(userId, MISSING);
			}
		} catch (final HttpClientErrorException e) {
			LOG.debug(() -> "Unable to retrieve user information. Response code : " + e.getStatusCode(), e);
			return UNKNOWN_USER;
		} catch (final IllegalStateException e) {
			/* If received "Cannot set authorization header because there is no authenticated principal" then ensure the KeycloakRestTemplate bean is created
			 * one per request. Avoid parallel processing !! */
			LOG.error(() -> "Error during look up for userid::" + userId, e);
			return UNKNOWN_USER;
		}
	}

	@Override
	public List<LookupResult> lookupUserNames(final Iterable<String> userIds) {
		return StreamSupport.stream(userIds.spliterator(), false)
				.map(this::lookupUserName)
				.collect(Collectors.toList());
	}
	
	private String getUserName(@Nullable final String firstName, @Nullable final String lastName) {
		final String fName = firstName == null ? "" : firstName;
		final String lName = lastName == null ? "" : lastName;
		return (fName + " " + lName).trim();
	}

}
