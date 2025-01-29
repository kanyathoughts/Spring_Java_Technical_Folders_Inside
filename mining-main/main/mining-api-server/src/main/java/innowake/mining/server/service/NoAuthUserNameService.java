/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.mining.server.service.UserNameService.LookupResult.Status.OK;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import innowake.mining.server.config.Profiles;


/**
 * Service to look up users names with no authentication.
 */
@Service
@Profile({Profiles.NO_IAM, Profiles.NO_AUTH})
public class NoAuthUserNameService implements UserNameService {

	@Override
	public LookupResult lookupUserName(final String userId) {
		return new LookupResult(userId, OK);
	}

	@Override
	public List<LookupResult> lookupUserNames(final Iterable<String> userIds) {
		return StreamSupport.stream(userIds.spliterator(), true)
				.map(this::lookupUserName)
				.collect(Collectors.toList());
	}

}
