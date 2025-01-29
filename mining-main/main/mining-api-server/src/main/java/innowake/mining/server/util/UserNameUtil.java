/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */

package innowake.mining.server.util;

import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.mining.server.service.UserNameService;
import innowake.mining.server.service.UserNameService.LookupResult;
import innowake.mining.shared.model.UserIdentifyable;

/**
 * Utility class for filling in user names on entities.
 */
@Service
public class UserNameUtil {

	private static final String MISSING_USER_NAME = "Unknown User";
	private static final String DELETED_USER_SUFFIX = " (deleted)";

	@Autowired
	private UserNameService userNameService;
	
	/**
	 * Fills a single {@code UserIdentifyable} entity.
	 *
	 * @param entity the entity to be filled
	 */
	public void fillUserName(final UserIdentifyable entity) {
		entity.setCreatedByUserName(getUserName(entity.getCreatedByUserId()));
		entity.setUpdatedByUserName(getUserName(entity.getUpdatedByUserId()));
	}

	/**
	 * Fills the given Iterable of {@code UserIdentifyable} entities.
	 *
	 * @param entities the entities to be filled
	 */
	public void fillUserNames(final Iterable<? extends UserIdentifyable> entities) {
		final Map<String, String> userNameInfo = StreamSupport.stream(entities.spliterator(), false) /* do not parallel process this stream as it causes issues
																										with KeycloakRestTemplate */
				.flatMap(entity -> Stream.of(entity.getCreatedByUserId(), entity.getUpdatedByUserId()))
				.distinct()
				.collect(Collectors.toMap(userId -> userId, this::getUserName));
		entities.forEach(entity -> {
			entity.setCreatedByUserName(userNameInfo.get(entity.getCreatedByUserId()));
			entity.setUpdatedByUserName(userNameInfo.get(entity.getUpdatedByUserId()));
		});
	}
	
	public String getUserName(final Optional<String> userId) {
		return getUserName(userId.orElse(""));
	}
	
	public String getUserName(final String userId) {
		if (StringUtils.isBlank(userId)) {
			return "";
		}
		final LookupResult userName = userNameService.lookupUserName(userId);
		if (userName.isMissing()) {
			return MISSING_USER_NAME;
		} else if (userName.isDeleted()) {
			return userName.getUserName() + DELETED_USER_SUFFIX;
		} else {
			return userName.getUserName();
		}
	}
}
