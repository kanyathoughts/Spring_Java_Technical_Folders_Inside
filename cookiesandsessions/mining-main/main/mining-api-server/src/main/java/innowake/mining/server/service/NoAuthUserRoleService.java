/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import innowake.mining.server.config.Profiles;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import innowake.mining.shared.service.UserRoleService;

/**
 * Implementation returning static information for usage in a non-authorized environment.
 */
@Service
@Profile(Profiles.NO_IAM)
public class NoAuthUserRoleService implements UserRoleService {
	
	/**
	 * This will get used for creating a list of IDs from 0 to 10.000 which get
	 * used in the data access when querying all clients. This will make sure that
	 * the user can retrieve all clients when not running with authorization
	 * enabled.
	 * 
	 * 10.000 was used because it seems like this number of entries has no
	 * measurable performance impact on the query time.
	 * 
	 * 100.000 entries did reduce the performance by ~10X.
	 */
	private static final int CLIENT_IDS = 10000;
	
	private static final List<Long> IDS = new ArrayList<>();
	
	static {
		for (int i = 0; i <= CLIENT_IDS; i++) {
			IDS.add(Long.valueOf(i));
		}
	}

	@Override
	public List<Long> getClientIds() {
		return IDS;
	}

	@Override
	public List<Long> getProjectIds() {
		return IDS;
	}

	@Override
	public List<Long> getClientAdminIds() {
		return IDS;
	}

	@Override
	public boolean isAdmin() {
		return true;
	}

	@Override
	public boolean hasPlatformRole() {
		return true;
	}

	@Override
	public Set<NatureType> getNaturesOnProject(final long projectId) {
		return EnumSet.allOf(NatureType.class);
	}

	@Override
	public Set<RoleType> getRolesOnProject(final long projectId) {
		return EnumSet.allOf(RoleType.class);
	}

	@Override
	public boolean isClientAdmin(Long projectId) {
		return true;
	}

	@Override
	public boolean hasRequiredRole(Long projectId, RoleType requiredRole) {
		return true;
	}

}
