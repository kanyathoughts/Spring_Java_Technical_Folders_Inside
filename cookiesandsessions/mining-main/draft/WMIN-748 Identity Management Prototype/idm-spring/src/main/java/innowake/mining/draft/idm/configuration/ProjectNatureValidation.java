/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.draft.idm.configuration;

import org.springframework.stereotype.Component;

@Component
public class ProjectNatureValidation {

	public boolean verifyForProject(final Long projectId, final String natureRequired) {
		final String nature = resolve(projectId);
		final boolean result = nature.equals(natureRequired);
		System.out.println( String.format("Verify nature for project id '%d'. Resolved to %s. Required %s. Result %s ",
				projectId, nature, natureRequired, String.valueOf(result)) );
		return result;
	}
	
	private String resolve(final Long projectId) {
		if (projectId.intValue() == 4711 ) {
			return "mining";
		} else if (projectId.intValue() == 4712 ) {
			return "discovery";			
		}
		return "n.a.";
	}
}
 