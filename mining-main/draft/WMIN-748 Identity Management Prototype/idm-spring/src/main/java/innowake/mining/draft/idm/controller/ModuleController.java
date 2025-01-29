/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.draft.idm.controller;

import static innowake.mining.draft.idm.configuration.Nature.Type.MINING;
import static innowake.mining.draft.idm.configuration.Role.Type.EDITOR;
import static innowake.mining.draft.idm.configuration.Role.Type.VIEWER;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import innowake.mining.draft.idm.configuration.Nature;
import innowake.mining.draft.idm.configuration.RequireDiscoveryAdminRole;
import innowake.mining.draft.idm.configuration.RequireDiscoveryEditorRole;
import innowake.mining.draft.idm.configuration.RequireDiscoveryManagerRole;
import innowake.mining.draft.idm.configuration.RequireDiscoveryViewerRole;
import innowake.mining.draft.idm.configuration.RequireMiningAdminRole;
import innowake.mining.draft.idm.configuration.RequireMiningEditorRole;
import innowake.mining.draft.idm.configuration.RequireMiningManagerRole;
import innowake.mining.draft.idm.configuration.RequireMiningViewerRole;
import innowake.mining.draft.idm.configuration.Role;

@RestController
@RequestMapping(value="/api/", produces=MediaType.APPLICATION_JSON_VALUE)
public class ModuleController {

	@GetMapping(value="/public/projects/{projectId}")
	public String publicCall(@PathVariable final Long projectId) {
		return msg(projectId, "n.a.", "n.a.");
	}
	
	@GetMapping(value="/v1/projects/{projectId}/mining/admin")
	@RequireMiningAdminRole
	public String minAdminCall(@PathVariable final Long projectId) {
		return msg(projectId, "mining", "admin");
	}

	@GetMapping(value="/v1/projects/{projectId}/mining/manager")
	@RequireMiningManagerRole
	public String minManagerCall(@PathVariable final Long projectId) {
		return msg(projectId, "mining", "manager");
	}

	@GetMapping(value="/v1/projects/{projectId}/mining/editor")
	@RequireMiningEditorRole
	public String minEditorCall(@PathVariable final Long projectId) {
		return msg(projectId, "mining", "editor");
	}

	@GetMapping(value="/v1/projects/{projectId}/mining/viewer")
	@RequireMiningViewerRole
	public String minViewerCall(@PathVariable final Long projectId) {
		return msg(projectId, "mining", "viewer");
	}
	
	@GetMapping(value="/v1/projects/{projectId}/discovery/admin")
	@RequireDiscoveryAdminRole
	public String disAdminCall(@PathVariable final Long projectId) {
		return msg(projectId, "discovery", "admin");
	}

	@GetMapping(value="/v1/projects/{projectId}/discovery/manager")
	@RequireDiscoveryManagerRole
	public String disManagerCall(@PathVariable final Long projectId) {
		return msg(projectId, "discovery", "manager");
	}

	@GetMapping(value="/v1/projects/{projectId}/discovery/editor")
	@RequireDiscoveryEditorRole
	public String disEditorCall(@PathVariable final Long projectId) {
		return msg(projectId, "discovery", "editor");
	}

	@GetMapping(value="/v1/projects/{projectId}/discovery/viewer")
	@RequireDiscoveryViewerRole
	public String disViewerCall(@PathVariable final Long projectId) {
		return msg(projectId, "discovery", "viewer");
	}
	
	@GetMapping(value="/v1/projects/{nonDefaultProjectIdName}/custom")
	@Role({EDITOR, VIEWER})
	@Nature( value = {MINING}, projectId = "#nonDefaultProjectIdName")
	public String custom(@PathVariable final Long nonDefaultProjectIdName) {
		return msg(nonDefaultProjectIdName, "custom", "custom");
	}

	@GetMapping(value="/v1/projects/{projectId}/defaultcustom")
	@Role({EDITOR, VIEWER})
	@Nature({MINING})
	public String defaultProjectIdParameterName(@PathVariable final Long projectId) {
		return msg(projectId, "custom", "custom");
	}

	
	private String msg(final Long projectId, final String nature, final String role) {
		return String.format("Success on call a method which require role '%s' on nature '%s' on project '%d'", role, nature, projectId);
	}
}
