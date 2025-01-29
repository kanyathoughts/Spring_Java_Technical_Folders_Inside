/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import java.util.HashSet;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.client.HttpClientErrorException;

import com.google.gson.JsonParseException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.service.KeycloakAuthorizationManagementService;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.security.RoleType;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;

/**
 * Migrate clients and projects to Keycloak.
 * Will check if Keycloak admin credentials are provided in command-line arguments or else ask user.
 * If a project or client already exists then it will not be changed.
 */
@MiningRestController
@Profile(Profiles.AUTH)
public class KeycloakMigrationController {

	@Autowired
	private KeycloakAuthorizationManagementService keycloakAuthMgmtSvc;

	@Autowired
	private ProjectService projectService;
	
	@Autowired
	private ClientService clientService;

	private static final Logger LOG = LoggerFactory.getLogger(KeycloakMigrationController.class);
	
	private static class LogRecorder {
		private final StringBuilder log = new StringBuilder();
		
		public void info(String info) {
			LOG.info(info);
			log.append(info);
			log.append(System.lineSeparator());
		}
		
		@Override
		public String toString() {
			return log.toString();
		}
	}

	@PostMapping("/api/v1/migrateToKeycloak")
	@Role(RoleType.ADMIN)
	@Operation(summary = "Migrate to Keycloak", operationId = "migrateToKeycloak")
	@ApiResponse(responseCode = "200", description = "if the request was successful")
	public ResponseEntity<String> runMigration() {
		LogRecorder log = new LogRecorder();
		try {
			doClientMigration(keycloakAuthMgmtSvc, log);
			doProjectMigration(keycloakAuthMgmtSvc, log);
		} catch (final JsonParseException e) {
			LOG.error(() -> "An error occured while migrating to Keycloak. Restart the server and try again.", e);
			throw e;
		} catch (final HttpClientErrorException e) {
			LOG.error(() -> "A HTTP error occured while migrating to Keycloak."
					+ " If more than five minutes have passed since starting the server the authentication token might no longer be valid."
					+ " Please create a ticket!", e);
			throw e;
		}
		log.info("Migration to Keycloak is done.");
		return ResponseEntity.ok().contentType(MediaType.TEXT_PLAIN).body(log.toString());
	}

	private void doClientMigration(final KeycloakAuthorizationManagementService kams, LogRecorder log) {
		for (final ClientPojo client : clientService.find(q -> q.withIdAbove(0l).sortNid(SortDirection.ASCENDING))) {
			if (kams.doesClientGroupExist(client.getId())) {
				log.info(String.format("Skipping clientId=%d because a group already exists.", client.getId()));
			} else {
				log.info(String.format("Creating clientId=%d.", client.getId()));
				kams.createClientAttributes(client.getId());
			}
		}
	}

	private void doProjectMigration(final KeycloakAuthorizationManagementService kams, LogRecorder log) {
		final Set<ProjectNature> defaultNatures = new HashSet<>();
		defaultNatures.add(ProjectNature.MINING);

		for (final ProjectPojo project : projectService.find(q -> q.withIdAbove(0l))) {
			if (kams.doProjectRolesExist(project.getClientNid(), project.getId())) {
				log.info(String.format(
						"Skipping clientId=%d,projectId=%d because roles already exist.",
						project.getClientNid(),
						project.getId()));
			} else {
				log.info(String.format("Creating clientId=%d,projectId=%d with natures %s.", project.getClientNid(), project.getId(), defaultNatures));
				kams.createProjectAttributes(project.getClientNid(), project.getId(), defaultNatures);
			}
		}
	}

}
