/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.components.scheduler.connection;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.Client;
import innowake.mining.shared.model.Project;

/**
 * Type for (de)serializing the project/client responses from the server in order to link to the local project.
 */
public class ProjectData {

	private final Long projectId;
	private final String projectName;
	private final Long clientId;
	private final String clientName;
	
	/**
	 * Creates a new object from the given project and server.
	 *  
	 *  Mainly used for serializing.
	 *  
	 * @param project the project retrieved from the server
	 * @param client the corresponding client retrieved from the server
	 */
	public ProjectData(final Project project, final Client client) {
		this(project.getId(), project.getName(), client.getId(), client.getName());
	}
	
	/**
	 * Creates a new object from the given elementary information.
	 * 
	 * Mainly used for deserializing.
	 * 
	 * @param projectId the id of the project
	 * @param projectName the name of the project
	 * @param clientId the id of the client
	 * @param clientName the name of the client
	 */
	public ProjectData(final Long projectId, final String projectName, final Long clientId, final String clientName) {
		this.projectId = projectId;
		this.projectName = projectName;
		this.clientId = clientId;
		this.clientName = clientName;
	}
	
	/**
	 * @return a human readable string, which can be shown to the user
	 */
	public String getViewName() {
		return String.format("%s (%s)", projectName, clientName);
	}
	
	/**
	 * @return the project ID
	 */
	public Long getProjectId() {
		return projectId;
	}
	
	/**
	 * @return the project name
	 */
	public String getProjectName() {
		return projectName;
	}
	
	/**
	 * @return the client ID
	 */
	public Long getClientId() {
		return clientId;
	}
	
	/**
	 * @return the client name
	 */
	public String getClientName() {
		return clientName;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + clientId.hashCode();
		result = prime * result + clientName.hashCode();
		result = prime * result + projectId.hashCode();
		result = prime * result + projectName.hashCode();
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final ProjectData other = (ProjectData) obj;
		if (clientId != other.clientId) {
			return false;
		}
		if ( ! clientName.equals(other.clientName)) {
			return false;
		}
		if (projectId != other.projectId) {
			return false;
		}
		return projectName.equals(other.projectName);
	}
	
}