/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.command;

import innowake.mining.plugin.deeplink.server.RequestType;

/**
 * Super class representing a command that can be executed by the deep links server.
 * 
 */
public abstract class Request {
	private Long projectId;
	private RequestType commandType;

	public Long getProjectId() {
		return projectId;
	}

	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
	
	/**
	 * Sets the command type and returns the current instance of command similar to the builder pattern.
	 *
	 * @param commandType The command type of this command instance.
	 * @return The command instance on which the commandtype is set.
	 */
	public Request addCommandType(final RequestType commandType) {
		this.commandType = commandType;
		return this;
	}

	public RequestType getCommandType() {
		return commandType;
	}
	
}
