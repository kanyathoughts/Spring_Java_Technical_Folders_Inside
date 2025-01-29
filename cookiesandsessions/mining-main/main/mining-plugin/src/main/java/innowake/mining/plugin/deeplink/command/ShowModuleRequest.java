/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.command;

/**
 * Class used to represent commands, which need a moduleId.
 */
public class ShowModuleRequest extends Request {
	private String path;

	public void setPath(final String path) {
		this.path = path;
	}
	
	public String getPath() {
		return path;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ModuleCommand [projectId=").append(getProjectId()).append(", path=").append(path).append("]");
		return builder.toString();
	}
}
