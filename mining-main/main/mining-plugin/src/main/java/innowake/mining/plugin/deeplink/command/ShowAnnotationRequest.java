/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.command;

import innowake.mining.shared.access.EntityId;

/**
 * Class used to represent commands which require a recordId.
 */
public class ShowAnnotationRequest extends Request {
	private EntityId id;

	public EntityId getId() {
		return id;
	}

	public void setId(final EntityId id) {
		this.id = id;
	}

	@Override
	public String toString() {
		return new StringBuilder(64)
				.append("IdCommand [id=")
				.append(id)
				.append(", projectId=")
				.append(getProjectId()).append("]")
				.toString();
	}
}
