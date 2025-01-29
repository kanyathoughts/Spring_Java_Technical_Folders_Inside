/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.event;

import java.io.Serializable;
import java.util.Optional;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;

/**
 * Abstract base class for application events that must be relayed to other instances in the Hazelcast cluster.
 */
public abstract class ClusteredEvent implements Serializable {

	@Nullable
	private UUID publishingMemberId;

	Optional<UUID> getPublishingMemberId() {
		return Optional.ofNullable(publishingMemberId);
	}

	public void setPublishingMemberId(final UUID publishingMemberId) {
		this.publishingMemberId = publishingMemberId;
	}

	public boolean isToBePublishedToCluster() {
		/* only publish events on the cluster that have originated locally */
		return publishingMemberId == null;
	}

}
