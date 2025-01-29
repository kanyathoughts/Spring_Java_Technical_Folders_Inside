/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.event;

import java.util.Optional;

import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.shared.access.EntityId;

/**
 * Event indicating that a data point source must be queried again for changes.
 */
public class MiningDataPointSourceInvalidatedEvent extends ProjectSpecificEvent {

	private final transient MiningDataPointSource src;

	public MiningDataPointSourceInvalidatedEvent(final MiningDataPointSource src) {
		this(src, Optional.empty());
	}

	public MiningDataPointSourceInvalidatedEvent(final MiningDataPointSource src, final Optional<EntityId> projectId) {
		super(projectId);
		this.src = src;
	}

	/**
	 * Get the {@link MiningDataPointSource} from which the event originated.
	 *
	 * @return The {@link MiningDataPointSource} that was invalidated.
	 */
	public MiningDataPointSource getDataPointSource() {
		return src;
	}

	@Override
	public boolean isToBePublishedToCluster() {
		return false;
	}

}
