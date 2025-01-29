/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Wrapper class for holding instances of {@link BuildingConsumer}.
 */
public class BuildingConsumerHolder {

	private final BuildingConsumer<?> buildingConsumer;

	public BuildingConsumerHolder(final BuildingConsumer<?> buildingConsumer) {
		this.buildingConsumer = buildingConsumer;
	}

	@SuppressWarnings("unchecked")
	public <T> BuildingConsumer<T> getBuildingConsumer() {
		return (BuildingConsumer<T>) buildingConsumer;
	}
}
