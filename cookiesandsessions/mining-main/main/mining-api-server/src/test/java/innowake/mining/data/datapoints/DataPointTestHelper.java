/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;
import java.util.UUID;

import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * Helper class for Data Point Test classes.
 */
public class DataPointTestHelper {

	/**
	 * Creates an instance of {@linkplain DataPointRegistry} using the provided {@linkplain MiningDataPointSource data point sources}.
	 *
	 * @param sources the list of {@linkplain MiningDataPointSource data point sources}
	 * @return the {@linkplain DataPointRegistry} instance
	 */
	public static DataPointRegistry getRegistry(final MiningDataPointSource... sources) {
		return getRegistry(false, sources);
	}

	/**
	 * Creates an instance of {@linkplain DataPointRegistry} using the provided {@linkplain MiningDataPointSource data point sources}.
	 * If {@code markProjectAsDeleted} is {@code true} then the returned DataPointRegistry will see all projects as "marked for deletion".
	 *
	 * @param markProjectAsDeleted whether all projects should be marked for deletion
	 * @param sources the list of {@linkplain MiningDataPointSource data point sources}
	 * @return the {@linkplain DataPointRegistry} instance
	 */
	public static DataPointRegistry getRegistry(boolean markProjectAsDeleted, final MiningDataPointSource... sources) {
		final Optional<ProjectPojo> project = Optional.ofNullable(new ProjectPojo(UUID.randomUUID(), CustomPropertiesMap.empty(), EntityId.VOID, null, null, 
				999l, "Mock Project",
				markProjectAsDeleted, null, null, "", null, Collections.emptyList(), null, null, Collections.emptyMap(), null));
		final ProjectService projectDaoMock = Mockito.mock(ProjectService.class);
		Mockito.when(projectDaoMock.find((EntityId) ArgumentMatchers.any())).thenReturn(project);
		final DataPointRegistry registry = new DataPointRegistry((event) -> {}, projectDaoMock);
		registry.initializeDataPointSources(Arrays.asList(sources));
		return registry;
	}

}
