/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Model class for {@link MiningDataPointSourceReflectionTest}.
 */
@MiningDataType(name = "TestEnumWithCustomName")
public enum TestEnum {
	FOO,
	BAR
}
