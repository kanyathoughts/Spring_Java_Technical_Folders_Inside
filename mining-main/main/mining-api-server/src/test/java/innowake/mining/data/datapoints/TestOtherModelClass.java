/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Model class for {@link MiningDataPointSourceReflectionTest}
 */
@MiningDataType(name = "TestOtherModelClassWithCustomName")
@NonNullByDefault(false) /* disable null checks for this test model class */
public class TestOtherModelClass {
	
	private String testStringProp;

	public String getTestStringProp() {
		return testStringProp;
	}

	public void setTestStringProp(String testStringProp) {
		this.testStringProp = testStringProp;
	}
}
