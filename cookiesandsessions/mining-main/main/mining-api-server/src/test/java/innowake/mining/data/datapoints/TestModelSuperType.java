/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

@MiningDataType(name = "TestModelSuperType")
public class TestModelSuperType {

	@Nullable
	private String superTypeProp;

	@Nullable
	public String getSuperTypeProp() {
		return superTypeProp;
	}

	public void setSuperTypeProp(final String superTypeProp) {
		this.superTypeProp = superTypeProp;
	}
}
