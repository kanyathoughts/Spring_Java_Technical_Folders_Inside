/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import innowake.lib.core.api.lang.Nullable;

public class TestModelSubType extends TestModelSuperType {

	@Nullable
	private String subTypeProp;

	@Nullable
	public String getSubTypeProp() {
		return subTypeProp;
	}

	public void setSubTypeProp(final String subTypeProp) {
		this.subTypeProp = subTypeProp;
	}
}
