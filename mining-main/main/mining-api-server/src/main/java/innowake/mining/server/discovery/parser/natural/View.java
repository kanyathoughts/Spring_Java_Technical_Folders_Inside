/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.natural;

import innowake.lib.core.lang.Assert;
import innowake.lib.core.lang.Nullable;
import innowake.ndt.naturalparser.ast.NaturalNode;

/**
 * View for model of {@link DefineDataParser}.
 * <p>
 * Almost originally copied from {@code /ndt-analysis-scripts/expert-base/innowake/expert/base/ndt/parser/definedata/View.java}.
 * Should be adapted to code guidelines if someone finds time.
 */
public class View extends Group {

	@Nullable
	private String ddmName;

	View(final String name, final FieldType type, final NaturalNode node) {
		super(name, type, node);
	}
	
	void setDDMName(final String ddmName) {
		this.ddmName = ddmName;
	}

	public String getDDMName() {
		return Assert.assertNotNull(ddmName);
	}
	
	@Override
	public boolean equals(@Nullable final Object obj) {
		return super.equals(obj);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
	
}
