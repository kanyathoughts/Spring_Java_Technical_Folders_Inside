/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.natural;

import innowake.lib.core.lang.Assert;
import innowake.lib.core.lang.Nullable;
import innowake.ndt.naturalparser.ast.NaturalNode;

/**
 * Redefinition for model of {@link DefineDataParser}.
 * <p>
 * Almost originally copied from {@code /ndt-analysis-scripts/expert-base/innowake/expert/base/ndt/parser/definedata/Redefinition.java}.
 * Should be adapted to code guidelines if someone finds time.
 */
public class Redefinition extends Group {

	private String redefinedFieldName;
	@Nullable
	private DataField redefinedField;
	
	Redefinition(final String name, final FieldType type, final NaturalNode node, final String redefinedFieldName) {
		super(name, type, node);
		this.redefinedFieldName = redefinedFieldName;
	}
	
	void setRedefinedField(final DataField redefinedField) {
		this.redefinedField = redefinedField;
	}
	
	@Override
	public DataField getRedefinedField() {
		return Assert.assertNotNull(redefinedField);
	}
	
	public String getRedefinedFieldName() {
		return redefinedFieldName;
	}
	
	@Override
	public boolean equals(@Nullable Object obj) {
		return super.equals(obj);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
	
}
