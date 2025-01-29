/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints.builder;

import java.util.Collection;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointFilterCallback;
import innowake.mining.shared.datapoints.definition.MiningDataPointFilterCallbacks;

/**
 * Implementation of {@link MiningDataPointBuilder.FilterBuilder}
 */
class FilterBuilderImpl implements MiningDataPointBuilder.FilterBuilder {

	@Nullable
	private final MiningDataPointDefinition.ScalarType scalarType;

	@Nullable
	private final String referenceTypeName;

	@Nullable
	private MiningDataPointFilterCallback<?, ?> eq;
	
	@Nullable
	private MiningDataPointFilterCallback<?, ?> notEq;
	
	@Nullable
	private MiningDataPointFilterCallback<?, ?> gte;
	@Nullable
	private MiningDataPointFilterCallback<?, ?> gt;
	
	@Nullable
	private MiningDataPointFilterCallback<?, ?> lte;
	@Nullable
	private MiningDataPointFilterCallback<?, ?> lt;
	
	@Nullable
	private MiningDataPointFilterCallback<?, ?> in;
	
	@Nullable
	private MiningDataPointFilterCallback<?, ?> notIn;
	
	@Nullable
	private MiningDataPointFilterCallback<?, ?> isFalse;
	
	@Nullable
	private MiningDataPointFilterCallback<?, ?> isTrue;
	
	@Nullable
	private MiningDataPointFilterCallback<?, ?> isAbsent;

	public FilterBuilderImpl(@Nullable final MiningDataPointDefinition.ScalarType scalarType, @Nullable final String referenceTypeName) {
		this.scalarType = scalarType;
		this.referenceTypeName = referenceTypeName;
	}

	@Override
	public <B, T> MiningDataPointBuilder.FilterBuilder eq(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(eq, "eq filter can only be defined once!");
		this.eq = cb;
		return this;
	}

	@Override
	public <B, T> MiningDataPointBuilder.FilterBuilder notEq(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(notEq, "notEq filter can only be defined once!");
		this.notEq = cb;
		return this;
	}

	@Override
	public <B, T extends Collection<?>> MiningDataPointBuilder.FilterBuilder in(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(in, "in filter can only be defined once!");
		this.in = cb;
		return this;
	}

	@Override
	public <B, T extends Collection<?>> MiningDataPointBuilder.FilterBuilder notIn(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(notIn, "notIn filter can only be defined once!");
		this.notIn = cb;
		return this;
	}

	@Override
	public <B, T> MiningDataPointBuilder.FilterBuilder isTrue(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(isTrue, "isTrue filter can only be defined once!");
		this.isTrue = cb;
		return this;
	}

	@Override
	public <B, T> MiningDataPointBuilder.FilterBuilder isFalse(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(isFalse, "isFalse filter can only be defined once!");
		this.isFalse = cb;
		return this;
	}

	@Override
	public <B, T> MiningDataPointBuilder.FilterBuilder isAbsent(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(isAbsent, "isAbsent filter can only be defined once!");
		this.isAbsent = cb;
		return this;
	}

	@Override
	public <B, T> MiningDataPointBuilder.FilterBuilder gte(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(gte, "gte filter can only be defined once!");
		this.gte = cb;
		return this;
	}
	@Override

	public <B, T> MiningDataPointBuilder.FilterBuilder gt(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(gt, "gt filter can only be defined once!");
		this.gt = cb;
		return this;
	}

	@Override
	public <B, T> MiningDataPointBuilder.FilterBuilder lte(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(lte, "lte filter can only be defined once!");
		this.lte = cb;
		return this;
	}

	@Override
	public <B, T> MiningDataPointBuilder.FilterBuilder lt(final MiningDataPointFilterCallback<B, T> cb) {
		Assert.assertNull(lt, "lte filter can only be defined once!");
		this.lt = cb;
		return this;
	}

	protected MiningDataPointFilterCallbacks build() {
		final MiningDataPointFilterCallbacks callbacks = new MiningDataPointFilterCallbacks()
				.setAssertiveFilters(isTrue, isFalse, isAbsent)
				.setCollectionFilters(in, notIn)
				.setEqualComparisonFilters(eq, notEq)
				.setNumericComparisonFilters(lte, lt, gte, gt);

		if (scalarType != null) {
			callbacks.setScalarType(scalarType);
		}
		if (referenceTypeName != null) {
			callbacks.setReferenceTypeName(referenceTypeName);
		}

		return callbacks;
	}
}
