/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;

/**
 * Collection of all the {@linkplain MiningDataPointFilterCallback filter callbacks} for each of the different filter operators
 * defined on a data point.
 */
public class MiningDataPointFilterCallbacks {

	public static final MiningDataPointFilterCallbacks EMPTY = new MiningDataPointFilterCallbacks();

	@Nullable
	private MiningDataPointDefinition.ScalarType scalarType;

	@Nullable
	private String referenceTypeName;

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
	
	/**
	 * Default Constructor.
	 */
	public MiningDataPointFilterCallbacks() {
		this.eq = null;
		this.notEq = null;
		this.lte = null;
		this.lt = null;
		this.gte = null;
		this.gt = null;
		this.in = null;
		this.notIn = null;
		this.isTrue = null;
		this.isAbsent = null;
		this.isFalse = null;
	}

	/**
	 * Initialize callback function for equals filter.
	 *
	 * @param eq the equal callback function
	 * @param notEq the not equal callback function
	 * @return the registered MiningDataPointFilterCallbacks
	 */
	public MiningDataPointFilterCallbacks setEqualComparisonFilters(final @Nullable MiningDataPointFilterCallback<?, ?> eq, 
			final @Nullable MiningDataPointFilterCallback<?, ?> notEq)  {
		this.eq = eq;
		this.notEq = notEq;
		return this;
	}
	
	/**
	 * Initialize callback function for greater than or less than filter.
	 *
	 * @param lte the less than or equal callback function
	 * @param lt the less than callback function
	 * @param gte the greater than or equal call back function
	 * @param gt the greater than call back function
	 * @return the registered MiningDataPointFilterCallbacks
	 */
	public MiningDataPointFilterCallbacks setNumericComparisonFilters(
			final @Nullable MiningDataPointFilterCallback<?, ?> lte,
			final @Nullable MiningDataPointFilterCallback<?, ?> lt,
			final @Nullable MiningDataPointFilterCallback<?, ?> gte,
			final @Nullable MiningDataPointFilterCallback<?, ?> gt)  {
		this.lte = lte;
		this.lt = lt;
		this.gte = gte;
		this.gt = gt;
		return this;
	}
	
	/**
	 * Initialize callback function for greater than or less than filter.
	 *
	 * @param isTrue the is true callback function
	 * @param isFalse the is false callback function
	 * @param isAbsent the is absent callback function
	 * @return the registered MiningDataPointFilterCallbacks
	 */
	public MiningDataPointFilterCallbacks setAssertiveFilters(final @Nullable MiningDataPointFilterCallback<?, ?> isTrue, 
			final @Nullable MiningDataPointFilterCallback<?, ?> isFalse, final @Nullable MiningDataPointFilterCallback<?, ?> isAbsent)  {
		this.isTrue = isTrue;
		this.isFalse = isFalse;
		this.isAbsent = isAbsent;
		return this;
	}
	
	/**
	 * Initialize callback function for in and not in collection filters.
	 *
	 * @param in the in callback function
	 * @param notIn the not in callback function
	 * @return the registered MiningDataPointFilterCallbacks
	 */
	public MiningDataPointFilterCallbacks setCollectionFilters(final @Nullable MiningDataPointFilterCallback<?, ?> in,
			final @Nullable MiningDataPointFilterCallback<?, ?> notIn) {
		this.in = in;
		this.notIn = notIn;
		return this;
	}

	/**
	 * Overrides the assumed scalar type of the filter argument. By default, it uses the same type as the data point.
	 * @param scalarType the scalar type to use for the filter argument
	 */
	public void setScalarType(@Nullable final MiningDataPointDefinition.ScalarType scalarType) {
		this.referenceTypeName = null;
		this.scalarType = scalarType;
	}

	/**
	 * Returns the scalar type to use for the filter argument or {@code null} if the type of the data point should be used.
	 * @return the scalar type to use for the filter argument or {@code null}
	 */
	@Nullable
	public MiningDataPointDefinition.ScalarType getScalarType() {
		return scalarType;
	}

	/**
	 * Overrides the assumed type of the filter argument by referencing another schema type by name. By default, the filter argument uses the same type
	 * as the data point.
	 * @param referenceTypeName the type name to use for the filter argument
	 */
	public void setReferenceTypeName(@Nullable final String referenceTypeName) {
		this.scalarType = null;
		this.referenceTypeName = referenceTypeName;
	}

	/**
	 * Returns the type name to use for the filter argument or {@code null} if the type of the data point should be used.
	 * @return the type name to use for the filter argument or {@code null}
	 */
	@Nullable
	public String getReferenceTypeName() {
		return referenceTypeName;
	}

	/**
	 * Returns the filter callback used with the "eq" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getEq() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) eq);
	}
	
	/**
	 * Returns the filter callback used with the "notEq" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getNotEq() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) notEq);
	}
	
	/**
	 * Returns the filter callback used with the "gte" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getGte() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) gte);
	}

	/**
	 * Returns the filter callback used with the "gt" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getGt() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) gt);
	}
	
	/**
	 * Returns the filter callback used with the "lte" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getLte() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) lte);
	}

	/**
	 * Returns the filter callback used with the "lt" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getLt() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) lt);
	}
	
	/**
	 * Returns the filter callback used with the "in" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getIn() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) in);
	}
	
	/**
	 * Returns the filter callback used with the "notIn" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getNotIn() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) notIn);
	}
	
	/**
	 * Returns the filter callback used with the "isFalse" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getIsFalse() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) isFalse);
	}
	
	/**
	 * Returns the filter callback used with the "isTrue" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getIsTrue() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) isTrue);
	}
	
	/**
	 * Returns the filter callback used with the "isAbsent" filter operator.
	 * @return the filter callback, if defined
	 * @param <B> the type of the builder used by the filter callback
	 * @param <T> the type of the value received by the filter callback (the type of the data point)
	 */
	@SuppressWarnings("unchecked")
	public <B, T> Optional<MiningDataPointFilterCallback<B, T>> getIsAbsent() {
		return Optional.ofNullable((MiningDataPointFilterCallback<B, T>) isAbsent);
	}
}
