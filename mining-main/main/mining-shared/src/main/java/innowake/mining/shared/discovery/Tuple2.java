/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery;

import java.util.function.Function;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import innowake.lib.core.lang.Nullable;

/**
 * A finite ordered list of elements of different types.
 * 
 * @param <T1> Type of the first element
 * @param <T2> Type of the second element
 */
public class Tuple2<T1, T2> {
	
	public final T1 e1;
	public final T2 e2;
	
	/**
	 * Creates a new tuple with two elements.
	 * 
	 * @param e1 the first element in the tuple
	 * @param e2 the second element in the tuple
	 */
	public Tuple2(final T1 e1, final T2 e2) {
		this.e1 = e1;
		this.e2 = e2;
	}
	
	/**
	 * Maps over the elements in this tuple.
	 *
	 * @param f1 the function to apply on {@link #e1}
	 * @param f2 the function to apply on {@link #e2}
	 * @return a new tuple with results of f1 and f2
	 */
	public <O1,O2> Tuple2<O1, O2> map(final Function<T1, O1> f1, final Function<T2, O2> f2) {
		return new Tuple2<>(f1.apply(e1), f2.apply(e2));
	}
	
	@Override
	public int hashCode() {
		return new HashCodeBuilder(17, 37)
				.append(e1)
				.append(e2)
				.toHashCode();
	}
	
	@Override
	public boolean equals(final @Nullable Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final Tuple2<?, ?> rhs = (Tuple2<?, ?>) obj;
		return new EqualsBuilder()
				.append(e1, rhs.e1)
				.append(e2, rhs.e2)
				.isEquals();
	}

}
