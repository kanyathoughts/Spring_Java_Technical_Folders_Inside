/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import innowake.lib.core.api.lang.Nullable;

/**
 * Stream Builder counting accepted elements.
 * @param <T> the type of stream elements
 */
public class CountingSteamBuilder<T> implements Stream.Builder<T> {
	
	private final AtomicInteger count = new AtomicInteger(0);
	private final Stream.Builder<T> builder = Stream.builder();
	
	public int count() {
		return count.get();
	}
	
	public boolean isEmpty() {
		return count.get() == 0;
	}
	
	@Override
	public void accept(@Nullable T t) {
		builder.accept(t);
		count.incrementAndGet();
	}
	
	@Override
	public Stream<T> build() {
		return builder.build();
	}
	
}
