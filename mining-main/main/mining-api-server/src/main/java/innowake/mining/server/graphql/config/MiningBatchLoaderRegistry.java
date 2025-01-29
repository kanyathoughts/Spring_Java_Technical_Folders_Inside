/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.config;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Consumer;

import org.apache.commons.lang3.tuple.Pair;
import org.dataloader.BatchLoaderEnvironment;
import org.dataloader.DataLoaderOptions;
import org.springframework.graphql.execution.BatchLoaderRegistry;
import org.springframework.graphql.execution.DefaultBatchLoaderRegistry;
import org.springframework.stereotype.Service;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * Ugly workaround for built-in DefaultBatchLoaderRegistry.
 * <p>
 * The {@link DefaultBatchLoaderRegistry} has a bug (or maybe working as intended?) that makes it incompatible
 * with our use case. It seems that it assumes that there is only a single instance
 * of {@link org.springframework.graphql.execution.GraphQlSource}, but we are actually creating (at least) one per project.
 * This leads to errors about duplicate registrations.
 * <p>
 * This implementation "de-duplicates" the loader registrations to avoid the errors.
 */
@Service
public class MiningBatchLoaderRegistry extends DefaultBatchLoaderRegistry {

	private static class NoOpRegistrationSpec<K, V> implements BatchLoaderRegistry.RegistrationSpec<K, V> {

		@Override
		public BatchLoaderRegistry.RegistrationSpec<K, V> withName(final String name) {
			return this;
		}

		@Override
		public BatchLoaderRegistry.RegistrationSpec<K, V> withOptions(final Consumer<DataLoaderOptions> optionsConsumer) {
			return this;
		}

		@Override
		public BatchLoaderRegistry.RegistrationSpec<K, V> withOptions(final DataLoaderOptions options) {
			return this;
		}

		@Override
		public void registerBatchLoader(final BiFunction<List<K>, BatchLoaderEnvironment, Flux<V>> loader) {
			/* no-op */
		}

		@Override
		public void registerMappedBatchLoader(final BiFunction<Set<K>, BatchLoaderEnvironment, Mono<Map<K, V>>> loader) {
			/* no-op */
		}
	}

	private final Set<String> existingNames = new HashSet<>();
	private final Set<Pair<Class<?>, Class<?>>> existingPairs = new HashSet<>();

	@Override
	public <K, V> RegistrationSpec<K, V> forTypePair(final Class<K> keyType, final Class<V> valueType) {
		if (existingPairs.add(Pair.of(keyType, valueType))) {
			return super.forTypePair(keyType, valueType);
		} else {
			return new NoOpRegistrationSpec<>();
		}
	}

	@Override
	public <K, V> RegistrationSpec<K, V> forName(final String name) {
		if (existingNames.add(name)) {
			return super.forName(name);
		} else {
			return new NoOpRegistrationSpec<>();
		}
	}
}
