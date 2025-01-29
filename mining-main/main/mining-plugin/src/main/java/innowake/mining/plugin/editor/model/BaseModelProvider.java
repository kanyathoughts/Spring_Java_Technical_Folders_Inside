/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.model;

import java.util.Optional;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.plugin.editor.model.ModelResult.Status;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.parsing.Parser;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.base.BaseParser;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.core.parsing.base.BindingConfiguration;

/**
 * An abstract base class for all language specific model providers.
 * 
 * @param <T> type of model provider 
 * @param <M> the type of AST model.
 */
public abstract class BaseModelProvider<T, M extends AstModel> implements IModelProvider<T> {
	
	private final Timer timer = new Timer();
	@Nullable private TimerTask timerTask;
	@Nullable private CompletableFuture<Optional<M>> currentFuture;
	private final Cache<T, ModelResult> modelCache = CacheBuilder.newBuilder()
	                                                             .maximumSize(1000)
	                                                             .build();
	
	@Override
	public Optional<ModelResult> getModel(final T module) {
		return Optional.ofNullable(modelCache.getIfPresent(module));
	}

	@Override
	public void updateModel(final T module) {
		final ModelResult modelResult = modelCache.getIfPresent(module);
		modelCache.put(module, new ModelResult(Status.ERROR, null));
		if (modelResult != null) {
			if (timerTask != null) {
				timerTask.cancel();
				timer.purge();
			}
			timerTask = getParserTask(module);
			timer.schedule(timerTask, 3000);
		} else {
			timer.schedule(getParserTask(module), 0);
		}
	}

	@Override
	public void invalidateModel(final T module) {
		modelCache.invalidate(module);
	}
	
	/**
	 * Instantiates the {@link BaseParser}. To be implemented by the language specific model provider.
	 *
	 * @return the instance of language specific instance of {@link BaseParser}
	 */
	protected abstract Parser<T, M> createParser();
	
	private TimerTask getParserTask(final T document) {
		return new TimerTask() {
			@Override
			public void run() {
				if (currentFuture != null &&  ! currentFuture.isDone()) {
					Assert.assertNotNull(currentFuture).cancel(true);
				}
				final CompletableFuture<Optional<M>> future = CompletableFuture.supplyAsync(() -> {
					final Parser<T, M> parser = createParser();
					return parser.parse(document);
				});
				try {
					currentFuture = future;
					final Optional<M> currentModel = future.get(10, TimeUnit.SECONDS);
					if (currentModel.isPresent()) {
						modelCache.put(document, new ModelResult(Status.OK, currentModel.get()));
					} else {
						modelCache.put(document, new ModelResult(Status.ERROR, null));
					}
				} catch (final InterruptedException | ExecutionException e) {
					modelCache.put(document, new ModelResult(Status.ERROR, null));
					Thread.currentThread().interrupt();
				} catch (final TimeoutException e) {
					modelCache.put(document, new ModelResult(Status.TIMEOUT, null));
				}
			}
		};
	}
	
	/**
	 * Creates the parser configuration which can be re used by language specific model providers.
	 *
	 * @return instance of {@link BaseParserConfiguration}
	 */
	protected BaseParserConfiguration<T> createParserConfiguration() {
		return new BaseParserConfiguration.Builder<T>()
				.setAssemblingDataProvider(getAssemblingDataProvider())
				.setNodeBindingConfiguration(BindingConfiguration.RESOLVE_ALL)
				.enableNodeLookup(true)
				.build();
	}
	
	/**
	 * Configures the assembling data provider for a language specific model provider.
	 *
	 * @return instance of {@link IAssemblingDataProvider}
	 */
	protected abstract IAssemblingDataProvider<T> getAssemblingDataProvider();

}
