/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import brave.Span;
import brave.Tracer;
import brave.Tracing;
import innowake.lib.core.lang.Assert;

/**
 * Serializer for tracing contexts, as these don't implement Serializable and therefore are de-/serialized to maps.
 */
@Component
public class TracingSerializer {
	
	@Autowired
	private Tracing tracing;
	@Autowired
	private Tracer tracer;
	
	/**
	 * Deserializes the given {@code tracingContext}.
	 *
	 * @param tracingContext the tracing context
	 * @return {@link Span} based on the deserialized context
	 */
	public Span deserializeTracingContext(final Map<String, String> tracingContext) {
		return tracer.toSpan(tracing.propagation().extractor(Map<String, String>::get).extract(tracingContext).context());
		
	}
	
	/**
	 * Serializes the the context of the {@link Span} currently in scope.
	 * 
	 * @return the serialized context
	 */
	public Map<String, String> serializeTracingContext() {
		final Map<String, String> tracingContext = new HashMap<>();
		final Span currentSpan = Assert.assertNotNull(tracer.currentSpan(), "There must be an active tracing in the current scope!");
		tracing.propagation().injector(Map<String, String>::put).inject(currentSpan.context(), tracingContext);
		return tracingContext;
	}
}
