/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.access;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.lang.BuildingConsumer;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Class that can be used to mock an inquiry builder and inspect the arguments that were passed to it in unit tests.
 * <p>
 * Example usage:
 *
 * <pre>
 *     // create the MockInquiryBuilder from an inquiry builder interface
 *     final MockInquiryBuilder<FunctionalBlockInquiryBuilder> mockInquiryBuilder = new MockInquiryBuilder<>(FunctionalBlockInquiryBuilder.class);
 *
 *     // specify which methods on the interface may consume a nested builder of the same type
 *     // (i.e. take parameter of type BuildingConsumer<FunctionalBlockInquiryBuilder>
 *     // required so that the arguments passed to nested builders can be captured as well
 *     mockInquiryBuilder.addMethodWithNestedBuilder("withChild");
 *     mockInquiryBuilder.addMethodWithNestedBuilder("withParent");
 *
 *     // call mockBuilder() to get the actual mocked interface - pass it to the respective building consumer
 *     buildingConsumer.prepare(mockInquiryBuilder.mockBuilder());
 *
 *     // now you can inspect the arguments that were set on the inquiry builder
 *
 *     // get argument set directly on the builder instance
 *     final FunctionalBlockType withType = mockInquiryBuilder.getFirstArgument("withType");
 *     // get argument set on a nested builder
 *     final UUID withChild = mockInquiryBuilder.getFirstArgument("withChild", "byUid");
 * </pre>
 *
 * @param <T> the type of the mocked interface
 */
public class MockInquiryBuilder<T> {

	private class Handler implements InvocationHandler {

		private final Map<String, List<Object>> localArgumentCaptorMap;

		private Handler(final Map<String, List<Object>> localArgumentCaptorMap) {
			this.localArgumentCaptorMap = localArgumentCaptorMap;
		}

		@Override
		public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
			final String methodName = method.getName();

			/* handle methods inherited from java.lang.Object */
			switch (methodName) {
				case "hashCode":
					return Objects.hash(proxy);
				case "equals":
					return Objects.equals(proxy, args[0]);
				case "toString":
					return Objects.toString(proxy);
				default:
					break;
			}

			if (methodsWithNestedBuilder.contains(methodName)) {
				@SuppressWarnings("unchecked")
				final BuildingConsumer<T> buildingConsumer = (BuildingConsumer<T>) args[0];
				@SuppressWarnings("unchecked")
				final Map<String, List<Object>> nestedArgumentCaptorMap = (Map<String, List<Object>>) localArgumentCaptorMap.computeIfAbsent(methodName, k ->
						List.of(new HashMap<String, List<Object>>())).get(0);
				final T nestedBuilder = mockBuilder(nestedArgumentCaptorMap);
				buildingConsumer.prepare(nestedBuilder);
			} else {
				localArgumentCaptorMap.put(methodName, Arrays.asList(args));
			}

			/* all methods on the InquiryBuilder interface return "this" */
			return proxy;
		}
	}

	private final Class<T> klass;
	private final Map<String, List<Object>> argumentCaptorMap = new HashMap<>();

	private final Set<String> methodsWithNestedBuilder = new HashSet<>();

	public MockInquiryBuilder(final Class<T> klass) {
		this.klass = klass;
	}

	/**
	 * Register a method on the mocked interface that consumes a nested builder of the same type. The method with the given name
	 * is expected to have a single argument of type {@code BuildingConsumer<T>}.
	 * <p>
	 * Arguments passed to this nested builder will be captured as well.
	 *
	 * @param methodName the name of a method on the mocked interface that consumes a nested builder
	 */
	public void addMethodWithNestedBuilder(final String methodName) {
		methodsWithNestedBuilder.add(methodName);
	}

	/**
	 * Get the mocked builder instance that can be passed to a building consumer.
	 * Any arguments passed to methods called on the mocked instance will be captured and can afterwards be retrieved
	 * with {@link #getArguments(String...)} or {@link #getFirstArgument(String...)}.
	 *
	 * @return the mocked builder
	 */
	public T mockBuilder() {
		return mockBuilder(argumentCaptorMap);
	}

	private T mockBuilder(final Map<String, List<Object>> argumentCaptorMap) {
		final Handler handler = new Handler(argumentCaptorMap);

		@SuppressWarnings("unchecked")
		final T mockInstance = (T) Proxy.newProxyInstance(getClass().getClassLoader(), new Class<?>[] {klass}, handler);

		return mockInstance;
	}

	/**
	 * Retrieve the list of arguments that was passed to the method of the given name on the mock builder instance.
	 * <p>
	 * You can give multiple method names to retrieve arguments set on nested builders. If multiple method names are given, then all but the last
	 * method name must be names of methods that were previously registered with {@link #addMethodWithNestedBuilder(String)}.
	 * <p>
	 * Specifying an invalid method name chain will not throw an exception, but will always return {@code null}.
	 *
	 * @param methodNames the name of the method from which to retrieve the arguments
	 * @return the list of arguments passed to the method, or {@code null} if the method was not called or if an invalid method name was given
	 */
	@Nullable
	public List<Object> getArguments(final String ...methodNames) {
		Map<String, List<Object>> currentMap = argumentCaptorMap;
		for (int i = 0; i < methodNames.length - 1; i++) {
			currentMap = getNestedMap(currentMap.get(methodNames[i]));
			if (currentMap == null) {
				break;
			}
		}

		if (currentMap != null) {
			return currentMap.get(methodNames[methodNames.length - 1]);
		}
		return null;
	}

	/**
	 * Convenience method returning the first argument that was passed to the given method, cast to the desired return type.
	 * <p>
	 * This method works in the same way as {@link #getArguments(String...)} but returns only the first argument that was passed (if any)
	 * and automatically casts it to the desired return type. Use this for conveniece when retrieving the value passed to single-argument
	 * methods on the inquiry builder.
	 *
	 * @param methodNames the name of the method from which to retrieve the arguments
	 * @return the first argument passed to the given method or {@code null} if the method was never called
	 * (note that it's also possible that {@code null} was given as argument)
	 * @param <V> the desired return type of the retrieved value
	 */
	@Nullable
	public <V> V getFirstArgument(final String ...methodNames) {
		final List<Object> arguments = getArguments(methodNames);
		if (arguments != null &&  ! arguments.isEmpty()) {
			@SuppressWarnings("unchecked")
			final V ret = (V) arguments.get(0);
			return ret;
		}
		return null;
	}

	private Map<String, List<Object>> getNestedMap(final Object v) {
		if (v instanceof List) {
			@SuppressWarnings("unchecked")
			final List<?> l = (List<?>) v;
			if (! l.isEmpty()) {
				final Object item = l.get(0);
				if (item instanceof Map) {
					@SuppressWarnings("unchecked")
					final Map<String, List<Object>> map = (Map<String, List<Object>>) item;
					return map;
				}
			}
		}
		return null;
	}
}
