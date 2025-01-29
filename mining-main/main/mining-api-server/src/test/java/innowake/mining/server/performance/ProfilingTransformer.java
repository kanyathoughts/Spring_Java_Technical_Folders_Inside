package innowake.mining.server.performance;

import java.lang.reflect.Method;
import java.security.ProtectionDomain;

import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.api.profiling.ProfilingSession;
import net.bytebuddy.agent.builder.AgentBuilder.Transformer;
import net.bytebuddy.asm.Advice;
import net.bytebuddy.asm.Advice.AllArguments;
import net.bytebuddy.asm.Advice.Enter;
import net.bytebuddy.asm.Advice.OnMethodEnter;
import net.bytebuddy.asm.Advice.OnMethodExit;
import net.bytebuddy.asm.Advice.Origin;
import net.bytebuddy.asm.AsmVisitorWrapper;
import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.dynamic.DynamicType;
import net.bytebuddy.matcher.ElementMatchers;
import net.bytebuddy.utility.JavaModule;

/**
 * Handles the Transformation of the classes to profile.
 */
public class ProfilingTransformer implements Transformer {

	public static final ProfilingSession profilingSession = ProfilingFactory.getProfilingSession();

	@Override
	public DynamicType.Builder<?> transform(final DynamicType.Builder<?> builder,
			final TypeDescription typeDescription,
			final ClassLoader classLoader,
			final JavaModule module,
			final ProtectionDomain protectionDomain) {

		final AsmVisitorWrapper visitor = Advice.to(RecordingAdvices.class)
				.on(ElementMatchers.isMethod().and(this::isNotInProfilingPackage));

		return builder.visit(visitor);
	}

	private boolean isNotInProfilingPackage(final MethodDescription md) {
		return ! md.getDeclaringType().getTypeName().startsWith("innowake.mining.server.performance");
	}

	public static class RecordingAdvices {

		@OnMethodEnter
		static Profiler enter(@Origin final Method method, @AllArguments final Object[] arguments) {
			final String methodName = fixMethodName(method, arguments);
			/* "getIndex" is some kind of Spring interceptor method, but I don't know what it actually does, so I don't remove it for now.
			if ("getIndex".equals(methodName)) {
				return null;
			}
			 */

			final String id = method.getDeclaringClass().getName();
			final Profiler profiler = profilingSession.getProfiler(id);
			profiler.start(methodName);
			return profiler;
		}

		@OnMethodExit
		static void exit(@Enter final Profiler profiler) {
			if (profiler != null) {
				profiler.stop();
			}
		}

		/**
		 * Spring intercepts the methods, resulting in a lot of "invoke" calls, which make the profiling data unreadable.
		 * There for this method changes the name to be profiled from "invoke" to the method called by invoke.
		 * 
		 * This is only done for invoke calls with a number as first parameter.
		 * 
		 * @param method to get the name from (subtracting spring interceptors)
		 * @param arguments of the method called.
		 * @return the method name
		 */
		public static String fixMethodName(final Method method, final Object[] arguments) {
			String methodName = method.getName();
			if ("invoke".equals(methodName)) {
				if (arguments.length > 0 && arguments[0] instanceof Number) {
					final int methodNum = ((Number) arguments[0]).intValue();
					final Method[] targetMethods = method.getDeclaringClass().getMethods();
					if (targetMethods.length >= methodNum) {
						final Method actualMethod = targetMethods[methodNum];
						methodName = actualMethod.getName();
						if ("invoke".equals(methodName) && arguments.length > 2 && arguments[2] instanceof Object[]) {
							methodName = fixMethodName(actualMethod, (Object[])arguments[2]);
						}
					}
				}
			}
			return methodName;
		}
	}
}
