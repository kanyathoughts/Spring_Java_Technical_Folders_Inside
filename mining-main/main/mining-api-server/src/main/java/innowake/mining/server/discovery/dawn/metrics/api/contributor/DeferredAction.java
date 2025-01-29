/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.contributor;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Annotates a method on a {@link DiscoveryContributor}. The annotated method can then be registered
 * as a deferred action using {@link ModuleBuilder#deferAction(String)}.
 * <p>
 * The method annotated with {@code @DeferredAction} may declare certain types of method parameters
 * which are injected automatically. The supported parameters are:
 * <ul>
 * 	<li> {@link DiscoveryBuilder} injects a DiscoveryBuilder
 * 	<li> {@link DiscoveryContext} injects the current DiscoveryContext
 * 	<li> {@link ModuleBuilder} injects a ModuleBuilder for the module on which the action was deferred
 * 	<li> {@link Module} injects the Module on which the action was deferred
 * 	<li> {@link ModuleLightweightPojo} injects the light weight Module containing minimal information on which the action was deferred</li>
 * 	<li> {@link SourcePojo} injects the SourcePojo of the module on which the action was deferred 
 *       - available only if that module is defined inside a source file, otherwise {@code null} will be injected 
 * 	<li> additionally, a context object of arbitrary type that was previously passed to {@link ModuleBuilder#deferAction(String, java.io.Serializable)} can be injected
 * </ul>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface DeferredAction {

	/**
	 * An optional name for the deferred action - if not present, the method name will be used instead.
	 *
	 * @return the name of the deferred action
	 */
	public String value() default "";
}
