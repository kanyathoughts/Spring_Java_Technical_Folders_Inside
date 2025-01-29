/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Optional;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.retrace.Retracer;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.assembling.retrace.RetracingSourceOffset;

/**
 * Translates a location information from assembled content to the actual module information.
 * @param <T> the type of the {@link IAssembling}.
 * @param <L> the Object representing the information to identify the module.
 */
public class AssemblyToModuleLocationProvider<T, L> {	

	protected static final Integer INVALID_OFFSET = Integer.valueOf(-1);
	protected final ModuleIdentifier<T, L> moduleIdentifier;
	private final Retracing<T> retracing;
	
	/**
	 * Constructor
	 * @param moduleIdentifier used to extract the module information from the assembly type
	 * @param assembling the {@link IAssembling} used to retrace an offset back to the original location information
	 */
	public AssemblyToModuleLocationProvider(final ModuleIdentifier<T, L> moduleIdentifier, final IAssembling<T> assembling) {
		this.moduleIdentifier = moduleIdentifier;
		retracing = new Retracer<T>().retrace(assembling);
	}
	
	/**
	 * Translates an offset from the assembled content to the information to identify the module together with the offset inside the module.
	 * If the offset cannot be resolved to a module {@link Optional#empty()} is returned for the module information with 
	 * {@value AssemblyToModuleLocationProvider#INVALID_OFFSET} as offset. 
	 * 
	 * @param offset the offset inside the assembled content
	 * @return a {@link Tuple2} where {@link Tuple2#a} contains the information to identify the module and {@link Tuple2#b} contains the offset inside the module
	 * 
	 */
	public Tuple2<Optional<L>, Integer> resolveOffset(final int offset) {
		final RetracingSourceOffset<T> sourceObject = retracing.retrace(offset);
		if (sourceObject != null) {
			final T originalSourceObject = sourceObject.getOriginal();
			if (originalSourceObject != null) {
				return new Tuple2<>(Optional.of(moduleIdentifier.identifyModule(originalSourceObject)), Integer.valueOf(sourceObject.getOffset()));
			}
		}
		return new Tuple2<>(Optional.empty(), INVALID_OFFSET);
	}
	
	/**
	 * Extracts the module information to identify a module from a type used by an {@link IAssembling}.
	 * 
	 * @param <T> the type of the {@link IAssembling}.
	 * @param <L> the Object representing the information to identify the module.
	 */
	@FunctionalInterface
	public interface ModuleIdentifier<T, L> {
		
		/**
		 * Extracts the module information form a source object.
		 * 
		 * @param sourceObject the object where the module information is extracted from
		 * @return the information to identify the module
		 */
		L identifyModule(T sourceObject);
	}
}
