/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import innowake.lib.core.api.lang.Nullable;

/**
 * Class representing the module that has a link from a given module.
 */
public class LinkedModule {

	@Nullable private String modulePath;
	@Nullable private ModuleLocation toLocation;
	@Nullable private ModuleLocation fromLocation;
	
	/**
	 * Constructor to initialize {@link LinkedModule} instance.
	 *
	 * @param modulePath the module path of the target module
	 * @param toLocation the location inside the target module
	 * @param fromLocation the location in the source module from which the target module is connected
	 */
	public LinkedModule(@Nullable final String modulePath, @Nullable final ModuleLocation toLocation, final ModuleLocation fromLocation) {
		this.toLocation = toLocation;
		this.modulePath = modulePath;
		this.fromLocation = fromLocation;
	}
	
	/**
	 * Creates an empty {@link LinkedModule} instance.
	 */
	public LinkedModule() {}
	
	/**
	 * Returns the module path of the target module.
	 *
	 * @return module path of the target module
	 */
	@Nullable
	public String getModulePath() {
		return modulePath;
	}
	
	/**
	 * Returns the location inside the target module.
	 *
	 * @return location inside the target module
	 */
	@Nullable
	public ModuleLocation getToLocation() {
		return toLocation;
	}
	
	/**
	 * Returns the location from the source module
	 *
	 * @return location from the source module
	 */
	@Nullable
	public ModuleLocation getFromLocation() {
		return fromLocation;
	}
	
}
