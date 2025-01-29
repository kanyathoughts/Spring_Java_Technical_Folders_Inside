/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.contributor;

import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Builder interface used by contributors that operate on source files.
 */
public interface DiscoveryBuilderFromSource extends DiscoveryBuilder {
	/**
	 * Declares a "root" module, i.e. a module that represents the entire source file. Only one such module can exist per file, so this method
	 * should only be called once. If multiple contributors declare a root module for the same file, the declarations are merged.
	 *
	 * @param name the name of the module
	 * @param type the type of the module
	 * @return a module builder
	 */
	ModuleBuilder declareRootModule(String name, ModuleType type);

	/**
	 * Declares a "sub" module, i.e. a module that represents a part of the source file or a logically nested module. Only one such module with the same
	 * name and type can exist per file. If multiple contributors declare the same module for the same file, the declarations are merged.
	 * 
	 * @param name the name of the module
	 * @param type the type of the module
	 * @return a module builder
	 */
	ModuleBuilder declareSubModule(String name, ModuleType type);

	/**
	 * Declares a "sub" module, i.e. a module that represents a part of the source file. This overload allows to specify the location of
	 * the sub module inside of the source file.
	 * 
	 * @param name the name of the module
	 * @param type the type of the module
	 * @param location the location of the sub module inside of the source file
	 * @return a module builder
	 */
	ModuleBuilder declareSubModule(String name, ModuleType type, ModuleLocation location);
}