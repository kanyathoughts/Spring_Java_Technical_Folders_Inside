/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import java.beans.ConstructorProperties;
import java.io.Serializable;

import innowake.mining.shared.model.ModuleLocation;

/**
 * Describes the location in the source code where a DataFlowNode is defined.
 */
public class SourceLocation implements Serializable {

	private final Long moduleId;
	private final String moduleName;
	private final ModuleLocation moduleLocation;
	
	@ConstructorProperties({"moduleId", "moduleName", "moduleLocation"})
	public SourceLocation(final Long moduleId, final String moduleName, final ModuleLocation moduleLocation) {
		this.moduleId = moduleId;
		this.moduleName = moduleName;
		this.moduleLocation = moduleLocation;
	}

	/**
	 * Returns the moduleId of the original module this node was defined in
	 *
	 * @return Returns the moduleId
	 */
	public Long getModuleId() {
		return moduleId;
	}

	/**
	 * Returns the name of the original module this node was defined in
	 *
	 * @return Returns the name
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Returns the moduleLocation the node was defined at
	 * 
	 * @return Returns the moduleLocation
	 */
	public ModuleLocation getModuleLocation() {
		return moduleLocation;
	}

	@Override
	public String toString() {
		return "SourceLocation [moduleId=" + moduleId + ", moduleName=" + moduleName + ", moduleLocation=" + moduleLocation + "]";
	}
}