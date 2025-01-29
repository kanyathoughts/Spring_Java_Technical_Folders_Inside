/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

/**
 * All constants in Mining schema.
 */
public final class SchemaConstants {
	
	public static final String PROJECT_ID = "id";
	
	public static final String MODULE = "module";
	public static final String MODULE_ID = "id";
	public static final String MODULE_NAME = "name";
	public static final String MODULE_PROJECT_LINK = "projectId";
	
	public static final String ADVANCED_MODULE_LOCATION = "AdvancedModuleLocation";
	public static final String RETRACED_LOCATION_OFFSET = "retracedOffset";
	public static final String RETRACED_LOCATION_LENGTH = "retracedLength";
	
	public static final String ASSEMBLED_LOCATION_OFFSET = "assembledOffset";
	public static final String ASSEMBLED_LOCATION_LENGTH = "assembledLength";
	
	public static final String ROOT_RELATIVE_LOCATION_OFFSET = "rootRelativeOffset";
	public static final String ROOT_RELATIVE_LOCATION_LENGTH = "rootRelativeLength";
	public static final String ROOT_RELATIVE_START_LINE_NUMBER = "rootRelativeStartLineNumber";
	public static final String ROOT_RELATIVE_END_LINE_NUMBER = "rootRelativeEndLineNumber";
	
	public static final String PROPERTY_CLASS = "@class";
	
	public static final String AST_NODE = "AstNode";
	public static final String AST_NODE_TYPE = "type";
	public static final String AST_NODE_LABEL = "label";
	public static final String AST_NODE_MODULE_ID = "moduleId";
	public static final String AST_NODE_MODULE_NAME = "moduleName";
	public static final String AST_NODE_PROJECT_ID = "projectId";
	public static final String AST_NODE_INCLUSION_CALLEE_MODULE_ID = "inclusionCalleeModuleId";
	public static final String AST_NODE_ADVANCED_MODULE_LOCATION = "advancedModuleLocation";
	public static final String AST_NODE_PROPERTIES = "properties";
	public static final String AST_NODE_PARENT = "parent";
	public static final String AST_NODE_CHILDREN = "children";
	public static final String AST_NODE_PREVIOUS_SIBLING = "previousSibling";
	public static final String AST_NODE_NEXT_SIBLING = "nextSibling";
	public static final String AST_NODE_SUPER_TYPES = "superTypes";
	public static final String AST_NODE_ASSEMBLED_OFFSET = "assembledOffset";
	public static final String AST_NODE_RETRACED_OFFSET = "retracedOffset";
	
	public static final String FLOWS_CONTROL = "FlowsControl";
	public static final String FLOWS_CONTROL_LABEL = "label";
	public static final String HAS_AST = "HasAst";
	public static final String REFERS_TO = "RefersTo";
	public static final String REDEFINES = "Redefines";
	public static final String INCLUDES = "Includes";
	public static final String HALT_POINT_LINK = "haltPointLink";
	public static final String RETURN_POINT_LINK = "returnPointLink";
	public static final String ENTRY_POINT_LINK = "entryPointLink";

	public static final String AST_BINDING = "AstBinding";
	public static final String AST_BINDING_LABEL = "label";

	public static final String ENTRY_POINT = "EntryPoint";
	public static final String RETURN_POINT = "ReturnPoint";
	public static final String HALT_POINT = "HaltPoint";
	
	/**
	 * The ID of the user which is used for system-generated data.
	 */
	public static final String SYSTEM_USER = "system_user";
	
	private SchemaConstants() {}
}
