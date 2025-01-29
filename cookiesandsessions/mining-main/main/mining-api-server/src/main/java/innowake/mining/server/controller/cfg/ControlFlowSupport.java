/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.cfg;

import java.util.*;

import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.dependency.graph.NodeType;

/**
 * Definition of Module types supported in the Control Flow Graph calculation.
 */
public final class ControlFlowSupport {
	
	private ControlFlowSupport() { }
	
	private static final List<NodeType> THEORETICALLY_SUPPORTED_MODULE_TYPES = new ArrayList<>();
	private static final Set<NodeType> ACTUALLY_SUPPORTED_MODULE_TYPES = new HashSet<>();
	
	static {
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.COBOL, Type.PROGRAM));
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.PL1, Type.PROGRAM));
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.PL1, Type.MAINPROGRAM));
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.NATURAL, Type.PROGRAM));
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.NATURAL, Type.SUBPROGRAM));
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.NATURAL, Type.SUBROUTINE));
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.C, Type.PROGRAM));
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.JCL, Type.JOB));
		ACTUALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.JAVA, Type.COMPILATION_UNIT));

		THEORETICALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.COBOL, Type.PROGRAM));
		THEORETICALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.NATURAL, Type.PROGRAM));
		THEORETICALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.CSD, Type.PROGRAM));
		THEORETICALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.EASYTRIEVE, Type.PROGRAM));
		THEORETICALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.NATURAL, Type.SUBPROGRAM));
		THEORETICALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.NATURAL, Type.SUBROUTINE));
		THEORETICALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.PL1, Type.SUBROUTINE));
		THEORETICALLY_SUPPORTED_MODULE_TYPES.add(NodeType.of(Technology.ASSEMBLER, Type.PROGRAM));
	}
	
	/**
	 * Gets the ACTUALLY_SUPPORTED_MODULE_TYPES list.
	 *
	 * @return ACTUALLY_SUPPORTED_MODULE_TYPES
	 */
	public static Set<NodeType> getActuallySupportedTypes() {
		/* this method is only needed until the central feature/function support configuration of WMIN-1573 is implemented */
		return Collections.unmodifiableSet(ACTUALLY_SUPPORTED_MODULE_TYPES);
	}
	
	/**
	 * Gets the THEORETICALLY_SUPPORTED_MODULE_TYPES list.
	 *
	 * @return THEORETICALLY_SUPPORTED_MODULE_TYPES
	 */
	public static List<NodeType> getTheoreticallySupportedTypes() {
		/* this method is only needed until the central feature/function support configuration of WMIN-1573 is implemented */
		return Collections.unmodifiableList(THEORETICALLY_SUPPORTED_MODULE_TYPES);
	}
	
}
