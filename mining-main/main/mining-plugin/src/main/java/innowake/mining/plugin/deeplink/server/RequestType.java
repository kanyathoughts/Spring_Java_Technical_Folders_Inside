/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.deeplink.server;

import innowake.mining.plugin.deeplink.command.*;

/**
 * This enumeration manages the types of commands, and the command classes they are parsed to.
 */
public enum RequestType {
	SHOW_MODULE(ShowModuleRequest.class),
	SHOW_ANNOTATION(ShowAnnotationRequest.class);
	
	final Class<? extends Request> type;
	
	<T extends Request> RequestType(Class<T> type) {
		this.type = type;
	}
}
