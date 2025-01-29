/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base.ui;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.ui.commands.ICommandService;

import innowake.mining.plugin.Logging;

/**
 * Utility class to execute commands.
 */
public final class CommandUtil {

	private CommandUtil() {}

	/**
	 * Executes a command with parameters.
	 *
	 * @param commandId the command ID being executed
	 * @param parameters the parameters to qualify the execution
	 */
	public static void executeCommand(final String commandId, final Map<?, ?> parameters) {
		final Command command = assertNotNull(WorkbenchUtil.getActivePart()).getSite().getService(ICommandService.class).getCommand(commandId);
		try {
			command.executeWithChecks(new ExecutionEvent(command, parameters, null, null));
		} catch (final ExecutionException | NotDefinedException | NotEnabledException | NotHandledException e) {
			Logging.error("Error while executing command.", e);
		}
	}
}
