/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import static innowake.mining.shared.io.WorkbookDefinition.SHEET_DEAD_CODE;
import static innowake.mining.shared.io.WorkbookDefinition.SHEET_DEPENDENCIES;
import static innowake.mining.shared.io.WorkbookDefinition.SHEET_ERRORS;
import static innowake.mining.shared.io.WorkbookDefinition.SHEET_MODULES;
import static innowake.mining.shared.io.WorkbookDefinition.SHEET_SQL;
import static innowake.mining.shared.io.WorkbookDefinition.SHEET_STATEMENTS;
import static innowake.mining.shared.io.WorkbookDefinition.SHEET_UNDISCOVERED;

import java.util.HashMap;
import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.Technology;

/**
 * Resolver for {@link RowCallback}.
 */
class RowCallbackResolver {

	private static final String MESSAGE = "Resolving callback for '%s' requires processing of callback for '" + SHEET_MODULES + "'";
	
	private final EntityId projectId;
	private final String fileId;
	private final ModuleService moduleService;

	@Nullable
	private ModulesCallback modulesCallback;
	@Nullable
	private Map<Long, EntityId> modulesNidToEid;
	@Nullable
	private Map<Long, Technology> modulesNidToTechnology;
	
	private ModuleParameters moduleParameters;
	
	RowCallbackResolver(final EntityId projectId, final String fileId, final ModuleService moduleService, final ModuleParameters moduleParameters) {
		this.projectId = projectId;
		this.fileId = fileId;
		this.moduleService = moduleService;
		this.moduleParameters = moduleParameters;
	}
	
	/**
	 * Returns a {@link RowCallback} depending on the given sheet name.
	 *
	 * @param sheetName the sheet name
	 * @return a {@link RowCallback}
	 */
	RowCallback resolve(final String sheetName) {
		switch (sheetName) {
			case SHEET_MODULES: 
				modulesCallback = new ModulesCallback(projectId, fileId, moduleService, moduleParameters);
				return modulesCallback;
				
			case SHEET_STATEMENTS: 
				if (modulesNidToEid != null && modulesNidToTechnology != null) {
					return new StatementsCallback(projectId, fileId, moduleService, modulesNidToEid, modulesNidToTechnology, false);
				} else {
					throw new IllegalStateException(String.format(MESSAGE, SHEET_STATEMENTS));
				}
				
			case SHEET_SQL: 
				if (modulesNidToEid != null && modulesNidToTechnology != null) {
					return new StatementsCallback(projectId, fileId, moduleService, modulesNidToEid, modulesNidToTechnology, true);
				} else {
					throw new IllegalStateException(String.format(MESSAGE, SHEET_SQL));
				}
			
			case SHEET_DEPENDENCIES: 
				if (modulesNidToEid != null) {
					return new DependenciesCallback(projectId, fileId, moduleService, modulesNidToEid, moduleParameters);
				} else {
					throw new IllegalStateException(String.format(MESSAGE, SHEET_DEPENDENCIES));
				}
			
			case SHEET_ERRORS: 
				if (modulesNidToEid != null) {
					return new ErrorsCallback(projectId, fileId, moduleService, modulesNidToEid);
				} else {
					throw new IllegalStateException(String.format(MESSAGE, SHEET_ERRORS));
				}
				
			case SHEET_UNDISCOVERED: 
				return new UndiscoveredCallback(projectId, fileId, moduleService);
				
			case SHEET_DEAD_CODE: 
				if (modulesNidToEid != null) {
					return new DeadCodeCallback(projectId, fileId, moduleService, modulesNidToEid);
				} else {
					throw new IllegalStateException(String.format(MESSAGE, SHEET_DEAD_CODE));
				}

			default: throw new ConstraintViolationException(String.format("Error while parsing Excel '%s'. Unknown sheet name '%s'. ", fileId, sheetName));
		}
	}
	
	/**
	 * Must be called after a {@link RowCallback} for the given {@code sheetName} finished the sheet.
	 *
	 * @param sheetName the sheet name
	 */
	void done(final String sheetName) {
		if (SHEET_MODULES.equals(sheetName)) {
			final var modulesCallbackLocal = modulesCallback;
			if (modulesCallbackLocal != null) {
				if (modulesNidToEid != null) {
					modulesNidToEid.putAll(modulesCallbackLocal.getModuleIdMap());
				} else {
					modulesNidToEid = new HashMap<>(modulesCallbackLocal.getModuleIdMap());
				}

				if (modulesNidToTechnology != null) {
					modulesNidToTechnology.putAll(modulesCallbackLocal.getModuleTechnologyMap());
				} else {
					modulesNidToTechnology = new HashMap<>(modulesCallbackLocal.getModuleTechnologyMap());
				}
			} else {
				throw new IllegalStateException("Method resolve(sheetName) must be called before methos done(sheetName)");
			}
		}
	}
}
