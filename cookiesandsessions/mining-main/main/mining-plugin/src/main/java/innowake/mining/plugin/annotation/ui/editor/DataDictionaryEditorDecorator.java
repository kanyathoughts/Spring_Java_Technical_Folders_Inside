/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.annotation.ui.editor;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ICoreRunnable;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewerExtension5;
import org.eclipse.swt.widgets.Display;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.MiningJobGroup;
import innowake.mining.plugin.base.ResourceUtil;
import innowake.mining.plugin.base.ui.EditorActionUtil;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.DataDictionaryVariableAttribute;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Decorates an editor with annotation based highlighting of {@linkplain DataDictionaryPojo data dictionary information}.
 */
public class DataDictionaryEditorDecorator implements IEditorDecorator {

	private static final Collection<String> ANNOTATION_TYPES = new ArrayList<>();
	private static final String ANNOTATION_TYPE = "innowake.mining.datadictionary.annotation";
	
	static {
		ANNOTATION_TYPES.add(ANNOTATION_TYPE);
	}
	
	private final IFile file;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param file the file in the editor
	 */
	public DataDictionaryEditorDecorator(final IFile file) {
		this.file = file;
	}
	
	@Override
	public void install(final ISourceViewerExtension5 sourceViewerExtension) {
		executeJob(file);
	}

	@Override
	public void uninstall() {
		/* For now do nothing */
	}

	private void executeJob(final IFile file) {
		final Job decorateJob = Job.create("Decorate Mining Data Dictionary Entries", createJob(file));
		decorateJob.setPriority(Job.DECORATE);
		decorateJob.setJobGroup(MiningJobGroup.INSTANCE);
		decorateJob.setSystem(false);
		decorateJob.schedule();
	}
	
	private ICoreRunnable createJob(final IFile file) {
		return monitor -> {
				final Optional<ModulePojo> findModuleResult = findModule(file);
				if ( ! findModuleResult.isPresent()) {
					Logging.error("Could not determine module for file " + file.getLocation() + ".");
					return;
				}
				final EntityId moduleId = findModuleResult.get().identity();
				
				MiningServiceExecutor
						.create(() ->
							ApiClient.dataDictionaryService(file.getProject())
							.findAllDataDictionaryEntries()
							.setModuleId(moduleId))
						.setValidResultConsumer(dataDictionaryEntriesArray ->
							Display.getDefault().asyncExec(() ->
								decorateEditor(Assert.assertNotNull(dataDictionaryEntriesArray))))
						.setInvalidResultConsumer(dataDictionaryEntriesResult ->
							Display.getDefault().asyncExec(() ->
								Logging.error(dataDictionaryEntriesResult.getExtendedStatusMessage())))
						.setExceptionConsumer(exception -> {
							if (exception instanceof CoreException || exception instanceof StorageException) {
								Logging.error("Error while searching for data dictionary information.", exception);
							} else if (exception instanceof IOException) {
								Logging.error(exception.getLocalizedMessage(), exception);
							}})
						.execute();
		};
	}
	
	private Optional<ModulePojo> findModule(final IFile file) {
		return MiningServiceExecutor
				.create(() ->
					ApiClient.moduleService(file.getProject())
					.findModuleByPath()
					.setPath(ResourceUtil.getProjectRelativePath(file)))
				.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception))
				.execute();
	}
	
	private void decorateEditor(final DataDictionaryPojo[] artifacts) {
		EditorActionUtil.decorateActiveEditor(buildAnnotations(artifacts), ANNOTATION_TYPES);
	}
	
	private Map<Annotation, Position> buildAnnotations(final DataDictionaryPojo[] artifacts) {
		return Arrays.stream(artifacts)
			.collect(Collectors.toMap(
						DataDictionaryEditorDecorator::createAnnotation, 
						DataDictionaryEditorDecorator::createPosition));
	}
	
	private static final Position createPosition(final DataDictionaryPojo entry) {
		final ModuleLocation moduleLocation = entry.getLocation().orElseThrow(() -> new IllegalStateException("Location in DataDictionary must be present"));
		final int offset = moduleLocation.getOffset().intValue();
		final int length = moduleLocation.getLength().intValue();
		return new Position(offset, length);
	}
	
	private static Annotation createAnnotation(final DataDictionaryPojo entry) {
		final StringBuilder tooltip = new StringBuilder();
		boolean addLineBreak = tooltipAppender(tooltip, "Description", entry.getDescription(), false);
		final Optional<String> format = entry.getFormat();
		if (format.isPresent()) {
			addLineBreak = tooltipAppender(tooltip, "Format", format.get(), addLineBreak);
		}

		final Map<DataDictionaryVariableScope, Map<String, String>> scopes = entry.getScopes();
		if ( ! scopes.isEmpty()) {
			tooltipAppender(tooltip, "Scopes", scopes.keySet().stream().map(DataDictionaryVariableScope::name).collect(Collectors.joining(", ")), true);
			for (final Entry<DataDictionaryVariableScope, Map<String, String>> scopeEntry : scopes.entrySet()) {
				switch (scopeEntry.getKey()) {
					case SQL_DATABASE:
						tooltipAppender(tooltip, "Tables", scopeEntry.getValue().get(DataDictionaryVariableAttribute.SQL_DATABASE_TABLES.getKey()), addLineBreak);
						break;
					case NATURAL_DATABASE:
						tooltipAppender(tooltip, "DDM", scopeEntry.getValue().get(DataDictionaryVariableAttribute.NATURAL_DATABASE_DDM.getKey()), addLineBreak);
						tooltipAppender(tooltip, "Adabas file", scopeEntry.getValue().get(DataDictionaryVariableAttribute.NATURAL_DATABASE_ADABAS_FILE.getKey()), addLineBreak);
						tooltipAppender(tooltip, "Adabas short name", scopeEntry.getValue().get(DataDictionaryVariableAttribute.NATURAL_DATABASE_ADABAS_SHORTNAME.getKey()), addLineBreak);
						break;
					case FILE:
						tooltipAppender(tooltip, "Dataset", scopeEntry.getValue().get(DataDictionaryVariableAttribute.FILE_DATASET.getKey()), addLineBreak);
						break;
					case CICS_UI:
						tooltipAppender(tooltip, "Mapset", scopeEntry.getValue().get(DataDictionaryVariableAttribute.CICS_UI_MAPSET.getKey()), addLineBreak);
						tooltipAppender(tooltip, "Map name", scopeEntry.getValue().get(DataDictionaryVariableAttribute.CICS_UI_MAPNAME.getKey()), addLineBreak);
						break;
					case NATURAL_UI:
						tooltipAppender(tooltip, "Map", scopeEntry.getValue().get(DataDictionaryVariableAttribute.NATURAL_UI_MAP.getKey()), addLineBreak);
						break;
					case OTHER:
						tooltipAppender(tooltip, "Source", scopeEntry.getValue().get(DataDictionaryVariableAttribute.OTHER_SOURCE.getKey()), addLineBreak);
						final String value = scopeEntry.getValue().get(DataDictionaryVariableAttribute.OTHER_SCOPE.getKey());
						if (value != null) {
							tooltipAppender(tooltip, "Scope", value, addLineBreak);
						}
						break;
					default:
				}
			}
		}
		return new Annotation(ANNOTATION_TYPE, false, tooltip.toString());
	}
	
	private static boolean tooltipAppender(final StringBuilder tooltip, final String key, @Nullable final String value, final boolean appendLinebreaks) {
		if (StringUtils.isEmpty(value)) {
			return false;
		}
		if (appendLinebreaks) {
			tooltip.append("\n\n");
		}
		tooltip.append(key);
		tooltip.append(": ");
		tooltip.append(value);
		return true;
	}

}
