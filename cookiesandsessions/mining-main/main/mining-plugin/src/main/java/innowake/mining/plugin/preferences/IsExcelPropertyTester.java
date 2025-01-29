/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences;

import java.util.Arrays;
import java.util.Optional;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.IStructuredSelection;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.base.ui.SelectionUtil;
import innowake.mining.plugin.module.importer.DiscoveryImportFileExtensions;

/**
 * Tester for enabling or disabling the context menu for importing Discovery Excel/CSV.
 */
public class IsExcelPropertyTester extends org.eclipse.core.expressions.PropertyTester {

	@Override
	public boolean test(
			@Nullable final Object receiver, 
			@Nullable final String property, 
			@Nullable final Object[] args, 
			@Nullable final Object expectedValue) {
		
		if ("isExcelOrCsv".equalsIgnoreCase(property)) {
			final Optional<IStructuredSelection> selected = SelectionUtil.getResourceSelection();
			if (selected.isPresent()) {
				IStructuredSelection structuredSelection = selected.get();
				if (structuredSelection.getFirstElement() instanceof IResource) {
					final IResource resource = (IResource) structuredSelection.getFirstElement();
					final String fileNameExtension = FilenameUtils.getExtension(resource.getName());
					return Arrays.asList(DiscoveryImportFileExtensions.values())
							.stream()
							.anyMatch(f -> f.getFileExtension().equalsIgnoreCase(fileNameExtension));
				}
			}
		}
		return false;
	}
}
