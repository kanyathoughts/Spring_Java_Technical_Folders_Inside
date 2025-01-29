/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.extensions;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.io.ParameterDescription;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.io.ParameterDescription.ParameterType;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Implementation of {@link MiningExportExtension} for testing purposes.
 */
public class TestExportExtension implements MiningExportExtension {

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.EDITOR;
	}

	@Override
	public String getFormatIdentifier() {
		return "txt";
	}

	@Override
	public String getDescription() {
		return "Sample Test extension";
	}

	@Override
	public ExportValue export(EntityId projectId, Map<String, List<String>> parameters)
			throws ExportException {
		return new ExportValue() {
			
			@Override
			public InputStream getInputStream() {
				return new ByteArrayInputStream("Sample Extension".getBytes(StandardCharsets.UTF_8));
			}
			
			@Override
			public String getFileName() {
				return "sample.txt";
			}
			
			@Override
			public String getContentType() {
				return "text";
			}
		};
	}
	
	@Override
	public List<ParameterDescription> getParameterDescriptions() {
		return Arrays.asList(
				new ParameterDescription("sample param 1", "sample test extension parameter 1", ParameterType.BOOLEAN, true, "false", false,
						"sample", Arrays.asList("true", "false")),
				new ParameterDescription("sample param 2", "sample test extension parameter 2", ParameterType.STRING, false, "ABC", false,
						"sample", Arrays.asList("ABC", "DEF")));
	}
	
	@Override
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "test category", "sample extension");
	}
}
