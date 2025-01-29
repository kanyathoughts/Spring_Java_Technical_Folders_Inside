/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.example;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.io.ParameterDescription;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;
import java.util.Map;

/**
 * Example implementation of {@link MiningExportExtension}
 */
@Component
public class ExampleExportExtension implements MiningExportExtension {

	@Autowired
	private ObjectMapper objectMapper;
	
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
		return "example-export";
	}

	@Override
	public String getDescription() {
		return "Example Export Extension";
	}

	@Override
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "Example-Extension", "Example-Export-Extension");
	}

	@Override
	public ExportValue export(final EntityId projectId, final Map<String, List<String>> parameters)  {
		return new ExportValue() {
			
			@Override
			public InputStream getInputStream() {
				try {
					final byte[] bytes = objectMapper.writeValueAsBytes(parameters);
					return new ByteArrayInputStream(bytes);
				} catch (JsonProcessingException e) {
					throw new IllegalStateException(e);
				}
			}
			
			@Override
			public String getFileName() {
				return "result.json";
			}
			
			@Override
			public String getContentType() {
				return "application/json";
			}
		};
	}

	@Override
	public List<ParameterDescription> getParameterDescriptions() {
		return ParametersData.getAllParameterData();
	}

}
