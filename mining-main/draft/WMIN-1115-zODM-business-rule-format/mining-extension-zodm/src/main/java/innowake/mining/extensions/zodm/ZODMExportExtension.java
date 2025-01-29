/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.zodm;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

@Service
public class ZODMExportExtension implements MiningExportExtension {
	
	//TODO: use this to get the annotations that we want to export
	@Autowired
	AnnotationService annotationDao;

	@Override
	public String getFormatIdentifier() {
		return "zODM";
	}
	
	@Override
	public String getDescription() {
		return "zODM export extension";
	}

	@Override
	public ExportValue export(EntityId projectId, Map<String, List<String>> parameters) throws ExportException {
		//TODO: example implementation
		final byte[] exampleData;
		try {
			final ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
			final ZipOutputStream zipOut = new ZipOutputStream(baos);
			zipOut.putNextEntry(new ZipEntry("hello.txt"));
			zipOut.write("Hello, World!".getBytes(StandardCharsets.UTF_8));
			zipOut.close();
			exampleData = baos.toByteArray();
		} catch (IOException e) {
			throw new ExportException(e);
		}
		
		return new ExportValue() {
			
			@Override
			public InputStream getInputStream() {
				return new ByteArrayInputStream(exampleData);
			}
			
			@Override
			public String getFileName() {
				// TODO: placeholder file name
				return "RuleProject.zip";
			}
			
			@Override
			public String getContentType() {
				return "application/zip";
			}
		};
	}

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.ADMIN;
	}
}
