/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.util;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import innowake.mining.shared.extensions.MiningExportExtension.ExportValue;
import innowake.mining.shared.model.RelationshipType;

/**
 * Util class to Export files.
 */
public class ExporterUtil {

	public static final String CONTENT_TYPE_CSV = "text/csv";
	public static final String CONTENT_TYPE_XML = "text/xml";
	public static final String CONTENT_TYPE_PLAIN = "text/plain";
	
	/**
	 * Private constructor to hide implicit one of the Util class. 
	 */
	private ExporterUtil() { }

	/**
	 * Set of valid {@link RelationshipType} types to be used when exporting files.
	 */
	public static final Set<RelationshipType> VALID_REFERENCE_TYPES = Arrays.stream(RelationshipType.values())
			/* do not consider relationship type NONE for call chain */
			.filter(rel -> rel != RelationshipType.NONE)
			/* I don't know what this is but sounds like it should be filtered */
			.filter(rel -> rel != RelationshipType.ARTIFICIAL)
			.collect(Collectors.toSet());

	/**
	 * Set of valid {@link RelationshipType} types to be used when exporting files.
	 */
	public static final Set<String> VALID_RELATIONSHIP_TYPES = VALID_REFERENCE_TYPES.stream()
			.map(type -> StringUtils.capitalize(type.name().toLowerCase()))
			.collect(Collectors.toSet());

	/**
	 * Creates {@link ExportValue}.
	 *
	 * @param inStream the input stream to export the content to file
	 * @param fileName the name of the export file
	 * @param contentType the content type of the export file
	 * @return the created {@link ExportValue}
	 */
	public static ExportValue createExportValue(final InputStream inStream, final String fileName, final String contentType) {
		return new ExportValue() {
			@Override
			public InputStream getInputStream() {
				return inStream;
			}

			@Override
			public String getFileName() {
				return fileName;
			}

			@Override
			public String getContentType() {
				return contentType;
			}
		};
	}
}
