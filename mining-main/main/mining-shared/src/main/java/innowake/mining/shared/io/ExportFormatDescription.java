/* Copyright (c) 2021 Deloitte. All rights reserved. */
package innowake.mining.shared.io;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.extensions.ParameterizedExtension;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Value object used as the return value of getExportFormats() of IoController.
 */
public class ExportFormatDescription {
	
	/**
	 * Supported {@link ParameterizedExtension} to be used in {@link ExportFormatDescription}
	 */
	public enum ExtensionType {
		
		/** {@link ExtensionType} for {@link MiningExportExtension} */
		EXPORT_EXTENSION,
		
		/** {@link ExtensionType} for MiningJobExtension */
		JOB_EXTENSION;
	}
	
	/**
	 * Unique identifier of the export format.
	 */
	public final String id;
	/**
	 * Short description of the export format to be displayed to the user.
	 */
	public final String description;
	/**
	 * {@link ExtensionType} of the export format.
	 */
	public final ExtensionType extensionType;
	/**
	 * Required {@link RoleType} of the parameter.
	 */
	public final RoleType requiredRole;
	/**
	 * Required {@link NatureType} of the export format.
	 */
	public final NatureType requiredNature;
	/**
	 * {@link List} of {@link ParameterDescription} for the export format.
	 */
	public final List<ParameterDescription> parameterDescriptions;
	/**
	 * {@link ShowOnExportPage} object for the export format.
	 */
	public final ShowOnExportPage showOnExportPage;
	/**
	 * {@link UploadDescription} describing a file that can be uploaded.
	 */
	public final UploadDescription uploadDescription;
	
	/**
	 * Initialize an object of {@link ExportFormatDescription} with parameters.
	 * 
	 * @param id identifier of the export format
	 * @param description the description of the export format to be displayed to the user
	 * @param extensionType {@link ExtensionType} of the export format
	 * @param requiredRole required {@link RoleType} of the parameter
	 * @param requiredNature required {@link NatureType} of the parameter
	 * @param parameterDescriptions {@link List} of {@link ParameterDescription}
	 * @param showOnExportPage {@link ShowOnExportPage} object for the export format
	 * @param uploadDescription the upload description
	 */
	@JsonCreator
	public ExportFormatDescription(
			@JsonProperty("id") final String id,
			@JsonProperty("description") final String description, 
			@JsonProperty("extensionType") final ExtensionType extensionType,
			@JsonProperty("requiredRole") final RoleType requiredRole, 
			@JsonProperty("requiredNature") final NatureType requiredNature,
			@JsonProperty("parameterDescriptions") final List<ParameterDescription> parameterDescriptions,
			@JsonProperty("showOnExportPage") final ShowOnExportPage showOnExportPage,
			@JsonProperty("uploadDescription") final UploadDescription uploadDescription
			) {
		super();
		this.id = id;
		this.description = description;
		this.extensionType = extensionType;
		this.requiredRole = requiredRole;
		this.requiredNature = requiredNature;
		this.parameterDescriptions = parameterDescriptions;
		this.showOnExportPage = showOnExportPage;
		this.uploadDescription = uploadDescription;
	}

	@Override
	public String toString() {
		return "ExportFormatDescription [" +
				"id='" + id + "'" +
				", description='" + description + "'" +
				", extensionType=" + extensionType +
				", requiredRole=" + requiredRole +
				", requiredNature=" + requiredNature +
				", parameterDescriptions=" + parameterDescriptions +
				", uploadDescription=" + uploadDescription +
				", showOnExportPage=" + showOnExportPage +
				"]";
	}
}
