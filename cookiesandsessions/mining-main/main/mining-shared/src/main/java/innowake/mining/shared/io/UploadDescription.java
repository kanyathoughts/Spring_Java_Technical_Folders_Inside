/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.io;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;

import java.util.Optional;

/**
 * A class describing parameters for a file upload. This is primarily used by mining extensions
 * that support file uploads (e.g. extensions that import data from a certain file format).
 */
public class UploadDescription {

	private final String name;
	private final String description;
	private final boolean supported;
	private final boolean required;
	@Nullable
	private final String accept;

	/**
	 * Returns an {@link UploadDescription} indicating that file upload is not supported.
	 * @return upload description indicating that file upload is not supported
	 */
	@JsonCreator
	public static UploadDescription notSupported() {
		return new UploadDescription("", "", false, false, null);
	}

	/**
	 * Returns an {@link UploadDescription} that contains a "unique file type specifier" indicating the type of file that can be uploaded.
	 * @param name a short descriptive name explaining the purpose of the uploaded file
	 * @param description a description for the uploaded file
	 * @param required whether file upload is required
	 * @param accept a string representing a "unique file type specifier" (see {@link #getAccept()} for details)
	 * @return a upload description describing the file to be uploaded
	 */
	@JsonCreator
	public static UploadDescription with(@JsonProperty("name") final String name,
			@JsonProperty("description") final String description,
			@JsonProperty("required") final boolean required,
			@JsonProperty("accept") final String accept) {
		return new UploadDescription(name, description, true, required, accept);
	}

	private UploadDescription(final String name, final String description, final boolean supported, final boolean required, @Nullable final String accept) {
		this.name = name;
		this.description = description;
		this.supported = supported;
		this.required = required;
		this.accept = accept;
	}

	/**
	 * Returns a "name" that is displayed to the user when choosing a file to upload.
	 * <p>
	 * This is not a file name but a short, descriptive name what the purpose of the file is,
	 * e.g. "Report Data", "Logo image" etc.
	 *
	 * @return a short descriptive name explaining the purpose of the uploaded file
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns a description for the uploaded file.
	 * <p>
	 * Compared to {@linkplain #getName() name}, this is supposed to return a longer description explaining
	 * the file that shall be uploaded, e.g. "File containing report data created by some-tool" or "Logo image for the client"
	 * @return a description for the uploaded file
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Returns whether or not a file can be uploaded. If this returns {@code false} then no file chooser must be shown to the user.
	 * @return whether file upload is supported
	 */
	public boolean isSupported() {
		return supported;
	}

	/**
	 * Returns whether uploading a file is mandatory. If this returns {@code true} then a file must be uploaded before triggering the action.
	 * @return whether file upload is required
	 */
	public boolean isRequired() {
		return required;
	}

	/**
	 * Returns a "unique file type specifier" that indicates the type of file that can be uploaded. If present, this should be used
	 * to restrict or filter the set of files that can be selected by the user.
	 * <p>
	 * Please see
	 * <a href="https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/file#unique_file_type_specifiers">Unique file type specifiers</a>
	 * on how to write these specifiers.
	 * @return a string representing a "unique file type specifier" or {@linkplain Optional#empty() EMPTY} if the file type is not restricted
	 */
	public Optional<String> getAccept() {
		return Optional.ofNullable(accept);
	}

	@Override
	public String toString() {
		return "UploadDescription [" +
				"name='" + name + "'" +
				", description='" + description + "'" +
				", supported=" + supported +
				", required=" + required +
				", accept='" + accept + "'" +
				"]";
	}
}