/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.discovery.config.core;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import innowake.lib.core.lang.Nullable;
import innowake.mining.shared.discovery.config.core.FileDetectionType;

/**
 * Represents one pattern file detection type entry in the config file.
 */
@XmlJavaTypeAdapter(FileDetectionType.FileDetectionTypeAdapter.class)
public class FileDetectionType implements Serializable {

	/**
	 * Represents file detection type.
	 */
	private final String type;
	
	/**
	 * Represents priority.
	 */
	private final String priority;

	/**
	 * Parameterized constructor for File detection type.
	 * @param type Represents file detection type.
	 * @param priority Represents priority.
	 */
	public FileDetectionType(final String type, final String priority) {
		super();
		this.type = type;
		this.priority = priority;
	}

	/**
	 * Returns file detection type.
	 *  
	 * @return File detection type.
	 */
	public String getType() {
		return type;
	}

	/**
	 * Returns file detection priority.
	 *  
	 * @return File detection priority.
	 */
	public String getPriority() {
		return priority;
	}
	
	@Override
	public String toString() {
		return String.format("type='%s', priority='%s'", type, priority);
	}

	public static class FileDetectionTypeAdapter extends XmlAdapter<FileDetectionAdapted, FileDetectionType> {

		@Override
		public FileDetectionType unmarshal(@Nullable final FileDetectionAdapted adapted) throws Exception {
			final FileDetectionAdapted adaptedNN = assertNotNull(adapted);
			
			return new FileDetectionType(assertNotNull(adaptedNN.type), assertNotNull(adaptedNN.priority));
		}

		@Override
		public FileDetectionAdapted marshal(@Nullable final FileDetectionType detectionType) throws Exception {
			final FileDetectionType fileDetectionType = assertNotNull(detectionType);

			final FileDetectionAdapted result = new FileDetectionAdapted();
			result.type = fileDetectionType.type;
			result.priority = fileDetectionType.priority;
			return result;
		}

	}

	@XmlRootElement(name = "file_detection_type")
	private static class FileDetectionAdapted {

		@XmlAttribute
		@Nullable
		private String type;

		@XmlAttribute
		@Nullable
		private String priority;

	}
}
