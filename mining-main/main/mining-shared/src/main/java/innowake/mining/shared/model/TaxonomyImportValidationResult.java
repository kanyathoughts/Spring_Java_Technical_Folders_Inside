/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.io.Serializable;
import java.util.List;

/**
 * Model class for the taxonomy assignment validation results.
 */
public class TaxonomyImportValidationResult implements Serializable {
	
	private final MarkerType overallResult;
	private final List<Marker> markers;
	
	/**
	 * Creates a new TaxonomyImportValidationResult.
	 * 
	 * @param overallResult the overall validation result 
	 * @param markers the {@linkplain Marker markers} for each module in the file to be imported
	 */
	public TaxonomyImportValidationResult(final MarkerType overallResult, final List<Marker> markers) {
		this.overallResult = overallResult;
		this.markers = markers;
	}

	
	/**
	 * Gets the overall validation result type.
	 *
	 * @return the {@linkplain Marker marker type}
	 */
	public MarkerType getOverallResult() {
		return overallResult;
	}

	
	/**
	 * Gets all the {@linkplain Marker markers} associated with import file .
	 *
	 * @return the {@linkplain List list} of {@linkplain Marker markers} present in the file to be imported
	 */
	public List<Marker> getMarkers() {
		return markers;
	}

	/**
	 * Validation result types.
	 */
	public enum MarkerType {
		
		/**
		 * ERROR.
		 */
		ERROR,
		
		/**
		 * WARNING.
		 */
		WARNING,
		
		/**
		 * INFORMATION.
		 */
		INFORMATION,
		
		/**
		 * NONE.
		 */
		NONE;
	}
	
	/**
	 * Validation marker for each taxonomy assignment.
	 */
	public static class Marker implements Serializable {

		private final int lineNumber;
		private final List<String> lineHeaders;
		private final List<String> lineContent;
		private final MarkerType markerType;
		private final String markerText;
		
		/**
		 * Creates a taxonomy assignment marker.
		 * 
		 * @param lineNumber the line number
		 * @param lineHeaders the line header
		 * @param lineContent the line content
		 * @param markerType the marker type
		 * @param markerText the marker text
		 */
		public Marker(final int lineNumber, final List<String> lineHeaders, final List<String> lineContent, final MarkerType markerType,
				final String markerText) {
			this.lineNumber = lineNumber;
			this.lineHeaders = lineHeaders;
			this.lineContent = lineContent;
			this.markerType = markerType;
			this.markerText = markerText;
		}

		
		/**
		 * Gets the line number.
		 *
		 * @return the line number
		 */
		public int getLineNumber() {
			return lineNumber;
		}

		
		/**
		 * Gets the line headers.
		 *
		 * @return the line headers
		 */
		public List<String> getLineHeaders() {
			return lineHeaders;
		}

		
		/**
		 * Gets the line content.
		 *
		 * @return the line content
		 */
		public List<String> getLineContent() {
			return lineContent;
		}

		
		/**
		 * Gets the marker type.
		 *
		 * @return the marker type
		 */
		public MarkerType getMarkerType() {
			return markerType;
		}

		
		/**
		 * Gets the marker text.
		 *
		 * @return the marker text
		 */
		public String getMarkerText() {
			return markerText;
		}

	}

}
