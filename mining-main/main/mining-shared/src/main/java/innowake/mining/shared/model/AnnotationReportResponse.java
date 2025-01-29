/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.model;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Model class for Annotation Reporting response.
 */
public class AnnotationReportResponse {

	private List<AnnotationReport> annotationReportList = new ArrayList<>();

	private boolean hasMoreRecords;

	private int numberOfRecords;
	
	public final int recordLimit;

	/**
	 * Constructor for {@link AnnotationReportResponse}.
	 * @param annotationReports the list of {@link AnnotationReport}
	 * @param numberOfRecord the number of records for the given search criteria
	 * @param hasMoreRecords if the database has more records than the retrieved result set
	 * @param recordLimit the limit on number of records
	 */
	@JsonCreator
	public AnnotationReportResponse(@JsonProperty("annotationReports") final List<AnnotationReport> annotationReports,
			@JsonProperty("numberOfRecords") final int numberOfRecord, @JsonProperty("hasMoreRecords") final boolean hasMoreRecords,
			@JsonProperty("recordLimit") int recordLimit) {
		this.annotationReportList = annotationReports;
		this.numberOfRecords = numberOfRecord;
		this.hasMoreRecords = hasMoreRecords;
		this.recordLimit = recordLimit;
	}

	/**
	 * Gets list of {@link AnnotationReport}.
	 *
	 * @return the list of {@link AnnotationReport}
	 */
	public List<AnnotationReport> getAnnotationReportList() {
		return annotationReportList;
	}

	/**
	 * Sets the {@link AnnotationReport} list.
	 *
	 * @param annotationReportList the {@link  AnnotationReport} list
	 */
	public void setAnnotationReportList(final List<AnnotationReport> annotationReportList) {
		this.annotationReportList = annotationReportList;
	}

	/**
	 * Returns if the response has more records. 
	 *
	 * @return if response has more records
	 */
	public boolean isHasMoreRecords() {
		return hasMoreRecords;
	}

	/**
	 * Sets the hasMoreRecord flag.
	 *
	 * @param hasMoreRecords if response has more records
	 */
	public void setHasMoreRecords(final boolean hasMoreRecords) {
		this.hasMoreRecords = hasMoreRecords;
	}

	/**
	 * Gets the total number of annotation records. 
	 *
	 * @return the number of annotation records
	 */
	public int getNumberOfRecords() {
		return numberOfRecords;
	}

	/**
	 * Sets the total number of annotation records.
	 *
	 * @param numberOfRecords the total number of annotation records
	 */
	public void setNumberOfRecords(final int numberOfRecords) {
		this.numberOfRecords = numberOfRecords;
	}
}
