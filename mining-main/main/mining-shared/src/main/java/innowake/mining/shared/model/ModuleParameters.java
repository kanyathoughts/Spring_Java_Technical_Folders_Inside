/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.io.Serializable;
import java.time.Instant;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ModulePojo;

/**
 * This model class is to hold the additional parameters for {@link Module} to be sent from Metrics creation / Metrics import process to Module creator.
 * 
 * For e.g., The {@code Project#getMetricsDate()} that is generated at the start of the DiscoverMetricsJob has to be used in all the 
 * {@link ModulePojo#getMetricsDate()} field. 
 */
public class ModuleParameters implements Serializable {

	private final Instant metricsDate;
	
	@Nullable
	private String jobId;
	
	/**
	 * Constructor to set the final fields.
	 * 
	 * @param metricsDate See {@link ModulePojo#getMetricsDate()}
	 */
	public ModuleParameters(final Instant metricsDate) {
		this.metricsDate = metricsDate;
	}

	/**
	 * See {@link ModulePojo#getMetricsDate()}.
	 *
	 * @return See {@link ModulePojo#getMetricsDate()}
	 */
	public Instant getMetricsDate() {
		return metricsDate;
	}
	
	/**
	 * Sets the job id.
	 *
	 * @param jobId the job id
	 */
	public void setJobId(final String jobId) {
		this.jobId = jobId;
	}
	
	/**
	 * Returns the job id.
	 *
	 * @return the job id
	 */
	@Nullable
	public String getJobId() {
		return jobId;
	}
}
