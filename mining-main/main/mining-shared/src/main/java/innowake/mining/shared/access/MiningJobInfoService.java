/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.mining.shared.entities.MiningJobInfoPojo;
import innowake.mining.shared.entities.MiningJobInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Specifies basic functions for accessing the {@code mining_job_info} database entity.
 */
public interface MiningJobInfoService {

	interface MiningJobInfoInquiryBuilder {
		
		/**
		 * Finds {@linkplain MiningJobInfoPojo} by it's job ID
		 * @param jobId the job ID
		 * @return filter builder
		 */
		MiningJobInfoInquiryBuilder byJobId(UUID jobId);
		
		/**
		 * Finds {@linkplain MiningJobInfoPojo} by it's job ID
		 * @param jobIds the list of job ID
		 * @return filter builder
		 */
		MiningJobInfoInquiryBuilder byJobIds(Collection<UUID> jobIds);
		
		/**
		 * Finds {@linkplain MiningJobInfoPojo}s belonging to a project ID
		 * @param project the project ID
		 * @return filter builder
		 */
		MiningJobInfoInquiryBuilder ofProject(EntityId project);
		
		/**
		 * Finds {@linkplain MiningJobInfoPojo}s belonging to a module ID
		 * @param module the module ID
		 * @return filter builder
		 */
		MiningJobInfoInquiryBuilder ofModule(EntityId module);
		
		/**
		 * Finds {@linkplain MiningJobInfoPojo}s belonging to provided module IDs
		 * @param modules the collection of module IDs
		 * @return filter builder
		 */
		MiningJobInfoInquiryBuilder ofModules(Collection<UUID> modules);
	}
	
	/**
	 * Counts number of job info present based on provided builder
	 *
	 * @param builder the query builder
	 * @return count of job information
	 */
	Long count(BuildingConsumer<MiningJobInfoInquiryBuilder> builder);
	
	/**
	 * Finds a job info present based on provided builder
	 *
	 * @param builder the query builder
	 * @return the job optionally
	 */
	Optional<MiningJobInfoPojo> findAny(BuildingConsumer<MiningJobInfoInquiryBuilder> builder);
	
	/**
	 * Finds job IDs based on provided builder
	 *
	 * @param builder the query builder
	 * @return list of jobs IDs
	 */
	List<UUID> findJobId(BuildingConsumer<MiningJobInfoInquiryBuilder> builder);
	
	/**
	 * Creates a job info
	 *
	 * @param jobInfo the job info prototype
	 */
	void create(MiningJobInfoPojoPrototype jobInfo);
	
	/**
	 * Deletes job informations based on provided builder
	 *
	 * @param builder the query builder
	 * @return number of jobs deleted
	 */
	int delete(BuildingConsumer<MiningJobInfoInquiryBuilder> builder);
	
	/**
	 * Deletes a job by it's username
	 *
	 * @param username the username
	 * @return number of jobs deleted
	 */
	int deleteByUserName(String username);
}
