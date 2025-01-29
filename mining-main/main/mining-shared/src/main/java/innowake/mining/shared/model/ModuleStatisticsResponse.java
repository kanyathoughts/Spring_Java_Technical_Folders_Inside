/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import innowake.lib.core.lang.Nullable;

/**
 * class for Module statistics
 */
public class ModuleStatisticsResponse {

	@Nullable
	private Long count;
	@Nullable
	private Long withErrorsCount;
	@Nullable
	private Long sourceCodeLineCount;
	@Nullable
	private Long sourceFileCount;
	@Nullable
	private Long missingCount;

	@Nullable
	private List<ModuleTechnologyStatistic> technologyStatistics;

	/**
	 * get count of modules
	 *
	 * @return count of module
	 */
	@Nullable
	public Long getCount() {
		return count;
	}

	/**
	 * set count of module for project
	 *
	 * @param count of Long
	 */
	public void setCount(final Long count) {
		this.count = count;
	}

	/**
	 * get count of error modules
	 *
	 * @return count of errors of type long
	 */
	@Nullable
	public Long getWithErrorsCount() {
		return withErrorsCount;
	}

	/**
	 * set count of error modules
	 *
	 * @param withErrorsCount count of error
	 */
	public void setWithErrorsCount(final Long withErrorsCount) {
		this.withErrorsCount = withErrorsCount;
	}

	/**
	 * get source Code line count
	 *
	 * @return source code line count as long
	 */
	@Nullable
	public Long getSourceCodeLineCount() {
		return sourceCodeLineCount;
	}

	/**
	 * set counts of line of source code
	 *
	 * @param sourceCodeLineCount count of line of code of Long
	 */
	public void setSourceCodeLineCount(final Long sourceCodeLineCount) {
		this.sourceCodeLineCount = sourceCodeLineCount;
	}
	
	/**
	 * Gets count of source files which is modules count with representation as PHYSICAL.
	 * 
	 * @return source files count
	 */
	@Nullable
	public Long getSourceFileCount() {
		return sourceFileCount;
	}

	/**
	 * Sets count of source files which is modules count with representation as PHYSICAL.
	 *
	 * @param sourceFileCount count of source files
	 */
	public void setSourceFileCount(final Long sourceFileCount) {
		this.sourceFileCount = sourceFileCount;
	}
	
	/**
	 * Gets counts of missing modules.
	 * 
	 * @return count of missing modules
	 */
	@Nullable
	public Long getMissingCount() {
		return missingCount;
	}

	/**
	 * Sets count of missing modules.
	 *
	 * @param missingCount count of missing modules
	 */
	public void setMissingCount(final Long missingCount) {
		this.missingCount = missingCount;
	}


	/**
	 * get list of technology statistics
	 *
	 * @return List of {@code ModuleTechnologyStatistic} 
	*/
	@Nullable
	public List<ModuleTechnologyStatistic> getTechnologies() {
		return technologyStatistics;
	}

	/**
	 * set list of {@code ModuleTechnologyStatistic} for the project
	 *
	 * @param technologyStatistics list of ModuleTechnologyStatistic
	 */
	public void setTechnologies(final List<ModuleTechnologyStatistic> technologyStatistics) {
		this.technologyStatistics = technologyStatistics;
	}

	/**
	 * set list of {@code ModuleTechnologyStatistic} for the project
	 * 
	 * @param technologyList Map of Technology name and count of line of code
	 *
	 */
	public void setModuleTechnologyList(final Map<String, Long> technologyList) {
		final List<ModuleTechnologyStatistic> technologies = new ArrayList<>();
		technologyList.forEach((technology, lineOfCode) -> technologies.add(new ModuleTechnologyStatistic(technology, lineOfCode)));
		setTechnologies(technologies);
	}

	public static class ModuleTechnologyStatistic {

		private String technologyName;

		private Long countLinesofCode;

		ModuleTechnologyStatistic(final String technologyName, final Long countLinesofCode) {
			this.technologyName = technologyName;
			this.countLinesofCode = countLinesofCode;
		}

		/**
		 * get Name of technology
		 *
		 * @return technology name
		 */
		@Nullable
		public String getName() {
			return technologyName;
		}

		/**
		 * set Name of technology
		 *
		 * @param technologyName of technology
		 */
		public void setName(final String technologyName) {
			this.technologyName = technologyName;
		}

		/**
		 * get count of lines of code
		 *
		 * @return count of lines of code of technology
		 */
		@Nullable
		public Long getCount() {
			return countLinesofCode;
		}

		/**
		 * set count of lines of code
		 *
		 * @param countLinesofCode of lines of code of technology
		 */
		public void setCount(final Long countLinesofCode) {
			this.countLinesofCode = countLinesofCode;
		}

	}
}