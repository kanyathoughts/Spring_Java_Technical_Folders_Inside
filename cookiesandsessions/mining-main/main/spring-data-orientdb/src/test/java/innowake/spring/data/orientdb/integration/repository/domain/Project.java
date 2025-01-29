package innowake.spring.data.orientdb.integration.repository.domain;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.RId;

/**
 * A vertex class mapped in orientDB.
 */
@JsonInclude(Include.NON_NULL)
@Entity
public class Project {
	
	@RId
	@Nullable private String rid;
	@Nullable private String name;
	
	/**
	 * Default constructor required to create project proxy instance.
	 */
	public Project() {
	}
	
	/**
	 * Initializes project class with given values
	 * 
	 * @param name project's name
	 */
	public Project(final String name) {
		this.name = name;
	}
	
	/**
	 * Name of the project.
	 * 
	 * @return project's name
	 */
	@Nullable
	public String getName() {
		return name;
	}

	/**
	 * Sets project's name.
	 * 
	 * @param name project name
	 */
	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return "Project [name=" + name + "]";
	}

}
