package innowake.mining.shared.entities;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * {@code module_undiscovered} entity class. Formerly know as {@code ExcelSheetUndiscovered}.
 */
public class ModuleUndiscoveredPojo {

	private final UUID project;
	private final String name;
	private final String path;

	public ModuleUndiscoveredPojo(
			@JsonProperty("project") final UUID project,
			@JsonProperty("name") final String name,
			@JsonProperty("path") final String path) {
		this.project = project;
		this.name = name;
		this.path = path;
	}

	public UUID getProject() {
		return project;
	}

	public String getName() {
		return name;
	}

	public String getPath() {
		return path;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("project", project)
				.append("name", name)
				.append("path", path)
				.toString();
	}
}