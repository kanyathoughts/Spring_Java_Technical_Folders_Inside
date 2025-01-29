/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.scheduler;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryPojo;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipFilter;
import innowake.mining.shared.entities.scheduler.SchedulerEntryType;
import innowake.mining.shared.entities.scheduler.SchedulerType;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipFilter.EntryFilter;

/**
 * Default implementation of the {@link SchedulerJobEntryResolver}, {@link SchedulerConditionEntryResolver} and {@link SchedulerEntryResolver} for Control-M.
 */
@Component
public class DefaultControlMEntryResolver implements SchedulerJobEntryResolver, SchedulerConditionEntryResolver, SchedulerEntryResolver {

	private static final String CONTENT_KEY_NAME = "NAME";
	private static final String CONTENT_KEY_SIGN = "SIGN";
	private static final List<ModuleType> MODULE_TYPES = List.of(ModuleType.JCL_JOB);

	private final SchedulerInfoService schedulerInfoService;

	public DefaultControlMEntryResolver(final SchedulerInfoService schedulerInfoService) {
		this.schedulerInfoService = schedulerInfoService;
	}

	@Override
	public SchedulerType resolverSchedulerType() {
		return SchedulerType.CONTROL_M;
	}

	@Override
	public String resolverSchedulerIdentifier() {
		return DEFAULT_SCHEDULER_IDENTIFIER;
	}

	/**
	 * Idenfifies the module from the given content and returns the module filter to further resolve the module.
	 * <p>
	 * The content is checked for the keys "MEMNAME" and "JOBNAME".
	 * If the key "JOBNAME" is present, the module name is resolved by the value of the key "JOBNAME".
	 * Else if the key "MEMNAME" is present, the module name is resolved by the value of the key "MEMNAME".
	 * If none of the keys are present, an empty optional is returned.
	 * Example XML:
	 * <pre>
	 *     &lt;JOB MEMNAME="JOBNAME" ...&gt;
	 *     ...
	 *     &lt;/JOB&gt;
	 *     &lt;JOB JOBNAME="JOBNAME" ...&gt;
	 *     ...
	 *     &lt;/JOB&gt;
	 *     </pre>
	 *
	 * @param content the content of the scheduler entry
	 * @return the module filter or an empty optional
	 */
	@Override
	public Optional<ModuleFilter> resolveModule(final EntityId project, final Map<String, String> content) {
		if (content.containsKey("JOBNAME")) {
			return Optional.of(new ModuleFilter().setNames(content.get("JOBNAME"))
					.setTypes(MODULE_TYPES));
		}
		if (content.containsKey("MEMNAME")) {
			return Optional.of(new ModuleFilter().setNames(content.get("MEMNAME"))
					.setTypes(MODULE_TYPES));
		}
		return Optional.empty();
	}

	@Override
	public ModuleType missingModuleType(final Map<String, String> content) {
		return ModuleType.fromTechnologyAndType(Technology.JCL, Type.JOB);
	}

	/**
	 * Resolves the scheduler entry type for the given node name and content.
	 * <p>
	 * The scheduler entry type is resolved by the node name. The node name is checked for the following values:
	 * <ul>
	 *     <li>JOB: {@link SchedulerEntryType#JOB}</li>
	 *     <li>INCOND: {@link SchedulerEntryType#CONDITION}</li>
	 *     <li>OUTCOND: {@link SchedulerEntryType#CONDITION}</li>
	 *     <li>TABLE: {@link SchedulerEntryType#TABLE}</li>
	 *     <li>FOLDER: {@link SchedulerEntryType#FOLDER}</li>
	 *     <li>UNKNOWN: {@link SchedulerEntryType#UNKNOWN}</li>
	 * </ul>
	 *
	 * @param nodeName the node name of the scheduler entry
	 * @param content the content of the scheduler entry
	 * @return the scheduler entry type
	 */
	@Override
	public SchedulerEntryType resolveSchedulerEntryType(final String nodeName, final Map<String, String> content) {
		final String nodeNameUpperCase = nodeName.toUpperCase();
		switch (nodeNameUpperCase) {
			case "JOB":
				return SchedulerEntryType.JOB;
			case "INCOND":
			case "OUTCOND":
				return SchedulerEntryType.CONDITION;
			default:
				if (nodeNameUpperCase.contains("TABLE")) {
					return SchedulerEntryType.TABLE;
				}
				if (nodeNameUpperCase.contains("FOLDER")) {
					return SchedulerEntryType.FOLDER;
				}
				return SchedulerEntryType.UNKNOWN;
		}
	}

	/**
	 * Resolves the relationships for the given scheduler entry. The target job entry is identified based on INCOND name attribute and OUTCOND name attribute.
	 * <p>
	 *     Example:
	 *     <pre>
	 *         &lt;INCOND NAME="PJ-JOB2-JOB3" ODATE="ODAT" AND_OR="A" /&gt;
	 *         &lt;OUTCOND NAME="PJ-JOB1-JOB3" ODATE="ODAT" SIGN="+" /&gt;
	 *
	 * @param schedulerEntry the scheduler entry
	 * @param moduleSupplier the supplier for the module
	 * @return the list of entry filters to resolve the entry relationships
	 */
	@Override
	public List<SchedulerEntryRelationshipFilter> resolveRelationships(final EntityId project, final SchedulerEntryPojo schedulerEntry,
			final Supplier<ModuleLightweightPojo> moduleSupplier) {
		final String identifier = schedulerEntry.getIdentifier();

		if (identifier.equals("INCOND")) {
			return schedulerInfoService.findEntries(
							e -> e.ofProject(project)
									.ofSchedulerImportId(schedulerEntry.getSchedulerImport())
									.withType(SchedulerEntryType.CONDITION)
									.withIdentifier("OUTCOND")
									.withContent(CONTENT_KEY_NAME, schedulerEntry.getContent().get("NAME"))
									.withContent(CONTENT_KEY_SIGN, "+")).stream()
					.map(e -> new SchedulerEntryRelationshipFilter(new EntryFilter().addEntryIds(e.getContainedIn()),
							new EntryFilter().addEntryIds(schedulerEntry.getContainedIn()), true, schedulerEntry.getContent().get("NAME")))
					.collect(Collectors.toList());
		}
		return Collections.emptyList();
	}
}
