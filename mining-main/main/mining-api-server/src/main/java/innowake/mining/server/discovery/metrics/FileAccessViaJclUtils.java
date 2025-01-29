/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.HashSetValuedHashMap;
import org.apache.commons.lang3.tuple.ImmutablePair;

import innowake.mining.data.access.ModelArtifactService;
import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.discovery.dawn.metrics.contributors.batch.JobControl;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Utilities for resolving file accesses in programs where the actual assignment of the physical dataset
 * happens in a JCL job, and the program just uses a symbolic name to access the file.
 */
public class FileAccessViaJclUtils {

	private FileAccessViaJclUtils() {
		throw new IllegalStateException();
	}

	/**
	 * Collects all JCL_STEPs that either execute the program directly via EXEC PGM
	 * or indirectly, by calling another program which in turn calls this program via "Calls".
	 * <p>
	 * The method recursively follows incoming "Calls" and collects all reachable JCL_EXEC_PGM modules.
	 * 
	 * 
	 * @param program the program to analyze
	 * @param modelArtifactService used for loading modules from DB
	 * @return all steps that call the target program, either directly or indirectly
	 */
	public static Collection<ModelArtifact> collectStepsThatCallThisProgram(final ModelArtifact program, final ModelArtifactService modelArtifactService) {
		return collectStepsThatCallThisProgramRecursively(new HashSet<>(), assertNotNull(program.getModuleId()).getUid(), modelArtifactService).values();
	}
	
	private static Map<UUID, ModelArtifact> collectStepsThatCallThisProgramRecursively(final Set<UUID> processedUids, final UUID currentUid,
			final ModelArtifactService modelArtifactService) {

		if ( ! processedUids.add(currentUid)) {
			return Collections.emptyMap();
		}

		final List<LazyModelArtifact> callingModules = modelArtifactService.find(b -> b.withDestinationRelationshipsTo(EntityId.of(currentUid), RelationshipType.CALLS));
		if (callingModules.isEmpty()) {
			return Collections.emptyMap();
		}

		final Map<UUID, ModelArtifact> stepsThatCallThisProgram = new HashMap<>(callingModules.size());
		callingModules.forEach(callingModule -> {
			if (callingModule.getType() == ResolveTarget.JCL_EXEC_PGM) {
				stepsThatCallThisProgram.put(assertNotNull(callingModule.getModuleId()).getUid(), callingModule);
			} else {
				stepsThatCallThisProgram
						.putAll(collectStepsThatCallThisProgramRecursively(processedUids, assertNotNull(callingModule.getModuleId()).getUid(), modelArtifactService));
			}
		});

		return stepsThatCallThisProgram;
	}
	
	/**
	 * Get a map DDName -> DataSetName (DSN) by examining all the DD statements of the given JCL step artifact.
	 *
	 * @param jclStep the JCL step to analyze (JCL_EXEC or JCL_EXEC_PGM)
	 * @return a map of file assignments
	 */
	public static MultiValuedMap<String, String> getDataDeclarationMapFromStep(final ModelArtifact jclStep) {
		final MultiValuedMap<String, String> ret = new HashSetValuedHashMap<>();
		jclStep.getDependencies()
			.filter(dep -> dep.getTarget().getType().equals(ResolveTarget.RESOURCE_FILE))
			.map(dep -> {
				/* extract the DD name from the dependency properties - awkwardly this is a list that contains a single map - I don't know why */
				@SuppressWarnings("unchecked")
				final Collection<Map<String,String>> propertiesSet = (Collection<Map<String, String>>) dep.getAttributes().get(ModelAttributeKey.PROPERTIES);
				final Map<String, String> properties = propertiesSet.stream().findFirst().orElse(Collections.emptyMap());
				final String ddName = properties.get(JobControl.ID_NAME);
				return ImmutablePair.of(ddName, dep.getTarget().getName());
			})
			.forEach(pair -> ret.put(pair.getLeft(), pair.getRight()));
		return ret;
	}
}
