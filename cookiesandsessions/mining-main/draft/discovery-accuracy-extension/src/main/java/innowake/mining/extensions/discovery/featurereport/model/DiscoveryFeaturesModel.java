/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.discovery.featurereport.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import innowake.mining.shared.model.Identification;

/**
 * Collects Discovery features grouped by technology and type
 */
public class DiscoveryFeaturesModel implements Serializable {

	private final Map<TechnologyAndType, DiscoveryFeatures> featureMap = new HashMap<>();

	public void merge(final DiscoveryFeatures features) {
		/*null check if features have*/
		if (features == null) {
			return;
		}
		final TechnologyAndType key = new TechnologyAndType(features.getTechnology(), features.getType());
		if ( ! featureMap.containsKey(key)) {
			featureMap.put(key, features);
		} else if (featureMap.get(key).getIdentifications().equals(Collections.singleton(Identification.MISSING))
				&& ! features.getIdentifications().equals(Collections.singleton(Identification.MISSING))) {
			/* If so far we have only added "MISSING" modules of this technology/type and the incoming module is not MISSING, then reverse the merge.
			 * This uses the newly incoming module as the baseline and then merges the current repo content on top of that.
			 * Otherwise, if we used the MISSING modules as baseline, all features would be merged to "SOMETIMES"
			 * as the original "MISSING" module that was in the repo did not support any features. */
			final DiscoveryFeatures original = featureMap.get(key);
			featureMap.put(key, features);
			merge(original);
		} else {
			/* merge features for this technology and type */
			final DiscoveryFeatures existing = featureMap.get(key);
			existing.getStorages().addAll(features.getStorages());
			existing.getOrigins().addAll(features.getOrigins());
			existing.getIdentifications().addAll(features.getIdentifications());
			existing.getRepresentations().addAll(features.getRepresentations());
			existing.getExcelTypes().addAll(features.getExcelTypes());

			features.getPossibleRelationships().forEach(existing::addPossibleRelationship);
			existing.getContains().addAll(features.getContains());
			existing.getContainedIn().addAll(features.getContainedIn());

			if (features.getIdentifications().equals(Collections.singleton(Identification.MISSING))) {
				/* do not merge supported features from "missing" modules
				 * as none of these things are supported on missing modules */
				return;
			}

			if (existing.getHasPath() != features.getHasPath()) {
				existing.setHasPath(Supported.SOMETIMES);
			}
			if (existing.getHasContainedModules() != features.getHasContainedModules()) {
				existing.setHasContainedModules(Supported.SOMETIMES);
			}
			if (existing.getHasContainingModule() != features.getHasContainingModule()) {
				existing.setHasContainingModule(Supported.SOMETIMES);
			}
			if (existing.getHasSourceCode() != features.getHasSourceCode()) {
				existing.setHasSourceCode(Supported.SOMETIMES);
			}
			if (existing.getHasLocation() != features.getHasLocation()) {
				existing.setHasLocation(Supported.SOMETIMES);
			}
			if (existing.getHasErrors() != features.getHasErrors()) {
				existing.setHasErrors(Supported.SOMETIMES);
			}
			if (existing.getHasAst() != features.getHasAst()) {
				existing.setHasAst(Supported.SOMETIMES);
			}

			if (existing.getSupportsPhysicalLines() != features.getSupportsPhysicalLines()) {
				existing.setSupportsPhysicalLines(Supported.SOMETIMES);
			}
			if (existing.getSupportsCodeLines() != features.getSupportsCodeLines()) {
				existing.setSupportsCodeLines(Supported.SOMETIMES);
			}
			if (existing.getSupportsCommentLines() != features.getSupportsCommentLines()) {
				existing.setSupportsCommentLines(Supported.SOMETIMES);
			}
			if (existing.getSupportsLinesOfDeadCode() != features.getSupportsLinesOfDeadCode()) {
				existing.setSupportsLinesOfDeadCode(Supported.SOMETIMES);
			}
			if (existing.getSupportsComplexity() != features.getSupportsComplexity()) {
				existing.setSupportsComplexity(Supported.SOMETIMES);
			}
			if (existing.getSupportsStatements() != features.getSupportsStatements()) {
				existing.setSupportsStatements(Supported.SOMETIMES);
			}
			if (existing.getSupportsSqlStatements() != features.getSupportsSqlStatements()) {
				existing.setSupportsSqlStatements(Supported.SOMETIMES);
			}
		}
	}

	public Map<TechnologyAndType, DiscoveryFeatures> getFeatures() {
		return featureMap;
	}
}
