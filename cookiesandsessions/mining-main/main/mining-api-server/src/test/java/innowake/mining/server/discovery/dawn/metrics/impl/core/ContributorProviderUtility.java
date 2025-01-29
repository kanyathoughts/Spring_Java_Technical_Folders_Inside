/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.core;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Technology;

/**
 * Test utility class to return list of implemented class for {@link DiscoveryContributorFromSource} and {@link DiscoveryContributor} .
 */
public class ContributorProviderUtility {

	private static class NaturalTestContributor implements DiscoveryContributorFromSource {

		@Override
		public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
			return sourceObject.getTechnology() == Technology.NATURAL;
		}

		@Override
		public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
			builder.declareRootModule("NATURAL-MODULE", ModuleType.NATURAL_PROGRAM);
		}
	}

	private static class CobolTestContributor implements DiscoveryContributorFromSource {

		@Override
		public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
			return sourceObject.getTechnology() == Technology.COBOL;
		}

		@Override
		public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
			builder.declareRootModule("COBOL-MODULE", ModuleType.COBOL_PROGRAM);
		}
	}

	private static class CustomExtensionContributor implements DiscoveryContributorFromSource {

		@Override
		public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
			return sourceObject.getTechnology() == Technology.COBOL || sourceObject.getTechnology() == Technology.JAVA;
		}

		@Override
		public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
			switch(sourceObject.getTechnology()) {
				case COBOL: builder.declareRootModule("CUSTOM-COBOL-MODULE", ModuleType.COBOL_PROGRAM);
					break;
				case JAVA: builder.declareRootModule("CUSTOM-JAVA-MODULE", ModuleType.JAVA_TYPE);
					break;
				default:
					break;
			}			
		}
	}

	private static class JavaTestContributor implements DiscoveryContributorFromSource {

		@Override
		public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
			return sourceObject.getTechnology() == Technology.JAVA;
		}

		@Override
		public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
			builder.declareRootModule("JAVA-MODULE", ModuleType.JAVA_TYPE);
		}
	}

	private static class TestContributorNoSource implements DiscoveryContributor {

		@Override
		public void contribute(final DiscoveryBuilder builder, final DiscoveryContext context) {
			builder.declareExternalModule("TEST-MODULE", ModuleType.COBOL_PROGRAM);
		}
	}
	
	private static class TestContributorNoSourceForExternalModuleWithOrigin implements DiscoveryContributor {

		@Override
		public void contribute(final DiscoveryBuilder builder, final DiscoveryContext context) {
			builder.declareExternalModule("DFSRRC00", ModuleType.UNKNOWN_UTILITY, Origin.ENVIRONMENT);
		}
	}

	/**
	 * Provides a list of {@link DiscoveryContributorFromSource} instances.
	 * 
	 * @return a list of {@link DiscoveryContributorFromSource} instances
	 */
	public static List<DiscoveryContributorFromSource> provideSourceContributors() {
		final List<DiscoveryContributorFromSource> sourceContributors = Arrays.asList(new NaturalTestContributor(), new CobolTestContributor(),
				new JavaTestContributor(), new CustomExtensionContributor());
		return sourceContributors;
	}

	/**
	 * Provides a list of {@link DiscoveryContributor} instances.
	 * 
	 * @return a list of {@link DiscoveryContributor} instances
	 */
	public static List<DiscoveryContributor> provideContributors() {
		final List<DiscoveryContributor> contributors = Collections.singletonList(new TestContributorNoSource());
		return contributors;
	}
	
	/**
	 * Provides a list of {@link DiscoveryContributor} instances.
	 * 
	 * @return a list of {@link DiscoveryContributor} instances
	 */
	public static List<DiscoveryContributor> provideContributorsForExternalModuleWithOrigin() {
		final List<DiscoveryContributor> contributors = Collections.singletonList(new TestContributorNoSourceForExternalModuleWithOrigin());
		return contributors;
	}

}
