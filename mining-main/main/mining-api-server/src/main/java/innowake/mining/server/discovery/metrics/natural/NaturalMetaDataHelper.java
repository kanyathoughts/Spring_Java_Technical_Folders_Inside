/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.meta.IMetaData;
import innowake.ndt.core.meta.IMetaParseUnit;
import innowake.ndt.core.meta.IMetaParseUnits;
import innowake.ndt.core.meta.natural.INaturalMetaAccessor;
import innowake.ndt.core.meta.natural.INaturalMetaAccessorContexts;
import innowake.ndt.core.meta.natural.NaturalMetaDataProvider;
import innowake.ndt.core.meta.natural.NaturalMetaParameters;
import innowake.ndt.core.parsing.ITokenPartitioning;

/**
 * Natural specific null-safe helper for metadata. 
 * <p>
 * This class was widely copied from {@code innowake.ndt.natclipse.core.NaturalMetaDataHelper}.
 */
class NaturalMetaDataHelper {

	private NaturalMetaDataHelper() {}
	
	/**
	 * Null-safe method that returns report mode or default mode.
	 *
	 * @param metaData the metadata or <code>null</code>
	 * @return the mode (not <code>null</code>)
	 */
	static NaturalProgrammingMode getProgrammingMode(@Nullable final IMetaData metaData) {
		final String s = getString(metaData, NaturalMetaParameters.MODE.getId());
		if (StringUtils.isNotEmpty(s) && Assert.assertNotNull(s).toLowerCase().startsWith("r")) {
			return NaturalProgrammingMode.REPORTING;
		}
		return NaturalProgrammingMode.STRUCTURED;
	}

	/**
	 * Creates the adequate Natural meta accessor for the given Natural type and the
	 * given partitioning.
	 * 
	 * @param type the type to create an {@link INaturalMetaAccessor} for
	 * @param partitioning explicitly given partitioning (not <code>null</code>)
	 * @return the {@link INaturalMetaAccessor}; if the type is <code>null</code>
	 *         the method returns the default accessor
	 */
	static INaturalMetaAccessor createNaturalMetaAccessor(final Type type, final ITokenPartitioning partitioning) {
		final Object context = mapNaturalMetaDataContext(type);
		return (INaturalMetaAccessor) NaturalMetaDataProvider.INSTANCE.createAccessor(partitioning, context);
	}
	
	/**
	 * Null-safe method that extracts the metadata from the given units; it 
	 * returns <code>null</code> if no metdata can be found in the given parse 
	 * units.
	 *
	 * @param units the parse units or <code>null</code>
	 * @return the metadata or <code>null</code>
	 */
	@Nullable
	static IMetaData accessMetaData(final IMetaParseUnits units) {
		final IMetaParseUnit unit = units.getFirst();
		return unit != null ? unit.getMetaData() : null;
	}
	
	@Nullable
	private static String getString(@Nullable final IMetaData metaData, final String id) {
		if (metaData != null && StringUtils.isNotEmpty(id)) {
			final Object s = metaData.get(id);
			if (s instanceof String) {
				return (String) s;
			}
		}
		return null;
	}
	
	private static Object mapNaturalMetaDataContext(final Type type) {
		return type == Type.MAP ? INaturalMetaAccessorContexts.CONTEXT_NATURAL_MAP : INaturalMetaAccessorContexts.CONTEXT_NATURAL_STANDARD;
	}
}
