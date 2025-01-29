/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import static innowake.mining.shared.model.Type.COPYCODE;
import static innowake.mining.shared.model.Type.DDM;
import static innowake.mining.shared.model.Type.DIALOG;
import static innowake.mining.shared.model.Type.GDA;
import static innowake.mining.shared.model.Type.HELP;
import static innowake.mining.shared.model.Type.LDA;
import static innowake.mining.shared.model.Type.MAP;
import static innowake.mining.shared.model.Type.PDA;
import static innowake.mining.shared.model.Type.PROGRAM;
import static innowake.mining.shared.model.Type.SUBPROGRAM;
import static innowake.mining.shared.model.Type.SUBROUTINE;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.CALLNAT;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.FETCH;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.FETCH_RETURN;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.GLOBAL_USING;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.HE;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.INCLUDE;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.LOCAL_USING;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.OPEN_DIALOG;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.PARAMETER_USING;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.PERFORM;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.USING_MAP;
import static innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType.VIEW_OF;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

import org.apache.commons.collections4.MultiValuedMap;

import com.google.common.collect.Sets;

import innowake.lib.core.util.collection.MultiValueMapUtil;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.parsing.INodeType;

/**
 * Manages reference types.
 * <p>
 * This class was widely copied from {@code innowake.ndt.natclipse.core.object.internal.InternalDependencyTypeManager}.
 */
class NaturalDependencyTypeManager {

	static final NaturalDependencyTypeManager INSTANCE = new NaturalDependencyTypeManager();

	private final MultiValuedMap<INodeType, Type> nodeTypeToMiningType = MultiValueMapUtil.createArrayListHashMap();

	private NaturalDependencyTypeManager() {
		nodeTypeToMiningType.put(VIEW_OF, DDM);
		nodeTypeToMiningType.put(PERFORM, SUBROUTINE);
		nodeTypeToMiningType.put(INCLUDE, COPYCODE);
		nodeTypeToMiningType.put(CALLNAT, SUBPROGRAM);
		nodeTypeToMiningType.put(USING_MAP, MAP);
		nodeTypeToMiningType.put(GLOBAL_USING, GDA);
		nodeTypeToMiningType.put(PARAMETER_USING, PDA);
		nodeTypeToMiningType.put(LOCAL_USING, LDA);
		nodeTypeToMiningType.put(LOCAL_USING, PDA);
		nodeTypeToMiningType.put(HE, HELP);
		nodeTypeToMiningType.put(FETCH, PROGRAM);
		nodeTypeToMiningType.put(FETCH_RETURN, PROGRAM);
		nodeTypeToMiningType.put(OPEN_DIALOG, DIALOG);
	}

	/**
	 * Returns an immutable {@link Set} of {@link Type}s for a given {@link INodeType}.
	 *
	 * @param nodeType an {@link INodeType}
	 * @return {@link Set} of {@link Type}s or an empty {@link Set}
	 */
	Set<Type> getTargetTypes(final INodeType nodeType) {
		final Collection<Type> result = nodeTypeToMiningType.get(nodeType);
		return result != null ? Sets.immutableEnumSet(result) : Collections.emptySet();
	}
}
