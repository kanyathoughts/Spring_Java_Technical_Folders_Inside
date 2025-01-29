/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.natural;

import org.apache.commons.io.FilenameUtils;

import innowake.lib.core.lang.NonNullByDefault;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.assembling.natural.INaturalAssemblingDataProvider;
import innowake.ndt.core.assembling.natural.INaturalAssemblingTypes;
import innowake.ndt.core.assembling.natural.NaturalAssemblingObjectType;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.natural.INaturalParseResult;
import innowake.ndt.core.parsing.natural.NaturalLightweightParsing;

/**
 * Natural assembling provider for Discovery.
 */
@NonNullByDefault(false)
public class AssemblingDataProvider implements INaturalAssemblingDataProvider<SourcePojo>, INaturalAssemblingTypes {

	private final SourceObjectResolver sourceObjectResolver;

	public AssemblingDataProvider(final SourceObjectResolver sourceObjectResolver) {
		this.sourceObjectResolver = sourceObjectResolver;
	}

	@Override
	public String getSource(final SourcePojo object) throws AssemblingException {
		return object.getContent().toString();
	}

	@Override
	public String getName(final SourcePojo object) {
		return FilenameUtils.getBaseName(object.getName());
	}

	@Override
	public String getPath(final SourcePojo object) {
		return object.getPath();
	}

	@Override
	public SourcePojo find(final SourcePojo root, final String name, final IAssemblingObjectType expectedType) {
		if (expectedType == NaturalAssemblingObjectType.DATAAREA) {
			SourcePojo result = sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.NATURAL, Type.LDA));
			if (result == null) {
				result = sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.NATURAL, Type.PDA));
				if (result == null) {
					result = sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.NATURAL, Type.GDA));
				}
			}
			if (result != null) {
				return result;
			}
		}

		return sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.NATURAL, mapTypeBackward(expectedType)));
	}

	@Override
	public NaturalAssemblingObjectType getType(final SourcePojo natObject) {
		return mapTypeForward(natObject.getType());
	}

	@Override
	public Object getHashable(final SourcePojo natObject) {
		return natObject;
	}

	@Override
	@SuppressWarnings("unchecked")
	public IAst<SourcePojo> getDependencyLightweightAst(final SourcePojo natObject) throws DiscoveryException {
		return (IAst<SourcePojo>) NaturalLightweightParsing.computeDependencyLightweightAst(natObject.getContent().toString());
	}

	@Override
	public INaturalParseResult getParseResult(final SourcePojo natObject) throws DiscoveryException {
		return NaturalLightweightParsing.computeNaturalTokens(natObject.getContent().toString());
	}

	@Override
	public boolean isObjectProxy(final SourcePojo natObject) {
		return false;
	}

	private static NaturalAssemblingObjectType mapTypeForward(final Type type) {
		switch (type) {
			case COPYCODE:
				return NaturalAssemblingObjectType.COPYCODE;
			case LDA:
				return NaturalAssemblingObjectType.LDA;
			case PDA:
				return NaturalAssemblingObjectType.PDA;
			case GDA:
				return NaturalAssemblingObjectType.GDA;
			default:
				return NaturalAssemblingObjectType.UNKNOWN;
		}
	}

	private static Type mapTypeBackward(final IAssemblingObjectType objectType) {
		switch ((NaturalAssemblingObjectType) objectType) {
			case COPYCODE:
				return Type.COPYCODE;
			case LDA:
				return Type.LDA;
			case PDA:
				return Type.PDA;
			case GDA:
				return Type.GDA;
			default:
				return Type.TEXT;
		}
	}

}
