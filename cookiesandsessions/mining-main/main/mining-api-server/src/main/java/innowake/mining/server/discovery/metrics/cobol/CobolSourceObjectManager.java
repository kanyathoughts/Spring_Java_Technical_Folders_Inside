/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol;

import static innowake.lib.core.lang.Assert.assertInstanceOf;
import static innowake.mining.shared.model.Type.COPYBOOK;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.AbstractSourceObjectManager;
import innowake.mining.server.discovery.metrics.SourceObjectDependency;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.IAstNode;
import innowake.ndt.core.parsing.IParseResult;
import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.cobol.CobolTokenParser;
import innowake.ndt.core.parsing.cobol.ast.CobolCopyNode;
import innowake.ndt.core.parsing.cobol.ast.CobolDependencyNode;
import innowake.ndt.core.parsing.cobol.ast.CobolLightweightParser;
import innowake.ndt.core.parsing.cobol.ast.CobolNodeTypes;

/***
 * Manager that provides {@link SourceObjectDependency} for {@code Cobol} {@link SourcePojo}s.
 * <p>
 * This class was widely copied from {@code innowake.ndt.cobolclipse.core.object.internal.CobolProjectObjectManager}.
 */
public class CobolSourceObjectManager extends AbstractSourceObjectManager<CobolDependency, IParseResult> {

	/**
	 * Constructor.
	 * 
	 * @param sourceObjectResolver instance of {@link SourceObjectResolver}
	 * @param configProperties instance of {@link GenericConfigProperties}
	 */
	public CobolSourceObjectManager(final SourceObjectResolver sourceObjectResolver, final GenericConfigProperties configProperties) {
		super(sourceObjectResolver, configProperties);
	}

	@Override
	protected IParseResult parse(final SourcePojo sourceObject) {
		return CobolTokenParser.INSTANCE.compute(sourceObject.getContent().toString());
	}

	@Override
	protected void calculateDependencies(final SourcePojo sourceObject, final IParseResult parseResult) {
		final IAst<SourcePojo> ast = CobolLightweightParser.<SourcePojo>getDefaultDependencyParser().parse(parseResult.getTokenPartitioning());
		final IAstNode[] copyNodes = ast.getNodes(CobolNodeTypes.COPY);
		final IAstNode[] sqlIncludeNodes = ast.getNodes(CobolNodeTypes.EXEC_SQL_INCLUDE);
		final IAstNode[] copyNodesAll =	(IAstNode[]) ArrayUtils.addAll(copyNodes, sqlIncludeNodes);
		for (final IAstNode astNode : copyNodesAll) {
			final CobolDependencyNode dependencyNode = (CobolDependencyNode) astNode;
			if (astNode instanceof CobolCopyNode) {
				final CobolCopyNode copyNode = (CobolCopyNode) astNode;
				final IToken beginToken = copyNode.getBeginToken();
				final var moduleLocation = new ModuleLocation(beginToken.getOffset(), sourceObject.getContent().toString().length());
				if (copyNode.isFromDictionary()) {
					registerOutgoingDependency(sourceObject, new CobolDependency(sourceObject, getTargetName(dependencyNode), COPYBOOK, true, sourceObjectResolver, moduleLocation));
					continue;
				}
				if (copyNode.getTargetLibraryToken() != null) {
					final CobolDependency cobolDependency = new CobolDependency(sourceObject, getTargetName(dependencyNode), COPYBOOK, false, sourceObjectResolver, moduleLocation);
					cobolDependency.setLibraryName(copyNode.getTargetLibrary());
					registerOutgoingDependency(sourceObject, cobolDependency);
					continue;
				}
			}
			registerOutgoingDependency(sourceObject, new CobolDependency(sourceObject, getTargetName(dependencyNode), COPYBOOK, false, sourceObjectResolver));
		}
	}
	
	private String getTargetName(final CobolDependencyNode node) {
		final String rawTarget = node.getTarget();
		if ( ! (node instanceof CobolCopyNode)) {
			return removeExtension(rawTarget);
		}
		if (assertInstanceOf(node, CobolCopyNode.class).isFromDictionary()) {
			return toConformName(rawTarget);
		} 
		return removeExtension(rawTarget);
	}

	/* this is copied from innowake.ndt.cobolclipse.core.object.NamePolicy */
	private static final String toConformName(final String arg) {
		return arg.startsWith("'") ? StringUtils.unwrap(arg, "'") : StringUtils.unwrap(arg, "\"");
	}

	/* this is copied from innowake.ndt.cobolclipse.core.object.NamePolicy */
	private static final String removeExtension(final String arg) {
		final String result = toConformName(arg);
		final int dotIndex = result.indexOf('.');
		return (dotIndex == -1) ? result : result.substring(0, dotIndex);
	}
}
