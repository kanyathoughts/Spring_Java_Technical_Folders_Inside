/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.model;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IFile;

import innowake.ndt.cobolclipse.core.object.CobolBaseType;
import innowake.ndt.cobolclipse.core.object.ICobolDependencyNode;
import innowake.ndt.cobolclipse.core.object.ICobolObject;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;
import innowake.ndt.core.parsing.spi.StringContent;
import innowake.ndt.fieldtracing.OffsetModel;
import innowake.ndt.fieldtracing.ParsedProgram;
import innowake.ndt.fieldtracing.cobol.parser.CobolParserActions;
import innowake.ndt.fieldtracing.model.TracedModule;
import innowake.ndt.ide.core.object.IIdeObjectBaseType;
import innowake.ndt.ide.core.object.IIdeTypeMatcher;

/**
 * Implementation of {@link LegacyTracedModule} based on {@link ICobolObject} and {@link IFile}
 */
public class CobolLegacyTracedModule extends LegacyTracedModule<ICobolObject, IFile> {

	/**
	 * Constructor to instantiate ICobol object instance
	 * 
	 * @param object type of program object
	 */
	public CobolLegacyTracedModule(final ICobolObject object) {
		super(object);
	}

	@Override
	public Type getType() {
	final IIdeObjectBaseType baseType = object.getType().getBaseType();
		if (baseType instanceof CobolBaseType) {
			switch((CobolBaseType) baseType) {
				case BMS:
					return Type.BMS;
				case COPYBOOK:
					return Type.COPYBOOK;
				case PROGRAM:
					return Type.PROGRAM;
				case RESOURCE:
					return Type.RESOURCE;
				default:
					break;
			}
		}
		return Type.UNKNOWN;
	}

	@Override
	public Language getLanguage() {
		return Language.COBOL;
	}

	@Override
	public ParsedProgram<IFile, ? extends AstModel, ? extends AstNode, ? extends AstNode, ? extends AstNode, ? extends OffsetModel<?>> getParsedProgram() {
		return new CobolParserActions<IFile>().parse(this);
	}

	@Override
	public ITokenPartitioning getTokenPartitioning() {
		final TokenPartitioner2 partitioner = TokenPartitioner2.create(CobolLexerFactory.get());
		return partitioner.doPartitioning(new StringContent(getContent()));
	}

	@Override
	public Optional<TracedModule<IFile>> findObject(final String name, final Type type) {
		final ICobolObject cobolObject = object.findObject(name, returnCobolType(type));
		return cobolObject != null ? Optional.of(new CobolLegacyTracedModule(cobolObject)) : Optional.empty();
	}

	private  IIdeTypeMatcher returnCobolType(final Type type) {
		IIdeTypeMatcher cobolType = null;
		switch(type) {
			case BMS:
				cobolType = CobolBaseType.BMS;
				break;
			case COPYBOOK:
				cobolType = CobolBaseType.COPYBOOK;
				break;
			case PROGRAM:
				cobolType = CobolBaseType.PROGRAM;
				break;
			case RESOURCE:
				cobolType = CobolBaseType.RESOURCE;
				break;
			default:
				break;
		}
		if (cobolType != null) {
		return cobolType;
		}
		else throw new IllegalStateException("Unsupported type for Cobol");
	}

	@Override
	public IFile getSourceObject() {
		return super.getObject().getFile();
	}

	@Override
	public List<TracedModule<IFile>> getIncomingDeps() {
		return object.createDependencyTree().getIncoming().stream()
				.map(ICobolDependencyNode::getTargetObject)
				.filter(Objects::nonNull)
				.map(CobolLegacyTracedModule::new)
				.collect(Collectors.toList());
	}

	@Override
	public List<TracedModule<IFile>> getOutgoingDeps() {
		return object.createDependencyTree().getOutgoing().stream()
				.map(ICobolDependencyNode::getTargetObject)
				.filter(Objects::nonNull)
				.map(CobolLegacyTracedModule::new)
				.collect(Collectors.toList());
	}
}
