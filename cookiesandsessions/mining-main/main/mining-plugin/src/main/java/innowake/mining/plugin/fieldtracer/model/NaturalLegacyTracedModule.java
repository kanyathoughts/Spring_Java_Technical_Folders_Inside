/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.model;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IFile;

import innowake.ndt.cobolclipse.core.object.ICobolObject;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.natural.NaturalLexerFactory;
import innowake.ndt.core.parsing.spi.StringContent;
import innowake.ndt.fieldtracing.OffsetModel;
import innowake.ndt.fieldtracing.ParsedProgram;
import innowake.ndt.fieldtracing.model.TracedModule;
import innowake.ndt.fieldtracing.natural.parser.NaturalParserActions;
import innowake.ndt.ide.core.object.IIdeObjectBaseType;
import innowake.ndt.natclipse.core.object.IDependencyNode;
import innowake.ndt.natclipse.core.object.INaturalObject;
import innowake.ndt.natclipse.core.object.NaturalType;

/**
 *  Implementation of {@link LegacyTracedModule} based on {@link ICobolObject} and {@link IFile}
 */
public class NaturalLegacyTracedModule extends LegacyTracedModule<INaturalObject, IFile> {

	/**
	 * Constructor to instantiate ICobol object instance
	 * 
	 * @param object type of program object
	 */
	public NaturalLegacyTracedModule(final INaturalObject object) {
		super(object);
	}

	@Override
	public Type getType() {
		final IIdeObjectBaseType baseType = object.getType().getBaseType();
		if (baseType.equals(NaturalType.TYPE_COPYCODE)) {
			return Type.TYPE_COPYCODE;
		} else if (baseType.equals(NaturalType.TYPE_IW_LDA)) {
			return Type.TYPE_IW_LDA;
		} else if (baseType.equals(NaturalType.TYPE_IW_GDA)) {
			return Type.TYPE_IW_GDA;
		} else if (baseType.equals(NaturalType.TYPE_IW_PDA)) {
			return Type.TYPE_IW_PDA;
		} else if (baseType.equals(NaturalType.TYPE_LDA)) {
			return Type.TYPE_LDA;
		} else if (baseType.equals(NaturalType.TYPE_GDA)) {
			return Type.TYPE_GDA;
		} else if (baseType.equals(NaturalType.TYPE_PDA)) {
			return Type.TYPE_PDA;
		} else if (baseType.equals(NaturalType.TYPE_PROGRAM)) {
			return Type.TYPE_PROGRAM;
		} else if (baseType.equals(NaturalType.TYPE_SUBPROGRAM)) {
			return Type.TYPE_SUBPROGRAM;
		} else if (baseType.equals(NaturalType.TYPE_SUBROUTINE)) {
			return Type.TYPE_SUBROUTINE;
		} else if (baseType.equals(NaturalType.TYPE_MAP)) {
			return Type.TYPE_MAP;
		} else 
			return Type.UNKNOWN;
		}

	private NaturalType returnNaturalType(final Type type) {
		switch (type) {
			case TYPE_GDA:
				return NaturalType.TYPE_GDA;
			case TYPE_IW_GDA:
				return NaturalType.TYPE_IW_GDA;
			case TYPE_IW_LDA:
				return NaturalType.TYPE_IW_LDA;
			case TYPE_IW_PDA:
				return NaturalType.TYPE_IW_PDA;
			case TYPE_LDA:
				return NaturalType.TYPE_LDA;
			case TYPE_MAP:
				return NaturalType.TYPE_MAP;
			case TYPE_PDA:
				return NaturalType.TYPE_PDA;
			case TYPE_PROGRAM:
				return NaturalType.TYPE_PROGRAM;
			case TYPE_SUBPROGRAM:
				return NaturalType.TYPE_PROGRAM;
			case TYPE_SUBROUTINE:
				return NaturalType.TYPE_PROGRAM;
			default:
				return NaturalType.TYPE_UNKNOWN;
		}
		
	}

	@Override
	public Language getLanguage() {
		return Language.NATURAL;
	}

	@Override
	public ParsedProgram<IFile, ? extends AstModel, ? extends AstNode, ? extends AstNode, ? extends AstNode, ? extends OffsetModel<?>> getParsedProgram() {
		return new NaturalParserActions<IFile>().parse(this);
	}

	@Override
	public Optional<TracedModule<IFile>> findObject(final String name, final Type type) {
		final INaturalObject natObject = object.findObject(name, returnNaturalType(type));
		return natObject != null ? Optional.of(new NaturalLegacyTracedModule(natObject)) : Optional.empty();
	}

	@Override
	public ITokenPartitioning getTokenPartitioning() {
		final TokenPartitioner2 partitioner = TokenPartitioner2.create(NaturalLexerFactory.DEFAULT);
		return partitioner.doPartitioning(new StringContent(getContent()));
	}

	@Override
	public IFile getSourceObject() {
		return super.getObject().getFile();
	}
	
	@Override
	public List<TracedModule<IFile>> getIncomingDeps() {
		return object.createDependencyTree().getIncoming().stream()
				.map(IDependencyNode::getTargetObject)
				.filter(Objects::nonNull)
				.map(NaturalLegacyTracedModule::new)
				.collect(Collectors.toList());
	}

	@Override
	public List<TracedModule<IFile>> getOutgoingDeps() {
		return object.createDependencyTree().getOutgoing().stream()
				.map(IDependencyNode::getTargetObject)
				.filter(Objects::nonNull)
				.map(NaturalLegacyTracedModule::new)
				.collect(Collectors.toList());
	}
}
