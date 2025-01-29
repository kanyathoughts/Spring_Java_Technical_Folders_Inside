/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.resources.IFile;

import innowake.mining.plugin.fieldtracer.model.CobolLegacyTracedModule;
import innowake.mining.plugin.fieldtracer.model.NaturalLegacyTracedModule;
import innowake.ndt.cobolclipse.core.CobolServices;
import innowake.ndt.cobolclipse.core.object.ICobolObject;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.fieldtracing.OffsetModel;
import innowake.ndt.fieldtracing.ParsedProgram;
import innowake.ndt.fieldtracing.cobol.parser.CobolParserActions;
import innowake.ndt.fieldtracing.natural.parser.NaturalParserActions;
import innowake.ndt.natclipse.ServiceRegistry;
import innowake.ndt.natclipse.core.object.INaturalObject;

/**
 * Factory for creating new {@link ParsedProgram}s.
 */
public class ParsedProgramFactory {

	private ParsedProgramFactory() {}

	/**
	 * Creates a new {@link ParsedProgram} for the gicen {@link IFile}.
	 *
	 * @param file the {@link IFile}
	 * @return the newly created {@link ParsedProgram}
	 */
	public static
	ParsedProgram<?, ? extends AstModel, ? extends AstNode, ? extends AstNode, ? extends AstNode, ? extends OffsetModel<?>>
	create(final IFile file) {
		final INaturalObject naturalObject = ServiceRegistry.get().getNaturalWorkspaceManager().findObject(file);
		if (naturalObject != null) {
			final NaturalLegacyTracedModule natModule = new NaturalLegacyTracedModule(naturalObject);
			return new NaturalParserActions<IFile>().parse(natModule);
		}
		final ICobolObject cobolObject = CobolServices.get().getWorkspaceManager().findObject(file);
		if (cobolObject != null) {
			final CobolLegacyTracedModule cobolModule = new CobolLegacyTracedModule(cobolObject);
			return new CobolParserActions<IFile>().parse(cobolModule);
		}
		throw new NotImplementedException(String.format("Fieldtracing of file %s is not supported. Currently only Cobol and Natural objects are supported.",
				file.getFullPath().toOSString()));
	}
}
