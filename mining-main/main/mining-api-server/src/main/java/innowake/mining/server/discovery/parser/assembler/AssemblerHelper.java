/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.assembler;

import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.assembler.parser.InstructionRegistry;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.assembler.AssemblerLexerConfiguration;
import innowake.ndt.core.parsing.assembler.AssemblerLexerFactory;
import innowake.ndt.core.parsing.spi.StringContent;

/**
 * Helper class for Assembler Parser
 */
public class AssemblerHelper {

	private final InstructionRegistry<SourcePojo> instructionRegistry;

	/**
	 * Creates the instance of {@link AssemblerHelper}
	 * @param assemblerSourceObjectResolver the assemblerSourceObjectResolver
	 */
	public AssemblerHelper(SourceObjectResolver assemblerSourceObjectResolver) {
		instructionRegistry = new InstructionRegistry<>(new AssemblerDataProvider(assemblerSourceObjectResolver));
	}
	
	public InstructionRegistry<SourcePojo> getInstructionRegistry() {
		return instructionRegistry;
	}
	
	public static ITokenPartitioning getPartitioning(final String content) {
		final TokenPartitioner2 partitioner = TokenPartitioner2.create(AssemblerLexerFactory.get(AssemblerLexerConfiguration.DEFAULT));
		return partitioner.doPartitioning(new StringContent(content));
	}
	
}
