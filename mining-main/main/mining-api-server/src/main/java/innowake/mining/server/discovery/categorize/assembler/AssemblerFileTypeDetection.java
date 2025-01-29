package innowake.mining.server.discovery.categorize.assembler;

import static innowake.mining.shared.model.discovery.ResolveTarget.*;
import static innowake.ndt.core.parsing.assembler.AssemblerLexerConfiguration.DEFAULT;

import java.util.Arrays;
import java.util.Set;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.server.discovery.categorize.cobol.CobolIdentificationToken;
import innowake.mining.server.discovery.parser.assembler.AssemblerHelper;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.assembler.model.ast.instruction.Instruction;
import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.assembler.AssemblerLexerFactory;
import innowake.ndt.core.parsing.assembler.AssemblerRegionType;
import innowake.ndt.core.parsing.spi.StringContent;

/**
 * File type detection for Assembler.
 */
public class AssemblerFileTypeDetection extends AbstractFileTypeDetection {

	private final Config config;
	private final AssemblerHelper assemblerHelper;
	
	/** WCFD-111 Adjust threshold to maybe identify ASSEMBLER to max 20% unknown tokens.
	 * Because assembler is very lax on code layout and therefore many known tokens could be found.
	 * These lines are valid assembler code
	 * <br>'REORG INDEX BQPFMIS1.IX_AE_7 PART 1'
	 * <br>'  COPY INDD=JOBLIBI,OUTDD=JOBLIBO' 
	 * @param config the configuration use for assembler detection
	 * @param assemblerHelper The Assembler Helper.
	 */
	public AssemblerFileTypeDetection(final Config config, final AssemblerHelper assemblerHelper) {
		super(getLanguage());
		this.config = config;
		this.assemblerHelper = assemblerHelper;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		final String content = resource.getContent().toString();
		final ITokenPartitioning partitioning = TokenPartitioner2.create(AssemblerLexerFactory.get(DEFAULT)).doPartitioning(new StringContent(content));
		final IToken[] tokens = partitioning.getTokens(AssemblerRegionType.TYPE_CODE_OPERATION);

		if (tokens.length == 0) {
			return null;
		}
		
		final boolean isMacro = "MACRO".equalsIgnoreCase(tokens[0].getText().toString());		

		if (isMacro) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.ASSEMBLER_MACRO, getLanguage());
		}

		final boolean hasCsect = Arrays.stream(tokens).map(IToken::getText).map(CharSequence::toString).anyMatch("CSECT"::equals);

		final boolean hasBMSsect = Arrays.stream(tokens).map(IToken::getText).map(CharSequence::toString)
				.anyMatch(CobolIdentificationToken.getTokens(CICS_BMS_MAPSET)::contains);

		if (hasCsect && ! hasBMSsect) {
			return new Identification(ID.YES, resource.getId(), ASSEMBLER_PROGRAM, getLanguage());
		}
		
		
		final double unknownCount = Arrays.stream(tokens)
				.map(IToken::getText)
				.map(CharSequence::toString)
				.map(name -> assemblerHelper.getInstructionRegistry().getTypeByOperation(resource, name))
				.filter(Instruction.Type.UNKNOWN::equals)
				.count();

		if (tokens.length > 10 && (unknownCount / tokens.length < config.getUnknownTokenThreshold())) {
			return new Identification(ID.MAYBE, resource.getId(), ASSEMBLER_PROGRAM, getLanguage());
		}

		return null;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.ASSEMBLER;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}
}
