package innowake.mining.server.discovery.categorize.cobol;

import static innowake.lib.core.lang.Assert.assertInstanceOf;
import static innowake.mining.shared.model.discovery.ResolveTarget.CICS_BMS_MAPSET;
import static innowake.mining.shared.model.discovery.ResolveTarget.COBOL_COPYBOOK;
import static innowake.mining.shared.model.discovery.ResolveTarget.COBOL_COPYLIB;
import static innowake.mining.shared.model.discovery.ResolveTarget.IMS_MFS;
import static innowake.mining.shared.model.discovery.ResolveTarget.COBOL_PROGRAM;
import static innowake.mining.shared.model.discovery.ResolveTarget.NONE;
import static innowake.ndt.core.assembling.cobol.UnisysCobolCopyLibParser.PROC_END_PATTERN;
import static innowake.ndt.core.assembling.cobol.UnisysCobolCopyLibParser.PROC_PATTERN;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.google.common.collect.Sets;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.IAstNode;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.LexerConfiguration;
import innowake.ndt.core.parsing.TokenPartitioner;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;
import innowake.ndt.core.parsing.cobol.CobolRegionType;
import innowake.ndt.core.parsing.cobol.ast.CobolCopyNode;
import innowake.ndt.core.parsing.cobol.ast.CobolExecNode;
import innowake.ndt.core.parsing.cobol.ast.CobolLightweightParser;
import innowake.ndt.core.parsing.cobol.ast.CobolNodeTypes;
import innowake.ndt.core.parsing.spi.StringContent;

/**
 * File type detection for Cobol.
 */
public class CobolFileTypeDetection extends AbstractFileTypeDetection {
	
	private static final String SECTION = " SECTION";
	private static final String DIVISION = " DIVISION";
	public static final String CACHE_KEY = ResolveTarget.COBOL.name();
	private static final Pattern COPY_NAME_NORMALIZER_PATTERN = Pattern.compile("\"|'");
	private enum TypeIndicator {
		PROGRAM_ID, IDENTIFICATION_DIVISION, PROCEDURE_DIVISION, ENVIRONMENT_DIVISION,
		OBJECT_SECTION, OBJECT_STORAGE_SECTION, WORKING_STORAGE_SECTION, DATA_DIVISION
	}

	private static final LexerConfiguration CONFIG = new LexerConfiguration();
	static {
		CONFIG.addRegionTypeToLex(CobolRegionType.TYPE_CODE_STANDARD);
		CONFIG.addRegionTypeToLex(CobolRegionType.TYPE_CODE_LITERAL);
	}
	
	private final EnumSet<TypeIndicator> identifiedIndicator = EnumSet.noneOf(TypeIndicator.class);
	private final Config config;
	private final DiscoveryJobCache discoveryCache;
	private final String jobId;	
	
	public CobolFileTypeDetection(final Config config, final DiscoveryJobCache discoveryCache, final String jobId) {
		super(getLanguage());
		this.config = config;
		this.discoveryCache = discoveryCache;
		this.jobId = jobId;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		final Identification result;
		ID identified = ID.NO;
		ResolveTarget type = NONE;
		identifiedIndicator.clear();
		
		try (final Scanner scanner = new Scanner(resource.getContent().toString())) {
			while (scanner.hasNextLine()) {
				final String line = scanner.nextLine().toUpperCase();
				if ( ! isEmptyOrComment(line)) {
					analyseLineForCobol(line);
					identified = isCobol();
					if (ID.YES == identified) {
						type = COBOL_PROGRAM;
						break;
					}
				}
			}
		}
		if (ID.MAYBE == identified) {
			type = COBOL_PROGRAM;
		}
		result = type == NONE ? null : new Identification(identified, resource.getId(), type, getLanguage());
		if (result != null && result.getId() != ID.NO) {
			LOG.debug(() -> "Filetype detection Cobol: " + result);
		}
		return result;
	}
	
	@Override
	public boolean identifyDependencies(final SourcePojo sourceObject) {
		boolean identifiedAnyDependencies = false;
		final CobolLightweightParser<SourcePojo> parser = CobolLightweightParser.getDefaultDependencyParser();
		final StringContent content = new StringContent(sourceObject.getContent().toString());
		final ITokenPartitioning partitioning = new TokenPartitioner(CobolLexerFactory.get(), CONFIG).doPartitioning(content);
		final IAst<SourcePojo> ast = parser.parse(partitioning);
		final IAstNode[] copies = ast.getNodes(CobolNodeTypes.COPY);
		/* Put results into cache for the main job to process later on, since only the job knows all resources. */
		identifiedAnyDependencies = copies.length > 0;
		Arrays.asList(copies).parallelStream()
			.forEach(copynode -> {
				final CobolCopyNode copy = assertInstanceOf(copynode, CobolCopyNode.class);
				final String target = COPY_NAME_NORMALIZER_PATTERN.matcher(copy.getTarget()).replaceAll("");
				discoveryCache.putMultiValue(jobId, CACHE_KEY, target);
			});
		
		final IAstNode[] includes = ast.getNodes(CobolNodeTypes.EXEC_SQL_INCLUDE);
		identifiedAnyDependencies |= includes.length > 0;
		Arrays.asList(includes).parallelStream()
			.forEach(node -> {
				final CobolExecNode copy = assertInstanceOf(node, CobolExecNode.class);
				final String target = COPY_NAME_NORMALIZER_PATTERN.matcher(copy.getTarget()).replaceAll("");
				discoveryCache.putMultiValue(jobId, CACHE_KEY, target);
			});
		
		return identifiedAnyDependencies;
	}

	@Override
	@Nullable
	public Identification identifyByContent(final SourcePojo resource) {
		final String content = resource.getContent().toString();
		final CobolTokenCounter tokenCounter = new CobolTokenCounter(content).count();
		if (tokenCounter.getBmsTokenCount() > 0) {
			final Identification identification = new Identification(ID.YES, resource.getId(), CICS_BMS_MAPSET, getLanguage());
			LOG.debug(() -> "Filetype detection Cobol: " + identification);
			return identification;
		} else if (tokenCounter.getFmsTokenCount() > 0) {
			final Identification identification = new Identification(ID.YES, resource.getId(), IMS_MFS, getLanguage());
			LOG.debug(() -> "Filetype detection Cobol: " + identification);
			return identification;
		} else if (isCopy(tokenCounter.getCopyTokenCount(), tokenCounter.getTokenCount())) {
			final Identification identification = new Identification(ID.YES, resource.getId(), COBOL_COPYBOOK, getLanguage());
			LOG.debug(() -> "Filetype detection Cobol: " + identification);
			return identification;
		} else if (isCopyLib(content)) {
			final Identification identification = new Identification(ID.YES, resource.getId(), COBOL_COPYLIB, getLanguage());
			LOG.debug(() -> "Filetype detection Cobol: " + identification);
			return identification;
		}
		return null;
	}

	private ID isCobol() {
		if (identifiedIndicator.contains(TypeIndicator.PROGRAM_ID) && identifiedIndicator.contains(TypeIndicator.IDENTIFICATION_DIVISION)) {
			return ID.YES;
		}
		final long otherSectionCount = identifiedIndicator.stream().filter(indicator -> 
			(TypeIndicator.ENVIRONMENT_DIVISION == indicator) || (TypeIndicator.OBJECT_SECTION == indicator)
			|| (TypeIndicator.OBJECT_STORAGE_SECTION == indicator) || (TypeIndicator.WORKING_STORAGE_SECTION == indicator)
			|| (TypeIndicator.DATA_DIVISION == indicator)).count();
		final boolean isProcedure = identifiedIndicator.contains(TypeIndicator.PROCEDURE_DIVISION);
		final boolean containsProcedureDivision =  isProcedure && (otherSectionCount >= Long.valueOf(1));
		
		if ((otherSectionCount >= Long.valueOf(3)) || containsProcedureDivision) {
			return ID.YES;
		}
		if (isProcedure || (otherSectionCount >= Long.valueOf(1))) {
			return ID.MAYBE;
		}
		return ID.NO;
	}

	private void analyseLineForCobol(final String line) {
		if (line.contains("PROGRAM-ID")) {
			identifiedIndicator.add(TypeIndicator.PROGRAM_ID);
		}
		if (line.contains("IDENTIFICATION ") && line.contains(DIVISION) || line.contains("ID ") && line.contains(DIVISION)) {
			identifiedIndicator.add(TypeIndicator.IDENTIFICATION_DIVISION);
		}
		analyseForAdditionalIndicators(line);
	}

	private void analyseForAdditionalIndicators(final String line) {
		if (line.contains("PROCEDURE ") && line.contains(DIVISION)) {
			identifiedIndicator.add(TypeIndicator.PROCEDURE_DIVISION);
		}
		if (line.contains("ENVIRONMENT ") && line.contains(DIVISION)) {
			identifiedIndicator.add(TypeIndicator.ENVIRONMENT_DIVISION);
		}
		if (line.contains("OBJECT ") && line.contains(SECTION)) {
			identifiedIndicator.add(TypeIndicator.OBJECT_SECTION);
		}
		if (line.contains("OBJECT-STORAGE ") && line.contains(SECTION)) {
			identifiedIndicator.add(TypeIndicator.OBJECT_STORAGE_SECTION);
		}
		if (line.contains("WORKING-STORAGE ") && line.contains(SECTION)) {
			identifiedIndicator.add(TypeIndicator.WORKING_STORAGE_SECTION);
		}
		if (line.contains("DATA ") && line.contains(DIVISION)) {
			identifiedIndicator.add(TypeIndicator.DATA_DIVISION);
		}
	}

	/**
	 * Check if the provided content is of type cobol copybook.
	 * The tokens are checked if 10% are of type <code>PIC</code> or begin with <code>X(</code>.
	 * @param picCount PIC count
	 * @param tokenCount token count
	 * 
	 * @return true if the content seems to be a cobol copybook.
	 */
	public boolean isCopy(final int picCount, final int tokenCount) {
		if (picCount > 0) {
			return picCount / (double) tokenCount >= config.getCopybookPicThreshold();
		}
		return false;
	}

	private boolean isEmptyOrComment(final String line) {
		/* for cobol */
		if (line.length() >= 7 && line.charAt(6) == '*') {
			return true;
		}
		/* for bms */
		return line.length() >= 1 && line.charAt(0) == '*';
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.COBOL;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

	@Override
	@Nullable
	public Identification identifyByExtension(final SourcePojo resource) {
		/* Doesn't support identify by extension */
		return null;
	}

	private boolean isCopyLib(final String content) {
		final Matcher matchProc = PROC_PATTERN.matcher(content);
		final Matcher matchEnd = PROC_END_PATTERN.matcher(content);
		int copyEndToken = 0;
		int copyProcToken = 0;
		while (matchProc.find()) {
			copyProcToken++;
		}
		while (matchEnd.find()) {
			copyEndToken++;
		}
		return (copyProcToken == copyEndToken) && (copyProcToken + copyEndToken) > 0;
	}
}
