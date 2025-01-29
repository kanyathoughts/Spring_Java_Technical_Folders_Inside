/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.cobol;

import com.google.common.base.Splitter;
import innowake.lib.core.lang.Boxing;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.categorize.Statistic;
import innowake.mining.server.discovery.metrics.cobol.CobolStatementUtility;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import javax.persistence.EntityNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Used to identify if a Cobol object contains more than one Cobol programs.
 * Also used to split this into multiple files.
 * The original file content is replaced with the first program and the
 * other extracted programs are stored in a new folder named after the original file
 * with the path <code>ORIGINAL-NAME/PROGRAM-ID</code> and the same file extension.
 * <br>
 * Example file named <code>E900VSA1.cbl</code>
 * <pre>
      **************************************************************
      ***                       E900VSA1 ENTRY                   ***
      **************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E900VSA1.
           COPY FUXVSACC.
       END PROGRAM E900VSA1.
      **************************************************************
      ***                       E900VSA2 ENTRY                   ***
      **************************************************************
--------------------------------SPLIT--------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E900VSA2.
           COPY FUXVSACC.
       END PROGRAM E900VSA2.
 * </pre>
 * Results in a modified file <code>E900VSA1.cbl</code> with the first program 
 * and additionally a new file <code>E900VSA1/E900VSA2.cbl</code> with the content of the 
 * second program.
 * <br><br><b>Limitations</b>
 * <ul>
 * <li>The header comments of the additional programs are not correct set in the new
 * file. It is not possible to distinguish between a program header or footer comment.
 * </ul>
 */
public final class CobolModuleSplitter {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CATEGORIZE_CATEGORIZER);

	private CobolModuleSplitter() {
	}

	/**
	 * Execute the Cobol program splitting.
	 * <br>Tasks:
	 * <ol>
	 * <li>Resolve the mapping of line numbers assigned to programs.</li>
	 * <li>Replace the content of the given Cobol module with the first program.</li>
	 * <li>Create new Cobol programs in a folder with the name of the original program next to it.</li>
	 * </ol>
	 *
	 * @param original The Cobol module to be split. 
	 * @param sourceService the dao class used to persist splitted contents.
	 * @param statistic the statistic summary.
	 * @return The provided Cobol object for chains.
	 */
	public static SourcePojo execute(final SourcePojo original, final SourceCachingService sourceService, final Statistic statistic) {		
		try {
			final String content = original.getContent().toString();
			final String[] lines = CobolStatementUtility.getLines(content);

			if (!isRequired(lines)) {
				return original;
			}

			final List<Integer> mapping = getMapping(lines);
			final List<Tuple2<String, String>> resolvedPrograms = resolveMapping(lines, mapping, original.getName());

			savePrograms(original, resolvedPrograms, sourceService, statistic);
		} catch (final DiscoveryException exception) {
			LOG.error(()->"Error while split of the cobol module " + original.getName() + ". Cause: " + exception.getMessage());
		}
		return original;
	}

	private static boolean isRequired(final String[] lines) {
		return Arrays.stream(lines)
				.filter(line -> ! CobolStatementUtility.isComment(line))
				.filter(CobolStatementUtility::isIdentificationDivision)
				.count() > 1;
	}
	
	private static void savePrograms(final SourcePojo original, final List<Tuple2<String, String>> resolvedPrograms, final SourceCachingService sourceService,
			final Statistic statistic) {
		boolean isFirstProgram = true;
		for (Tuple2<String, String> resolvedProgram : resolvedPrograms) {
			final String resolvedName = resolvedProgram.e1;
			final BinaryString resolvedContent = new BinaryString(resolvedProgram.e2);
			final String path = new StringBuilder(FilenameUtils.getPath(original.getPath()))
					.append(isFirstProgram ? "" : (original.getName() + '/'))
					.append(resolvedName)
					.append(".")
					.append(FilenameUtils.getExtension(original.getPath()))
					.toString();
			if (isFirstProgram) {
				LOG.info(() -> "Splitting first COBOL module of " + original.getPath() + " into " + path);
			} else {
				LOG.info(() -> "Splitting next COBOL module of " + original.getPath() + " into " + path);
			}
			boolean sourceObjectExists;
			final SourcePojo existingSourceObject;
			try {
				existingSourceObject = sourceService.cachingByProjectPath(original.getProject().getNid(), path);
				/* performs upsert operation on the main cobol object */
				sourceService.update(new SourcePojoPrototype()
						.setProject(original.getProject())
						.setUid(existingSourceObject.getUid())
						.setName(resolvedName)
						.setPath(path)
						.setTechnology(original.getTechnology())
						.setType(original.getType())
						.setContent(resolvedContent));
				sourceObjectExists = true;
			} catch (final EntityNotFoundException e) {
				sourceObjectExists = false;
			}
			if ( ! sourceObjectExists) {
				sourceService.create(new SourcePojoPrototype()
						.setProject(original.getProject())
						.setName(resolvedName)
						.setPath(path)
						.setTechnology(original.getTechnology())
						.setType(original.getType())
						.setContent(resolvedContent));
				statistic.addDiscovered(new Identification(ID.YES, original.getId(), ResolveTargetHelper.fromTechnologyAndType(
						original.getTechnology(), original.getType()), ResolveTargetHelper.fromTechnology(original.getTechnology())));
			}
			isFirstProgram = false;
		}
	}

	private static List<Tuple2<String, String>> resolveMapping(final String[] lines, final List<Integer> mapping,
			final String sourceObjectName) throws DiscoveryException {
		int startIndex = 0;
		final List<Tuple2<String, String>> result = new ArrayList<>(mapping.size());
		for (int i = 0; i < mapping.size(); i++) {
			final int index = mapping.get(i).intValue();
			result.add(resolveProgramNameAndBody(startIndex, index, lines, sourceObjectName, i));
			startIndex = index;
		}
		result.add(resolveProgramNameAndBody(startIndex, lines.length, lines, sourceObjectName, mapping.size()));

		return result;
	}

	private static Tuple2<String, String> resolveProgramNameAndBody(final int fromLine, final int toLine, final String[] lines,
			final String sourceObjectName, final int programIndex) throws DiscoveryException {
		final String body = cutProgramBody(fromLine, toLine, lines);
		String name;
		if (programIndex == 0) {
			name = sourceObjectName;
		} else {
			name = cutProgramName(body).orElse(sourceObjectName + "_" + programIndex);
		}
		return new Tuple2<>(name, body);
	}

	private static String cutProgramBody(final int fromLine, final int toLine, final String[] lines) throws DiscoveryException {
		if (toLine <= fromLine) {
			throw new DiscoveryException("FromLine must be <= toLine.");
		}

		if (toLine > lines.length) {
			throw new DiscoveryException("ToLine must be < available lines.");
		}

		final StringBuilder actualProgram = new StringBuilder((toLine - fromLine) * 80);
		for (int i = fromLine; i < toLine; i++) {
			actualProgram.append(lines[i]).append("\r\n");
		}

		return actualProgram.toString();
	}

	private static List<Integer> getMapping(final String[] lines) {
		final List<Integer> result = new ArrayList<>();
		int currentLine = -1;

		for (final String line : lines) {
			currentLine++;
			if ( ! CobolStatementUtility.isComment(line) && CobolStatementUtility.isIdentificationDivision(line)) {
				result.add(Boxing.box(currentLine));
			}
		}
		return result.subList(1, result.size());
	}

	/**
	 * Look for the <code>PROGRAM-ID</code> line and extract the ID.
	 * Example
	 * <pre>
	 *        PROGRAM-ID. YYYYAHBB IS COMMON PROGRAM.
	 * </pre>
	 *
	 * @param body The program body to identify the <code>PROGRAM-ID</code>.
	 * @return The program identification
	 * @throws IOException Thrown if the line split failed.
	 * @throws IllegalStateException if the <code>PROGRAM-ID</code> could not be found.
	 */
	private static Optional<String> cutProgramName(final String body) {
		final String[] lines = CobolStatementUtility.getLines(body);
		final String programLines = Arrays.stream(lines)
				.filter(line -> ! CobolStatementUtility.isComment(line))
				.map(CobolStatementUtility::trimDescription)
				.map(CobolStatementUtility::trimSequence)
				.map(line -> line.replace(".", ""))
				.collect(Collectors.joining(" "));
		if (programLines.isEmpty()) {
			return Optional.empty();
		} else {
			final List<String> programLineList = Splitter.on(' ').omitEmptyStrings().splitToList(programLines);
			final int proIndex = programLineList.indexOf("PROGRAM-ID");
			if (proIndex != -1 && proIndex + 1 < programLineList.size()) {
				/* Remove the single quote or double quotes if any */
				final var programName = StringUtils.strip(programLineList.get(proIndex + 1), "'\"");
				return Optional.of(programName);
			} else {
				return Optional.empty();
			}
		}
	}
}
