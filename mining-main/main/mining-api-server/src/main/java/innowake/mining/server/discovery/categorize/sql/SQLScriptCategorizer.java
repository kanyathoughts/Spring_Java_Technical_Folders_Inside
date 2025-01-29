/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.sql;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import com.google.common.collect.Sets;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.spi.StringContent;
import innowake.ndt.parsing.scanner.generic.GenericRegionType;
import innowake.ndt.parsing.scanner.generic.LocHelper;
import innowake.ndt.parsing.scanner.plsql.PlSqlLexerFactory;

/**
 * Categorizer for SQL files.
 */
public class SQLScriptCategorizer extends AbstractFileTypeDetection {

	private static final Pattern IF_STATEMENT = Pattern.compile(
			"IF\\s.+?\\sEND\\s+IF",
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
	
	private static final Pattern DECLARE_CURSOR = Pattern.compile(
			"DECLARE\\s.+?CURSOR",
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
	
	private static final Pattern WHILE_LOOP = Pattern.compile(
			"WHILE\\s.+?\\sDO\\s.+?\\sEND\\s+WHILE",
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
	
	private static final Pattern FOR_LOOP = Pattern.compile(
			"FOR\\s.+?\\sDO\\s.+?\\sEND\\s+FOR",
			Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
	
	private static final Pattern CREATE_PROCEDURE = Pattern.compile(
			"CREATE\\s+PROCEDURE",
			Pattern.CASE_INSENSITIVE);
	
	private static final Pattern LANGUAGE_SQL = Pattern.compile(
			"LANGUAGE\\s+SQL",
			Pattern.CASE_INSENSITIVE);
	
	private static final Pattern SQL_RESERVED_KEYWORDS = Pattern.compile(
			"(SELECT\\s.*FROM\\s)",
			Pattern.CASE_INSENSITIVE);
	
	private static final Pattern SQL_CRUD_KEYWORDS = Pattern.compile(
			"(INSERT\\s)|(UPDATE\\s)|(TRUNCATE\\s)",
			Pattern.CASE_INSENSITIVE);
	
	private static final Pattern SQL_DML_KEYWORDS_AND_OTHERS = Pattern.compile(
			"(CREATE\\s+TABLE)|(CREATE\\s+(UNIQUE\\s+)?INDEX)|(ALTER\\s+TABLE)|(TRUNCATE\\s+TABLE)|(DELETE\\s+FROM)|(DROP\\s+TABLE)|(DROP\\s+INDEX)|(WHERE\\s)|(ORDER\\s+BY)|(GROUP\\s+BY)",
			Pattern.CASE_INSENSITIVE);
	
	private static final Pattern SQL_COMMENT = Pattern.compile("--\\s");
	
	private static final List<Pattern> PATTERNS = Arrays.asList(
			IF_STATEMENT, WHILE_LOOP, DECLARE_CURSOR, FOR_LOOP,
			CREATE_PROCEDURE, SQL_RESERVED_KEYWORDS, SQL_CRUD_KEYWORDS, SQL_DML_KEYWORDS_AND_OTHERS
	);

	private static final TokenPartitioner2 TOKEN_PARTITIONER = TokenPartitioner2.create(PlSqlLexerFactory.get());

	/**
	 * Constructor.
	 */
	public SQLScriptCategorizer() {
		super(getLanguage());
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.SQL;
	}

	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

	@Override
	@Nullable
	public Identification identifyByContent(final SourcePojo resource) {
		final String result = resource.getContent().toString();
		final ITokenPartitioning partitioning = TOKEN_PARTITIONER.doPartitioning(new StringContent(result));
		final String codeContent = partitioning.getFilteredSource(GenericRegionType.TYPE_CODE_STANDARD);
		if (processReliable(codeContent)) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.SQL_SCRIPT, getLanguage());
		} else if (processNonReliable(codeContent, partitioning)) {
			final long matchCount =
					PATTERNS.stream()
							.filter(match -> match.matcher(codeContent).find())
							.count();
			if (matchCount > 3) {
				return new Identification(ID.YES, resource.getId(), ResolveTarget.SQL_SCRIPT, getLanguage());
			} else {
				return new Identification(ID.MAYBE, resource.getId(), ResolveTarget.SQL_SCRIPT, getLanguage());
			}
		}
		return Identification.notIdentified(resource, getLanguage());
	}

	@Override
	public @Nullable Identification identifyMainObject(final SourcePojo sourceObject) throws DiscoveryException {
		return null;
	}

	private boolean processNonReliable(final String content, final ITokenPartitioning partitioning) {
		if (PATTERNS.stream().anyMatch(pattern -> pattern.matcher(content).find())) {
			return true;
		}
		return SQL_COMMENT.matcher(content).find() && (getSemicolonCount(partitioning) > LocHelper.countLinesOfCode(partitioning) / 2);
	}

	private static int getSemicolonCount(final ITokenPartitioning partitioning) {
		final int semiColonCount = (int) Arrays.stream(partitioning.getTokens(GenericRegionType.TYPE_CODE_STANDARD))
				.map(IToken::getText)
				.filter(";"::equals)
				.count();
		return semiColonCount;
	}

	private boolean processReliable(final String content) {
		return LANGUAGE_SQL.matcher(content).find();
	}
}
