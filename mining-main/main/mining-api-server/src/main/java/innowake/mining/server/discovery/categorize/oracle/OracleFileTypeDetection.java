/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.oracle;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * File type detection for Oracle.
 */
public class OracleFileTypeDetection extends AbstractFileTypeDetection {

	private static final Pattern DEFAULT_LB = Pattern.compile("\n");
	private final Config config;
	
	private static final List<String> CDO_PATTERN = Arrays.asList("ATTACH TO COMPOSITE", "CHANGE COLLECTION", "CHANGE CONTEXT", "CHANGE DATABASE",
			"CHANGE FIELD", "CHANGE FILE_ELEMENT", "CHANGE GENERIC", "CHANGE PARTITION", "CHANGE PROTECTION", "CHANGE RECORD", "CLEAR NOTICES",
			"CLOSE FILE_ELEMENT", "COMMIT", "CONSTRAIN", "CONVERT", "COPY", "DEFINE COLLECTION", "DEFINE CONTEXT", "DEFINE DATABASE", "DEFINE DIRECTORY",
			"DEFINE FIELD", "DEFINE FILE_ELEMENT", "DEFINE GENERIC", "DEFINE KEY", "DEFINE PARTITION", "DEFINE PROTECTION", "DEFINE RECORD",
			"DEFINE REPOSITORY", "DEFINE RMS_DATABASE", "DELETE COLLECTION", "DELETE CONTEXT", "DELETE DATABASE", "DELETE DIRECTORY", "DELETE FIELD",
			"DELETE FILE_ELEMENT", "DELETE GENERIC", "DELETE HISTORY", "DELETE PARTITION", "DELETE PROTECTION", "DELETE RECORD", "DELETE REPOSITORY",
			"DELETE RMS_DATABASE", "DETACH FROM COMPOSITE", "DIRECTORY", "EDIT", "ENTER", "EXIT", "EXTRACT", "FETCH", "HELP", "MERGE", "MOVE REPOSITORY", "ON",
			"OPEN FILE_ELEMENT", "PROMOTE", "PURGE", "REMOVE", "REPLACE", "RESERVE", "ROLLBACK", "SET CHARACTER_SET", "SET CONTEXT", "SET DEFAULT", "SET KEY",
			"SET OUTPUT", "SET VERIFY", "SHOW ALL", "SHOW CHARACTER_SET", "SHOW COLLECTION", "SHOW CONTEXT", "SHOW DATABASE", "SHOW DEFAULT", "SHOW FIELD",
			"SHOW FILE_ELEMENT", "SHOW GENERIC", "SHOW KEY", "SHOW NOTICES", "SHOW PARTITION", "SHOW PRIVILEGES", "SHOW PROTECTION", "SHOW PROTOCOL",
			"SHOW RECORD", "SHOW REPOSITORIES", "SHOW RESERVATIONS", "SHOW RMS_DATABASE", "SHOW UNUSED", "SHOW USED_BY", "SHOW USES", "SHOW VERSION",
			"SHOW WHAT_IF", "SPAWN", "START_TRANSACTION", "UNRESERVE", "UPDATE", "VERIFY");
	
	private static final List<String> SQLMOD_PATTERN = Arrays.asList("DIALECT", "AUTHORIZATION", "ALIAS", "PARAMETER COLONS", "DEFAULT CHARACTER SET",
			"CATALOG ADMINISTRATION", "DECLARE", "PROCEDURE");
	/**
	 * Create a new instance of the Oracle file type detector.
	 * 
	 * @param config Project configuration file.
	 */
	public OracleFileTypeDetection(final Config config) {
		super(getLanguage());
		this.config = config;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		return null;
	}

	@Override
	@Nullable
	public Identification identifyByContent(final SourcePojo resource) {
		final String result = resource.getContent().toString();
		final List<String> lines = Arrays.asList(DEFAULT_LB.split(result));
		if (isCDO(lines)) {
			return new Identification(ID.MAYBE, resource.getId(), ResolveTarget.CDO_FILE, getLanguage());
		} else if (containsSQLMODRequiredContents(lines)) {
			if (ResolveTargetHelper.matchesAnyExtension(ResolveTarget.SQLMOD, FilenameUtils.getExtension(resource.getPath())) 
					|| containsSQLMODOptionalContents(lines)) {
				return new Identification(ID.YES, resource.getId(), ResolveTarget.SQLMOD, getLanguage());
			} else {
				return new Identification(ID.MAYBE, resource.getId(), ResolveTarget.SQLMOD, getLanguage());
			}
		} else {
			return null;
		}
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.ORACLE;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

	private boolean isCDO(final List<String> lines) {		
		final boolean startsWith$ = lines.stream()
				.anyMatch(line -> line.startsWith("$"));
		if (startsWith$) {
			return false;
		}
		final List<String> capitalizedContent = lines.stream().map(StringUtils::upperCase).collect(Collectors.toList());
		return discoverByThreshold(capitalizedContent, CDO_PATTERN, config.getCdoPatternThreshold());
	}
	
	private boolean containsSQLMODRequiredContents(final List<String> lines) {
		final boolean containsModule = lines.stream().anyMatch(line -> line.startsWith("MODULE"));
		if ( ! containsModule) {
			return false;
		}
		return lines.stream().anyMatch(line -> line.startsWith("LANGUAGE"));					
	}
	
	private boolean containsSQLMODOptionalContents(final List<String> lines) {		
		final List<String> capitalizedContent = lines.stream().map(StringUtils::upperCase).collect(Collectors.toList());
		return discoverByThreshold(capitalizedContent, SQLMOD_PATTERN, config.getSqlmodPatternThreshold());
	}

}
