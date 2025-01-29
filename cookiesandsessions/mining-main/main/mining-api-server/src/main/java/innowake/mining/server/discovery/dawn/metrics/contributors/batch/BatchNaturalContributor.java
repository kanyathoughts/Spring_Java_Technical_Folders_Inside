/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.batch;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;

/**
 * Collects all the Natural related dependencies referred within the JCL
 */
public class BatchNaturalContributor implements ExternalContributor {

	final DiscoveryBuilder builder;
	final DiscoveryContext context;
	final ModuleBuilder sourceModule;
	final ParserProviderService parserProvider;
	private ModelAttributeMap<Object> attributes;
	private boolean isANaturalProgram;
	private static final String LOGON = "LOGON";
	private static final Pattern NEWLINE_PATTERN = Pattern.compile("\\R+");
	private static final int LIBRARY_START_POSITION = "LOGON ".length();
	private static final Pattern SPACES_PATTERN = Pattern.compile("\\s+.*");
	private static final Pattern LOGON_PATTERN = Pattern.compile("(?m)^\\s*" + Pattern.quote(LOGON) + "\\b");
	private static final List<String> NATURAL_SYSTEM_COMMANDS = Arrays.asList("AIV", "BUS", "CATALL", "CATALOG", "CAT",
			"CHECK", "CLEAR", "COMPOPT", "CPINFO", "DELETE", "DUMP", "EDIT", "EDT", "EXECUTE", "FIN", "GLOBALS", "HELP",
			"INPL", "KEY", "LASTMSG", "LAST", "LIST COUNT", "LIST XREF", "LISTDBRM", "LISTSQLB", "LISTSQL", "LIST",
			"LOGOFF", LOGON, "MAIL", "MAINMENU", "NATQVS", "NOCOPT", "NOCSHOW", "NOCSTAT", "PROFILER", "PROFILE", "RDC",
			"READ", "RENAME", "RENUMBER", "RETURN", "ROUTINES", "RPCERR", "RUN", "SAVE", "SCAN", "SCRATCH", "SETUP",
			"SQLDIAG", "SQLERR", "STOW", "STRUCT", "SYSADA", "SYSAPI", "SYSBPM", "SYSCP", "SYSDB2", "SYSDDM", "SYSEDT",
			"SYSERR", "SYSEXT", "SYSEXV", "SYSFILE", "SYSMAIN", "SYSNCP", "SYSOBJH", "SYSPARM", "SYSPROD", "SYSPROF",
			"SYSRPC", "SYSTP", "TECH", "TEST DBLOG", "TEST", "UNCATALOG", "UNLOCK", "UPDATE", "XREF");

	BatchNaturalContributor(final ParserProviderService parserProvider, final DiscoveryBuilder builder, final DiscoveryContext context,
			final ModuleBuilder sourceModule, final ModelAttributeMap<Object> attributes) {
		this.builder = builder;
		this.context = context;
		this.sourceModule = sourceModule;
		this.parserProvider = parserProvider;
		this.attributes = attributes;
	}

	@Override
	public void collectMetrics(final String... sourceContent) {
		final String instreamProperty = sourceContent[0];
		if (StringUtils.isNotBlank(instreamProperty) && !instreamProperty.contains('/' + LOGON)) {
			/*
			 * we expect the following format: LOGON <LIBRARY> <SYSTEM-COMMAND> (optional)
			 * ... <PROG-NAME> <SYSTEM-COMMAND> (optional) <data> (optional) FIN (optional)
			 */
			final var matcher = LOGON_PATTERN.matcher(instreamProperty);
			final int indexOfLogon = matcher.find() ? matcher.start() : -1;
			if (indexOfLogon == -1) {
				return;
			}
			final String[] instreamContent = NEWLINE_PATTERN
					.split(instreamProperty.substring(indexOfLogon, instreamProperty.length()));
			final String library = extractNaturalLibraryFromContent(instreamContent);
			final Optional<String> program = extractNaturalProgramFromContent(instreamContent);
			if (program.isPresent()) {
				final String libraryPathpattern = "src/natural/" + library + "/**";
				final ModuleFilter nameAndTypeFilter = new ModuleFilter().setNames(program.get()).setTypes(ModuleType.NATURAL_PROGRAM);
				final ModuleFilter pathFilter = new ModuleFilter().setPathPatterns(libraryPathpattern);
				final List<ModuleFilter> moduleFilterList = new ArrayList<>();
				moduleFilterList.add(nameAndTypeFilter);
				moduleFilterList.add(pathFilter);
				sourceModule.declareDependency(RelationshipType.CALLS, moduleFilterList).setAttributes(attributes).setBinding(Binding.LATE);
				isANaturalProgram = true;
			}
		} 
	}
	
	/**
	 * Returns true if the target matches the natural program pattern
	 *
	 * @return true if the target matches the natural program pattern
	 */
	public boolean isANaturalProgram() {
		return isANaturalProgram;
	}

	private static String extractNaturalLibraryFromContent(final String[] instreamContent) {
		final String naturalLogon = instreamContent[0];
		final String naturalLibrary = naturalLogon.substring(LIBRARY_START_POSITION);
		return SPACES_PATTERN.split(StringUtils.trim(naturalLibrary))[0];
	}

	private static Optional<String> extractNaturalProgramFromContent(final String[] instreamContent) {
		if (instreamContent.length >= 2) {
			for (int instreamContentIndex = 1; instreamContentIndex < instreamContent.length; instreamContentIndex++) {
				final String naturalProgramName = instreamContent[instreamContentIndex];
				final String lineAtIndex = StringUtils.trimToEmpty(naturalProgramName);
				if (NATURAL_SYSTEM_COMMANDS.stream().noneMatch(lineAtIndex::startsWith)) {
					return Optional.of(SPACES_PATTERN.split(StringUtils.trim(naturalProgramName))[0]);
				}
			}
		}
		return Optional.empty();
	}

}
