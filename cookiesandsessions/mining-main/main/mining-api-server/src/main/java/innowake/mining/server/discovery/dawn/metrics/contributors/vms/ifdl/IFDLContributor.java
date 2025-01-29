/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.vms.ifdl;

import java.util.Arrays;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.metrics.vms.IFDLLightParser;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Contributor for IFDL files in VMS.
 */
@Component
public class IFDLContributor implements DiscoveryContributorFromSource {
	
	private static final Pattern NEW_LINE_SPLITTER = Pattern.compile("\r?\n");
	
	@Autowired
	private ParserProviderService parserProviderService;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return sourceObject.getTechnology() == Technology.VMS && sourceObject.getType() == Type.IFDL_FORM;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final String content = sourceObject.getContent().toString();
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.IFDL_FORM);
		collectLinesOfCode(content, rootModule);
		final var parser = parserProviderService.createIfdlLightParser();
		final var parseResult = parser.parse(content);
		parseResult.getCalls().entrySet().stream()
				.collect(Collectors.toMap(entry -> entry.getKey().toUpperCase(), Map.Entry::getValue, (existing, replacement) -> existing))
				.forEach((call, binding) -> rootModule
						.declareDependency(RelationshipType.CALLS, new ModuleFilter().setNames(call).setTypes(ModuleType.COBOL_PROGRAM, ModuleType.BASIC_FUNCTION))
						.setBinding(binding));
	}
	
	private void collectLinesOfCode(final String content, final ModuleBuilder rootModule) {
		final var sourceMetrics = new SourceMetrics();
		final int multiLineComment = patternMatcher(IFDLLightParser.MULTILINECOMMENT, content);
		final int multiLineBraceComment = patternMatcher(IFDLLightParser.MULTILINEBRACECOMMENT, content);
		final int linesOfComments = multiLineComment + multiLineBraceComment;
		final String[] linesOfCode = NEW_LINE_SPLITTER.split(content);
		final int linesOfCodeCount = (int) Arrays.stream(linesOfCode).filter(StringUtils::isNotBlank).count();
		final int codeLines = linesOfCodeCount - linesOfComments;
		sourceMetrics.setCodeLines(Integer.valueOf(codeLines));
		sourceMetrics.setCommentLines(Integer.valueOf(linesOfComments));
		rootModule.addAdditionalInfo(sourceMetrics);
	}
	
	private int patternMatcher(final Pattern pattern, final String text) {
		var count = 0;
		final var matcher = pattern.matcher(text);
		while (matcher.find()) {
			count += NEW_LINE_SPLITTER.split(matcher.group()).length;
		}
		return count;
	}
}
