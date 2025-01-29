/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.vg;

import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Identifier for the Email, Report generator & SORT taxonomy.
 */
@Component
public class ModuleStatementBasedTaxonomyIdentifier implements TaxonomyIdentifier<ModuleBasePojo> {

	private final ModuleService moduleService;
	private final Pattern emailPattern;
	private final Pattern reportGeneratorPattern;

	@Autowired
	public ModuleStatementBasedTaxonomyIdentifier(final ModuleService moduleService,
			@Value("${mining.taxonomies.regex.report-generator:[V],[A-Z][A-Z0-9]{0,7}}") final String reportGeneratorPattern,
			@Value("${mining.taxonomies.regex.email:A,SMTP}") final String emailPattern) {
		this.moduleService = moduleService;
		this.emailPattern = Pattern.compile("SYSOUT=" + emailPattern);
		this.reportGeneratorPattern = Pattern.compile("SYSOUT=" + reportGeneratorPattern);
	}

	@Override
	public List<Tuple2<Name, TypeName>> identify(final ModuleBasePojo moduleBasePojo) {
		final List<Tuple2<Name, TypeName>> result = new ArrayList<>();
		if (moduleBasePojo.getType() == Type.JOB) {
			final List<StatementPojo> statementPojoList = moduleService.findStatements(q -> q.ofModule(moduleBasePojo.identity()));
			for (final StatementPojo statementPojo : statementPojoList) {
				final String text = statementPojo.getText();
				/*
				 * Split the text into sections based on the comma and space separator. Then split each section into key-value pairs based on the colon and space separator.
				 * Finally, collect the key-value pairs into a map.
				 * The key is the property name and the value is a list of values.
				 * The values are split based on the comma and space separator.
				 * The values are then split based on the curly brace separator.
				 * The first part of the value is the key and the second part is the value.
				 * The key is the property name and the value is a list of values.
				 */
				final Map<String, List<String>> properties = Arrays.stream(text.split(",(?![^\\[]*\\]) ")).map(section -> section.split(": \\[|, "))
						.filter(keyValue -> keyValue.length > 1).collect(Collectors.toMap(keyValue -> keyValue[0],
								keyValue -> Arrays.asList(keyValue[1].replaceAll("[\\[\\]]", "").split("(?<=\\}),"))));
				if (properties.getOrDefault("Properties", Collections.emptyList()).contains("PGM=IEBGENER")
						&& matchesDataDefinitionValue(properties, emailPattern)) {
					result.add(new Tuple2<>(Name.SENDS_EMAIL, TypeName.OPERATIONS));
				}
				if (matchesDataDefinitionValue(properties, reportGeneratorPattern)) {
					result.add(new Tuple2<>(Name.GENERATES_REPORT, TypeName.OPERATIONS));
				}
				if (properties.getOrDefault("Properties", Collections.emptyList()).contains("PGM=SORT") && properties.containsKey("Data Definitions")) {
					if (properties.get("Data Definitions")
							.stream()
							.anyMatch(x -> x.startsWith("SORTOF"))) {
						result.add(new Tuple2<>(Name.SPLITTER, TypeName.OPERATIONS));
					}
					if (properties.get("Data Definitions")
							.stream()
							.filter(x -> x.startsWith("SORTIN"))
							.count() > 1) {
						result.add(new Tuple2<>(Name.MERGER, TypeName.OPERATIONS));
					}
				}
			}
		}
		return result;
	}

	private boolean matchesDataDefinitionValue(final Map<String, List<String>> properties, final Pattern pattern) {
		return properties.getOrDefault("Data Definitions", Collections.emptyList()).stream()
				.map(part -> part.split("\\{"))
				.filter(keyValue -> keyValue.length > 1)
				.map(keyValue -> keyValue[1].substring(0, keyValue[1].length() - 1))
				.anyMatch(value -> pattern.matcher(value).matches());
	}
}
