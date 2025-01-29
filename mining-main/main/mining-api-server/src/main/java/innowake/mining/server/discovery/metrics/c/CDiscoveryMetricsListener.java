/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics.c;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;

import org.antlr.v4.runtime.misc.Interval;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.apache.commons.lang.StringUtils;

import com.google.common.io.Files;

import C.CBaseListener;
import C.CParser.AssignmentExpressionContext;
import C.CParser.DeclarationContext;
import C.CParser.FunctionDefinitionContext;
import C.CParser.HashIncludeContext;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;

/**
 * A listener that gathers metrics about the C program.
 */
public class CDiscoveryMetricsListener extends CBaseListener {
	
	private static final Pattern FUNCTION_PATTERN = Pattern.compile("((\\w++)\\s*+\\()");
	private final Set<DependencyDefinitionPojo> unvalidatedDependencies = new LinkedHashSet<>();
	private final MultiValuedMap<String, String> assignmentValues = new ArrayListValuedHashMap<>();
	private final Set<DependencyDefinitionPojo> cHeaderDependencies = new LinkedHashSet<>();
	private final Set<DependencyDefinitionPojo> cFunctionDependencies = new LinkedHashSet<>();

	public Set<DependencyDefinitionPojo> getUnvalidatedDependencies() {
		return unvalidatedDependencies;
	}
	
	public MultiValuedMap<String, String> getAssignmentValues() {
		return assignmentValues;
	}

	public Set<DependencyDefinitionPojo> getHeaderDependencies() {
		return cHeaderDependencies;
	}

	public Set<DependencyDefinitionPojo> getFunctionDependencies() {
		return cFunctionDependencies;
	}

	@Override
	public void enterAssignmentExpression(@Nullable final AssignmentExpressionContext ctx) {
		if (ctx != null && ctx.getText().startsWith("\"") && ctx.getParent() != null && ctx.getParent().getParent() != null
				&& ctx.getParent().getParent().getStart() != null) {
			/* this would also include some functions like printf which can be ignored */
			assignmentValues.put(ctx.getParent().getParent().getStart().getText(), ctx.getText().replace('\"', ' ').trim());
		}
	}
	
	@Override
	public void exitHashInclude(@Nullable final HashIncludeContext ctx) {
		super.exitHashInclude(ctx);
		if (ctx == null) {
			return;
		}

		final var includeText = CHeaderIncludeVisitor.INCLUDE_PATTERN.matcher(ctx.getText()).replaceFirst("").trim();
		if (includeText.startsWith("<")) {
			return;
		}

		final var cleanedIncludeText = CHeaderIncludeVisitor.QUOTATION_PATTERN.matcher(includeText).replaceAll("").trim();
		final var nameWithoutExtension = Files.getNameWithoutExtension(cleanedIncludeText);
		final var headerFilter = new ModuleFilter().setNames(nameWithoutExtension).setTypes(ModuleType.C_HEADER);
		final var dependencyDefinition = new DependencyDefinitionPojo(UUID.randomUUID(), Binding.EARLY, List.of(headerFilter), RelationshipType.INCLUDES);
		cHeaderDependencies.add(dependencyDefinition);
	}

	@Override
	public void exitDeclaration(@Nullable final DeclarationContext ctx) {
		if (ctx == null) {
			return;
		}
		final var text = ctx.start.getInputStream().getText(Interval.of(ctx.start.getStartIndex(), ctx.stop.getStopIndex()));
		final var matcher = FUNCTION_PATTERN.matcher(text);
		/* Check for linkage to Cobol program */
		if (text.trim().startsWith(("#pragma linkage"))) {
			final var subString = text.substring(text.indexOf("(") + 1, text.indexOf(")"));
			final var strArr = subString.split(",");
			final var programId = strArr[0];
			final var fileType = strArr[1];

			if (fileType.trim().equalsIgnoreCase("COBOL") && StringUtils.isNotBlank(programId)) {
				final var programFilter = new ModuleFilter().setNames(programId).setTypes(ModuleType.COBOL_PROGRAM);
				final var dependencyDefinitionPojo = new DependencyDefinitionPojo(UUID.randomUUID(), Binding.LATE, List.of(programFilter),
						RelationshipType.CALLS);
				unvalidatedDependencies.add(dependencyDefinitionPojo);
			}
		} else if (matcher.find()) {
			final var functionName = matcher.group(2);
			if ( ! functionName.equals("decimal")) {
				final var functionFilter = new ModuleFilter().setNames(functionName).setTypes(ModuleType.C_FUNCTION);
				final var dependencyDefinition = new DependencyDefinitionPojo(UUID.randomUUID(), Binding.EARLY, new ModuleLocation(ctx.start.getStartIndex(), 
						ctx.getStop().getStopIndex()), List.of(functionFilter), RelationshipType.CALLS);
				cFunctionDependencies.add(dependencyDefinition);
			}
		}
	}
	
	@Override
	public void exitFunctionDefinition(@Nullable final FunctionDefinitionContext ctx) {
		if (ctx == null) {
			return;
		}

		final var context = ctx.start.getInputStream().getText(Interval.of(ctx.start.getStartIndex(), ctx.stop.getStopIndex()));
		final var matcher = FUNCTION_PATTERN.matcher(context);
		
		if (matcher.find()) {
			final var functionName = matcher.group(2);
			if ( ! functionName.equals("decimal")) {
				final var functionFilter = new ModuleFilter().setNames(functionName).setTypes(ModuleType.C_FUNCTION);
				final var dependencyDefinition = new DependencyDefinitionPojo(UUID.randomUUID(), Binding.EARLY, new ModuleLocation(ctx.start.getStartIndex(), 
						ctx.getStop().getStopIndex()), List.of(functionFilter), RelationshipType.CALLS);
				cFunctionDependencies.add(dependencyDefinition);
			}
		}
	}
}
