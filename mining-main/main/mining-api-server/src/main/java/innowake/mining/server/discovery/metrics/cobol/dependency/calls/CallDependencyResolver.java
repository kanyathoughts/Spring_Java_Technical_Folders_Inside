/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol.dependency.calls;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dawn.metrics.contributors.resolver.cobol.DefaultCobolReferenceResolver;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.model.ModuleType;
import innowake.ndt.cobol.parser.ast.model.CobolConstantReference;
import innowake.ndt.cobol.parser.ast.model.CobolExpression;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.model.CobolReference;
import innowake.ndt.cobol.parser.ast.model.CobolReferenceExpression;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt.CobolUsing;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt.UsingType;

/**
 * Dependency resolver for call statements.
 */
public abstract class CallDependencyResolver {
	protected static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);
	
	/**
	 * Resolve the dependencies which a related to this call.<br>
	 * Dependencies will be resolved by inspecting the USINGS and the contained 
	 * CobolConstantReference or CobolFieldReference
	 * @param dawnCobolReferenceResolver a cobol reference resolver
	 * @param call a call statement from a parsed cobol file
	 * @return a list the (potential) call dependencies
	 */
	public List<ModuleFilter> getDependencies(final DefaultCobolReferenceResolver dawnCobolReferenceResolver, final CobolCallStmt call) {
		final List<CobolUsing> usings = call.getUsings();
		final CobolReference target = call.getTarget();
		if (target instanceof CobolConstantReference) {
			final String callVal = ((CobolConstantReference) target).getValue().toString().replace("\"", "");
			if ( ! accept(callVal)) {
				return Collections.emptyList();
			}
		}
		if (usings.isEmpty() || getRelevantIndex() >= usings.size()) {
			return Collections.emptyList();
		}
		final int relevantIndex = getRelevantIndex();
		final CobolUsing using = usings.get(relevantIndex);
		final CobolExpression expr = using.getExpression();
		if (using.getUsingType() != UsingType.BY_DESCRIPTOR || ! (expr instanceof CobolReferenceExpression)) {
			return Collections.emptyList();
		}
		final CobolReference ref = ((CobolReferenceExpression) expr).getOp1();
		if (ref instanceof CobolConstantReference) {
			final String refName = getTargetModuleName(((CobolConstantReference) ref).getValue().toString());
			final var moduleFilter = new ModuleFilter().setNames(refName).setTypes(getTargetModuleType());
			return Arrays.asList(moduleFilter);
		} else if (ref instanceof CobolFieldReference) {
			return dawnCobolReferenceResolver.resolve(ref)
					.stream()
					.distinct()
					.filter(this::filter)
					.map(this::getTargetModuleName)
					.map(name -> new ModuleFilter().setNames(name).setTypes(getTargetModuleType()))
					.collect(Collectors.toList());
		}
		return Collections.emptyList();
	}
	
	/**
	 * From the content of a string in a 'BY DESCRIPTOR' / 'BY VALUE'-field retrieve the
	 * actual dependency name.
	 *
	 * @param stringWithArtifact a string containing the actual dependency, e.g. 'SCREENS:SOME_IFDL_FORM.FORM'
	 * @return the dependency name, e.g. 'SOME_IFDL_FORM'
	 */
	protected abstract String getTargetModuleName(final String stringWithArtifact);
	
	/**
	 * Get the index in the using where the dependency is stored,
	 * e.g. for <br>
	 * <pre>CALL "FORMS$eNABLE" USING 
       BY VALUE FORMS$AR_FORM_TABLE 
       BY DESCRIPTOR "SYS$INPUT" 
       BY DESCRIPTOR SESSION_ID 
       BY DESCRIPTOR FORM_FILE 
       BY DESCRIPTOR FORM__SUB
       OMITTED OMITTED OMITTED OMITTED OMITTED
       BY DESCRIPTOR PARENT_REQUEST_ID
       GIVING FORMS_STATUS</pre>
	   this would be index 3.
	 * @return the 0-based index where the information of the dependency in the USING is located.
	 */
	protected abstract int getRelevantIndex();
	
	/**
	 * Accept a given call-statement based on the call-value.
	 * @param target the string in the call-statement
	 * @return true, if this dependency resolver can handle this call-statement
	 */
	protected abstract boolean accept(final String target);
	
	/**
	 * @return the ModuleType of target module.
	 */
	protected abstract ModuleType getTargetModuleType();
	
	/**
	 * Filter potential artifacts by content criterias.
	 * 
	 * @param potentialName the potential dependency name found in init-contents or from MOVE-statements of variables in COBOL.
	 * @return a list of dependency names ( either filenames or virtual dependencies )
	 */
	protected abstract boolean filter(final String potentialName);
}
