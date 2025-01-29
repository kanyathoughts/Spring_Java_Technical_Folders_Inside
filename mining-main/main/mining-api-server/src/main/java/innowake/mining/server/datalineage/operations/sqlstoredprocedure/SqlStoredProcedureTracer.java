/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.operations.sqlstoredprocedure;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import org.springframework.stereotype.Component;

import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.operations.DataLineageTracer;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.sqllightweight.SqlLightWeightParseResultProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.mining.shared.model.datalineage.ProxyContainerPrototype;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.parsing.parser.sql.lightweight.ast.CreateProcedureStatement;
import innowake.ndt.parsing.parser.sql.lightweight.ast.ProcedureParameter;

/**
 * Data Lineage Tracer component for tracing the data flow through an SQL stored procedure.
 * <p>
 * This component creates an {@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#ENTRY_POINT} using the parameter declarations from the
 * stored procedure definition.
 * <p>
 * If the stored procedure definition contains the {@code EXTERNAL} keyword, then a
 * second{@link innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type#CALL} container is created
 * for passing parameters on to the implementation program.
 */
@Component
public class SqlStoredProcedureTracer implements DataLineageTracer {

	private ModuleService moduleService;
	private final SourceCachingService sourceService;
	private final ParserProviderService parserProviderService;

	/**
	 * Constructor.
	 * @param moduleService Module data access service.
	 * @param sourceService SourceObject data access service.
	 * @param parserProviderService service to create new instances of parsers
	 */
	public SqlStoredProcedureTracer(final ModuleService moduleService,
									final SourceCachingService sourceService,
									final ParserProviderService parserProviderService) {
		this.moduleService = moduleService;
		this.sourceService = sourceService;
		this.parserProviderService = parserProviderService;
	}

	@Override
	public boolean isSupported(final ModuleLightweightPojo module) {
		return module.getTechnology() == Technology.SQL && module.getType() == Type.STORED_PROCEDURE;
	}

	@Override
	public DataLineageResult trace(final DataLineageContext context, final ModuleLightweightPojo module) {
		final ModuleLightweightPojo moduleLightweight = moduleService.findAnyModuleLightweight(b -> b.byNid(module.getId()))
				.orElseThrow(() -> new IllegalArgumentException("SQL Stored Procedure module " + module.getId() + " not found"));

		/* there is no StoreAst support for SQL scripts at the moment, so we must always parse the file from source */
		final AstNode root = parseSqlScriptFile(context, moduleLightweight);

		final String path = moduleLightweight.getPath() != null ? moduleLightweight.getPath() : moduleLightweight.getParentPath();
		/* there might be multiple CREATE PROCEDURE statements in the SQL script, so find the right one using the procedure's name */
		final CreateProcedureStatement procedureStatement = root.getChildren(CreateProcedureStatement.class).stream()
				.filter(proc -> proc.getName().equals(moduleLightweight.getName()))
				.findFirst()
				.orElseThrow(() -> new IllegalStateException("SQL Stored Procedure " + moduleLightweight.getName() + " not found in file " + path));

		/* create an ENTRY_POINT container on the Stored Procedure Module using the parsed parameter list */
		final ProxyContainerPrototype entryPointContainer = new ProxyContainerPrototype(DataFlowId.forProxyContainer(module.getId(), ProxyContainerPojo.Type.ENTRY_POINT, null));
		entryPointContainer.setType(ProxyContainerPojo.Type.ENTRY_POINT);
		populateParameters(entryPointContainer, procedureStatement);

		final Optional<String> externalName = procedureStatement.getExternalName();
		if (externalName.isPresent()) {
			final String name = MetricsUtility.trimQuotesSpaces(externalName.get());
			final ProxyContainerPrototype callContainer = new ProxyContainerPrototype(DataFlowId.forProxyContainer(module.getId(), ProxyContainerPojo.Type.CALL, null));
			callContainer.setType(ProxyContainerPojo.Type.CALL);
			callContainer.setProperties(Map.of(ProxyContainerPojo.Property.TARGET_NAME, name));
			populateParameters(callContainer, procedureStatement);
			final List<DataFlowNodePrototype> fakeStatements =
					linkContainers(new ArrayList<>(entryPointContainer.fields.getNonNull()), new ArrayList<>(callContainer.fields.getNonNull()), EntityId.of(module.getId()), procedureStatement);

			return DataLineageResult.ofProxyContainers(List.of(entryPointContainer, callContainer)).withStatements(fakeStatements);
		}
		return DataLineageResult.ofProxyContainers(List.of(entryPointContainer));
	}
	
	@Override
	public DataLineageResult discoverProxyContainersOfType(final DataLineageContext context, final ModuleLightweightPojo module, final ProxyContainerPojo.Type type) {
		return trace(context, module);
	}

	private void populateParameters(final ProxyContainerPrototype container, final CreateProcedureStatement procedureStatement) {
		final List<DataFlowNodePrototype> fields = new ArrayList<>();
		int index = 0;
		for (final ProcedureParameter parameter : procedureStatement.getParameters()) {
			fields.add(new DataFlowNodePrototype(DataFlowId.forProxyField(container.dataFlowId, index++))
					.setName(parameter.getName()));
		}
		container.setFields(fields);
	}

	private List<DataFlowNodePrototype> linkContainers(final List<DataFlowNodePrototype> entryPointFields, final List<DataFlowNodePrototype> callFields,
			final EntityId moduleId, final CreateProcedureStatement procedureStatement) {

		/* in the future, fake statements should not be required any more */
		final List<DataFlowNodePrototype> fakeStatements = new ArrayList<>();
		for (int i = 0; i < entryPointFields.size(); i++) {
			final DataFlowNodePrototype leftNode = entryPointFields.get(i);
			final DataFlowNodePrototype rightNode = callFields.get(i);
			final ProcedureParameter parameter = procedureStatement.getParameters().get(i);

			/* including the field name on the dummy statement only to ensure consistent ordering in tests ... */
			final String name = "(dummy statement for " + leftNode.name.get() + ")";
			final DataFlowNodePrototype fakeStatement = new DataFlowNodePrototype(DataFlowId.forUniqueStatement(moduleService.getModuleNid(moduleId), name));
			fakeStatement.setName(name);
			fakeStatements.add(fakeStatement);

			final ProcedureParameter.ParameterAccessType accesType = parameter.getParameterAccesType();
			if (accesType == ProcedureParameter.ParameterAccessType.IN || accesType == ProcedureParameter.ParameterAccessType.INOUT) {
				leftNode.addDataFlow(DataFlow.toFieldViaStatement(rightNode, fakeStatement));
			}
			if (accesType == ProcedureParameter.ParameterAccessType.OUT || accesType == ProcedureParameter.ParameterAccessType.INOUT) {
				rightNode.addDataFlow(DataFlow.toFieldViaStatement(leftNode, fakeStatement));
			}
		}

		return fakeStatements;
	}

	private AstNode parseSqlScriptFile(final DataLineageContext context, final ModuleLightweightPojo moduleLightweight) {
		final SqlLightWeightParseResultProvider parser = parserProviderService.createSqlLightWeightParser(Config.getDefaultConfig(),
				context.getTimedWorker(), context.getJobId());

		final String path = Objects.requireNonNull(moduleLightweight.getPath() == null ? moduleLightweight.getParentPath() : moduleLightweight.getPath());
		final SourcePojo sourceObject = sourceService.findAny(q -> q.ofProject(context.getProjectId()).withPath(path))
				.orElseThrow(() -> new MiningEntityNotFoundException("Source not found for project: " + context.getProjectId() + " and path: " + path));

		final AstModel parseResult;
		try {
			parseResult = parser.getParseResult(sourceObject);
		} catch (final DiscoveryException | TimedWorker.WorkerCancellationException e) {
			throw new IllegalStateException("parsing of SQL Stored Procedure" + sourceObject.getPath() + " has failed", e);
		}

		return parseResult.getRoot()
				.orElseThrow(() -> new IllegalStateException("parsing of SQL Stored Procedure" + sourceObject.getPath() + " has failed"));
	}
}
