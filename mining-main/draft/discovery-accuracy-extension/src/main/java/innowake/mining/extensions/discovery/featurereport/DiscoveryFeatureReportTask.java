/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.discovery.featurereport;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.extensions.discovery.featurereport.model.DiscoveryFeatures;
import innowake.mining.extensions.discovery.featurereport.model.RelationshipTo;
import innowake.mining.extensions.discovery.featurereport.model.Supported;
import innowake.mining.extensions.discovery.featurereport.model.TechnologyAndType;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.spring.data.orientdb.commons.core.SessionManager;
import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;

/**
 * Task that collects the features used by a single module
 */
public class DiscoveryFeatureReportTask extends Task<DiscoveryFeatures> {

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryFeatureReportJob.class);

	private final EntityId moduleId;

	@Autowired
	private transient ModuleService moduleDao;

	public DiscoveryFeatureReportTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId moduleId) {
		super(progressMonitor, jobId);
		this.moduleId = moduleId;
	}

	@Override
	protected Result<DiscoveryFeatures> run(final ProgressMonitor progressMonitor) {
		try {
			final var module = moduleDao.getModule(moduleId);

			final Technology technology = module.getTechnology();
			final Type type = module.getType();

			LOG.debug(() -> String.format("Fetching the info from module: name - %s, id - %s, technology - %s, type - %s", module.getName(), module.getId(),
					module.getTechnology(), module.getType()));

			final DiscoveryFeatures features = new DiscoveryFeatures(technology, type);

			features.addStorage(module.getStorage());
			features.addOrigin(module.getOrigin());
			features.addIdentification(module.getIdentification());
			features.addRepresentation(module.getRepresentation().orElse(Representation.PHYSICAL).toString());
			if (module.getPath() != null) {
				features.setHasPath(Supported.YES);
			}

			if (module.getParentPath() != null) {
				features.setContentsPath(Supported.YES);
			}
			
			final List<ModuleRelationshipPojo> contains = moduleDao.findRelationship(q -> q.ofSource(module.identity()).withType(RelationshipType.CONTAINS));
			
			if ( ! contains.isEmpty()) {
				features.setHasContainingModule(Supported.YES);
				features.addContainedIn(new TechnologyAndType(
						assertNotNull(assertNotNull(assertNotNull(moduleV2.getInContainsModule()).getOut()).getObjectTypeLink()).getTechnologyLink(),
						assertNotNull(assertNotNull(assertNotNull(moduleV2.getInContainsModule()).getOut()).getObjectTypeLink()).getTypeLink()));
			}

			final List<Module> modules = jdbcTemplate.query(
					"SELECT id, name, path, objectTypeLink.technologyLink.name, objectTypeLink.typeLink.name FROM Module WHERE projectLink.id = ? "
					+ "AND in_ContainsModule.out.id = ?",
					new RowMapper<Module>() {
						@Override
						@Nullable
						public Module mapRow(final ResultSet rs, final int rowNum) throws SQLException {
							final Module m = new Module();
							m.setId(rs.getLong(1));
							m.setName(rs.getString(2));
							m.setPath(rs.getString(3));
							m.setTechnology(rs.getString(4));
							m.setType(rs.getString(5));
							return m;
						}
					}, module.getProjectId(), module.getId());

			if ( ! modules.isEmpty()) {
				features.setHasContainedModules(Supported.YES);
				modules.forEach(m -> features.addContains(new TechnologyAndType(m.getTechnology(), m.getType())));
			}
			if (moduleV2.getSourceAttachmentLink() != null) {
				features.setHasSourceCode(Supported.YES);
			}
			if (module.getLocation() != null) {
				features.setHasLocation(Supported.YES);
			}
			if (module.getErrors() != null && assertNotNull(module.getErrors()).intValue() > 0) {
				features.setHasErrors(Supported.YES);
			}
			if (moduleV2.getOutHasAst() != null) {
				features.setHasAst(Supported.YES);
			}
			try {
				final SourceMetrics sourceMetrics = assertNotNull(module).getSourceMetrics();

				if (sourceMetrics != null) {
					/* null or -1 means "not supported" */
					if (sourceMetrics.getPhysicalLines() != null && assertNotNull(sourceMetrics.getPhysicalLines()).intValue() >= 0) {
						features.setSupportsPhysicalLines(Supported.YES);
					}
					if (sourceMetrics.getCodeLines() != null && assertNotNull(sourceMetrics.getCodeLines()).intValue() >= 0) {
						features.setSupportsCodeLines(Supported.YES);
					}
					if (sourceMetrics.getCommentLines() != null && assertNotNull(sourceMetrics.getCommentLines()).intValue() >= 0) {
						features.setSupportsCommentLines(Supported.YES);
					}
					if (assertNotNull(sourceMetrics.getDeadCodeLines()).intValue() >= 0) {
						features.setSupportsLinesOfDeadCode(Supported.YES);
					}
					if (sourceMetrics.getComplexityMcCabe() != null && assertNotNull(sourceMetrics.getComplexityMcCabe()).intValue() >= 0) {
						features.setSupportsComplexity(Supported.YES);
					}
				}
			} catch (final NoRecordFoundException e) {
				/* no Source Metrics for this Module */
			}
			if (module.getStatements() != null && assertNotNull(module.getStatements()).intValue() > 0) {
				features.setSupportsStatements(Supported.YES);
			}
			if (module.getSqlStatements() != null && assertNotNull(module.getSqlStatements()).intValue() > 0) {
				features.setSupportsSqlStatements(Supported.YES);
			}

			final Set<String> relationships = new HashSet<>();
			final Set<Relationship> depRelationships = new HashSet<>();
			if (moduleV2.getOutCalls() != null) {
				assertNotNull(moduleV2.getOutCalls()).forEach(edge -> {
					depRelationships.add(Relationship.CALLS);
					final Map<String, Set<String>> edgeProperties = assertNotNull(edge.getProperties()).entrySet().stream().collect(
							Collectors.toMap(Map.Entry::getKey, value -> new HashSet<>(Collections.singletonList(value.getValue()))));

					features.addPossibleRelationship(new RelationshipTo(
							new TechnologyAndType(assertNotNull(assertNotNull(edge.getIn()).getObjectTypeLink()).getTechnologyLink(),
									assertNotNull(assertNotNull(edge.getIn()).getObjectTypeLink()).getTypeLink()),
							Relationship.CALLS, edgeProperties));
				});

			}
			if (moduleV2.getOutIncludes() != null) {
				relationships.clear();
				depRelationships.clear();
				assertNotNull(moduleV2.getOutIncludes()).forEach(edge -> {
					depRelationships.add(Relationship.INCLUDES);
					final Map<String, Set<String>> edgeProperties = assertNotNull(edge.getProperties()).entrySet().stream().collect(
							Collectors.toMap(Map.Entry::getKey, value -> new HashSet<>(Collections.singletonList(value.getValue()))));

					features.addPossibleRelationship(new RelationshipTo(
							new TechnologyAndType(assertNotNull(assertNotNull(edge.getIn()).getObjectTypeLink()).getTechnologyLink(),
									assertNotNull(assertNotNull(edge.getIn()).getObjectTypeLink()).getTypeLink()),
							Relationship.INCLUDES, edgeProperties));
				});
			}
			if (moduleV2.getOutReadsWrites() != null) {
				relationships.clear();
				depRelationships.clear();
				assertNotNull(moduleV2.getOutReadsWrites()).forEach(edge -> {
					depRelationships.add(Relationship.READS_WRITES);
					final Map<String, Set<String>> edgeProperties = assertNotNull(edge.getProperties()).entrySet().stream().collect(
							Collectors.toMap(Map.Entry::getKey, value -> new HashSet<>(Collections.singletonList(value.getValue()))));

					features.addPossibleRelationship(new RelationshipTo(
							new TechnologyAndType(assertNotNull(assertNotNull(edge.getIn()).getObjectTypeLink()).getTechnologyLink(),
									assertNotNull(assertNotNull(edge.getIn()).getObjectTypeLink()).getTypeLink()),
							Relationship.READS_WRITES, edgeProperties));
				});
			}
			if (moduleV2.getOutReferences() != null) {
				relationships.clear();
				depRelationships.clear();
				assertNotNull(moduleV2.getOutReferences()).forEach(edge -> {
					depRelationships.add(Relationship.REFERENCES);
					final Map<String, Set<String>> edgeProperties = assertNotNull(edge.getProperties()).entrySet().stream().collect(
							Collectors.toMap(Map.Entry::getKey, value -> new HashSet<>(Collections.singletonList(value.getValue()))));

					features.addPossibleRelationship(new RelationshipTo(
							new TechnologyAndType(assertNotNull(assertNotNull(edge.getIn()).getObjectTypeLink()).getTechnologyLink(),
									assertNotNull(assertNotNull(edge.getIn()).getObjectTypeLink()).getTypeLink()),
							Relationship.REFERENCES, edgeProperties));
				});
			}

			return new Result<>(features);
		} finally {
			sessionManager.closeConnection();
		}
	}

}
