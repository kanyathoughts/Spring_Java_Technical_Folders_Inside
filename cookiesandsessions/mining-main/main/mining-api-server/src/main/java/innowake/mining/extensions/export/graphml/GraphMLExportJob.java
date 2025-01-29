/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.graphml;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyTypePojo;

/**
 * A {@link Job} exporting a project as GraphML.
 */
public class GraphMLExportJob extends MiningJob<FileSystemResult> {

	public static final String ATTRIBUTES_KEY = "attributes";

	private static final Logger LOG = LoggerFactory.getLogger(GraphMLExportJob.class);
	public static final String SOURCE_METRICS_CODE_LINES = "sourceMetrics.codeLines";
	public static final String SOURCE_METRICS_COMMENT_LINES = "sourceMetrics.commentLines";
	public static final String SOURCE_METRICS_COMPLEXITY_MC_CABE = "sourceMetrics.complexityMcCabe";
	public static final String SOURCE_METRICS_DEAD_CODE_LINES = "sourceMetrics.deadCodeLines";
	public static final String SOURCE_METRICS_PHYSICAL_LINES = "sourceMetrics.physicalLines";
	public static final String TAXONOMY_PREFIX = "Tax: ";

	@Autowired
	private transient ProjectService projectService;
	@Autowired
	private transient ModuleService moduleService;
	@Autowired
	private transient TaxonomyService taxonomyService;

	private final Map<String, List<String>> parameters;
	
	private boolean containsTaxonomies = false;
	
	private boolean detailedTaxonomyGraphmlExportfeatureToggle;

	/**
	 * Creates a new job instance of exporting a project as GraphML.
	 *
	 * @param projectId the ID of the project to export
	 * @param parameters the job parameters controlling the execution logic
	 * @param featureToggleFlag The FF4j flag detailedTaxonomyGraphMLExport
	 */
	public GraphMLExportJob(final EntityId projectId, final Map<String, List<String>> parameters, final boolean featureToggleFlag) {
		super(projectId);
		this.parameters = parameters;
		this.detailedTaxonomyGraphmlExportfeatureToggle = featureToggleFlag;
	}

	@Override
	public Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		/* For performance reasons we do not use tinkerpop for the graphml file. Unfortunately all tag and property names are in a package private class */
		try (final BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(createResultFile(), StandardCharsets.UTF_8))) {
			writer.append("<?xml version=\"1.0\"?>"
						+ "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
						+ "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.1/graphml.xsd\">");

			final List<String> dataPointPaths = getDataPointPaths();
			writePropertyKeys(writer, dataPointPaths);
			writer.append("<graph id=\"G\" edgedefault=\"directed\">");

			/* GraphQl */
//			final Long projectNid = projectService.getNid(projectId);
//			final var result = queryService.queryDataPointsByPath(Optional.of(projectNid), "modules", Map.of("projectId", projectNid), dataPointPaths);

			final int limit = 100_000;
			int size = 0;
			int pageIndex = 0;
			do {
				final var page = moduleService.findModules(Pagination.at(pageIndex++, limit), q -> q.ofProject(projectId));
				final var content = page.getContent();
				/* remove with the removal of the detailedTaxonomyGraphmlExport feature flag (WMIN-13565)*/
				if (detailedTaxonomyGraphmlExportfeatureToggle) {
					final Map<Long, List<Tuple2<String, String>>> taxonomies;
					if (containsTaxonomies) {
						taxonomies = buildTaxonomiesForModule(content);
					} else {
						taxonomies = Collections.emptyMap();
					}
					writeModuleVertices(writer, dataPointPaths, page.getContent(), null, taxonomies);
				} else {
					final Map<Long, List<String>> taxonomies;
					if (dataPointPaths.contains("taxonomies")) {
						taxonomies = taxonomyService.findTaxonomiesPerModule(q -> q.ofModules(content.stream().map(ModulePojo::identity).collect(Collectors.toList())))
														.entrySet().stream()
														.collect(Collectors.toMap(e -> e.getKey().getNid(), 
																				  e -> e.getValue().stream().map(TaxonomyPojo::getName).collect(Collectors.toList())));
					} else {
						taxonomies = Collections.emptyMap();
					}
					writeModuleVertices(writer, dataPointPaths, page.getContent(), taxonomies, null);
				}
				
				size = page.getSize();
			} while (size == limit);

			pageIndex = 0;
			do {
				final var page = moduleService.findRelationships(Pagination.at(pageIndex++, limit), q -> q.ofProject(projectId));
				writeRelationshipEdges(writer, page.getContent());
				size = page.getSize();
			} while (size == limit);

			writer.append("</graph></graphml>");

			final String fileName = String.format("Export_%d_%s.graphml", projectService.getNid(projectId), new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()));
			return new Result<>(new FileSystemResult("application/xml", fileName));
		} catch (final Exception e) {
			LOG.error(() -> "Error while creating GraphMl export for project: " + projectId, e);
			return new Result<>(new Status(e));
		}
	}

	private void writePropertyKeys(final BufferedWriter writer, final List<String> dataPointPaths) throws IOException {
		/* keys for vertices */
		writer.append("<key id=\"labelV\" for=\"node\" attr.name=\"labelV\" attr.type=\"string\"/>");

		final Set<String> keys = new HashSet<>(dataPointPaths.size());
		for (String path : dataPointPaths) {
			final int index = path.lastIndexOf('.');
			if (index != -1) {
				path = path.substring(index + 1);
			}
			if (keys.add(path)) {
				writer.append("<key id=\"")
					  .append(path)
					  .append("\" for=\"node\" attr.name=\"")
					  .append(path)
					  .append("\" attr.type=\"");
				switch (path) {
					case "nid":
						writer.append("long\"/>");
						break;
					case "errors":
					case "statements":
					case "sqlStatements":
					case "physicalLines":
					case "codeLines":
					case "commentLines":
					case "complexityMcCabe":
					case "deadCodeLines":
						writer.append("int\"/>");
						break;
					case "requiresReview":
						writer.append("boolean\"/>");
						break;
					default:
						writer.append("string\"/>");
						break;
					
				}
			}
		}

		/* keys for edges */
		writer.append("<key id=\"labelE\" for=\"edge\" attr.name=\"labelE\" attr.type=\"string\"/>");
		writer.append("<key id=\"dependencyBinding\" for=\"edge\" attr.name=\"dependencyBinding\" attr.type=\"string\"/>");
		writer.append("<key id=\"dependencyBinding\" for=\"edge\" attr.name=\"dependencyBinding\" attr.type=\"string\"/>");
		writer.append("<key id=\"validIfReachedFrom\" for=\"edge\" attr.name=\"validIfReachedFrom\" attr.type=\"string\"/>");
		writer.append("<key id=\"idE\" for=\"edge\" attr.name=\"id\" attr.type=\"string\"/>");
		writer.append("<key id=\"dstLocation\" for=\"edge\" attr.name=\"dstLocation\" attr.type=\"string\"/>");
		writer.append("<key id=\"srcLocation\" for=\"edge\" attr.name=\"srcLocation\" attr.type=\"string\"/>");
		writer.append("<key id=\"properties\" for=\"edge\" attr.name=\"properties\" attr.type=\"string\"/>");
	}

	/* The duplicate taxonomies should be removed with the removal of the feature flag */
	@SuppressWarnings("unchecked")
	private void writeModuleVertices(final BufferedWriter writer, final List<String> dataPointPaths, final List<ModulePojo> modules, 
			@Nullable final Map<Long, List<String>> taxonomies, @Nullable final Map<Long, List<Tuple2<String, String>>> taxonomiesNew) throws IOException {
		final String tagClose = "</data>";
		for (final ModulePojo module : modules) {
			writer.append("<node id=\"").append(module.getUid().toString());
			writer.append("\"><data key=\"labelV\">").append(module.getName()).append(tagClose);
			writer.append("<data key=\"name\">").append(module.getName()).append(tagClose);
			writer.append("<data key=\"nid\">").append(module.getId().toString()).append(tagClose);
			writer.append("<data key=\"uid\">").append(module.getUid().toString()).append(tagClose);

			final Optional<SourceMetricsPojo> sourceMetrics = module.getSourceMetrics();
			for (final String path : dataPointPaths) {
				/* In case we got taxonomies present we need to filter differently, since the datapoints will have the names of the taxonomy types */
				if (detailedTaxonomyGraphmlExportfeatureToggle && taxonomiesNew != null && path.contains(TAXONOMY_PREFIX)) {
					final List<Tuple2<String, String>> taxs = taxonomiesNew.get(module.getId());
					
					if (taxs != null) {
						for (final Tuple2<String, String> t : taxs) {
							if (path.contains(t.e1)) {
								writer.append("<data key=\"" + TAXONOMY_PREFIX + t.e1 + "\">").append(t.e2).append(tagClose);
							}
						}
					}
					continue;
				}

				switch (path) {
					case "id":
					case "nid":
					case "uid":
					case "name":
						/* already added */
						break;
					case SOURCE_METRICS_PHYSICAL_LINES:
						if (sourceMetrics.isPresent()) {
							final Integer value = sourceMetrics.get().getPhysicalLines();
							if (value != null) {
								writer.append("<data key=\"physicalLines\">").append(value.toString()).append(tagClose);
							}
						}
						break;
					case SOURCE_METRICS_CODE_LINES:
						if (sourceMetrics.isPresent()) {
							final Integer value = sourceMetrics.get().getCodeLines();
							if (value != null) {
								writer.append("<data key=\"codeLines\">").append(value.toString()).append(tagClose);
							}
						}
						break;
					case SOURCE_METRICS_COMMENT_LINES:
						if (sourceMetrics.isPresent()) {
							final Integer value = sourceMetrics.get().getCommentLines();
							if (value != null) {
								writer.append("<data key=\"commentLines\">").append(value.toString()).append(tagClose);
							}
						}
						break;
					case SOURCE_METRICS_COMPLEXITY_MC_CABE:
						if (sourceMetrics.isPresent()) {
							final Integer value = sourceMetrics.get().getComplexityMcCabe();
							if (value != null) {
								writer.append("<data key=\"complexityMcCabe\">").append(value.toString()).append(tagClose);
							}
						}
						break;
					case SOURCE_METRICS_DEAD_CODE_LINES:
						if (sourceMetrics.isPresent()) {
							final Integer value = sourceMetrics.get().getDeadCodeLines();
							if (value != null) {
								writer.append("<data key=\"deadCodeLines\">").append(value.toString()).append(tagClose);
							}
						}
						break;
					case "taxonomies":
						/* remove with the removal of the detailedTaxonomyGraphmlExport feature flag*/
						if (taxonomies != null) {
							final List<String> taxs = taxonomies.get(module.getId());
							if (taxs != null) {
								writer.append("<data key=\"taxonomies\">").append(taxs.toString()).append(tagClose);
							}
						}
						break;
					default:
						try {
							final Field field = ModulePojo.class.getDeclaredField(path);
							field.setAccessible(true);
							Object value = field.get(module);
							if (value instanceof Optional) {
								value = ((Optional<Object>) value).orElse(null);
							}
							if (value != null) {
								writer.append("<data key=\"").append(path).append("\">").append(value.toString()).append(tagClose);
							}
							
							break;
						} catch (final Exception e) {
							throw new IllegalStateException("Unhandled module property in GraphMl export: " + path);
						}
				}
			}
			writer.append("</node>");
		}
	}

	private void writeRelationshipEdges(final BufferedWriter writer, final List<ModuleRelationshipPojo> relationships) throws IOException {
		for (final var relationship : relationships) {
			writer.append("<edge id=\"").append(relationship.getId().toString())
				  .append("\" source=\"").append(relationship.getSrcModule().toString())
				  .append("\" target=\"").append(relationship.getDstModule().toString())
				  .append("\"><data key=\"labelE\">").append(StringUtils.capitalize(relationship.getRelationship().name().toLowerCase()))
				  .append("</data><data key=\"idE\">").append(relationship.getId().toString()).append("</data>");

			final var srcLocation = relationship.getSrcLocation();
			if (srcLocation.isPresent()) {
				writer.append("<data key=\"srcLocation\">ModuleLocation{offset:").append(srcLocation.get().getOffset().toString())
					  .append(",length:").append(srcLocation.get().getLength().toString()).append("}</data>");
			}
			final var dstLocation = relationship.getDstLocation();
			if (dstLocation.isPresent()) {
				writer.append("<data key=\"dstLocation\">ModuleLocation{offset:").append(dstLocation.get().getOffset().toString())
					  .append(",length:").append(dstLocation.get().getLength().toString()).append("}</data>");
			}
			final var properties = relationship.getProperties();
			if (properties.isPresent()) {
				writer.append("<data key=\"properties\">").append(properties.get().toString()).append("</data>");
			}
			final var binding = relationship.getDependencyBinding();
			if (binding.isPresent()) {
				writer.append("<data key=\"dependencyBinding\">").append(binding.get().name()).append("</data>");
			}
			final var attributes = relationship.getDependencyAttributes();
			if (attributes.isPresent()) {
				writer.append("<data key=\"dependencyBinding\">").append(attributes.get()).append("</data>");
			}
			final var validIfReachedFrom = relationship.getValidIfReachedFrom();
			if ( ! validIfReachedFrom.isEmpty()) {
				writer.append("<data key=\"validIfReachedFrom\">").append(validIfReachedFrom.toString()).append("</data>");
			}
			writer.append("</edge>");
		}
	}

	@Override
	public String getJobName() {
		return "GraphML Exporter";
	}

	private List<String> getDataPointPaths() {
		/* for GraphQl use "content."*/
		final List<String> dataPointPaths = new ArrayList<>(16);
		dataPointPaths.add("uid");
		dataPointPaths.add("nid");
		dataPointPaths.add("name");

		if (parameters.containsKey(ATTRIBUTES_KEY)) {
			LOG.info(() -> "GraphML property selection available. Only querying the information for the selection: " + parameters.get(ATTRIBUTES_KEY));
			/* for GraphQl test if all fields start with "content."*/
			dataPointPaths.addAll(new HashSet<>(parameters.get(ATTRIBUTES_KEY)));
		} else {
			LOG.info(() -> "Empty GraphML property selection. Querying all information.");
			/* for GraphQl "content.taxonomies.name"*/
			/* remove with the removal of the detailedTaxonomyGraphmlExport feature flag*/
			if (detailedTaxonomyGraphmlExportfeatureToggle) {
				final List<String> taxonomies = getTaxonomies();
				if (taxonomies.size() > 0) {
					containsTaxonomies = true;
				}
				dataPointPaths.addAll(taxonomies);
			} else {
				dataPointPaths.add("taxonomies");
			}
			dataPointPaths.add("description");
			dataPointPaths.add("path");
			dataPointPaths.add("identification");
			dataPointPaths.add("representation");
			dataPointPaths.add("technology");
			dataPointPaths.add("type");
			dataPointPaths.add("storage");
			dataPointPaths.add("origin");
			dataPointPaths.add("metricsDate");
			dataPointPaths.add("modifiedDate");
			dataPointPaths.add("statements");
			dataPointPaths.add("sqlStatements");
			/* SourceMetrics */
			dataPointPaths.add(SOURCE_METRICS_PHYSICAL_LINES);
			dataPointPaths.add(SOURCE_METRICS_CODE_LINES);
			dataPointPaths.add(SOURCE_METRICS_COMMENT_LINES);
			dataPointPaths.add(SOURCE_METRICS_COMPLEXITY_MC_CABE);
			dataPointPaths.add(SOURCE_METRICS_DEAD_CODE_LINES);
		}

		return dataPointPaths;
	}
	
	private Map<Long, List<Tuple2<String, String>>> buildTaxonomiesForModule(final List<ModulePojo> content) {
		final Map<EntityId, List<TaxonomyPojo>> taxonomies = taxonomyService
				.findTaxonomiesPerModule(q -> q.ofModules(content.stream().map(ModulePojo::identity).collect(Collectors.toList())));
		
		final Map<Long, List<Tuple2<String, String>>> result = new HashMap<Long, List<Tuple2<String,String>>>();
		
		for (final Entry<EntityId, List<TaxonomyPojo>> e : taxonomies.entrySet()) {
			
			final List<Tuple2<String, String>> tupleList = new ArrayList<Tuple2<String,String>>();
			
			for (final TaxonomyPojo tax : e.getValue()) {
				tupleList.add(new Tuple2<String, String>(tax.getType().getName(), tax.getName()));
			}
			
			result.put(e.getKey().getNid(), tupleList);
		}

		return result;
	}
	
	private List<String> getTaxonomies() {
		return taxonomyService.find(q -> q.ofProject(projectId)).stream()
				.map(TaxonomyPojo::getType)
				.map(TaxonomyTypePojo::getName)
				.distinct()
				.map(s -> TAXONOMY_PREFIX + s)
				.collect(Collectors.toList());
	}
}
