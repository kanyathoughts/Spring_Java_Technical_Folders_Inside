package innowake.mining.server.discovery.metrics.generic;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.metrics.generic.MetricException.AstRootNotAvailableException;
import innowake.mining.server.discovery.metrics.generic.input.AstModelProvider;
import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.mining.server.discovery.metrics.generic.input.MetricInputProvider;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.visitor.TopDownVisitor;
import innowake.ndt.core.parsing.error.ParserError;

/**
 * Executes all registered metrics. Enabled metrics are grouped by {@link InputType}, so each input type 
 * has to be only resolved once. Instances of {@link AstBasedMetric} are handled special, since the {@link AstModel}
 * should only be traversed once for each analyzed artifact. 
 */
public class GenericMetricsContributor  {

	private final MetricInputProvider inputProvider;
	private final Set<AstBasedMetric> astMetrics = new HashSet<>();
	private final Map<InputType<?>, List<Metric<?>>> nonAstMetrics = new HashMap<>();
	
	/**
	 * Constructor to initialize Generic Metrics Contributor.
	 * 
	 * @param inputProvider provider of inputs to different metrics
	 */
	public GenericMetricsContributor(final MetricInputProvider inputProvider) {
		this.inputProvider = inputProvider;
	}
	
	/**
	 * Registers a concrete metric instance. Since the metrics are instantiated by the language contributor custom 
	 * metric implementations are possible.
	 * 
	 * @param metric the metric to register
	 */
	public void enable(final Metric<?> metric) {
		if (metric instanceof AstBasedMetric) {
			astMetrics.add((AstBasedMetric) metric);
		} else {
			nonAstMetrics.computeIfAbsent(metric.getInputType(), type -> new ArrayList<>()).add(metric);
		}
	}
	
	/**
	 * Executes all enabled metrics for the given artifact and returns the results to the caller.
	 * Caller is able to modify the result before applying it to the artifact. 
	 * 
	 * @param artifact the artifact to calculate the metrics for
	 * @return map containing the result for each metric
	 * @throws MetricException if sth. went wrong
	 */
	public Map<MetricType<?>, MetricResult<?>> execute(final ModelArtifact artifact) throws MetricException {
		
		final Map<MetricType<?>, MetricResult<?>> results = new HashMap<>();
		
		if ( ! astMetrics.isEmpty()) {
			executeAstMetrics(artifact, results);
		}
		
		nonAstMetrics.forEach((inputType, metrics) -> {
			final Object input = inputProvider.getInput(inputType, artifact);
			metrics.forEach(metric -> results.put(metric.getMetricType(), metric.executeGeneric(input)));
		});	
		
		return results;
	}
	
	/**
	 * Executes all enabled metrics for the given artifact and applies all results to it.
	 * 
	 * @param artifact the artifact to calculate the metrics for
	 * @throws MetricException if sth. went wrong
	 */
	public void executeAndApplyResults(final ModelArtifact artifact) throws MetricException {
		execute(artifact).values().forEach(result -> result.applyTo(artifact));
	}
	
	private void executeAstMetrics(final ModelArtifact entry, final Map<MetricType<?>, MetricResult<?>> results) throws MetricException {
		final AstModel astModel = ((AstModelProvider) inputProvider).getAstModel(entry);
		final AstNode root = getRootFromModel(astModel);
		final Set<AstBasedMetric> tmpMetrics = new HashSet<>(astMetrics);
		new TopDownVisitor<AstNode>(node -> {
			final Iterator<AstBasedMetric> iterator = tmpMetrics.iterator();
			while (iterator.hasNext()) {
				if (iterator.next().handle(node)) {
					iterator.remove();
				}
			}
			return ! tmpMetrics.isEmpty();
		}).visit(root);

		astMetrics.forEach(metric -> results.put(metric.getMetricType(), metric.getResult()));
	}

	private AstNode getRootFromModel(final AstModel astModel) throws MetricException {
		final Optional<AstNode> rootOptional = astModel.getRoot();
		if (rootOptional.isPresent()) {
			return rootOptional.get();
		} else {
			if (astModel.getParserErrors().isEmpty()) {
				throw new AstRootNotAvailableException();
			} else {
				throw new MetricException(astModel.getParserErrors().stream()
						.map(ParserError::getError)
						.collect(Collectors.joining(", ")));
			}
		}
	}
}
