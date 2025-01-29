package innowake.mining.shared.model.datalineage.util;

import static com.github.systemdir.gml.YedGmlWriter.PrintLabels.PRINT_EDGE_LABELS;
import static com.github.systemdir.gml.YedGmlWriter.PrintLabels.PRINT_GROUP_LABELS;
import static com.github.systemdir.gml.YedGmlWriter.PrintLabels.PRINT_VERTEX_LABELS;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import innowake.lib.core.api.lang.Nullable;

import org.jgrapht.DirectedGraph;
import org.jgrapht.Graph;
import org.jgrapht.UndirectedGraph;

import com.github.systemdir.gml.YedGmlWriter;
import com.github.systemdir.gml.YedGmlWriter.PrintLabels;
import com.github.systemdir.gml.model.EdgeGraphicDefinition;
import com.github.systemdir.gml.model.NodeGraphicDefinition;
import com.github.systemdir.gml.model.UniqueIntIdFunction;
import com.github.systemdir.gml.model.YedGmlGraphicsProvider;

/**
 * Provides a writer that is able to format a .gml file for yEd such that graph nodes can be nested.
 * @param <V> type of vertex
 * @param <E> type of edge
 * @param <G> type of group
 */
public class YedGmlWriterWithNesting<V, E, G> {
	private final String LABEL = "label";
	
	/**
	 * 
	 * Builder pattern for YedGmlWriterWithNesting to separate instantiation and parameter definition.
	 * @param <V1> type of vertex
	 * @param <E1> type of edge
	 * @param <G1> type of group
	 */
	public static class Builder<V1, E1, G1> {
		@Nullable
		private Function<G1, String> groupLabelProvider;
		@Nullable
		private Map<G1, ? extends Set<V1>> groupMapping;
		@Nullable
		private Function<V1, String> vertexIDProvider;
		@Nullable
        private Function<V1, String> vertexLabelProvider;
		@Nullable
        private Function<E1, String> edgeIDProvider;
		@Nullable
        private Function<E1, String> edgeLabelProvider;
        @Nullable
        private YedGmlGraphicsProvider<V1, E1, G1> graphicsProvider;
        @Nullable
        private EnumSet<PrintLabels> printLabels;
        
        /**
         * Defines how the elements' shape, color and names will be displayed in yEd.
         *
         * @param graphProvider Definition for shape, color, arrows etc. for the resulting graph nodes and edges.
         * @param labels The labels for name displaying. 
         * @return The builder with set graphProvider and labels.
         */
        public Builder<V1, E1, G1> setGraphicsProviderAndLabels(final YedGmlGraphicsProvider<V1, E1, G1> graphProvider, final PrintLabels... labels) {
        	this.graphicsProvider = graphProvider;
        	EnumSet<PrintLabels> temp = EnumSet.noneOf(PrintLabels.class);
        	temp.addAll(Arrays.asList(labels));
        	this.printLabels = temp;
        	
        	return this;
        }
        
        /**
         * 
         * Defines how nodes are grouped, i.e. nested inside other nodes. Allows for multiple levels of nesting.
         *
         * @param groupMapping Maps parent nodes to set of children nodes.
         * @param groupLabelProvider Provides the label of a group being displayed in yEd.
         * @return The builder with set groupMapping and groupLabelProviders.
         */
        public Builder<V1, E1, G1> setGroups(final Map<G1, ? extends Set<V1>> groupMapping, final Function<G1, String> groupLabelProvider) {
            this.groupMapping = groupMapping;
            this.groupLabelProvider = groupLabelProvider;
            return this;
        }
        
        public Builder<V1, E1, G1> setVertexIdProvider(final Function<V1, String> vertexIDprovider) {
        	this.vertexIDProvider = vertexIDprovider;
        	return this;
        }
        
        /**
         * 
         * Defines how vertices are being labeled in yEd.
         *
         * @param vertexLabelProvider A function that maps each vertex to a String name.
         * @return The builder with set vertexLabelProvider.
         */
        public Builder<V1, E1, G1> setVertexLabelProvider(final Function<V1, String> vertexLabelProvider) {
            this.vertexLabelProvider = vertexLabelProvider;
            return this;
        }
        
        /**
         * 
         * Defines how edges are being labeled in yEd.
         *
         * @param edgeLabelProvider A function that maps each edge to a String name.
         * @return The builder with set edgeLabelProvider.
         */
        public Builder<V1, E1, G1> setEdgeLabelProvider(final Function<E1, String> edgeLabelProvider) {
            this.edgeLabelProvider = edgeLabelProvider;
            return this;
        }
        
        /**
         * 
         * Instantiates YedGmlWriterWithNesting from its Builder.
         *
         * @return Useful instance of YedGmlWriterWithNesting.
         */
        public YedGmlWriterWithNesting<V1, E1, G1> build() {
        	return new YedGmlWriterWithNesting<>(this);
        }
	}
	
	protected static final String CREATOR = "JGraphT GML Exporter - modified by Hayato Hess, Andreas Hofstadler and augmented by Christian Valenti";
    protected static final String VERSION = "1";
    @Nullable
    protected final Map<G, ? extends Set<V>> groupMapping;
    @Nullable
    protected final Map<V, G> reversedGroupMapping;
    protected final Function<V, String> vertexIDProvider;
    protected final Function<V, String> vertexLabelProvider;
    protected final Function<? super E, String> edgeIDProvider;
    protected final Function<E, String> edgeLabelProvider;
    protected final YedGmlGraphicsProvider<V, E, G> graphProvider;
    protected final Function<G, String> groupLabelProvider;
    protected final Function<? super G, String> groupIdProvider;
    protected final EnumSet<PrintLabels> printLabels;
    
    private YedGmlWriterWithNesting(final Builder<V, E, G> builder) {
    	this.graphProvider = assertNotNull(builder.graphicsProvider, "GraphicsProvider must not be null");
    	this.printLabels = assertNotNull(builder.printLabels, "PrintLabels must not be null");
    	this.groupMapping = builder.groupMapping;
    	
    	this.vertexLabelProvider = builder.vertexLabelProvider != null
    			? builder.vertexLabelProvider
    			: Objects::toString;
    	this.edgeLabelProvider = builder.edgeLabelProvider != null
    			? builder.edgeLabelProvider
    			: Objects::toString;
    	
    	final UniqueIntIdFunction<V> uniqueVertexIDFunction = new UniqueIntIdFunction<>();
    	final UniqueIntIdFunction<Object> uniqueEdgeIDFunction = new UniqueIntIdFunction<>();
    	
    	this.vertexIDProvider = builder.vertexIDProvider != null
    			? builder.vertexIDProvider
    			: uniqueVertexIDFunction;
    	this.edgeIDProvider = builder.edgeIDProvider != null
    			? builder.edgeIDProvider
    			: uniqueEdgeIDFunction;
    	
    	if (groupMapping != null) {
    		reversedGroupMapping = new HashMap<>();
			for (final Map.Entry<G, ? extends Set<V>> group : assertNotNull(groupMapping).entrySet()) {
				for (V grouped : group.getValue()) {
					assertNotNull(reversedGroupMapping).put(grouped, group.getKey());
				}
			}
			this.groupLabelProvider = builder.groupLabelProvider != null
					? builder.groupLabelProvider
					: Objects::toString;
			groupIdProvider = uniqueEdgeIDFunction;
		} else {
			reversedGroupMapping = null;
			groupIdProvider = uniqueEdgeIDFunction;
			groupLabelProvider = Objects::toString;
		}
    }
    
    protected String quoted(final String s) {
    	StringBuilder sb = new StringBuilder(s.length() + 2);
    	sb.append('"');
    	for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            switch (ch) {
                case '"':
                    sb.append("&quot;");
                    break;
                case '&':
                    sb.append("&amp;");
                    /*
                     * '&' needs to append the character in any case, therefore no break-statement
                     */
					//$FALL-THROUGH$
				default:
                    sb.append(ch);
            }
        }
        sb.append('"');

        return sb.toString();
    }
    
    protected void exportHeader(final PrintWriter out) {
    	out.println("Creator" + YedGmlWriter.delim + quoted(CREATOR));
        out.println("Version" + YedGmlWriter.delim + VERSION);
    }
    
    protected void exportVertices(final PrintWriter out, final Graph<V, E> graph) {
    	for (final V vertex : graph.vertexSet()) {
			/* don't print vertices added as groups*/
    		if (groupMapping != null && groupMapping.containsKey(vertex)) {
				continue;
			}
    		out.println(YedGmlWriter.tab1 + "node");
    		out.println(YedGmlWriter.tab1 + "[");
    		out.println(YedGmlWriter.tab2 + "id" + YedGmlWriter.delim + vertexIDProvider.apply(vertex));
    		
    		boolean printVertexLabels = printLabels.contains(PRINT_VERTEX_LABELS);
    		if (printVertexLabels) {
                String label = vertexLabelProvider.apply(vertex);
                out.println(YedGmlWriter.tab2 + LABEL + YedGmlWriter.delim + quoted(label));
            }
    		
    		final NodeGraphicDefinition definition = graphProvider.getVertexGraphics(vertex);
    		if (definition != null)
                out.print(definition.toString(printVertexLabels));
    		if (reversedGroupMapping != null && reversedGroupMapping.containsKey(vertex))
    			out.println(YedGmlWriter.tab2 + "gid" + YedGmlWriter.delim + groupIdProvider.apply(assertNotNull(reversedGroupMapping).get(vertex)));
          
            out.println(YedGmlWriter.tab1 + "]");
		}
    }
    
    protected void exportGroups(final PrintWriter out) {
    	if (groupMapping == null) {
    		return;
    	}
    	
    	for (final Map.Entry<G, ? extends Set<V>> groupEntry : assertNotNull(groupMapping).entrySet()) {
    		final G group = groupEntry.getKey();
            out.println(YedGmlWriter.tab1 + "node");
            out.println(YedGmlWriter.tab1 + "[");
            out.println(YedGmlWriter.tab2 + "id" + YedGmlWriter.delim + groupIdProvider.apply(group));

            final boolean printGroupLabels = printLabels.contains(PRINT_GROUP_LABELS);
            if (printGroupLabels) {
            	final String label = groupLabelProvider.apply(group);
                out.println(YedGmlWriter.tab2 + LABEL + YedGmlWriter.delim + quoted(label));
            }
            
            final NodeGraphicDefinition definition = graphProvider.getGroupGraphics(group, groupEntry.getValue());
            if (definition != null) {
            	out.print(definition.toString(printGroupLabels));
            }
            out.println(YedGmlWriter.tab2 + "isGroup" + YedGmlWriter.delim + "1");

            if (reversedGroupMapping != null && reversedGroupMapping.containsKey(group)) {
            	out.println(YedGmlWriter.tab2 + "gid" + YedGmlWriter.delim + groupIdProvider.apply(assertNotNull(reversedGroupMapping).get(group)));            	
            }
            out.println(YedGmlWriter.tab1 + "]");
    	}
    }
    
    protected void exportEdges(final PrintWriter out, final Graph<V, E> graph) {
    	for (final E edge : graph.edgeSet()) {
            out.println(YedGmlWriter.tab1 + "edge");
            out.println(YedGmlWriter.tab1 + "[");
            final String id = edgeIDProvider.apply(edge);
            out.println(YedGmlWriter.tab2 + "id" + YedGmlWriter.delim + id);
            final String s = vertexIDProvider.apply(graph.getEdgeSource(edge));
            out.println(YedGmlWriter.tab2 + "source" + YedGmlWriter.delim + s);
            final String t = vertexIDProvider.apply(graph.getEdgeTarget(edge));
            out.println(YedGmlWriter.tab2 + "target" + YedGmlWriter.delim + t);


            final boolean printEdgeLabels = printLabels.contains(PRINT_EDGE_LABELS);

            if (printEdgeLabels) {
            	final String label = edgeLabelProvider.apply(edge);
                out.println(YedGmlWriter.tab2 + LABEL + YedGmlWriter.delim + quoted(label));
            }

            final EdgeGraphicDefinition definition = graphProvider.getEdgeGraphics(edge, graph.getEdgeSource(edge), graph.getEdgeTarget(edge));
            if (definition != null) {
                final Pattern pattern = Pattern.compile("(?i)(?<=style\\s\")[^\"]+");
                final Matcher matcher = pattern.matcher(definition.toString(printEdgeLabels));
                StringBuffer output = new StringBuffer();

                while (matcher.find()) {
                    String match = matcher.group();
                    matcher.appendReplacement(output, match.toLowerCase());
                }

                matcher.appendTail(output);
                String result = output.toString();
                out.print(result);
            }
            out.println(YedGmlWriter.tab1 + "]");
        }
    }
    
    protected void export(final Writer output, final Graph<V, E> graph, final boolean directed) {
    	final PrintWriter out = new PrintWriter(output);
    	/* Assign IDs in vertex set iteration order. The ID provider hereby stores already "seen" objects. */
        graph.vertexSet().forEach(vertexIDProvider::apply);
        
        /* Print GML header. */
        exportHeader(out);
        out.println("graph");
        out.println("[");
        out.println(YedGmlWriter.tab1 + LABEL + YedGmlWriter.delim + quoted(""));
        if (directed) {
            out.println(YedGmlWriter.tab1 + "directed" + YedGmlWriter.delim + "1");
        } else {
            out.println(YedGmlWriter.tab1 + "directed" + YedGmlWriter.delim + "0");
        }

        /* Export graph elements */
        exportVertices(out, graph);
        exportGroups(out);
        exportEdges(out, graph);

        /* Finish output operations */
        out.println("]");
        out.flush();
    }
    
    /**
     * 
     * Exports an undirected graph.
     *
     * @param output The writer that outputs the graph.
     * @param graph The graph to be exported.
     */
    public void export(final Writer output, final UndirectedGraph<V, E> graph) {
    	export(output, graph, false);
    }
    
    /**
     * 
     * Exports a directed graph.
     *
     * @param output The writer that outputs the graph.
     * @param graph The graph to be exported.
     */
    public void export(final Writer output, final DirectedGraph<V, E> graph) {
    	export(output, graph, true);
    }
}
