package innowake.mining.shared.model.datalineage.util;

import static com.github.systemdir.gml.YedGmlWriter.PrintLabels.PRINT_EDGE_LABELS;
import static com.github.systemdir.gml.YedGmlWriter.PrintLabels.PRINT_GROUP_LABELS;
import static com.github.systemdir.gml.YedGmlWriter.PrintLabels.PRINT_VERTEX_LABELS;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jgrapht.DirectedGraph;
import org.jgrapht.Graph;
import org.jgrapht.UndirectedGraph;

import com.github.systemdir.gml.YedGmlWriter;
import com.github.systemdir.gml.YedGmlWriter.PrintLabels;
import com.github.systemdir.gml.model.EdgeGraphicDefinition;
import com.github.systemdir.gml.model.NodeGraphicDefinition;
import com.github.systemdir.gml.model.UniqueIntIdFunction;
import com.github.systemdir.gml.model.YedGmlGraphicsProvider;

public class YedGmlWriterWithNesting<V, E, G> {
	
	public static class Builder<V1, E1, G1> {
		Function<G1, String> groupLabelProvider;
		Map<G1, ? extends Set<V1>> groupMapping;
		Function<Object, String> vertexIDProvider;
        Function<V1, String> vertexLabelProvider;
        Function<E1, String> edgeIDProvider;
        Function<E1, String> edgeLabelProvider;
        
        final YedGmlGraphicsProvider<V1, E1, G1> graphicsProvider;
        final EnumSet<PrintLabels> printLabels;
        
        public Builder(final YedGmlGraphicsProvider<V1, E1, G1> graphProvider, final PrintLabels... labels) {
        	this.graphicsProvider = graphProvider;
        	
        	EnumSet<PrintLabels> temp = EnumSet.noneOf(PrintLabels.class);
        	temp.addAll(Arrays.asList(labels));
        	this.printLabels = temp;
        }
        
        public Builder<V1, E1, G1> setGroups(final Map<G1, ? extends Set<V1>> groupMapping, final Function<G1, String> groupLabelProvider) {
            this.groupMapping = groupMapping;
            this.groupLabelProvider = groupLabelProvider;
            return this;
        }
        
        public Builder<V1, E1, G1> setVertexIDProvider(final Function<Object, String> vertexIDProvider) {
            this.vertexIDProvider = vertexIDProvider;
            return this;
        }
        
        public Builder<V1, E1, G1> setVertexLabelProvider(final Function<V1, String> vertexLabelProvider) {
            this.vertexLabelProvider = vertexLabelProvider;
            return this;
        }
        
        public Builder<V1, E1, G1> setEdgeIDProvider(final Function<E1, String> edgeIDProvider) {
            this.edgeIDProvider = edgeIDProvider;
            return this;
        }
        
        public Builder<V1, E1, G1> setEdgeLabelProvider(final Function<E1, String> edgeLabelProvider) {
            this.edgeLabelProvider = edgeLabelProvider;
            return this;
        }
        
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
    
    @NotNull
    protected final Function<Object, String> vertexIDProvider;
    @NotNull
    protected final Function<V, String> vertexLabelProvider;
    @NotNull
    protected final Function<? super E, String> edgeIDProvider;
    @NotNull
    protected final Function<E, String> edgeLabelProvider;
    @NotNull
    protected final YedGmlGraphicsProvider<V, E, G> graphProvider;
    @NotNull
    protected final Function<G, String> groupLabelProvider;
    @NotNull
    protected final Function<? super G, String> groupIdProvider;
    
    @NotNull
    protected final EnumSet<PrintLabels> printLabels;
    
    public YedGmlWriterWithNesting(final Builder<V, E, G> builder) {
    	this.graphProvider = builder.graphicsProvider;
    	this.printLabels = builder.printLabels;
    	this.groupMapping = builder.groupMapping;
    	
    	this.vertexLabelProvider = builder.vertexLabelProvider != null
    			? builder.vertexLabelProvider
    			: Objects::toString;
    	this.edgeLabelProvider = builder.edgeLabelProvider != null
    			? builder.edgeLabelProvider
    			: Objects::toString;
    	
    	final UniqueIntIdFunction<Object> uniqueIDFunction = new UniqueIntIdFunction<>();
    	
    	if (builder.vertexIDProvider != null) {
    		this.vertexIDProvider = builder.vertexIDProvider;			
		} else {
			this.vertexIDProvider = uniqueIDFunction;
		}
    	
    	if (builder.edgeIDProvider != null) {
            this.edgeIDProvider = builder.edgeIDProvider;
        } else {
            this.edgeIDProvider = uniqueIDFunction;
        }
    	
    	if (groupMapping != null) {
    		reversedGroupMapping = new HashMap<>();
			for (Map.Entry<G, ? extends Set<V>> group : groupMapping.entrySet()) {
				for (V grouped : group.getValue()) {
					reversedGroupMapping.put(grouped, group.getKey());
				}
			}
			this.groupLabelProvider = builder.groupLabelProvider != null
					? builder.groupLabelProvider
					: Objects::toString;
			groupIdProvider = uniqueIDFunction;
		} else {
			reversedGroupMapping = null;
			groupIdProvider = uniqueIDFunction;
			groupLabelProvider = Objects::toString;
		}
    }
    
    protected String quoted(final String s) {
    	final StringBuilder sb = new StringBuilder(s.length() + 2);
    	sb.append('"');
    	for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            switch (ch) {
                case '"':
                    sb.append("&quot;");
                    break;
                case '&':
                    sb.append("&amp;");
                    sb.append(ch);
                    break;
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
    	for (V vertex : graph.vertexSet()) {
			// don't print vertices added as groups
    		if (groupMapping != null && groupMapping.containsKey(vertex)) {
				continue;
			}
    		out.println(YedGmlWriter.tab1 + "node");
    		out.println(YedGmlWriter.tab1 + "[");
    		out.println(YedGmlWriter.tab2 + "id" + YedGmlWriter.delim + vertexIDProvider.apply(vertex));
    		
    		boolean printVertexLabels = printLabels.contains(PRINT_VERTEX_LABELS);
    		if (printVertexLabels) {
                String label = vertexLabelProvider.apply(vertex);
                out.println(YedGmlWriter.tab2 + "label" + YedGmlWriter.delim + quoted(label));
            }
    		
    		final NodeGraphicDefinition definition = graphProvider.getVertexGraphics(vertex);
    		if (definition != null)
                out.print(definition.toString(printVertexLabels));
    		if (reversedGroupMapping != null && reversedGroupMapping.containsKey(vertex))
    			out.println(YedGmlWriter.tab2 + "gid" + YedGmlWriter.delim + groupIdProvider.apply(reversedGroupMapping.get(vertex)));
          
            out.println(YedGmlWriter.tab1 + "]");
		}
    }
    
    protected void exportGroups(final PrintWriter out) {
    	if (groupMapping == null)
            return;
    	
    	for (Map.Entry<G, ? extends Set<V>> groupEntry : groupMapping.entrySet()) {
    		final G group = groupEntry.getKey();
            out.println(YedGmlWriter.tab1 + "node");
            out.println(YedGmlWriter.tab1 + "[");
            out.println(YedGmlWriter.tab2 + "id" + YedGmlWriter.delim + groupIdProvider.apply(group));

            boolean printGroupLabels = printLabels.contains(PRINT_GROUP_LABELS);
            if (printGroupLabels) {
                String label = groupLabelProvider.apply(group);
                out.println(YedGmlWriter.tab2 + "label" + YedGmlWriter.delim + quoted(label));
            }
            
            final NodeGraphicDefinition definition = graphProvider.getGroupGraphics(group, groupEntry.getValue());
            if (definition != null)
                out.print(definition.toString(printGroupLabels));
            out.println(YedGmlWriter.tab2 + "isGroup" + YedGmlWriter.delim + "1");

            if (reversedGroupMapping != null && reversedGroupMapping.containsKey(group))
            	out.println(YedGmlWriter.tab2 + "gid" + YedGmlWriter.delim + groupIdProvider.apply(reversedGroupMapping.get(group)));
            out.println(YedGmlWriter.tab1 + "]");
    	}
    }
    
    protected void exportEdges(final PrintWriter out, final Graph<V, E> graph) {
    	for (E edge : graph.edgeSet()) {
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
                out.println(YedGmlWriter.tab2 + "label" + YedGmlWriter.delim + quoted(label));
            }

            final EdgeGraphicDefinition definition = graphProvider.getEdgeGraphics(edge, graph.getEdgeSource(edge), graph.getEdgeTarget(edge));
            if (definition != null)
                out.print(definition.toString(printEdgeLabels));


            out.println(YedGmlWriter.tab1 + "]");
        }
    }
    
    protected void export(final Writer output, final Graph<V, E> graph, final boolean directed) {
    	PrintWriter out = new PrintWriter(output);
    	/*
    	 * Assign IDs in vertex set iteration order. The ID provider hereby stores already "seen" objects.
    	 */
        graph.vertexSet().forEach(vertexIDProvider::apply);
        
        /*
         *  Print GML header.
         */
        exportHeader(out);
        out.println("graph");
        out.println("[");
        out.println(YedGmlWriter.tab1 + "label" + YedGmlWriter.delim + quoted(""));
        if (directed) {
            out.println(YedGmlWriter.tab1 + "directed" + YedGmlWriter.delim + "1");
        } else {
            out.println(YedGmlWriter.tab1 + "directed" + YedGmlWriter.delim + "0");
        }

        /*
         *  Export graph elements
         */
        exportVertices(out, graph);
        exportGroups(out);
        exportEdges(out, graph);

        /*
         *  Finish output operations
         */
        out.println("]");
        out.flush();
    }
    
    public void export(final Writer output, final UndirectedGraph<V, E> graph) {
    	export(output, graph, false);
    }
    
    public void export(final Writer output, final DirectedGraph<V, E> graph) {
    	export(output, graph, true);
    }
}
