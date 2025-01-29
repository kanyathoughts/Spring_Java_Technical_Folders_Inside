package innowake.mining.shared.model.datalineage.util;

import com.github.systemdir.gml.model.EdgeGraphicDefinition;
import com.github.systemdir.gml.model.GraphicDefinition;
import innowake.lib.core.api.lang.Nullable;
import org.jgrapht.graph.DefaultEdge;

import java.awt.Color;
import java.util.Set;

public class DataLineageGraphicsProviderForSeparateCopybooks extends DataLineageGraphicsProvider {
	private final Set<DefaultEdge> bidirectionalEdges;

    public DataLineageGraphicsProviderForSeparateCopybooks(final Set<DefaultEdge> biEdges) {
        super();
        bidirectionalEdges = biEdges;
    }

    @Override
    public final EdgeGraphicDefinition getEdgeGraphics(final @Nullable DefaultEdge edge,
                                                       final @Nullable Element edgeSource,
                                                       final @Nullable Element edgeTarget) {
    	final EdgeGraphicDefinition.Builder builder = new EdgeGraphicDefinition.Builder()
    			.setTargetArrow(EdgeGraphicDefinition.ArrowType.PLAIN)
    			.setLineColor(Color.black);
    	
        if (edgeSource instanceof Copybook) {
            builder.setLineType(GraphicDefinition.LineType.DASHED);
        }
        if (bidirectionalEdges.contains(edge)) {
        	builder.setSourceArrow(EdgeGraphicDefinition.ArrowType.PLAIN);
        }

        return builder.build();
    }
}
