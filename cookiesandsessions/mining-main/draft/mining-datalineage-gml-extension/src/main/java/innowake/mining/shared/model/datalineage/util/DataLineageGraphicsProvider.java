package innowake.mining.shared.model.datalineage.util;

import java.awt.Color;
import java.util.Set;

import org.jgrapht.graph.DefaultEdge;

import com.github.systemdir.gml.model.EdgeGraphicDefinition;
import com.github.systemdir.gml.model.GraphicDefinition.LineType;
import com.github.systemdir.gml.model.NodeGraphicDefinition;
import com.github.systemdir.gml.model.NodeGraphicDefinition.Form;
import com.github.systemdir.gml.model.YedGmlGraphicsProvider;

import innowake.lib.core.api.lang.Nullable;

/**
 * Definition of all the graphical representation in our resulting GML-graph.
 */
public class DataLineageGraphicsProvider implements YedGmlGraphicsProvider<Element, DefaultEdge, Module> {
    private final NodeGraphicDefinition dataInterfaceDefinition;
    private final NodeGraphicDefinition moduleDefinition;
    private final NodeGraphicDefinition fieldDefinition;
    private final NodeGraphicDefinition statementDefinition;
    private final NodeGraphicDefinition dataInterfaceGroupDefinition;
    private final NodeGraphicDefinition copybookDefinition;

    /**
     * Constructor to set shapes and colors of all different elements.
     */
    public DataLineageGraphicsProvider() {
        dataInterfaceDefinition = new NodeGraphicDefinition.Builder()
                .setFill(Color.yellow)
                .setForm(Form.rectangle)
                .setLineType(LineType.DASHED)
                .build();
        moduleDefinition = new NodeGraphicDefinition.Builder()
                .setFill(Color.lightGray)
                .setForm(Form.rectangle)
                .setLineType(LineType.DASHED)
                .build();
        fieldDefinition = new NodeGraphicDefinition.Builder()
                .setFill(Color.green)
                .setForm(Form.rectangle)
                .setLineType(LineType.DASHED)
                .build();
        statementDefinition = new NodeGraphicDefinition.Builder()
                .setFill(Color.white)
                .setForm(Form.rectangle)
                .setLineType(LineType.DASHED)
                .build();
        dataInterfaceGroupDefinition = new NodeGraphicDefinition.Builder()
                .setFill(Color.white)
                .setForm(Form.rectangle)
                .setLineType(LineType.DASHED)
                .build();
        copybookDefinition = new NodeGraphicDefinition.Builder()
                .setFill(Color.gray)
                .setForm(Form.rectangle)
                .setLineType(LineType.DOTTED)
                .build();
    }

    @Override
    public final NodeGraphicDefinition getVertexGraphics(final @Nullable Element vertex) {
        if (vertex instanceof DataInterface) {
            return dataInterfaceDefinition;
        } else if (vertex instanceof Field) {
            return fieldDefinition;
        } else if (vertex instanceof Statement) {
            return statementDefinition;
        } else if (vertex instanceof Copybook) {
            return copybookDefinition;
        } else if (vertex instanceof Module) {
            return moduleDefinition;
        } else {
            return dataInterfaceGroupDefinition;
        }
    }

    @Override
    public EdgeGraphicDefinition getEdgeGraphics(final @Nullable DefaultEdge edge,
                                                       final @Nullable Element edgeSource,
                                                       final @Nullable Element edgeTarget) {

        return new EdgeGraphicDefinition.Builder()
                .setTargetArrow(EdgeGraphicDefinition.ArrowType.PLAIN)
                .setLineType(LineType.NORMAL)
                .setLineColor(Color.black)
                .build();
    }

    @Override
    public NodeGraphicDefinition getGroupGraphics(final @Nullable Module group,
                                                  final @Nullable Set<Element> groupElements) {
        return moduleDefinition;
    }
}
