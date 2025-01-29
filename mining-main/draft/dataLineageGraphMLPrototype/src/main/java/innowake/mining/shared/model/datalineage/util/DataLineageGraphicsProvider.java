package innowake.mining.shared.model.datalineage.util;

import java.awt.Color;
import java.util.Set;

import org.jgrapht.graph.DefaultEdge;

import com.github.systemdir.gml.model.EdgeGraphicDefinition;
import com.github.systemdir.gml.model.GraphicDefinition;
import com.github.systemdir.gml.model.NodeGraphicDefinition;
import com.github.systemdir.gml.model.NodeGraphicDefinition.Form;
import com.github.systemdir.gml.model.YedGmlGraphicsProvider;
import com.github.systemdir.gml.model.GraphicDefinition.LineType;

public class DataLineageGraphicsProvider implements YedGmlGraphicsProvider<Element, DefaultEdge, Module> {
	private final NodeGraphicDefinition dataInterfaceDefinition;
	private final NodeGraphicDefinition moduleDefinition;
	private final NodeGraphicDefinition fieldDefinition;
	private final NodeGraphicDefinition statementDefinition;
	private final NodeGraphicDefinition dataInterfaceGroupDefinition;
	
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
	}

	@Override
	public NodeGraphicDefinition getVertexGraphics(final Element vertex) {
		if (vertex instanceof DataInterface) {
			return dataInterfaceDefinition;
		} else if (vertex instanceof Field) {
			return fieldDefinition;
		} else if (vertex instanceof Statement) {
			return statementDefinition;
		} else if (vertex instanceof Module) {
			return moduleDefinition;
		} else {
			return dataInterfaceGroupDefinition;
		}
	}

	@Override
	public EdgeGraphicDefinition getEdgeGraphics(final DefaultEdge edge, final Element edgeSource, final Element edgeTarget) {
		return new EdgeGraphicDefinition.Builder()
				.setTargetArrow(EdgeGraphicDefinition.ArrowType.PLAIN)
				.setLineType(GraphicDefinition.LineType.NORMAL)
				.setLineColor(Color.black)
				.build();
	}

	@Override
	public NodeGraphicDefinition getGroupGraphics(final Module group, final Set<Element> groupElements) {
		return moduleDefinition;
	}

}
