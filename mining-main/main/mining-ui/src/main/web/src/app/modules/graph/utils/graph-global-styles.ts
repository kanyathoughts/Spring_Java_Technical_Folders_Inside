import {
    IGraph,
    ShapeNodeShape,
    Size,
    ShapeNodeStyle,
    SolidColorFill,
    Stroke,
    DefaultLabelStyle,
    Font,
    HorizontalTextAlignment,
    VerticalTextAlignment,
    TextWrapping,
    DashStyle,
    PolylineEdgeStyle,
    Arrow,
    ArrowType,
    ShapeNodeStyleRenderer,
    IconLabelStyle,
    ILabelStyle,
    ExteriorLabelModel,
    Rect,
    Point,
    IEdgeStyle,
    InteriorStretchLabelModelPosition,
    InteriorStretchLabelModel,
    Insets,
    CollapsibleNodeStyleDecorator,
    InteriorLabelModel,
    DefaultFolderNodeConverter,
    INode,
    NodeStyleDecorationInstaller,
    StyleDecorationZoomPolicy,
    GraphComponent,
    Color,
    GraphDecorator,
    LabelStyleDecorationInstaller,
    EdgeStyleDecorationInstaller,
    IEdge,
    ILabel,
    ILabelModelParameter
} from 'yfiles';
import { LABEL_TEXT_SIZE, DEFAULT_LABEL_FONT, NODE_LABEL_CONFIG } from './node-label-configuration';
import { GraphNodeLabelStyle } from '../models/graph-node-label-style.model';
import { NodeColor, NODE_COLORS } from './node-colors';
export const DEFAULT_STROKE_THICKNESS = 1;
export const SELECTED_NODE_STROKE_THICKNESS = 3;

export const DEFAULT_NODE_SHAPE = ShapeNodeShape.ROUND_RECTANGLE;
export const CONTROL_FLOW_NODE_SHAPE_MAPPING: { [type: string]: ShapeNodeShape } = {
    'terminal': ShapeNodeShape.ROUND_RECTANGLE,
    'decision': ShapeNodeShape.HEXAGON,
    'group': ShapeNodeShape.RECTANGLE,
    'process': DEFAULT_NODE_SHAPE
};

export const DEFAULT_NODE_FILL_ALPHA = 20;

export const DEFAULT_EDGE_COLOR = '#7B7D80';
export const DELETED_EDGE_COLOR = '#ff0000';
export const ARTIFICIAL_EDGE_COLOR = 'blue';
export const DEFAULT_EDGE_THICKNESS = 1;
export const DEFAULT_EDGE_SMOOTHING = 15;

export const DEFAULT_EDGE_LABEL_INSET: [number, number, number, number] = [3, 5, 3, 5];

export const DEFAULT_NODE_ICON_SIZE: [number, number] = [20, 20];

export const DEFAULT_NODE_VERTICAL_MARGIN = 5;
export const CLUE_NODE_VERTICAL_MARGIN = 1;
export const DEFAULT_NODE_HORIZONTAL_MARGIN = 10;
export const WIDE_NODE_HORIZONTAL_MARGIN = 45;
export const CLUE_NODE_HORIZONTAL_MARGIN = 6;

export const VISUAL_CLUE_BORDER_RADIUS = 10;

export const GROUP_INSET = 33.5;
export const GROUP_LABEL_INSET = 8;
export const GROUP_LABEL_OFFSET = 2;

const FOLDER_NODE_SIZE = new Size(220, 40);
export const BODY_BACKGROUND_COLOR = '#f8f9fa';
export const FUNCTIONAL_COUNT_COLOR = '#fff';
export const FUNCTIONAL_COUNT_SIZE = 10;
export const FUNCTIONAL_COUNT_FILL = '#14c4c4';
const DASHED_BORDER = '1px dashed #bfbfbf';

export class GraphGlobalStyles {
    /**
     * Set the default styling properties for nodes, edges and labels at graph initialisation.
     * @param graph the graph on which the styles will be applied.
     * @param nodeMaxWidth the max width that nodes will have by default.
     * @param nodeMaxHeight the max height that nodes will have by default.
     */
    public static setDefaultStyles(graph: IGraph, nodeMaxWidth: number, nodeMaxHeight: number): void {
        graph.nodeDefaults.size = new Size(nodeMaxWidth, nodeMaxHeight);
        graph.nodeDefaults.labels.style = GraphGlobalStyles.getNodeLabelStyle(nodeMaxWidth, nodeMaxHeight, NODE_LABEL_CONFIG.default);
        graph.edgeDefaults.style = GraphGlobalStyles.getEdgeStyle();
        graph.edgeDefaults.labels.style = GraphGlobalStyles.getEdgeLabelStyle();
        const collapsibleStyleDec: CollapsibleNodeStyleDecorator = graph.groupNodeDefaults.style as CollapsibleNodeStyleDecorator;
        collapsibleStyleDec.buttonPlacement = InteriorLabelModel.NORTH_EAST;
        collapsibleStyleDec.wrapped = new ShapeNodeStyle({ fill: NODE_COLORS['GROUP'].fillColor, stroke: NODE_COLORS['GROUP'].strokeColor });
        collapsibleStyleDec.insets = new Insets(GROUP_INSET, GROUP_INSET, GROUP_INSET, GROUP_INSET);
        graph.groupNodeDefaults.style = collapsibleStyleDec;
        graph.groupNodeDefaults.labels.style = GraphGlobalStyles.getNodeLabelStyle(nodeMaxWidth, nodeMaxHeight, NODE_LABEL_CONFIG.default,
            HorizontalTextAlignment.LEFT, VerticalTextAlignment.TOP);
        graph.groupNodeDefaults.labels.layoutParameter = new InteriorStretchLabelModel({
            insets: new Insets(GROUP_LABEL_INSET, GROUP_LABEL_INSET, GROUP_LABEL_INSET, GROUP_LABEL_INSET)
        }).createParameter(InteriorStretchLabelModelPosition.NORTH);
    }

    /**
     * Returns a YFile ShapeNodeStyle object to apply styles on a node.
     * @param color a NodeColor object containing the fill and stroke colors to apply to the node.
     * @param nodeShape an YFile ShapeModeShape to apply to the node.
     * @param strokeThickness value to applay to the stroke thickness.
     * @param isHeader apply different styles whether the node is a header or not.
     * @param isClue apply different styles whether the node is a visual clue or not.
     * @param dashedBorder apply dashed border to the node.
     */
    public static getShapeNodeStyle(
        color: NodeColor = NODE_COLORS.default,
        nodeShape: ShapeNodeShape, strokeThickness: number,
        isHeader: boolean = false,
        isClue: boolean = false,
        dashedBorder: boolean = false
    ): ShapeNodeStyle {
        const fill = new SolidColorFill(color.fillColor);
        const stroke = dashedBorder ? DASHED_BORDER : new Stroke(new SolidColorFill(color.strokeColor), strokeThickness);
        const nodeStyle = new ShapeNodeStyle({
            shape: nodeShape,
            fill,
            stroke
        });
        if (isHeader) {
            (nodeStyle.renderer as ShapeNodeStyleRenderer).roundRectArcRadius = 15;
        }
        if (isClue) {
            (nodeStyle.renderer as ShapeNodeStyleRenderer).roundRectArcRadius = VISUAL_CLUE_BORDER_RADIUS;
        }

        return nodeStyle;
    }

    /**
     * Returns a layout object to be applyed to a specific node.
     * @param point YFile point of the node.
     * @param contentSize  new content size of the node, the label preferred size can be passed to resize the node to its content.
     * @param isWide Apply specific left and right margin if the node contains is wide (ex.: nodes with icons, branch nodes).
     */
    public static getResizedNodeLayout(point: Point, contentSize: Size, isWide: boolean, isClue: boolean = false): Rect {
        let horizontalMargin = isWide ? WIDE_NODE_HORIZONTAL_MARGIN : DEFAULT_NODE_HORIZONTAL_MARGIN;
        horizontalMargin = isClue ? CLUE_NODE_HORIZONTAL_MARGIN : horizontalMargin;
        const verticalMargin = isClue ? CLUE_NODE_VERTICAL_MARGIN : DEFAULT_NODE_VERTICAL_MARGIN;
        return new Rect(point, contentSize).getEnlarged([verticalMargin, horizontalMargin]);
    }

    /**
     * Returns an object containing styles for labels.
     * @param maxWidth the width allowed for the label before text wrapping.
     * @param maxHeight the height allowed for the label before text wrapping.
     * @param labelStyle an object containing the font family, the font weight and the font color.
     * @param horizontalAlignment horizontal alignment of the label
     * @param verticalAlignment vertical alignment of the label
     */
    public static getNodeLabelStyle(nodeMaxWidth: number, nodeMaxHeight: number, labelStyle: GraphNodeLabelStyle, horizontalAlignment: HorizontalTextAlignment
        = HorizontalTextAlignment.CENTER, verticalAlignment: VerticalTextAlignment = VerticalTextAlignment.CENTER): ILabelStyle {
        const labelFont = new Font({
            fontFamily: labelStyle.fontFamily,
            fontWeight: labelStyle.fontWeight
        });

        return new DefaultLabelStyle({
            textFill: labelStyle.color,
            textSize: LABEL_TEXT_SIZE,
            font: labelFont,
            horizontalTextAlignment: horizontalAlignment,
            verticalTextAlignment: verticalAlignment,
            wrapping: TextWrapping.WORD_ELLIPSIS,
            maximumSize: new Size(nodeMaxWidth, nodeMaxHeight)
        });
    }

    /**
     * Returns the style object for labels containing an icon.
     * @param iconPath the path to the icon file.
     * @param baseLabelStyle the label base styles.
     * @param insets the insets to apply to the label.
     * @param defaultIconPlacement the default placement of the icon.
     */
    public static getNodeIconLabelStyle(iconPath: string, baseLabelStyle: ILabelStyle,
        insets: Insets = new Insets(0, 0, 0, 0),
        defaultIconPlacement: ILabelModelParameter = ExteriorLabelModel.WEST): IconLabelStyle {
        return new IconLabelStyle({
            icon: iconPath,
            iconSize: DEFAULT_NODE_ICON_SIZE,
            iconPlacement: defaultIconPlacement,
            wrapped: baseLabelStyle,
            wrappedInsets: insets
        });
    }

    /**
     * Returns an object containing styles for edges.
     * @param dashStyle apply different edge line styles ex: dash. solid.
     * @param color apply different colors.
     */
    public static getEdgeStyle(dashStyle: DashStyle = DashStyle.SOLID, color: string = DEFAULT_EDGE_COLOR, isBidirectional?: boolean): IEdgeStyle {
        const strokeStyle = new Stroke({
            fill: color,
            thickness: DEFAULT_EDGE_THICKNESS,
            dashStyle
        });
        const defaultEdgeArrowStroke = strokeStyle.clone();
        defaultEdgeArrowStroke.dashStyle = DashStyle.SOLID;
        const defaultEdgeArrow = new Arrow({
            type: ArrowType.TRIANGLE,
            stroke: defaultEdgeArrowStroke,
            fill: color,
            cropLength: 1
        });
        return new PolylineEdgeStyle({
            stroke: strokeStyle,
            targetArrow: defaultEdgeArrow,
            sourceArrow: isBidirectional ? defaultEdgeArrow : undefined,
            smoothingLength: DEFAULT_EDGE_SMOOTHING
        });
    }

    /**
     * Returns an object containing styles for labels on edges.
     * @param textColor apply text color for the edge label.
     * @param strokeColor apply stroke color for the edge.
     */
    public static getEdgeLabelStyle(textColor: string = DEFAULT_EDGE_COLOR, strokeColor: string = DEFAULT_EDGE_COLOR): ILabelStyle {
        return new DefaultLabelStyle({
            textFill: textColor,
            textSize: LABEL_TEXT_SIZE,
            font: DEFAULT_LABEL_FONT,
            backgroundFill: BODY_BACKGROUND_COLOR,
            backgroundStroke: strokeColor,
            insets: DEFAULT_EDGE_LABEL_INSET,
            horizontalTextAlignment: HorizontalTextAlignment.CENTER,
            verticalTextAlignment: VerticalTextAlignment.CENTER,
            wrapping: TextWrapping.WORD_ELLIPSIS
        });
    }

    public static getFolderNodeConverter(annotationNode?: boolean, node?: INode, graph?: IGraph): DefaultFolderNodeConverter {
        const defaultFolderNodeConverter = new DefaultFolderNodeConverter();
        defaultFolderNodeConverter.copyFirstLabel = true;
        defaultFolderNodeConverter.folderNodeSize = FOLDER_NODE_SIZE;
        if (annotationNode) {
            const style = new ShapeNodeStyle({ fill: NODE_COLORS['GROUP'].fillColor, stroke: NODE_COLORS['GROUP'].strokeColor });
            const collapsibleDecorator = new CollapsibleNodeStyleDecorator(style);
            collapsibleDecorator.buttonPlacement = InteriorLabelModel.NORTH_EAST;
            graph.setStyle(node, collapsibleDecorator);
            defaultFolderNodeConverter.folderNodeStyle = collapsibleDecorator;
        }
        return defaultFolderNodeConverter;
    }

    /**
     * Set the node highlight styling on click and hover.
     * @param blueStroke color of the stroke.
     * @param decorator decorate the nodes, edges and labels with custom hover highlight styles.
     * @param item node/edge item which hovered/clicked.
     */
    public static nodeHighlightStyles(blueStroke: Stroke, decorator: GraphDecorator, item: INode = null, nodeTypes = 'sameSizeNodes'): void {
        const sameSizeNodeCondition = item == null || nodeTypes === 'sameSizeNodes';
        let shapeType = sameSizeNodeCondition ? ShapeNodeShape.ROUND_RECTANGLE : CONTROL_FLOW_NODE_SHAPE_MAPPING[item?.tag?.shapeType];
        if (IEdge.isInstance(item)) {
            shapeType = sameSizeNodeCondition ? ShapeNodeShape.ROUND_RECTANGLE : CONTROL_FLOW_NODE_SHAPE_MAPPING[item?.sourceNode.tag.shapeType];
            this.nodeStyleDecider(shapeType, blueStroke, nodeTypes, decorator, item?.sourceNode);
            shapeType = sameSizeNodeCondition ? ShapeNodeShape.ROUND_RECTANGLE : CONTROL_FLOW_NODE_SHAPE_MAPPING[item?.targetNode.tag.shapeType];
            this.nodeStyleDecider(shapeType, blueStroke, nodeTypes, decorator, item?.targetNode);
        } else if(ILabel.isInstance(item)) {
            shapeType = sameSizeNodeCondition ? ShapeNodeShape.ROUND_RECTANGLE : CONTROL_FLOW_NODE_SHAPE_MAPPING[item.owner['sourceNode'].tag.shapeType];
            this.nodeStyleDecider(shapeType, blueStroke, nodeTypes, decorator, item.owner['sourceNode'] as INode);
            shapeType = sameSizeNodeCondition ? ShapeNodeShape.ROUND_RECTANGLE : CONTROL_FLOW_NODE_SHAPE_MAPPING[item.owner['targetNode'].tag.shapeType];
            this.nodeStyleDecider(shapeType, blueStroke, nodeTypes, decorator, item.owner['targetNode'] as INode);
        } else {
            this.nodeStyleDecider(shapeType, blueStroke, nodeTypes, decorator, item);
        }
    }

    /**
     * Set the edge highlight styling on click and hover.
     * @param blueStroke color of the stroke.
     * @param decorator decorate the nodes, edges and labels with custom hover highlight styles.
     */
    public static edgeHighlightStyles(blueStroke: Stroke, decorator: GraphDecorator): void {
        // a similar style for the edges, however cropped by the highlight's insets
        const dummyCroppingArrow = new Arrow({
            stroke: blueStroke,
            type: ArrowType.TRIANGLE
        });
        const edgeStyle = new PolylineEdgeStyle({
            stroke: blueStroke,
            targetArrow: dummyCroppingArrow,
            smoothingLength: DEFAULT_EDGE_SMOOTHING
        });
        const edgeStyleHighlight = new EdgeStyleDecorationInstaller({
            edgeStyle,
            zoomPolicy: StyleDecorationZoomPolicy.WORLD_COORDINATES
        });
        decorator.edgeDecorator.highlightDecorator.setImplementation(edgeStyleHighlight);
    }

    /**
     * Set the label highlight styling on click and hover.
     * @param blueStroke color of the stroke.
     * @param decorator decorate the nodes, edges and labels with custom hover highlight styles.
     */
    public static labelHighlightStyles(blueStroke: Stroke, decorator: GraphDecorator, isSharedResource: boolean = false): void {
        const edgeLabel = new DefaultLabelStyle({
            textFill: DEFAULT_EDGE_COLOR,
            textSize: LABEL_TEXT_SIZE,
            font: DEFAULT_LABEL_FONT,
            backgroundFill: BODY_BACKGROUND_COLOR,
            backgroundStroke: blueStroke,
            insets: DEFAULT_EDGE_LABEL_INSET,
            horizontalTextAlignment: HorizontalTextAlignment.CENTER,
            verticalTextAlignment: VerticalTextAlignment.CENTER,
            wrapping: TextWrapping.WORD_ELLIPSIS,
            shape: isSharedResource ? 'pill' : 'rectangle'
          });

        const labelStyleHighlight = new LabelStyleDecorationInstaller({
            labelStyle: edgeLabel,
            margins: 0,
            zoomPolicy: StyleDecorationZoomPolicy.WORLD_COORDINATES
        });
        decorator.labelDecorator.highlightDecorator.setImplementation(labelStyleHighlight);
    }

    /**
     * Set the label highlight color on click and hover.
     * @param type type of event.
     * @param graphComponent graphComponent instance.
     */
    public static initializeColorHighlights(type: string, graphComponent: GraphComponent): { blueStroke: Stroke, decorator: GraphDecorator } {
        // we want to create a non-default nice highlight stylin
        // for the hover highlight, create semi transparent orange stroke first
        const color = type === 'click' ? Color.DODGER_BLUE : Color.LIGHT_SKY_BLUE;
        // Color of the stroke
        const blueStroke = new Stroke(color.r, color.g, color.b, 220, type === 'click'? 1:  3);
        // freeze it for slightly improved performance
        blueStroke.freeze();

        // now decorate the nodes and edges with custom hover highlight styles
        const decorator = graphComponent.graph.decorator;
        return { blueStroke, decorator };
    }

    /**
     * method to decide the highlighting size of node(on hove or click)
     * @param  shapeType Node shapes
     * @param  blueStroke color of the strokes
     * @param  nodeTypes graph is having same shape nodes of different types
     * @param  decorator a factory for lookup decorators for the various items that make up an
     * @param  item hovered or clicked node
     */
    public static nodeStyleDecider(shapeType: ShapeNodeShape, blueStroke: Stroke, nodeTypes: string, decorator: GraphDecorator, item: INode = null): void {
        const highlightShape = new ShapeNodeStyle({
            shape: shapeType,
            stroke: blueStroke,
            fill: null
        });
       const nodeStyleHighlight = new NodeStyleDecorationInstaller({
            nodeStyle: highlightShape,
            // that should be slightly larger than the real node
            margins: 0,
            // but have a fixed size in the view coordinates
            zoomPolicy: StyleDecorationZoomPolicy.WORLD_COORDINATES
        });
        if ( item || nodeTypes !== 'sameSizeNodes') { // changes are made to set highlight for specific node.
           decorator.nodeDecorator.highlightDecorator.setImplementation(item, nodeStyleHighlight);
        } else {
            decorator.nodeDecorator.highlightDecorator.setImplementation(nodeStyleHighlight);
        }
    }
}
