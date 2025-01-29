import {
    License,
    GraphComponent,
    IGraph,
    ShapeNodeStyle,
    SolidColorFill,
    Point,
    Size,
    Rect,
    ExteriorLabelModel,
    Arrow,
    DashStyle,
    ArrowType,
    Insets,
    TextWrapping,
    DefaultLabelStyle,
    PolylineEdgeStyle
} from 'yfiles';
import { environment } from '@env/environment';
import {
    GraphGlobalStyles,
    DEFAULT_NODE_SHAPE,
    DEFAULT_STROKE_THICKNESS,
    DEFAULT_NODE_ICON_SIZE,
    DEFAULT_NODE_HORIZONTAL_MARGIN,
    DEFAULT_EDGE_COLOR,
    DEFAULT_EDGE_SMOOTHING,
    DEFAULT_EDGE_THICKNESS,
    ARTIFICIAL_EDGE_COLOR,
    DEFAULT_EDGE_LABEL_INSET,
    DELETED_EDGE_COLOR
} from './graph-global-styles';
import { NodeWidthSizes, NodeHeightSizes } from './node-configurations';
import { NODE_COLORS } from './node-colors';
import { NODE_LABEL_CONFIG, LABEL_TEXT_SIZE, DEFAULT_LABEL_FONT } from './node-label-configuration';

describe('GraphGlobalStyles', () => {
    License.value = environment.yFilesLicense;
    let graph: IGraph;
    let component: GraphComponent;
    beforeEach(() => {
        component = new GraphComponent();
        graph = component.graph;
    });

    it('Should apply default styles on graph', () => {
        GraphGlobalStyles.setDefaultStyles(graph, NodeWidthSizes.S, NodeHeightSizes.S);
        const expectNodeLabelStyle = GraphGlobalStyles.getNodeLabelStyle(NodeWidthSizes.S, NodeHeightSizes.S, NODE_LABEL_CONFIG.default);
        const expectEdgeStyle = GraphGlobalStyles.getEdgeStyle();
        const expectEdgeLabelStyle = GraphGlobalStyles.getEdgeLabelStyle();
        expect(graph.nodeDefaults.size).toEqual(new Size(NodeWidthSizes.S, NodeHeightSizes.S));
        expect(graph.nodeDefaults.labels.style.renderer).toEqual(expectNodeLabelStyle.renderer);
        expect(graph.edgeDefaults.style.renderer).toEqual(expectEdgeStyle.renderer);
        expect(graph.edgeDefaults.labels.style.renderer).toEqual(expectEdgeLabelStyle.renderer);
    });

    it('Should apply default node styles', () => {
        const resultStyle: ShapeNodeStyle = GraphGlobalStyles.getShapeNodeStyle(NODE_COLORS[''], DEFAULT_NODE_SHAPE, DEFAULT_STROKE_THICKNESS);
        const expectedFillColor = new SolidColorFill(NODE_COLORS.default.fillColor);
        const expectedStrokeColor = new SolidColorFill(NODE_COLORS.default.strokeColor);
        expect((resultStyle.fill as SolidColorFill).color).toEqual(expectedFillColor.color);
        expect((resultStyle.stroke.fill as SolidColorFill).color).toEqual(expectedStrokeColor.color);
        expect(resultStyle.stroke.thickness).toBe(DEFAULT_STROKE_THICKNESS);
        expect(resultStyle.shape).toBe(DEFAULT_NODE_SHAPE);
    });

    for (const key in NODE_COLORS) {
        if (NODE_COLORS.hasOwnProperty(key)) {
            it('should apply fill and stroke colors for ' + key + ' nodes', () => {
                const resultStyle: ShapeNodeStyle = GraphGlobalStyles.getShapeNodeStyle(NODE_COLORS[key], DEFAULT_NODE_SHAPE, DEFAULT_STROKE_THICKNESS);
                const expectedFillColor = new SolidColorFill(NODE_COLORS[key].fillColor);
                const expectedStrokeColor = new SolidColorFill(NODE_COLORS[key].strokeColor);
                expect((resultStyle.fill as SolidColorFill).color).toEqual(expectedFillColor.color);
                expect((resultStyle.stroke.fill as SolidColorFill).color).toEqual(expectedStrokeColor.color);
            });
        }
    }

    it('Should return a resized node layout', () => {
        const pt: Point = new Point(0, 0);
        const size: Size = new Size(50, 50);
        const result: Rect = GraphGlobalStyles.getResizedNodeLayout(pt, size, false);
        expect(result.width).toBe(50 + 2 * DEFAULT_NODE_HORIZONTAL_MARGIN);
        expect(result.height).toBe(60);
    });

    it('Should return a resized node wide layout', () => {
        const pt: Point = new Point(0, 0);
        const size: Size = new Size(50, 50);
        const result: Rect = GraphGlobalStyles.getResizedNodeLayout(pt, size, true);
        expect(result.width).toBe(140);
        expect(result.height).toBe(60);
    });

    it('Should apply maximum size for node label', () => {
        const resultStyle = GraphGlobalStyles.getNodeLabelStyle(NodeWidthSizes.M, NodeHeightSizes.M, NODE_LABEL_CONFIG.default);
        expect((resultStyle as DefaultLabelStyle).maximumSize).toEqual(new Size(NodeWidthSizes.M, NodeHeightSizes.M));
        expect((resultStyle as DefaultLabelStyle).textSize ).toEqual(LABEL_TEXT_SIZE);
    });

    for (const key in NODE_LABEL_CONFIG) {
        if (NODE_COLORS.hasOwnProperty(key)) {
            it('should apply label styling for ' + key, () => {
                const resultStyle = GraphGlobalStyles.getNodeLabelStyle(NodeWidthSizes.L, NodeHeightSizes.L, NODE_LABEL_CONFIG[key]);
                expect((resultStyle as DefaultLabelStyle).font.fontFamily).toEqual(NODE_LABEL_CONFIG[key].fontFamily);
                expect((resultStyle as DefaultLabelStyle).font.fontWeight).toEqual(NODE_LABEL_CONFIG[key].fontWeight);
                const expectedColor = new SolidColorFill(NODE_LABEL_CONFIG[key].color);
                expect(((resultStyle as DefaultLabelStyle).textFill as SolidColorFill).color).toEqual(expectedColor.color);
            });
        }
    }

    it('Should apply styles for node label with an icon', () => {
        const iconPath = 'Path/to/icon';
        const labelStyle = GraphGlobalStyles.getNodeLabelStyle(NodeWidthSizes.S, NodeHeightSizes.S, NODE_LABEL_CONFIG.default);
        const resultStyle = GraphGlobalStyles.getNodeIconLabelStyle(iconPath, labelStyle);
        expect(resultStyle.iconSize).toEqual(new Size(DEFAULT_NODE_ICON_SIZE[0], DEFAULT_NODE_ICON_SIZE[1]));
        expect(resultStyle.wrapped).toBe(labelStyle);
        expect(resultStyle.iconPlacement).toBe(ExteriorLabelModel.WEST);
        expect(resultStyle.icon).toBe(iconPath);
    });

    it('should apply default style for edge', () => {
        const resultStyle = GraphGlobalStyles.getEdgeStyle();
        const expectColor = new SolidColorFill(DEFAULT_EDGE_COLOR);
        expect(((resultStyle as PolylineEdgeStyle).stroke.fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((resultStyle as PolylineEdgeStyle).smoothingLength).toBe(DEFAULT_EDGE_SMOOTHING);
        expect((resultStyle as PolylineEdgeStyle).stroke.dashStyle).toBe(DashStyle.SOLID);
        expect((resultStyle as PolylineEdgeStyle).stroke.thickness).toBe(DEFAULT_EDGE_THICKNESS);
        expect(((resultStyle as PolylineEdgeStyle).targetArrow as Arrow).type).toBe(ArrowType.TRIANGLE);
        expect((((resultStyle as PolylineEdgeStyle).targetArrow as Arrow).fill as SolidColorFill).color).toEqual(expectColor.color);
    });

    it('Should apply style for dashed ans artificial edges', () => {
        const resultStyle = GraphGlobalStyles.getEdgeStyle(DashStyle.DASH, ARTIFICIAL_EDGE_COLOR);
        const expectColor = new SolidColorFill(ARTIFICIAL_EDGE_COLOR);
        expect(((resultStyle as PolylineEdgeStyle).stroke.fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((((resultStyle as PolylineEdgeStyle).targetArrow as Arrow).fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((resultStyle as PolylineEdgeStyle).stroke.dashStyle).toBe(DashStyle.DASH);
    });

    it('Should apply red and dashed stroke for deleted node edges', () => {
        const resultStyle = GraphGlobalStyles.getEdgeStyle(DashStyle.DASH, DELETED_EDGE_COLOR);
        const expectColor = new SolidColorFill(DELETED_EDGE_COLOR);
        expect(((resultStyle as PolylineEdgeStyle).stroke.fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((((resultStyle as PolylineEdgeStyle).targetArrow as Arrow).fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((resultStyle as PolylineEdgeStyle).stroke.dashStyle).toBe(DashStyle.DASH);
    });

    it('Should apply bidirectional edge when bidirectional is true', () => {
        const resultStyle = GraphGlobalStyles.getEdgeStyle(DashStyle.SOLID, DEFAULT_EDGE_COLOR, true);
        const expectColor = new SolidColorFill(DEFAULT_EDGE_COLOR);
        expect(((resultStyle as PolylineEdgeStyle).stroke.fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((((resultStyle as PolylineEdgeStyle).targetArrow as Arrow).fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((((resultStyle as PolylineEdgeStyle).sourceArrow as Arrow).fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((resultStyle as PolylineEdgeStyle).stroke.dashStyle).toBe(DashStyle.SOLID);
    });

    it('Should apply default styles for edge label', () => {
        const resultStyle = GraphGlobalStyles.getEdgeLabelStyle();
        const expectColor = new SolidColorFill(DEFAULT_EDGE_COLOR);
        expect(((resultStyle as DefaultLabelStyle).textFill as SolidColorFill).color).toEqual(expectColor.color);
        expect(((resultStyle as DefaultLabelStyle).backgroundStroke.fill as SolidColorFill).color).toEqual(expectColor.color);
        expect((resultStyle as DefaultLabelStyle).textSize).toBe(LABEL_TEXT_SIZE);
        expect((resultStyle as DefaultLabelStyle).font.fontFamily).toBe(DEFAULT_LABEL_FONT);
        const expectInset = new Insets(DEFAULT_EDGE_LABEL_INSET[1], DEFAULT_EDGE_LABEL_INSET[0], DEFAULT_EDGE_LABEL_INSET[3], DEFAULT_EDGE_LABEL_INSET[2]);
        expect((resultStyle  as DefaultLabelStyle).insets).toEqual(expectInset);
        expect((resultStyle  as DefaultLabelStyle).wrapping).toEqual(TextWrapping.WORD_ELLIPSIS);
    });

    it('Should apply styles for artificial edge label', () => {
        const resultStyle = GraphGlobalStyles.getEdgeLabelStyle(ARTIFICIAL_EDGE_COLOR, ARTIFICIAL_EDGE_COLOR);
        const expectColor = new SolidColorFill(ARTIFICIAL_EDGE_COLOR);
        expect(((resultStyle  as DefaultLabelStyle).textFill as SolidColorFill).color).toEqual(expectColor.color);
        expect(((resultStyle as DefaultLabelStyle).backgroundStroke.fill as SolidColorFill).color).toEqual(expectColor.color);
    });
});
