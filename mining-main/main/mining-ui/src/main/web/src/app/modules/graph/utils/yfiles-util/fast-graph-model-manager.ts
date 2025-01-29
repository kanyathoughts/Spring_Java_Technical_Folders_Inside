/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import {
  BaseClass,
  CanvasComponent,
  Exception,
  GraphComponent,
  GraphModelManager,
  ICanvasContext,
  ICanvasObject,
  ICanvasObjectDescriptor,
  ICanvasObjectGroup,
  Size,
  VoidLabelStyle,
  VoidPortStyle,
  WebGLPolylineEdgeStyle,
  WebGLShapeNodeStyle,
  INodeStyle,
  IEdgeStyle,
  ILabelStyle,
  IPortStyle,
  Arrow,
  ArrowType,
  Fill,
  PolylineEdgeStyle,
} from 'yfiles';

export class FastGraphModelManager extends GraphModelManager {
  public graphComponent: GraphComponent;
  public $contentGroup: ICanvasObjectGroup;
  public fastNodeDescriptor: AutoSwitchDescriptor;
  public fastEdgeDescriptor: AutoSwitchDescriptor;
  public fastLabelDescriptor: AutoSwitchDescriptor;
  public fastPortDescriptor: AutoSwitchDescriptor;
  public $intermediateZoomThreshold: number;
  public $overviewZoomThreshold: number;
  public $refreshImageZoomFactor: number;
  public $imageSizeFactor: number;
  public $maximumCanvasSize: Size;
  public $dirty: boolean;
  public $drawNodeCallback: any;
  public $drawEdgeCallback: any;
  public $drawNodeLabelCallback: any;
  public $drawEdgeLabelCallback: any;
  public $graphOptimizationMode: number;

  constructor(canvas: CanvasComponent, contentGroup: ICanvasObjectGroup) {
    super(canvas, contentGroup);
    this.initFastGraphModelManager();
    if (!(canvas instanceof GraphComponent)) {
      throw new Exception('Canvas must be a GraphComponent');
    }
    this.graphComponent = canvas;
    this.$contentGroup = contentGroup;

    // set the graph for the component view
    this.graph = this.graphComponent.graph;

    this.fastNodeDescriptor = new AutoSwitchDescriptor(
      GraphModelManager.DEFAULT_NODE_DESCRIPTOR,
      this
    );
    this.fastEdgeDescriptor = new AutoSwitchDescriptor(
      GraphModelManager.DEFAULT_EDGE_DESCRIPTOR,
      this
    );
    this.fastLabelDescriptor = new AutoSwitchDescriptor(
      GraphModelManager.DEFAULT_LABEL_DESCRIPTOR,
      this
    );
    this.fastPortDescriptor = new AutoSwitchDescriptor(
      GraphModelManager.DEFAULT_PORT_DESCRIPTOR,
      this
    );

    // create the custom descriptors for graph items
    this.nodeDescriptor = this.fastNodeDescriptor;
    this.edgeDescriptor = this.fastEdgeDescriptor;
    this.nodeLabelDescriptor = this.fastLabelDescriptor;
    this.edgeLabelDescriptor = this.fastLabelDescriptor;
    this.portDescriptor = this.fastPortDescriptor;

    // initialize the intermediate and overview styles with default values
    this.overviewNodeStyle = new WebGLShapeNodeStyle();
    this.overviewEdgeStyle = new WebGLPolylineEdgeStyle();
    const defaultEdgeArrow = new Arrow({
      type: ArrowType.SHORT,
      stroke: 'black',
      fill: Fill.BLACK,
      cropLength: 1
    });
    const defaultEdgeStyle = new PolylineEdgeStyle({
      stroke: 'black',
      targetArrow: defaultEdgeArrow
    });
    this.intermediateEdgeStyle = defaultEdgeStyle;
    // we don't render ports and labels in other levels than the default level
    this.overviewLabelStyle = VoidLabelStyle.INSTANCE;
    this.overviewPortStyle = VoidPortStyle.INSTANCE;
    this.fastLabelDescriptor.intermediateStyle = null;

    // set default values
    this.$intermediateZoomThreshold = 0.8;
    this.$overviewZoomThreshold = 0.4;
    this.$refreshImageZoomFactor = 0.5;
    this.$imageSizeFactor = 2;
    this.$maximumCanvasSize = new Size(3000, 2000);

    this.$dirty = false;
    this.$drawNodeCallback = null;
    this.$drawEdgeCallback = null;
    this.$drawNodeLabelCallback = null;
    this.$drawEdgeLabelCallback = null;

    // register to graphComponent events that could trigger a visualization change
    /* eslint-disable @typescript-eslint/no-unsafe-argument */
    this.graphComponent.addZoomChangedListener(this.onGraphComponentZoomChanged.bind(this));
  }

  get graphOptimizationMode(): number {
    return this.$graphOptimizationMode;
  }
  get intermediateNodeStyle() {
    return this.fastNodeDescriptor.intermediateStyle;
  }
  get intermediateEdgeStyle() {
    return this.fastEdgeDescriptor.intermediateStyle;
  }
  get overviewNodeStyle() {
    return this.fastNodeDescriptor.overviewStyle;
  }
  get overviewEdgeStyle() {
    return this.fastEdgeDescriptor.overviewStyle;
  }
  get overviewLabelStyle() {
    return this.fastLabelDescriptor.overviewStyle;
  }
  get overviewPortStyle() {
    return this.fastPortDescriptor.overviewStyle;
  }
  get intermediateZoomThreshold() {
    return this.$intermediateZoomThreshold;
  }
  get overviewZoomThreshold() {
    return this.$overviewZoomThreshold;
  }
  get refreshImageZoomFactor() {
    return this.$refreshImageZoomFactor;
  }
  get imageSizeFactor() {
    return this.$imageSizeFactor;
  }
  get maximumCanvasSize() {
    return this.$maximumCanvasSize;
  }
  get drawNodeCallback() {
    return this.$drawNodeCallback;
  }
  get drawEdgeCallback() {
    return this.$drawEdgeCallback;
  }
  get drawNodeLabelCallback(): any {
    return this.$drawNodeLabelCallback;
  }
  get drawEdgeLabelCallback(): any {
    return this.$drawEdgeLabelCallback;
  }
  get dirty(): boolean {
    return this.$dirty;
  }

  set graphOptimizationMode(value: number) {
    this.$graphOptimizationMode = value;
    this.updateEffectiveStyles();
  }
  set intermediateNodeStyle(value: INodeStyle) {
    this.fastNodeDescriptor.intermediateStyle = value;
  }

  set intermediateEdgeStyle(value: IEdgeStyle) {
    this.fastEdgeDescriptor.intermediateStyle = value;
  }

  set overviewNodeStyle(value: INodeStyle) {
    this.fastNodeDescriptor.overviewStyle = value;
  }

  set overviewEdgeStyle(value: IEdgeStyle) {
    this.fastEdgeDescriptor.overviewStyle = value;
  }

  set overviewLabelStyle(value: ILabelStyle) {
    this.fastLabelDescriptor.overviewStyle = this.fastLabelDescriptor.intermediateStyle = value;
  }

  set overviewPortStyle(value: IPortStyle) {
    this.fastPortDescriptor.overviewStyle = this.fastPortDescriptor.intermediateStyle = value;
  }

  set intermediateZoomThreshold(value: number) {
    this.$intermediateZoomThreshold = value;
  }

  set overviewZoomThreshold(value: number) {
    this.$overviewZoomThreshold = value;
  }

  set refreshImageZoomFactor(value: number) {
    this.$refreshImageZoomFactor = value;
  }

  set imageSizeFactor(value: number) {
    this.$imageSizeFactor = value;
  }

  set maximumCanvasSize(value: Size) {
    this.$maximumCanvasSize = value;
  }

  set drawNodeCallback(value: any) {
    this.$drawNodeCallback = value;
  }

  set drawEdgeCallback(value: any) {
    this.$drawEdgeCallback = value;
  }

  set drawNodeLabelCallback(value: any) {
    this.$drawNodeLabelCallback = value;
  }

  set drawEdgeLabelCallback(value: any) {
    this.$drawEdgeLabelCallback = value;
  }

  set dirty(value: boolean) {
    this.$dirty = value;
  }

  updateEffectiveStyles(): void {
    this.fastNodeDescriptor.updateEffectiveStyle();
    this.fastLabelDescriptor.updateEffectiveStyle();
    this.fastPortDescriptor.updateEffectiveStyle();
  }

  onGraphComponentZoomChanged(): void {
    this.updateEffectiveStyles();
  }

  initFastGraphModelManager(): void {
    this.$graphOptimizationMode = OptimizationMode.DEFAULT;
  }
}

export const OptimizationMode = {

  DEFAULT: 0,

  STATIC: 1,

  LEVEL_OF_DETAIL: 2,

  SVG_IMAGE: 3,

  DYNAMIC_CANVAS_WITH_DRAW_CALLBACK: 4,

  DYNAMIC_CANVAS_WITH_ITEM_STYLES: 5,

  STATIC_CANVAS: 6,

  WEBGL: 7
};

class AutoSwitchDescriptor extends BaseClass(ICanvasObjectDescriptor) {
  backingInstance: ICanvasObjectDescriptor;
  manager: FastGraphModelManager;
  shouldUseImage: boolean;
  effectiveStyle: any;
  $intermediateStyle: any;
  $overviewStyle: any;

  constructor(backingInstance: ICanvasObjectDescriptor, manager: FastGraphModelManager) {
    super();
    this.backingInstance = backingInstance;
    this.manager = manager;
    // whether we are using an image for the rendering and should thus not normally be considered dirty
    this.shouldUseImage = false;
    this.effectiveStyle = null;
  }

  get intermediateStyle() {
    return this.$intermediateStyle;
  }

  get overviewStyle() {
    return this.$overviewStyle;
  }

  set intermediateStyle(value: any) {
    this.$intermediateStyle = value;
    this.updateEffectiveStyle();
  }

  set overviewStyle(value: any) {
    this.$overviewStyle = value;
    this.updateEffectiveStyle();
  }

  updateEffectiveStyle() {
    const manager = this.manager;
    if (manager.graphOptimizationMode === OptimizationMode.LEVEL_OF_DETAIL) {
      // delegate rendering to overview styles
      if (manager.graphComponent.zoom < manager.overviewZoomThreshold) {
        this.effectiveStyle = this.$overviewStyle;
      } else if (manager.graphComponent.zoom < manager.intermediateZoomThreshold) {
        this.effectiveStyle = this.$intermediateStyle ? this.$intermediateStyle : null;
      } else {
        this.effectiveStyle = null;
      }
    } else {
      this.effectiveStyle = null;
    }
  }

  isDirty(context: ICanvasContext, canvasObject: ICanvasObject) {
    return !this.shouldUseImage || this.backingInstance.isDirty(context, canvasObject);
  }

  getVisualCreator(item: any) {
    const style = this.effectiveStyle || item.style;
    return style.renderer.getVisualCreator(item, style);
  }

  getBoundsProvider(item: any) {
    const style = this.effectiveStyle || item.style;
    return style.renderer.getBoundsProvider(item, style);
  }

  getVisibilityTestable(item: any) {
    const style = this.effectiveStyle || item.style;
    return style.renderer.getVisibilityTestable(item, style);
  }

  getHitTestable(item: any) {
    const style = this.effectiveStyle || item.style;
    return style.renderer.getHitTestable(item, style);
  }
}
