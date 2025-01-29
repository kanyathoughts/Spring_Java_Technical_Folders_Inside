import {
    IModelItem,
    INode,
    IEdge,
    ILabel,
    HighlightIndicatorManager,
    GraphComponent,
    Color,
    Stroke,
} from 'yfiles';
import { graphSelectionIndicators } from './graph.util';
import { GraphGlobalStyles } from './graph-global-styles';

/**
 * Highlights the hovered/selected item.
 * @param item item that is hovered/selected.
 * @param manager to highlight the item.
 * @param graphComponent complete graph component.
 */
export const addHighlights = (item: IModelItem, manager: HighlightIndicatorManager<IModelItem>, graphComponent: GraphComponent): void => {
    // then see where we are hovering over, now
    const newItem = item;
    if (newItem !== null) {
        if (INode.isInstance(newItem)) {
            manager.addHighlight(newItem);
            // and if it's a node, we highlight all adjacent edges, too
            graphComponent.graph.edgesAt(newItem).forEach((edge: IEdge) => {
                highlightEdge(edge, manager);
            });
            // we are checking the for edge
        } else if (IEdge.isInstance(newItem)) {
            manager.addHighlight(newItem);
            // if it's an edge - we highlight the adjacent nodes
            highlightEdge(newItem, manager);
        } else if (ILabel.isInstance(newItem) && newItem.owner instanceof IEdge) {
            highlightEdge(newItem.owner, manager);
        }
    }
};

/**
 * method to highlight edge and its edge label
 * @param  edge edge that needs to be highlighted
 * @param  manager HighlightIndicatorManager for edge
 */
export const highlightEdge = (edge: IEdge, manager: HighlightIndicatorManager<IModelItem>): void => {
    manager.addHighlight(edge);
    manager.addHighlight(edge.sourceNode);
    manager.addHighlight(edge.targetNode);
    edge.labels.forEach(label => {
        manager.addHighlight(label);
    });
};

/**
 * method to initialize the highlight styles for the node, edge and label
 * @param  type  event type click or hover
 * @param  graphComponent complete graph
 * @param  item particular node
 * @param  nodeHighlightStyles node style decided based on its size
 */
export const initializeHighlightStyles = (type: string, graphComponent: GraphComponent,
    item: INode = null, nodeHighlightStyles?: string, isSharedResource: boolean = false): void => {
    const colorHighlights = GraphGlobalStyles.initializeColorHighlights(type, graphComponent);
    GraphGlobalStyles.edgeHighlightStyles(colorHighlights.blueStroke, colorHighlights.decorator);
    GraphGlobalStyles.nodeHighlightStyles(colorHighlights.blueStroke, colorHighlights.decorator, item, nodeHighlightStyles);
    GraphGlobalStyles.labelHighlightStyles(colorHighlights.blueStroke, colorHighlights.decorator, isSharedResource);
    if (item) {
        graphComponent.graph.neighbors(item).forEach((edge: IEdge | INode | ILabel) => {
          if (!edge.tag?.IsSelected) {
            GraphGlobalStyles.nodeHighlightStyles(colorHighlights.blueStroke, colorHighlights.decorator, edge as INode, nodeHighlightStyles);
          }
        });
        if( type === 'click') { /// changes are made to specific node click
            const color = Color.DODGER_BLUE;
            const blueStroke = new Stroke(color.r, color.g, color.b, 220, 3);
           if (IEdge.isInstance(item) || ILabel.isInstance(item)) {
            GraphGlobalStyles.edgeHighlightStyles(blueStroke, colorHighlights.decorator);
            GraphGlobalStyles.labelHighlightStyles(blueStroke, colorHighlights.decorator, isSharedResource);
           } else {
            GraphGlobalStyles.nodeHighlightStyles(blueStroke, colorHighlights.decorator, item, nodeHighlightStyles);
           }
        } else {
            GraphGlobalStyles.nodeHighlightStyles(colorHighlights.blueStroke, colorHighlights.decorator, item, nodeHighlightStyles);
        }
    }
};

/**
 * method to set graph selection indicator
 * @param  graphComponent complete graph component
 */
export const setGraphSelectionIndicators = (graphComponent: GraphComponent): void => {
    graphSelectionIndicators(graphComponent);
};
