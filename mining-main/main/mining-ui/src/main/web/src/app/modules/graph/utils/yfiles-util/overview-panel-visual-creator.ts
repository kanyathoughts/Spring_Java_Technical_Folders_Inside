/* eslint-disable */
import { GraphOverviewCanvasVisualCreator, IRenderContext, Visual, INode, IEdge } from 'yfiles';

export class OverviewPanelVisualCreator extends GraphOverviewCanvasVisualCreator {

    /**
     * Method to create the visual in the overview panel.
     * @param context - The render context
     */
    createVisual(context: IRenderContext): Visual {
        // Method has to be implemented since it is a part of the abstract class IVisualCreator
        return null;
    }

    /**
     * Method to update the visual in the overview panel.
     * @param context - The render context
     * @param oldVisual - Visual to be updated
     */
    updateVisual(context: IRenderContext, oldVisual: Visual): Visual {
        // Method has to be implemented since it is a part of the abstract class IVisualCreator
        return null;
    }

    /**
     * Paints the given node.
     * @param renderContext The render context.
     * @param ctx The HTML canvas rendering context.
     * @param node The node to paint.
     */
    paintNode(renderContext: IRenderContext, ctx: CanvasRenderingContext2D, node: INode) {
        ctx.fillStyle = '#2E3A59';
        const layout = node.layout;
        ctx.fillRect(layout.x, layout.y, layout.width, layout.height);
        ctx.strokeStyle = '#ffffff';
    }

    /**
     * Paints the given edge.
     * @param renderContext The render context.
     * @param ctx The HTML canvas rendering context.
     * @param edge The edge to paint.
     */
    paintEdge(renderContext: IRenderContext, ctx: CanvasRenderingContext2D, edge: IEdge) {
        ctx.beginPath();
        ctx.moveTo(edge.sourcePort.location.x, edge.sourcePort.location.y);
        edge.bends.forEach(bend => {
            ctx.lineTo(bend.location.x, bend.location.y);
        });
        ctx.lineTo(edge.targetPort.location.x, edge.targetPort.location.y);
        ctx.strokeStyle = '#2E3A59';
        ctx.lineWidth = 5;
        ctx.stroke();
    }
}
