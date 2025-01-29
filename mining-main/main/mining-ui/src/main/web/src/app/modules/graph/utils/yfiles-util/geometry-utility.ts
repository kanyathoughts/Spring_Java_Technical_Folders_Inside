import { INode, Insets, Rect } from 'yfiles';

export class GeometryUtility {

    /**
     * Finds a rectangle which contains two nodes.
     * @param nodes points that the rectangle needs to contain
     * @param horizontalInset the left/right margin of the rectangle
     * @param verticalInset the top/bottom margin of the rectangle
     * @returns rectangle containing the points
     */
    public static findSurroundingRect(nodes: [INode, INode], horizontalInset: number, verticalInset: number): Rect {
        let surroundingRect = new Rect(nodes[0].layout.toPoint(), nodes[1].layout.toPoint());
        let right = horizontalInset;
        let bottom = verticalInset;

        /* Enlarge rectangle by node width/height of right-/bottom-most node (Because INode.layout.toPoint returns the topleft corner of the node) */
        if (nodes[1].layout.x > nodes[0].layout.x) {
            right += nodes[1].layout.width;
        } else {
            right += nodes[0].layout.width;
        }
        if (nodes[1].layout.y > nodes[0].layout.y) {
            bottom += nodes[1].layout.height;
        } else {
            bottom += nodes[0].layout.height;
        }

        surroundingRect = surroundingRect.getEnlarged(new Insets(horizontalInset, verticalInset, right, bottom));
        return surroundingRect;
    }

    /**
     * Finds the node in an array of nodes which has the lowest distance to a specified node.
     * @param startingNode The node for which the closest node is supposed to be found
     * @param nodes Array of potential closest nodes
     * @returns Node in nodes which is closest to startingNode
     */
    public static findClosestNode(startingNode: INode, nodes: INode[]): INode {
        let currentClosestNode: INode;
        let currentClosestDistance: number;
        const firstNodePoint = startingNode.layout.toPoint();
        nodes.forEach((node: INode) => {
            const point = node.layout.toPoint();
            const distance = point.distanceTo(firstNodePoint);
            if ( ! currentClosestNode || distance < currentClosestDistance) {
                currentClosestNode = node;
                currentClosestDistance = distance;
            }
        });
        return currentClosestNode;
    }
}
