import { GraphComponent } from 'yfiles';

/**
 * Disables the visual decorations of selected and focus elements.
 * @param graphComponent graphComponent instance.
 */
export const graphSelectionIndicators = (graphComponent: GraphComponent): void => {
    graphComponent.selectionIndicatorManager.enabled = false;
    graphComponent.focusIndicatorManager.enabled = false;
    graphComponent.highlightIndicatorManager.enabled = true;
};
