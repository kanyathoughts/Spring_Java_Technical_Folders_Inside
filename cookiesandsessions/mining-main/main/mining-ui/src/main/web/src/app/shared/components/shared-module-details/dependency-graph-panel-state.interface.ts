/**
 * Interface for maintaing state of each accordion in module detail panel.
 */
export interface ModuleDetailPanelState {
    characteristics: boolean;
    metrics: boolean;
    description: boolean;
    taxonomy: boolean;
    annotations: boolean;
    customProperty: boolean;
}

export interface DependencyGraphPanelState {
    nodePanelState: ModuleDetailPanelState;
    edgePanelState: EdgeLabelPanelState;
}

export interface EdgeLabelPanelState {
    module: boolean;
    relationship: boolean;
    reference: boolean;
}
