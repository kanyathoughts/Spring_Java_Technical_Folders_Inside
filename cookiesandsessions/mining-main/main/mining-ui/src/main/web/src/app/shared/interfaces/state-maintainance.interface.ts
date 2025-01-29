export interface MiningServiceDataInterface {
    'clearState': string;
    'storageType': string;
}
export enum ClearStateOn {
    ProjectChange = 'ProjectChange'
}
export enum StateStorage {
    Session = 'session',
    Local = 'local'
}

/**
 * Enum that constains the list of state key's
 */
 export enum StateKey {
    BrowseModuleTableStateKey = 'MiningBrowseModuleTableState',
    TaxonomyReportingTableStateKey = 'MiningTaxonomyReportingTableState',
    TaxonomyReportingTableMultiSelectStateKey = 'MiningTaxonomyReportingTableMultiSelectState',
    DeepLinkPortKey = 'DeepLinkPort',
    MiningTableRowSelectionKey = 'MiningTableRowSelection'
  }
