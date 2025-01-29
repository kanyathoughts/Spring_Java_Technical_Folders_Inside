/**
 * Interface for row in module listing table
 */
 export interface ModuleTableItem {
   id: number;
   module: string;
   path: string;
   technology: string;
   type: string;
   validationResult: IconDetails;
   rowClassName?: string
}

/**
 * Interface for repeated  and invalid modules
 */
 export interface RepeatedAndInvalidModules {
    invalidModules: string[];
    duplicateModules: string[]
 }

/**
 * Interface for data from response
 */
 export interface Modules {
   content: any[];
   size: number;
   totalElements: number;
 }

/**
 * Interface for Icon details
 */
export interface IconDetails {
    iconType: string;
    iconTheme: string;
}

/**
 * Interface for data send from modal to parent component
 */
export interface ModuleDetails {
  modulesId: string[];
  modulesName: string[];
  selectedOption: string;
  repeatedModuleIds: number[][]
}

/**
 * Interface for maintaining modal state.
 */
export interface ModuleListingState {
  start: ModuleTableItem[],
  end: ModuleTableItem[]
}
