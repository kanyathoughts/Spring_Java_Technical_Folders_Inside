import { Injectable } from '@angular/core';
import { ModuleListingState, ModuleTableItem } from './module-listing.interface';

@Injectable({
  providedIn: 'root',
})

export class ModuleListingService {

  moduleState: ModuleListingState = {
    start: [],
    end: []
  };

  /**
   * Method to set the table state of module list for start/end module.
   * @param moduleTable state of module list table
   * @param moduleType start/end module type.
   */
  setModuleTableState(moduleTable: ModuleTableItem[], moduleType: string): void {
    this.moduleState[moduleType] = moduleTable;
  }

  /**
   * Method to get current state of moduleList table.
   * @param moduleType whether start or end type module.
   * @returns current state of start module list table.
   */
  getModuleTableState(moduleType: string): ModuleTableItem[] {
    return this.moduleState[moduleType];
  }
}
