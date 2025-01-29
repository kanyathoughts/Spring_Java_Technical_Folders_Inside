import { Component, OnInit } from '@angular/core';
import { MiningTableRow } from '@app/shared/components/mining-table/mining-table-config.interface';
import { JobManagerService } from '@app/core/services/job-manager/job-manager.service';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { JobControllerService, MiningDataPointDefinitionWithPath, PropagationData } from '@innowake/mining-api-angular-client';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { CustomizableTableQueryParameter } from '@app/core/services/user-customizable-table/customizable-table-query-parameters.interface';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { take } from 'rxjs';

@Component({
  selector: 'app-taxonomy-propagation',
  templateUrl: './taxonomy-propagation.component.html',
})
export class TaxonomyPropagationComponent implements OnInit{
  affectedModules: Record<any, any>;
  usage: Usages;
  graphQlType: string;
  projectId: number;
  pageType: string;
  moduleIdForPreFiltering: number[];
  internalDataPoints: MiningDataPointDefinitionWithPath[] = [];
  allowSelectDeselectRows: boolean;
  defaultDataPointsToShow = ['Module Name', 'Technology', 'Type'];
  selectedTaxonomyNames: string;
  propagationDataToBeSubmitted: PropagationData[] = [];
  onlyStartModulesIdentified = false;
  startModules: number[] = [];
  preSelectedModules: number[] = [];
  parentTableParams: CustomizableTableQueryParameter;
  parentSelectedColumns: string[] = [];
  parentDefaultColumns: string[] = [];

  constructor(private modal: NzModalRef,
    private jobController: JobControllerService,
    private parametersService: CustomizableTableParametersService,
    private userCustomizableTable: CustomizableTableColumnService,
    private readonly jobManagerService: JobManagerService,
  ) {
    this.parentTableParams = {...this.parametersService.currentTableParameters};
    this.parametersService.resetCurrentTableParameters();

    // Taking the data points selected by the user in the parent component table, thus using take(1) to get the data points only once.
    this.userCustomizableTable.getSelectedDataPoints().pipe(take(1)).subscribe((columns: MiningDataPointDefinitionWithPath[]) => {
      columns.forEach((column: MiningDataPointDefinitionWithPath) => {
        this.parentSelectedColumns.push(column.id);
        this.parentDefaultColumns.push(column.displayName);
      });
    });
  }

  ngOnInit(): void {
    const identifiedModules = this.affectedModules?.object[1] as Array<Record<string, any>>;
    this.onlyStartModulesIdentified = this.checkIfOnlyStartModulesIdentified(identifiedModules, this.startModules);

    this.startModules.forEach((moduleId) => {
      if (this.moduleIdForPreFiltering.includes(moduleId)) {
        this.preSelectedModules.push(moduleId);
      }
    });

    this.modal.afterClose.subscribe(() => {
      this.handleParentComponentTable();
    });
  }

  /**
   * method to handle the selected affected modules
   * @param  event event emitted by customizable core table with selected row details
   */
  handleSelectedRowsOfCoreTable(event: MiningTableRow): void {
    this.propagationDataToBeSubmitted = this.affectedModules?.object[1].filter((module: any) => event.some((ids: any) =>
      ids === module.moduleId));
  }

  /**
   * method to submit taxonomy propagation
   */
  confirmTaxonomyPropagation(): void {
    this.jobController.submitTaxonomyPropagation(this.projectId, this.propagationDataToBeSubmitted).subscribe((jobProgressId: any) => {
      this.modal.close();
      this.jobManagerService.register({ jobId: jobProgressId as string, foreground: true, cancellable: true });
    });
  }

  /**
   * method to handle cancel
   */
  handleCancel(): void {
    this.modal.close();
  }

  private checkIfOnlyStartModulesIdentified(identifiedModules: Array<Record<string, any>>, startModules: number[]): boolean {
    if (identifiedModules && identifiedModules.length === startModules.length)  {
      for (const module of identifiedModules) {
        if ( ! startModules.includes(module.moduleId as number)) {
          return false;
        }
      }
      return true;
    }
    return false;
  }

  /**
   * This method is used to handle the parent component table, So that any changes in the child component table
   * will not affect the parent component table.
   * Its kind of hacky way to maintain the state of the parent component table, Can be improved in future.
   */
  private handleParentComponentTable(): void {
    const params = this.parentTableParams;
    this.parametersService.setParameters(params.filter, params.page,params.sort);
    this.userCustomizableTable.defaultColumnsToShow = this.parentDefaultColumns;
    this.userCustomizableTable.setColumnIdsSelection(this.parentSelectedColumns);
  }
}
