import { Component, Input, OnInit } from '@angular/core'; import { LanguageProviderService } from '@app/core/services/monaco-editor/language-provider.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { MiningTableRow } from '@app/shared/components/mining-table/mining-table-config.interface';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { Subject } from 'rxjs';

@Component({
  selector: 'mn-module-errors',
  templateUrl: './module-errors.component.html',
  providers: [LanguageProviderService, CustomizableTableParametersService, CustomizableTableColumnService]
})
export class ModuleErrorsComponent implements OnInit {
  @Input() projectId: number;
  @Input() moduleId: number;
  @Input() code: string;
  pageType = TypeName.PAGEERRORMARKER;
  graphQlType = 'errorMarkers';
  usage = Usages.MODULERRORTABLE;
  rowSelection = new Subject<{code: string, offset: number, length: number}>();
  additionalGraphQLPreFilter: { [key: string]: any } = {};
  tableConfig: any = { paginator: true };
  isErrorMarker = false;

  ngOnInit(): void {
    this.additionalGraphQLPreFilter = {
      content_module: {
        eq: this.moduleId
      }
    };
  }


  /**
   * Method to capture data from selected row and show side panel code view
   * @param rowData selected row data
   */
  openSidePanelCodeView(rowData: MiningTableRow): void {
    this.isErrorMarker = true;
    setTimeout(() => {
      this.rowSelection.next({
        code: this.code,
        offset: rowData.location.rootRelativeOffset,
        length: rowData.errorText.length
      });
    }, 100);
  }


  /**
   * method to close side panel code view
   * @param event:boolean
   */
  showCfgMetaDataDrawer(event: boolean): void {
    this.isErrorMarker = event;
  }
}
