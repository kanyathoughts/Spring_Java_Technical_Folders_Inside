import { ChangeDetectionStrategy, ChangeDetectorRef, Component, Input, OnInit } from '@angular/core';
import { NzFormatEmitEvent, NzTreeNodeOptions } from 'ng-zorro-antd/tree';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';
import { MiningDataPointDefinitionWithPath } from '@innowake/mining-api-angular-client';
import { Usages } from '@app/shared/interfaces/datapoints-labels.interface';

@Component({
  changeDetection: ChangeDetectionStrategy.OnPush,
  selector: 'mn-setting-popover',
  templateUrl: './mining-setting-popover.component.html',
})

export class MiningSettingPopOverComponent implements OnInit {

  @Input() searchValue: string;

  miningTableData: NzTreeNodeOptions[] = [];
  columnData: NzTreeNodeOptions[] = [];
  defaultCheckedKeys: string[] = [];
  expandAll = true;
  totalElementCount = 0;
  constructor(
    private customizableColumnService: CustomizableTableColumnService,
    private changeDetector: ChangeDetectorRef
  ) { }

  ngOnInit(): void {
    this.columnData = this.customizableColumnService.getColumnData();
    this.columnData.forEach((node: NzTreeNodeOptions) => {
      node.selectable = false;
      node.children.forEach((childNode: NzTreeNodeOptions) => {
        this.totalElementCount = this.totalElementCount + 1;
        childNode.selectable = false;
        /// setting the check box to checked reachability column
        if(this.customizableColumnService.usage === Usages.REACHABILITYTABLE || this.customizableColumnService.usage === Usages.MODULERRORTABLE) {
          const getCurrentColumn: MiningDataPointDefinitionWithPath[] = this.customizableColumnService.getCheckedDataPoint();
          const isIntermediateSelected = getCurrentColumn.filter((currentItem: MiningDataPointDefinitionWithPath) => currentItem.id === childNode.id);
          childNode.checked = !! isIntermediateSelected.length;
        }
        if (childNode.checked === true) {
          this.miningTableData.push(childNode);
          this.defaultCheckedKeys.push(childNode.key);
        }
      });
    });
  }

  /**
   * Method to update the columns in mining table.
   * @param event when user select/deselects column data.
   */
  updateMiningTable(event: NzFormatEmitEvent): void {
    if (event.node.origin) {
      if (event.node.origin.children) {
        event.node.origin.children.forEach((childNode: NzTreeNodeOptions) => {
          if (this.miningTableData.indexOf(childNode) < 0) {
            this.miningTableData.push(childNode);
          }
        });
      } else {
        this.miningTableData.push(event.node.origin);
      }
    }
    this.miningTableData = this.miningTableData.filter(x => x.checked === true);
    this.miningTableData.sort((a,b) => Number(a.key) - Number(b.key));
    void this.customizableColumnService.updateSelectedColumns(this.miningTableData, false);
  }

  /**
   * Method to expand the tree nodes when searchValue is empty.
   * @param searchValue the search string.
   */
  onSearchValueChange(searchValue: string): void {
    if (searchValue === '') {
      this.expandAll = true;
    } else {
      this.expandAll = false;
    }
    this.changeDetector.detectChanges();
  }

  /**
   * Method will called on click of every node of the tree
   * @param  event details related to particular node of the tree
   */
   toggleNode(event: NzFormatEmitEvent): void {
    event.node.setSyncChecked(!event.node.origin.checked);
    this.updateMiningTable(event);
  }
}
