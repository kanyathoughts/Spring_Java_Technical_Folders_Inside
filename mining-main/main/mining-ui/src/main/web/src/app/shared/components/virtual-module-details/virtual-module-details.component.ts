import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';
import { NzTreeFlatDataSource, NzTreeFlattener } from 'ng-zorro-antd/tree-view';
import { FlatTreeControl } from '@angular/cdk/tree';
import { RouteBuilder } from '@app/core/utils/route-builder.utils';
import { TranslateService } from '@ngx-translate/core';
import { FlatNode, TreeNode } from '@app/shared/interfaces/tree-view.interface';
import { AggregationResultModuleFieldName, ModulePojo } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'virtual-module-details',
  templateUrl: './virtual-module-details.component.html'
})

export class VirtualModuleDetailsComponent implements OnChanges {
  @Input() selectedModule: ModulePojo;
  @Input() virtualModuleDetails: AggregationResultModuleFieldName[];

  title: string;
  description: string;
  treeControl = new FlatTreeControl<FlatNode>(
    node => node.level,
    node => node.expandable
  );
  treeFlattener = new NzTreeFlattener(
    this.transformer,
    node => node.level,
    node => node.expandable,
    node => node.children
  );
  dataSource = new NzTreeFlatDataSource(this.treeControl, this.treeFlattener);

  constructor(
    private translateService: TranslateService  ) { }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['virtualModuleDetails']) {
      if (this.selectedModule.parent) {
        this.title = this.translateService.instant('virtualModule.subModuleTitle');
        this.description = this.translateService.instant('virtualModule.subModuleDesc');
      } else {
        this.title = this.translateService.instant('virtualModule.cardTitle');
        this.description = this.translateService.instant('virtualModule.cardDescription');
      }
      this.setVirtualModuleDetails();
    }
  }

  private transformer(node: TreeNode, level: number) {
    return { expandable: !!node.children && node.children.length > 0, name: node.name, level, key: node.key, hyperlink: node.hyperlink};
  }

  private setVirtualModuleDetails(): void {
    const nodes: TreeNode[] = [];
    this.virtualModuleDetails.forEach(element => {
      let Id: number = element.group.CONTAINING_MODULE_ID as any;
      const TREE_DATA: TreeNode = {
        name: element.group.CONTAINING_MODULE_NAME as any,
        key: element.group.CONTAINING_MODULE_ID as any,
        hyperlink: this.navigateToModuleDetails(Id),
        children: [
        ]
      };
      const fieldNames = element.fields.NAME as string[];
      for (let i = 0; i < fieldNames.length; i++) {
        Id = element.fields.ID[i];
        TREE_DATA.children.push({
          name: fieldNames[i],
          key: element.fields.ID[i],
          hyperlink: this.navigateToModuleDetails(Id),
        });
      }
      nodes.push(TREE_DATA);
      this.dataSource.setData(nodes);
      this.treeControl.expandAll();
    });
  }

  private navigateToModuleDetails(moduleId: number): string {
    return RouteBuilder.buildModuleRoute(this.selectedModule.projectId, moduleId, 'details/overview');
  }
}
