import { NgModule } from '@angular/core';
import { NzIconModule, NzIconService } from 'ng-zorro-antd/icon';
import { NzLayoutModule } from 'ng-zorro-antd/layout';
import { NzGridModule } from 'ng-zorro-antd/grid';
import { NzMenuModule } from 'ng-zorro-antd/menu';
import { NzListModule } from 'ng-zorro-antd/list';
import { NzCardModule } from 'ng-zorro-antd/card';
import { NzPageHeaderModule } from 'ng-zorro-antd/page-header';
import { NzToolTipModule } from 'ng-zorro-antd/tooltip';
import { NzButtonModule } from 'ng-zorro-antd/button';
import { NzModalModule } from 'ng-zorro-antd/modal';
import { NzInputModule } from 'ng-zorro-antd/input';
import { NzMessageModule } from 'ng-zorro-antd/message';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { NzUploadModule } from 'ng-zorro-antd/upload';
import { NzFormModule } from 'ng-zorro-antd/form';
import { NzCheckboxModule } from 'ng-zorro-antd/checkbox';
import { NzSelectModule } from 'ng-zorro-antd/select';
import { NzPopconfirmModule } from 'ng-zorro-antd/popconfirm';
import { NzTableModule } from 'ng-zorro-antd/table';
import { NzSliderModule } from 'ng-zorro-antd/slider';
import { NzBreadCrumbModule } from 'ng-zorro-antd/breadcrumb';
import { NzAvatarModule } from 'ng-zorro-antd/avatar';
import { NzTabsModule } from 'ng-zorro-antd/tabs';
import { NzTypographyModule } from 'ng-zorro-antd/typography';
import { NzCollapseModule } from 'ng-zorro-antd/collapse';
import { NzDrawerModule } from 'ng-zorro-antd/drawer';
import { NzSpaceModule } from 'ng-zorro-antd/space';
import { NzRadioModule } from 'ng-zorro-antd/radio';
import { NzDividerModule } from 'ng-zorro-antd/divider';
import { NzInputNumberModule } from 'ng-zorro-antd/input-number';
import { NzBadgeModule } from 'ng-zorro-antd/badge';
import { NzDescriptionsModule } from 'ng-zorro-antd/descriptions';
import { NzEmptyModule } from 'ng-zorro-antd/empty';
import { NzSpinModule } from 'ng-zorro-antd/spin';
import { NzAlertModule } from 'ng-zorro-antd/alert';
import { NzStatisticModule } from 'ng-zorro-antd/statistic';
import { NzSwitchModule } from 'ng-zorro-antd/switch';
import { NzTreeSelectModule } from 'ng-zorro-antd/tree-select';
import { NzTagModule } from 'ng-zorro-antd/tag';
import { NzTreeViewModule } from 'ng-zorro-antd/tree-view';
import { NzNoAnimationModule } from 'ng-zorro-antd/core/no-animation';
import { NzPopoverModule } from 'ng-zorro-antd/popover';
import { NzHighlightModule } from 'ng-zorro-antd/core/highlight';
import { NzTreeModule } from 'ng-zorro-antd/tree';
import { NzSkeletonModule } from 'ng-zorro-antd/skeleton';
import { NzResultModule } from 'ng-zorro-antd/result';
import { NzProgressModule } from 'ng-zorro-antd/progress';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { NzNotificationModule } from 'ng-zorro-antd/notification';
import { NzPaginationModule } from 'ng-zorro-antd/pagination';
import { NzStepsModule } from 'ng-zorro-antd/steps';
import { NzCascaderModule } from 'ng-zorro-antd/cascader';
import { NZ_CONFIG, NzConfig } from 'ng-zorro-antd/core/config';

const ngZorroConfig: NzConfig = {
  card: { nzSize: 'small' }
};

@NgModule({
  exports: [
    NzLayoutModule,
    NzGridModule,
    NzMenuModule,
    NzIconModule,
    NzListModule,
    NzCardModule,
    NzPageHeaderModule,
    NzToolTipModule,
    NzButtonModule,
    NzModalModule,
    NzInputModule,
    NzMessageModule,
    NzDropDownModule,
    NzUploadModule,
    NzFormModule,
    NzCheckboxModule,
    NzPopconfirmModule,
    NzSelectModule,
    NzTableModule,
    NzSliderModule,
    NzBreadCrumbModule,
    NzRadioModule,
    NzDividerModule,
    NzSpaceModule,
    NzInputNumberModule,
    NzAvatarModule,
    NzTabsModule,
    NzDescriptionsModule,
    NzEmptyModule,
    NzBadgeModule,
    NzTypographyModule,
    NzCollapseModule,
    NzDrawerModule,
    NzSpinModule,
    NzAlertModule,
    NzStatisticModule,
    NzSwitchModule,
    NzTreeSelectModule,
    NzPopoverModule,
    NzTreeModule,
    NzTagModule,
    NzTreeViewModule,
    NzNoAnimationModule,
    NzHighlightModule,
    NzSkeletonModule,
    NzResultModule,
    NzProgressModule,
    NzNotificationModule,
    NzPaginationModule,
    NzCascaderModule,
    NzStepsModule
  ],
  providers: [
    { provide: NZ_CONFIG, useValue: ngZorroConfig }
  ],
})
export class AntDesignImportsModule {
  constructor(private iconService: NzIconService) {
    const basePath = getBasePath();
    this.iconService.changeAssetsSource(basePath);
  }
}
