import { Injectable } from '@angular/core';
import { ModalWindowComponent } from '@app/shared/components/modal-window/modal-window.component';
import { SharedModuleDetailsComponent } from '@app/shared/components/shared-module-details/shared-module-details.component';
import { DependencyGraphPanelState, EdgeLabelPanelState, ModuleDetailPanelState }
  from '@app/shared/components/shared-module-details/dependency-graph-panel-state.interface';
import { FormTypes, ModalWindowParams } from '@app/shared/models/modal-window-params.model';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { ModulePojo } from '@innowake/mining-api-angular-client';

@Injectable({
    providedIn: 'root',
})

export class ModuleDetailsDPSidePanelService {
    sharedModuteDetails: SharedModuleDetailsComponent;
    sidePanelState: DependencyGraphPanelState = {
      nodePanelState: {
        characteristics: false,
        metrics: false,
        description: false,
        taxonomy: false,
        annotations: false,
        customProperty: false
      },
      edgePanelState: {
        module: false,
        relationship: false,
        reference: false
      }
    };
    descriptionModal: NzModalRef;

    constructor(private translateService: TranslateService,
        private modalService: NzModalService) {}


    /**
     * Method to set the Panel state (open or closed)
     * @param maintainSidePanelState contains info on state of each section of panel
     * @param type whether it's module panel or edge panel
     */
    setPanelState(maintainSidePanelState: ModuleDetailPanelState | EdgeLabelPanelState, type: string): void {
        this.sidePanelState[type] = maintainSidePanelState;
    }

    /**
     * Method to get the Panel state
     * @param type whether it's module panel or edge panel
     */
    getPanelState(type: string): ModuleDetailPanelState | EdgeLabelPanelState {
        return this.sidePanelState[type];
    }

    /**
     * Method to edit module description from modal.
     * @param moduleDetails details of editing module.
     * @param saveDesc callback method to save the updated description.
     */
    editModuleDescription(moduleDetails: ModulePojo, saveDesc: () => this): void {
        const editModuleParam: ModalWindowParams[] = [
            {
              id: moduleDetails.id,
              key: this.translateService.instant('description'),
              value: moduleDetails.description,
              type: FormTypes.TEXT,
              label: this.translateService.instant('description'),
              deleteFeature: false
            }];
          const config = {
            data: editModuleParam,
            header: this.translateService.instant('editModuleDescLabel'),
            saveCallBack: saveDesc.bind(this)
          };
          this.descriptionModal = this.modalService.create<ModalWindowComponent>({
            nzTitle: this.translateService.instant('editModuleDescLabel'),
            nzClosable: true,
            nzMaskClosable: false,
            nzClassName: 'module-details__edit-window',
            nzWrapClassName: 'vertical-center-modal',
            nzKeyboard: true,
            nzWidth: '70vw',
            nzContent: ModalWindowComponent
          });
          const instance = this.descriptionModal.getContentComponent();
          instance.customDynamicDialogConfig = config;
          instance.projectId = moduleDetails.projectId;
          instance.moduleId = moduleDetails.id;
          instance.placeHolderText = this.translateService.instant('editModuleDescPlaceholder');
        }

}
