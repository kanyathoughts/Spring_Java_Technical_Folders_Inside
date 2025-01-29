import { Component } from '@angular/core';
import {
  StateMaintainenceService,
  MiningPageState
} from '../../../core/services/state-maintenance/state-maintainence.service';
import { DeepLinkService } from '../../../core/services/deep-link.service';
import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { BaseFormModalComponent } from '../base-form-modal/base-form-modal.component';
import { ClearStateOn, StateKey, StateStorage } from '@app/shared/interfaces/state-maintainance.interface';

export const MIN_PORT = 1;
export const MAX_PORT = 99999;

@Component({
  selector: 'app-settings-component',
  templateUrl: './settings.component.html'
})
export class SettingsComponent extends BaseFormModalComponent {
  featureIsActive = false;
  currentPort: number;
  newPort: string;
  isPortChanged = false;
  isPortInvalid = false;
  invalidPortMessage: string;

  constructor(
    public modal: NzModalRef,
    public messageService: NzMessageService,
    private stateMaintainenceService: StateMaintainenceService,
    private translateService: TranslateService,
    deepLinkService: DeepLinkService
  ) {
    super(modal, messageService);
    this.currentPort = deepLinkService.getPort();
    this.newPort = '' + deepLinkService.getPort();
    this.invalidPortMessage = this.translateService.instant('deepLink.invalidPortNumber', {min: MIN_PORT, max: MAX_PORT});
  }

  /**
   * Set isPortChanged and isPortInvalid flag when user changes port.
   */
  portChange(): void {
    const newPortNbr = Number(this.newPort);
    this.isPortChanged = this.currentPort !== newPortNbr;
    this.isPortInvalid = newPortNbr < MIN_PORT || newPortNbr > MAX_PORT || isNaN(newPortNbr);
  }

  /**
   * Calls the method to set the new deep-link port and displays the success message.
   */
  onSubmit(): void {
    this.setDeepLinksPort(Number(this.newPort));
    this.closeModal('success', this.translateService.instant('deepLink.successSave') as string, []);
  }

  /**
   * Sets the DeepLink port.
   */
  setDeepLinksPort(port: number): void {
    this.stateMaintainenceService.setState(
      new MiningPageState(
        StateStorage.Session,
        StateKey.DeepLinkPortKey,
        ClearStateOn.ProjectChange
      )
    );
    sessionStorage.setItem(StateKey.DeepLinkPortKey, JSON.stringify(port));
  }
}
