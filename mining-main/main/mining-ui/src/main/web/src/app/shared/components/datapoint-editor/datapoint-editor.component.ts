import { HttpClient, HttpErrorResponse } from '@angular/common/http';
import { Component, Input, OnInit, Optional } from '@angular/core';
import { UntypedFormControl, UntypedFormGroup } from '@angular/forms';
import { Logger } from '@app/core';
import { getBasePath } from '@app/core/utils/base-path.utils';
import { getValueAtPath, replaceTemplateString } from '@app/core/utils/graphql.util';
import { FormResult } from '@app/shared/interfaces/annotation-editor.interface';
import { DataPointControllerService, EntityId, MiningDataPointDefinitionWithPath, UsagesModel } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzDrawerRef } from 'ng-zorro-antd/drawer';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { forkJoin, Observable } from 'rxjs';

const log = new Logger('DataPointEditorComponent');

interface FormField {
  key: string;
  label: string;
  endpoint: string;
  fieldName: string;
  editAs: UsagesModel.EditModeAttributesEnum & ('text' | 'textArea');
  dataPoint: MiningDataPointDefinitionWithPath
}

@Component({
  selector: 'app-datapoint-editor',
  templateUrl: './datapoint-editor.component.html'
})
export class DataPointEditorComponent implements OnInit {

  @Input() dataTypeName: string;
  @Input() data: Record<string, any>;
  @Input() context: Record<string, EntityId>;
  formGroup: UntypedFormGroup;
  formFields: FormField[];
  isSaving = false;

  constructor(private readonly dataPointController: DataPointControllerService,
    private readonly httpClient: HttpClient,
    private messageService: NzMessageService,
    private translateService: TranslateService,
    private notificationService: NzNotificationService,
    @Optional() private drawerRef: NzDrawerRef<string>) { }


  ngOnInit(): void {
    this.dataPointController.getDataPointsForType(this.context.projectId, this.dataTypeName, [UsagesModel.UsagesEnum.general_editMode])
      .subscribe(dataPoints => this.initForm(dataPoints));
  }

  /**
   * Submits the form data.
   */
  onSubmit(): void {
    this.isSaving = true;

    const endpoints = new Set<string>();
    for (const field of this.formFields) {
      endpoints.add(field.endpoint);
    }

    const requests: Array<Observable<any>> = [];

    for (const endpoint of endpoints) {
      const fields = this.formFields.filter(field => field.endpoint === endpoint);
      const url = replaceTemplateString(fields[0].dataPoint, endpoint, this.context, this.data);
      const record: Record<string, any> = {};
      for (const field of fields) {
        record[field.fieldName] = this.formGroup.get(field.key)?.value;
      }

      requests.push(this.httpClient.put(getBasePath() + url, record));
    }

    forkJoin(requests).subscribe(() => {
      this.isSaving = false;
      this.messageService.success(this.translateService.instant('dataPointEditor.saveSuccess') as string);
      if (this.drawerRef) {
        this.drawerRef.close(FormResult.Saved);
      }
    }, (error: HttpErrorResponse) => {
      this.onCancel();
      this.notificationService.error(
        this.translateService.instant('dataPointEditor.saveError') as string,
        this.translateService.instant('contactSupport') as string,
        { nzDuration: 0 }
      );
      log.error(error);
    });
  }

  /**
   * Cancels the form submission and closes the editor.
   */
  onCancel(): void {
    if (this.drawerRef) {
      this.drawerRef.close();
    }
  }

  /**
   * Returns whether the form can be submiited.
   * @returns true or false
   */
  canSubmitForm(): boolean {
    return this.formGroup && ! this.formGroup.pristine;
  }

  private initForm(dataPoints: MiningDataPointDefinitionWithPath[]) {
    const formFields: FormField[] = [];
    const formControls: { [key: string]: UntypedFormControl } = {};
    for (const dp of dataPoints) {
      const key = dp.path.split('.').join('_');
      const value = getValueAtPath(this.data, dp.path);
      formControls[key] = new UntypedFormControl(value);
      formFields.push({
        key,
        label: dp.displayName,
        endpoint: dp.usageAttributes?.[UsagesModel.UsagesEnum.general_editMode]?.[UsagesModel.EditModeAttributesEnum.editEndpoint],
        fieldName: dp.usageAttributes?.[UsagesModel.UsagesEnum.general_editMode]?.[UsagesModel.EditModeAttributesEnum.editEndpointFieldName],
        editAs: dp.usageAttributes?.[UsagesModel.UsagesEnum.general_editMode]?.[UsagesModel.EditModeAttributesEnum.editAs] as any,
        dataPoint: dp
      });
    }
    this.formFields = formFields;
    this.formGroup = new UntypedFormGroup(formControls);
  }
}
