import { Component,OnInit} from '@angular/core';
import { NzModalRef, NzModalService } from 'ng-zorro-antd/modal';
import { TypeName, Usages } from '@app/shared/interfaces/datapoints-labels.interface';
import { MiningTableRow } from '../mining-table/mining-table-config.interface';
import { AnnotationPojo, AnnotationControllerService } from '@innowake/mining-api-angular-client';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { CustomizableTableColumnService } from '@app/core/services/user-customizable-table/customizable-table-column.service';

@Component({
  selector: 'app-identify-business-related-modal',
  templateUrl: './identify-business-related-modal.component.html',
  providers:[CustomizableTableParametersService, CustomizableTableColumnService]
})

export class IdentifyBusinessRelatedModalComponent implements OnInit {
  annotationIds: number[];
  filteredAnnotations: AnnotationPojo[];
  updateType: string;
  projectId: number;
  dataDictionaryName: string;
  loadSave = false;
  internalDataPoints = [
    { name: 'id', path: 'content.id' },
    { name: 'id', path: 'content.module.id' },
    { name: 'offset', path: 'content.location.offset' }
  ];
  allowSelectDeselectRows: boolean;
  usage: Usages = Usages.ANNOTATIONTABLE;
  pageType = TypeName.PAGEANNOTATION;
  graphQlType = 'annotations';
  additionalGraphQLPreFilter: { [key: string]: any } = {};
  annotationToBeSubmitted: AnnotationPojo[] = [];
  refreshCoreTable: boolean;
  confirmModal: NzModalRef;

  constructor(private modal: NzModalRef,
    private annotationController: AnnotationControllerService,
    private modalService: NzModalService,
    private translateService: TranslateService,
    private messageService: NzMessageService
    ) { }

  ngOnInit(): void {
    this.additionalGraphQLPreFilter = {
      content_id: {
        in: this.annotationIds
      }
    };
    this.refreshCoreTable = false;
  }

 /**
  * method to handle the selected affected modules
  * @param  event event emitted by customizable core table with selected row details
  */
  handleSelectedRowsOfCoreTable(event: MiningTableRow): void {
    this.annotationToBeSubmitted = this.filteredAnnotations?.filter((annotation: any) => event.some((ids: any) =>
      ids === annotation.id));
  }

  /**
   * method to submit taxonomy propagation
   */
  onSubmit(): void {
    this.loadSave = true;
    this.annotationController.updateCategoryAndMetadataOfAnnotation(this.projectId, this.annotationToBeSubmitted, this.updateType)
      .subscribe((response: string[]) => {
      if (response) {
        this.loadSave = false;
        this.modal?.destroy();
        const headerTitle: string = this.translateService.instant('identifyBusinessRelated.onSuccessModalTitle',
          { dataDictionaryName: '\'' + this.dataDictionaryName + '\''}
        );
        this.modalService.success({
          nzTitle: this.translateService.instant(headerTitle),
          nzClassName: 'identify-business-related__modal',
          nzWidth: '40vw',
          nzContent: this.translateService.instant('identifyBusinessRelated.onSuccessModalContent'),
          nzOkText: 'Done',
          nzClosable: false
        });
      }
    }, () => {
      this.messageService.error(`${this.translateService.instant('identifyBusinessRelated.errorMessage')}`);
      this.loadSave = false;
      this.modal?.destroy();
    });
  }

  /**
   * method to handle cancel
   */
  handleCancel(): void {
    this.modal?.destroy();
  }
}
