import { Component, Input, OnDestroy, OnInit, Output, ViewChild, EventEmitter, ChangeDetectorRef, ElementRef, AfterViewInit } from '@angular/core';
import { monacoEditorStyle } from 'theme/monaco-editor-style';
import { NgForm } from '@angular/forms';
import { AnnotationElementData, AnnotationsFunctionalGroups } from '../mining-code-viewer';
import { Logger } from '@app/core';
import { ScrollEventService } from '@app/core/services/scroll-event/scroll-event.service';
import { Subscription } from 'rxjs';
import { NzNotificationService } from 'ng-zorro-antd/notification';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalService } from 'ng-zorro-antd/modal';
import { CustomizableTableParametersService } from '@app/core/services/user-customizable-table/customizable-table-parameters.service';
import { ListFunctionalGroupComponent } from '@app/shared/components/list-funtional-group/list-functional-group.component';
import {
  CreateEditFunctionalGroupComponent, RESULT_CANCELLED, RESULT_DELETED,
  RESULT_EDITED
} from '@app/shared/components/create-edit-functional-group/create-edit-functional-group.component';
import { GraphQlControllerService } from '@app/core/services/graphql.service';
import { ClipboardService, Entity } from '@app/core/services/clipboard.service';
import { SharedAnnotationEditorService } from '@app/shared/components/shared-annotation-editor/shared-annotation-editor.service';
import { AnnotationControllerService, FunctionalBlockControllerService } from '@innowake/mining-api-angular-client';
import { FunctionalGroupService } from '@app/core/services/functional-group.service';

/* Id prefix for the element that can be used in other components for naming. */
export const annotationElementIdPrefix = 'code-annotation-editor-component-';

const logger = new Logger();

const sharedAnnotationEditorClass = 'mining__card-drawer shared-side-viewer__drawer-editor shared-side-viewer__drawer-editor--open-';

const wrapClassName = 'vertical-center-modal';

@Component({
  selector: 'code-annotation-editor-component',
  templateUrl: './code-annotation-editor.component.html'
})

/**
 * The component holding a concrete annotation. This component is created by the monaco editor.
 * by creating a DOM element: document.create('code-annotation-editor-element').
 * To be able to achieve this we use "angular elements".
 * If you want to use it as HTML Element you need to use the tag otherwise to use it as angular component please use the component selector only.
 * The callback functions are set in the mining-module-annotation-editor.component.
 */
export class CodeAnnotationEditorComponent implements OnInit, OnDestroy, AfterViewInit {
  @Input() data: AnnotationElementData;
  @Input() annotationsOfModule: any[] = [];
  @Input() projectId: number;
  @Input() deletedCallback?: () => void;
  @Input() isCfg?: boolean;
  @Input() openEditor?: () => void;
  @Input() showError: (message: string) => void;
  @Input() disableActionButtons?: boolean;
  @Input() projectLevelFgs: number;
  @Output() currentAnnotationElement = new EventEmitter<{ type: string, annotation: AnnotationElementData }>();
  @ViewChild('formCodeAnnotation') public formCodeAnnotation: NgForm;
  @ViewChild('annotationContent',{ static: false }) annotationContent!: ElementRef;
  @ViewChild('functionalGcontent',{ static: false }) functionalGcontent!: ElementRef;

  moduleFunctionalGroupData: AnnotationsFunctionalGroups[] = [];
  showHide: string;
  annotationStyles: any;
  displayShowMore = false;
  showAllContent = false;
  showFgAllContent = false;
  styles = monacoEditorStyle;
  annotationViewDiv: any;
  sharedAnnotationEditorStatusClass: string;
  showMore = false;
  listOfFunctionalGroups: any[] = [];
  listOfCurrentFunctionalUnits: string[] = [];
  categoryNameCompare: number;
  contentLengthCompare: number;
  fgContentLengthCompare: number;
  contentLengthCompareNofg: number;
  contentLengthCompareNoAnnotation: number;
  private scrollEventSubscription: Subscription;
  private refreshAnnotationSubscription: Subscription;

  constructor(
    private annotationController: AnnotationControllerService,
    private scrollEventService: ScrollEventService,
    private notification: NzNotificationService,
    private translateService: TranslateService,
    private messageService: NzMessageService,
    private parameterService: CustomizableTableParametersService,
    private modalService: NzModalService,
    private functionalBlocksController: FunctionalBlockControllerService,
    private graphQlControllerService: GraphQlControllerService,
    private clipBoardService: ClipboardService,
    private cdr: ChangeDetectorRef,
    private sharedAnnotationService: SharedAnnotationEditorService,
    private functionalGroupService: FunctionalGroupService
  ) { }

  ngOnInit(): void {
    this.sharedAnnotationEditorStatusClass = sharedAnnotationEditorClass + this.data.annotation.state.toLowerCase().replace('_', '-');
    this.annotationViewDiv = {
      'max-height.px': this.styles.annotationMaxHeight,
      'min-height.px': this.styles.annotationMinHeight
    };
    this.refreshAnnotationSubscription?.unsubscribe();
    if (! this.disableActionButtons) {
      this.refreshAnnotationSubscription = this.sharedAnnotationService.refreshCodeAnnotations$.subscribe(() => {
        this.getFunctionalGroupsOfCurrentModule();
      });
    }
    this.scrollEventSubscription =
      this.scrollEventService.getScrollObservable().subscribe((scrollPosition) => {
        this.showHide = 'shared-side-viewer__drawer-editor-header-' + scrollPosition;
      });
    }

  ngAfterViewInit(): void {
    this.cdr.detectChanges();
    this.rebuildGrid();
    }

  rebuildGrid(): void {
    setTimeout(() => {
      if(this.data.annotation.name){
        this.contentLengthCompare = (this.annotationContent.nativeElement as HTMLElement).offsetWidth * 0.11;
        this.contentLengthCompareNofg = (this.annotationContent.nativeElement as HTMLElement).offsetWidth * 0.15;
      }
      if(this.data.functionalBlockDetails?.listOfFunctionalGroupNames){
        this.fgContentLengthCompare = (this.functionalGcontent.nativeElement).offsetWidth * 0.11;
        this.contentLengthCompareNoAnnotation = (this.functionalGcontent.nativeElement).offsetWidth * 0.13;
      }
    },100);
  }

  /**
   * Changes the content view in the view mode on click of show more/less.
   */
  toggleContentView(): void {
    this.showAllContent = ! this.showAllContent;
  }

  /**
   * Changes the functional Group content view in the view mode on click of show more/less.
   */
  toggleFunctionalGroupContentView(): void {
    this.showFgAllContent = ! this.showFgAllContent;
  }

  /**
   * Calls the API server to delete the annotation
   */
  deleteAnnotation(): void {
    this.annotationController.deleteAnnotation(this.data.projectId, this.data.annotation.id).subscribe(() => {
      this.parameterService.setReloadTableDataValue(true);
      this.messageService.success(this.translateService.instant('sharedAnnotationEditorComponent.deleteSuccessMessage') as string);
      const selectedElement: { type: string, annotation: AnnotationElementData } = { type: 'delete', annotation: this.data };
      this.currentAnnotationElement.emit(selectedElement);
      if (this.deletedCallback) {
        this.deletedCallback();
      }
    }, (error) => {
      logger.error(error);
      this.notification.error(
        this.translateService.instant('sharedAnnotationEditorComponent.deleteErrorMessage') as string,
        this.translateService.instant('contactSupport') as string,
        { nzDuration: 0 }
      );
    });
  }

  openAnnotationEditor(): void {
    this.data.isEdit = true;
    const selectedElement: { type: string, annotation: AnnotationElementData } = { type: 'edit', annotation: this.data };
    this.currentAnnotationElement.emit(selectedElement);
    if (this.openEditor) {
      this.openEditor();
      this.rebuildGrid();
    }
  }

  /**
   * Opens a modal based on funtional block available for the annotation id.
   */
  openFunctionalGroupModals(): void {
    this.functionalGroupService.getFunctionalGroupTree(this.projectId, {}, 0, 30).subscribe((response) => {
      this.projectLevelFgs = response.length;
      if (this.projectLevelFgs > 0) {
        this.getFunctionalGroupsOfCurrentModule();
        setTimeout(() => {
          this.openFunctionalGroupListModal(true);
        }, 200);
      } else {
        this.openCreateFunctionalGroupModal();
      }
    });
  }

  /**
   * Copies the annotation details to the clipboard.
   */
  copyAnnotationDetails(): void {
    this.clipBoardService.copyToClipboard(Entity.ANNOTATION, this.data.annotation.id);
  }

  ngOnDestroy(): void {
    this.scrollEventSubscription?.unsubscribe();
    this.refreshAnnotationSubscription?.unsubscribe();
  }

  /**
   * Opens a modal for creating a functional group.
   * @returns void and emits the result when the modal is closed.
   */
  private openCreateFunctionalGroupModal(modalType?: string): void {
    const modal = this.modalService.create<CreateEditFunctionalGroupComponent>({
      nzTitle: this.translateService.instant('functionalBlock.createFunctionalBlockModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzWrapClassName: wrapClassName,
      nzKeyboard: true,
      nzContent: CreateEditFunctionalGroupComponent,
      nzOnCancel: () => {
        if(this.listOfFunctionalGroups.length !== 0) {
          this.openFunctionalGroupListModal(true);
        }
      }
    });
    const instance = modal.getContentComponent();
    instance.annotationIds = [this.data.annotation.id];
    instance.projectId = this.data.projectId;
    instance.listOfFgsByModule = this.listOfFunctionalGroups;
    modal.afterClose.subscribe((result) => {
      if (result.state === RESULT_CANCELLED && modalType === 'createNew') {
        this.openFunctionalGroupListModal(true);
      } else {
        const childBlock: AnnotationsFunctionalGroups = {
          uid: result.children[0],
          name: '',
          generatedFrom: {
            annotationId: this.data.annotation.id
          }
        };
        const parentBlock: AnnotationsFunctionalGroups = {
          uid: result.uid,
          name: result.name,
          generatedFrom: null,
          children: {
            content: [childBlock]
          }
        };
        this.listOfFunctionalGroups.push(parentBlock);
        this.functionalBlocksController.computeFunctionalBlock(this.data.projectId, new Set([result.uid] as string[])).subscribe(() => {
          this.updateFunctionalGroups(this.listOfFunctionalGroups);
          this.openFunctionalGroupListModal(false);
        });
      }
    });
  }

  /**
   * Opens a modal for Listing a functional group.
   * @returns An observable that emits the result when the modal is closed.
   */
  private openFunctionalGroupListModal(buttonState?: boolean): void {
    const matchedAnnotation = this.checkForAnnotation(this.listOfFunctionalGroups, this.data?.annotation);
    const modal = this.modalService.create<ListFunctionalGroupComponent>({
      nzTitle: this.translateService.instant('listFunctionalBlock.listFunctionalBlockModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzWrapClassName: wrapClassName,
      nzKeyboard: true,
      nzClassName: 'code-annotation-editor-component__title',
      nzBodyStyle: { height: '400px' },
      nzWidth: '80vw',
      nzContent: ListFunctionalGroupComponent
    });
    const instance = modal.getContentComponent();
    instance.projectId = this.data.projectId;
    instance.annotationIds = [this.data.annotation.id];
    instance.annotation = matchedAnnotation as { [key: string]: any };
    instance.annotationsOfModule = this.annotationsOfModule;
    instance.moduleId =  this.data.moduleId;
    instance.saveButtonState = buttonState;
    modal.afterClose.subscribe((result) => {
      if (result?.save) {
        if(result?.FgIds?.length > 0){
          setTimeout(() => {
            this.getFunctionalGroupsOfCurrentModule();
            }, 200);
        }
        if(result?.updatIds){
          setTimeout(() => {
          this.getFunctionalGroupsOfCurrentModule();
          }, 200);
        }
        this.cdr.markForCheck();
        this.sharedAnnotationService.triggerRefresh();
        if(result?.deletedAnnotationIds?.length > 0 && result.deletedAnnotationIds.includes(this.data.annotation.id)){
          const selectedElement: { type: string, annotation: AnnotationElementData } = { type: 'delete', annotation: this.data };
          this.currentAnnotationElement.emit(selectedElement);
          if (this.deletedCallback) {
             this.deletedCallback();
          }
        }
        this.rebuildGrid();
      } else if (result?.createNew) {
        this.openCreateFunctionalGroupModal('createNew');
      } else if (result?.editUid) {
        this.openEditModal(result?.editUid);
      }
    });
  }

  private openEditModal(uid: any): void {
    const modal = this.modalService.create<CreateEditFunctionalGroupComponent>({
      nzTitle: this.translateService.instant('functionalBlock.editFunctionalBlockModalTitle'),
      nzClosable: true,
      nzMaskClosable: false,
      nzWidth : '600px',
      nzWrapClassName: wrapClassName,
      nzClassName: 'functional-analysis-tree-component__edit-window',
      nzKeyboard: true,
      nzContent: CreateEditFunctionalGroupComponent,
      nzOnCancel: () => {
        setTimeout(() => {
          this.openFunctionalGroupListModal(true as boolean);
        }, 200);
      }
    });
    const instance = modal.getContentComponent();
    instance.annotationGroupToEdit = uid;
    instance.annotationIds = [];
    instance.projectId = this.projectId;
    instance.listOfFgsByModule = this.listOfFunctionalGroups;
    modal.afterClose.subscribe((result) => {
      this.getFunctionalGroupsOfCurrentModule();
      this.updateFunctionalGroups(this.listOfFunctionalGroups);
      this.cdr.markForCheck();
      this.sharedAnnotationService.triggerRefresh();
      if (result.state === RESULT_EDITED || result.state === RESULT_DELETED || result.state === RESULT_CANCELLED) {
        setTimeout(() => {
          this.openFunctionalGroupListModal(result.buttonState as boolean);
        }, 200);
      }
    });
  }

/**
 * Filters functional groups based on the annotation ID from the list of functional groups.
 * @returns An array of filtered functional groups.
 */
private filterFunctionalGroupsByAnnotation(listOfFunctionalGroups: any): AnnotationsFunctionalGroups[] {
    const filteredFunctionalBlocks: AnnotationsFunctionalGroups[] = [];
    listOfFunctionalGroups.forEach((parentBlock: AnnotationsFunctionalGroups) => {
      const functionalGroupData = {
        name: parentBlock.name,
        uid: parentBlock.uid,
      };
      if ( ! this.moduleFunctionalGroupData.some(moduleFunctionalGroup =>
        moduleFunctionalGroup.uid === functionalGroupData.uid)) {
        this.moduleFunctionalGroupData.push(functionalGroupData);
      }
      if (parentBlock.children && parentBlock.children.content) {
        const filteredChildren = parentBlock.children.content
          .filter((childBlock: AnnotationsFunctionalGroups) =>
            childBlock.generatedFrom?.annotationId === this.data.annotation.id
          );
        if (filteredChildren.length > 0) {
          filteredFunctionalBlocks.push({
            ...parentBlock,
            children: {
              content: filteredChildren
            }
          });
        }
      }
    });
    return filteredFunctionalBlocks;
  }

  /**
   * Updates the functional groups based on the annotation and refreshes the data.
   */
  private updateFunctionalGroups(fgsList: any): void {
    if (this.data) {
      const functionalGroupsByAnnotation = this.filterFunctionalGroupsByAnnotation(fgsList);
        const functionalGroupsDetails = {
          listOfFunctionalGroupNames: functionalGroupsByAnnotation.map(fb => fb?.name).join(', '),
          listOfFunctionalGroupsUid: functionalGroupsByAnnotation.map(fb => fb?.uid),
          functionalGroupsAssociatedToAnnotation: functionalGroupsByAnnotation
        };
        this.data.functionalBlockDetails = functionalGroupsDetails;
        this.cdr.detectChanges();
    }
  }

  private checkForAnnotation(functionalGps: any, annotation: any): any {
    let annotationDetails = {};
    if (functionalGps.length > 0) {
      annotationDetails = { ...annotation};
      functionalGps.forEach((item: any) => {
        item.children.content.forEach((contentItem: any) => {
          if (contentItem.generatedFrom?.annotationId !== annotation.id) {
            annotationDetails = { ...annotation, uid: contentItem.uid };
          }
        });
      });
    } else {
      annotationDetails = annotation;
    }
    return annotationDetails;
  }

  private getFunctionalGroupsOfCurrentModule(): void {
    const query = `query functionalGroupsOfModule($projectId:Long, $moduleId: EntityId) {
      functionalBlocks(projectId: $projectId, filterObject:
        { content_resolvedModuleParts_moduleId: { eq: $moduleId }, content_type:
        { eq:  FUNCTIONAL_GROUP } }) {
        content {
          uid
          name
          description
          type
          generatedFrom {
            annotationId
          }
          children(filterObject: {content_type: {eq: FUNCTIONAL_UNIT}}) {
            content {
              uid
              name
              description
              type
              generatedFrom {
                annotationId
              }
            }
          }
        }

      }
    }`;
    this.graphQlControllerService.graphQl({
      query,
      variables: {
        projectId: this.projectId,
        moduleId: this.data.moduleId
      }
    }).subscribe((response: any) => {
      const fbs: any[] = [];
      const possibleFunctionalBlock = response.data.functionalBlocks.content;
      possibleFunctionalBlock.forEach((item: { children: {content: any[]} }) => {
        if(item.children.content.length > 0){
          fbs.push(item);
        };
       });
       this.listOfFunctionalGroups  = fbs;
      this.updateFunctionalGroups(fbs);
    });
  }
}
