import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CdkDragDrop, moveItemInArray } from '@angular/cdk/drag-drop';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { AnnotationPojo, EntityId} from '@innowake/mining-api-angular-client';

@Component({
  selector: 'assign-functional-groups',
  templateUrl: './assign-functional-groups.component.html',
})
export class AssignFunctionalGroupComponent {
  @Input() mappedData: { [key: string]: any } = {} ;
  @Output() removeFG = new EventEmitter();
  @Output() removeFGFromTree = new EventEmitter();
  @Output() deleteAnnotationsFromFB = new EventEmitter();
  @Input() uids: string[];
  @Input() projectId: number;
  @Input() moduleId: EntityId;
  @Input() annotation: any;
  @Input() annotationDetails: AnnotationPojo[] = [];
  @Output() updatedFunctionalGroup = new EventEmitter();

  annotationMappingWithIds: { [key: string]: any } = {};
  panelState: { [key: string]: any } = {};
  constructor( private modalRef: NzModalRef){}

  /**
   * Removes an annotation from a group
   * @param uid: particular UId for the group
   * @param annotationId: annotation id of annotation
   */
  removeItem(uid: string, annotationId: number): void {
    if (this.isFunctionalTypeAnnotation(uid, annotationId)) {
      const annotationUUIDs = this.mappedData[uid].annotations
          .filter((annotation: any) => annotation.id !== annotationId)
          .map((annotation: any) => annotation.uid);
      const requestBody = {
        uid,
        description: this.mappedData[uid].description,
        name: this.mappedData[uid].name,
        children: annotationUUIDs,
        annotationId
      };
      this.deleteAnnotationsFromFB.emit(requestBody);
    }
    this.mappedData[uid].annotations = this.mappedData[uid].annotations
    .filter((annotationItem: any) => annotationItem.id !== annotationId);
    this.mappedData[uid].updated = true;
    this.updatedFunctionalGroup.emit(this.mappedData);
  }

  isFunctionalTypeAnnotation(uid: string, annotationId: number): boolean {
    const annotation = this.mappedData[uid].annotations.find((annotation: { id: number }) => annotation.id === annotationId);
    return annotation && annotation.typeLabel === 'FUNCTIONAL';
  }

  /**
   * drag and drop functionality provided by zorro
   * @param event:CdkDragDrop<string[]>|any
   * @param uid: UId of functional group
   */
  drop(event: CdkDragDrop<string[]> | { [key: string]: any }, uid: string): void {
    moveItemInArray(this.mappedData[uid].annotations as Array<{ [key: string]: any }>, event.previousIndex as number, event.currentIndex as number);
    this.mappedData[uid].updated = true;
    this.updatedFunctionalGroup.emit(this.mappedData);
  }

  /**
   * method to toggle the panel of the FG
   * @param event: click event emitted by click on the panel
   * @param item: item which has been clicked
   */
  toggleDropDown(event: Event, item: { [key: string]: any }): void {
    event.stopPropagation();
    this.panelState[item.uid] = ! this.panelState[item.uid];
  }

  openEditModal(uid: string): void {
    this.modalRef?.destroy({ editUid: uid });
  }

  /**
   * method to avoid closing the panel when clicking inside the panel
   * @param event: Click even generated once click inside the panel
   */
  stopBubbling(event: Event): void {
    event.stopPropagation();
  }
}
