import { Component, OnInit } from '@angular/core';
import { NzModalRef } from 'ng-zorro-antd/modal';

@Component({
  selector: 'app-scheduler-data-modal',
  templateUrl: './scheduler-data-modal.component.html'
})
export class SchedulerDataModalComponent implements OnInit {

  fileList: any;

  constructor(private modal: NzModalRef) { }

  ngOnInit(): void {
    this.fileList = [];
  }

  /**
   * Handles the file submited and block the automatic upload of the component
   * @param file NG-Zorro file
   */
  beforeUpload = (file: File): boolean => {
    this.fileList = [file];
    return false;
  };

  /**
   * Handles the file deletion
   * @param file NG-Zorro file
   * @returns `true` if the file is deleted, otherwise `false`
   */
  onFileDelete = (file: File): boolean => {
    this.fileList.splice(this.fileList.indexOf(file), 1);
    return true;
  };

  /**
   * Method sets the modal on clicking import button
   */
  onClick(): void {
    this.modal.close({ status: 'success', file: this.fileList });
  }

  /**
   * Method sets the modal on closing the modal
   */
  closeModal(): void {
    this.modal?.destroy();
  }
}
