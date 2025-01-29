import { Component, OnInit } from '@angular/core';
import { AbstractControl, UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { NzModalRef } from 'ng-zorro-antd/modal';

@Component({
  selector: 'merge-block',
  templateUrl: './merge-block.component.html'
})
export class MergeBlockComponent implements OnInit {
  mergeBlockForm: UntypedFormGroup;

  constructor(private formBuilder: UntypedFormBuilder,
    private modal: NzModalRef
  ) { }

  get validateName(): AbstractControl {
    return this.mergeBlockForm.get('name');
  }

  ngOnInit(): void {
    this.mergeBlockForm = this.formBuilder.group({
      name: ['', Validators.required],
      description: ['']
    });
  }

  /**
   * Handles the cancel action for the modal.
   * Closes the modal and destroys it.
   */
  handleCancel(): void {
    this.modal?.destroy();
  }

  /**
   * Send user entered details(name,description) on saving the modal.
   */
  sendToParent(): void {
    this.modal.close({
      blockName: this.mergeBlockForm.value.name,
      blockDescription: this.mergeBlockForm.value.description
    });
  }
}
