import { Component, Input, OnInit } from '@angular/core';
import { UntypedFormArray, UntypedFormBuilder, Validators } from '@angular/forms';
import { BaseFormModalComponent } from '@app/shared/components/base-form-modal/base-form-modal.component';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';
import { NzModalRef } from 'ng-zorro-antd/modal';
import { ClientControllerV2Service, ClientPojo, ProjectControllerService, ProjectControllerV2Service,
  ProjectPojo, ProjectPojoPrototype } from '@innowake/mining-api-angular-client';

@Component({
  selector: 'mn-project-form-modal',
  templateUrl: './project-form-modal.component.html'
})
export class ProjectFormModalComponent extends BaseFormModalComponent implements OnInit {

  @Input() project?: ProjectPojo;

  @Input() clientName?: string;

  @Input() clientId?: number;

  clientList: ClientPojo[];

  natureList: string[];
  labels: { [key: string]: { [key: string]: string } };
  selectedProjects: ProjectPojoPrototype.NaturesEnum[];
  isLoading = false;

  projectForm = this.fb.group({
    name: ['', Validators.required],
    client: ['', Validators.required],
    natures: this.fb.array([])
  });

  constructor(
    public modal: NzModalRef,
    public messageService: NzMessageService,
    private fb: UntypedFormBuilder,
    private projectV2Service: ProjectControllerV2Service,
    private projectService: ProjectControllerService,
    private clientService: ClientControllerV2Service,
    private translateService: TranslateService
  ) {
    super(modal, messageService);
  }

  get projectName(): string {
    return this.projectForm.get('name').value;
  }

  get formClientId(): number {
    return this.projectForm.get('client').value;
  }

  get projectNatures(): UntypedFormArray {
    return this.projectForm.get('natures') as UntypedFormArray;
  }

  ngOnInit(): void {
    if (! this.clientId) {
      this.clientService.getAllClients().subscribe((pageClient) => {
        this.clientList = pageClient.content;
      });
    } else {
      this.clientList = [{id: this.clientId, name: this.clientName}];
      this.projectForm.get('client').setValue(this.clientId);
      this.projectForm.get('client').disable();
    }
    if (this.project) {
      this.mode = 'edit';
      this.projectForm.get('name').setValue(this.project.name);
      /* this.projectV2Service.findProjectNatures(this.project.id).subscribe(natures => {
        this.natureList = Object.keys(ProjectPojoPrototype.NaturesEnum).map((nature: ProjectPojoPrototype.NaturesEnum) => {
          const hasNature: boolean = natures.indexOf(nature) >= 0;
          if (hasNature) {
            this.projectNatures.push(this.fb.control({ value: true, disabled: true }));
          } else {
            this.projectNatures.push(this.fb.control({ value: false, disabled: true }));
          }
          return this.labelMappingService.mapLabel(LabelType.PROJECT, nature);
        });
      }, () => {
        this.setEmptyProjectNatures();
      });
    } else {
      this.setEmptyProjectNatures(); */
    }
    this.selectedProjects = [ProjectPojoPrototype.NaturesEnum.DISCOVERY, ProjectPojoPrototype.NaturesEnum.MINING];
  }

  /**
   * Handles actions on submit button
   */
  onSubmit(): void {
    this.isLoading = true;
    if (this.mode === 'create') {
      this.createProject();
    } else {
      this.updateProject();
    }
  }

  private createProject(): void {
    const newProject: ProjectPojoPrototype = {
      name: this.projectName,
      client: this.formClientId,
      natures: this.getSelectedNatures()
    };
    this.projectV2Service.createProject(newProject).subscribe((projectUpdated) => {
      this.project = projectUpdated;
      this.closeModal('success', `${this.translateService.instant('successCreate', {name: projectUpdated.name})}`, projectUpdated);
      this.isLoading = false;
    }, () => {
      this.closeModal('error', `${this.translateService.instant('errorCreate', {name: newProject.name})}`, false);
      this.isLoading = false;
    });
  }

  private updateProject(): void {
    const updateProject = {...this.project};
    updateProject.name = this.projectName;
    updateProject.clientId = this.clientId;
    this.projectService.updateProject(this.project.id, updateProject as ProjectPojoPrototype).subscribe((projectUpdated) => {
      this.project = projectUpdated;
      /* if (this.projectNatures.dirty) {
        this.projectV2Service.changeProjectNatures(projectUpdated.id, this.getSelectedNatures()).subscribe(() => {
          this.closeModal('success', `${this.translateService.instant('successEdit', {name: projectUpdated.name})}`, projectUpdated);
        }, () => {
          this.closeModal('warning', `${this.translateService.instant('projectForm.errorEditNature')}`, projectUpdated);
        });
      } else { */
      this.closeModal('success', `${this.translateService.instant('successEdit', {name: projectUpdated.name})}`, projectUpdated);
      /* } */
      this.isLoading = false;
    }, () => {
      this.closeModal('error', `${this.translateService.instant('errorEdit', {name: updateProject.name})}`, false);
      this.isLoading = false;
    });
  }

  private getSelectedNatures(): Set<ProjectPojoPrototype.NaturesEnum> {
    const projectNaturesEnum = new Set<ProjectPojoPrototype.NaturesEnum>();
    this.selectedProjects.forEach((nature) => {
      /* if (this.projectNatures.controls[index].value) {
        projectNaturesEnum.add(nature);
      } */
      projectNaturesEnum.add(nature);
    });
    return projectNaturesEnum;
  }

  /* private setEmptyProjectNatures() {
    this.selectedProjects = [];
    this.natureList = Object.keys(ProjectPojoPrototype.NaturesEnum).filter(
      nature => nature !== ProjectPojoPrototype.NaturesEnum.DISCOVERY_LIGHT && nature !== ProjectPojoPrototype.NaturesEnum.DB_CUTTER
    ).map((nature: ProjectPojoPrototype.NaturesEnum) => {
      if (nature === ProjectPojoPrototype.NaturesEnum.MINING) {
        this.projectNatures.push(this.fb.control({ value: true, disabled: true }));
      } else if (nature === ProjectPojoPrototype.NaturesEnum.DISCOVERY) {
        this.projectNatures.push(this.fb.control({ value: true, disabled: false }));
      } else {
        this.projectNatures.push(this.fb.control({ value: false, disabled: true }));
      }
      this.selectedProjects.push(ProjectPojoPrototype.NaturesEnum[nature]);
      return this.labelMappingService.mapLabel(LabelType.PROJECT, nature);
    });
  } */
}
