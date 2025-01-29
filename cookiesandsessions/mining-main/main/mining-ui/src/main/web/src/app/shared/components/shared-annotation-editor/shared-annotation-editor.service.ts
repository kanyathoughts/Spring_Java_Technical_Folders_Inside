import { Injectable } from '@angular/core';
import { NzDrawerRef } from 'ng-zorro-antd/drawer';
import { BehaviorSubject } from 'rxjs';

@Injectable({
    providedIn: 'root',
})

export class SharedAnnotationEditorService {
    isEditorOpen = false;
    isFormChanged = false;
    refreshCodeAnnotationsSubject = new BehaviorSubject<boolean>(false);
    refreshCodeAnnotations$ = this.refreshCodeAnnotationsSubject.asObservable();

    private drawerRef: NzDrawerRef;


    /**
     * Sets the state(open/close) of shared annotation editor.
     * @param selectedState user selected state.
     */
    setEditorState(selectedState: boolean = false): void {
        this.isEditorOpen = selectedState;
    }

    /**
     * Sets the state(open/close) of shared annotation editor form.
     * @param formSate annotation editor form state.
     */
    setEditorFormState(formSate: boolean): void {
        this.isFormChanged = formSate;
    }

    /**
     * Method to get state(open/close) of shared annotation editor form.
     * @returns current state of annotation editor form.
     */
    getEditorFormState(): boolean {
        return this.isFormChanged;
    }

    /**
     * Method to get the current state shared annotation editor.
     * @returns current state of collapsible section.
     */
    getEditorState(): boolean {
        return this.isEditorOpen;
    }

    /**
     * Sets the reference of the drawer.
     * @param ref - The reference to the NzDrawerRef.
     */
    setDrawerRef(ref: NzDrawerRef): void {
        this.drawerRef = ref;
    }

    /**
     * Closes the drawer if it's open.
     */
    closeDrawer(): void {
        if (this.drawerRef) {
            this.drawerRef.close();
        }
    }

    /**
     * Triggers a refresh for code annotations.
     * This method updates the state of code annotations, triggering a UI refresh.
     */
    triggerRefresh(): void {
        this.refreshCodeAnnotationsSubject.next(true);
    }
}
