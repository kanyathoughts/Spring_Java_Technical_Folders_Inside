import { Injectable } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { NzMessageService } from 'ng-zorro-antd/message';

export enum Entity {
    ANNOTATION = 'annotation'
}
@Injectable({
    providedIn: 'root',
})

export class ClipboardService {
    private 'localStorageKey' = 'clipboardData';

    constructor(
        private messageService: NzMessageService,
        private translateService: TranslateService ) {}

    /**
     * Method to copy the in to the clipboard.
     * @param entity - The entity to copy.
     * @param content - The content to copy.
     */
    /* eslint-disable @typescript-eslint/explicit-module-boundary-types */
    copyToClipboard(entity: string, content: any): void {
        const clipboardData = this.getClipboardData();
        clipboardData[entity] = content;
        this.setClipboardData(clipboardData);
        this.messageService.success(this.translateService.instant('annotationFields.annotationCopied') as string);
    }

    /**
     * Method to get the content from the clipboard.
     * @param entity - The entity to get.
     * @returns The content from the clipboard.
     */
    getFromClipboard(entity: string): string | undefined {
        const clipboardData = this.getClipboardData();
        return clipboardData[entity];
    }

    /**
     * Method to clear all the clipboard.
     */
    clearAll(): void {
        localStorage.removeItem(this.localStorageKey);
    }

    private getClipboardData(): { [key: string]: any } {
        const dataString = localStorage.getItem(this.localStorageKey);
        return dataString ? JSON.parse(dataString) : {};
    }

    private setClipboardData(data: { [key: string]: string }): void {
        localStorage.setItem(this.localStorageKey, JSON.stringify(data));
    }
}
