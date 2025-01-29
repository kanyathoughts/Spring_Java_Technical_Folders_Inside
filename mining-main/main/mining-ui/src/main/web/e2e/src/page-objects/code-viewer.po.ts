import { error } from 'console';
import { browser, by, element, ElementArrayFinder } from 'protractor';
import { protractor } from 'protractor/built/ptor';
import { TIMEOUT } from './app-shared.po';

export const moduleId = 2010;
export const projectId = 'project-1';

const EC = protractor.ExpectedConditions;

export class CodeViewerPage {

    /**
     * Navigate to code-viewer page
     */
    navigateTo() {
        return browser.get(`/#/${projectId}/module-${moduleId}/code-viewer`);
    }

    /**
     * Selects a line for add Annotation.
     *
     * @param content The code in the monaco editor.
     */
    async selectLineForAnnotation1(content: ElementArrayFinder) {
        await content.first().all(by.className('mtk1')).first().click();
        await browser.actions()
            .keyDown(protractor.Key.SHIFT)
            .sendKeys(protractor.Key.END)
            .keyUp(protractor.Key.SHIFT)
            .perform();
    }

    /**
     * Selects another line for add Annotation.
     *
     * @param content The code in the monaco editor.
     */
    async selectLineForAnnotation2(content: ElementArrayFinder) {
        await content.get(5).all(by.className('mtk1')).first().click();
        await browser.actions()
            .keyDown(protractor.Key.SHIFT)
            .sendKeys(protractor.Key.END)
            .keyUp(protractor.Key.SHIFT)
            .perform();
    }

    /**
     * Select keyword for adding data dictionary.
     */
    async selectDataDictionaryKeyword() {
        await browser.wait(EC.invisibilityOf(element(by.className('cfg-loader__spinner'))), TIMEOUT);

        await element(by.className('view-lines')).all(by.className('view-line')).get(14).all(by.css('span .mtk1')).first().click();

        await browser.actions()
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .perform();

        await browser.actions()
            .keyDown(protractor.Key.SHIFT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .sendKeys(protractor.Key.ARROW_RIGHT)
            .keyUp(protractor.Key.SHIFT)
            .perform();
    }

    /**
     * Hover on the Data Dictionary
     */
    async hoverOnDataDictionary() {
        await browser.wait(EC.invisibilityOf(element(by.className('cfg-loader__spinner'))), TIMEOUT);

        await browser.actions()
            .mouseMove(element.all(by.css('.annotation-editor-container__data-dictionary')).first())
            .perform();
    }

    /**
     * Helper method to add new annotation.
     *
     * @param description The Annotation description.
     * @param editor type of editor.
     */
    async addAnnotation(description: string, func: (element: ElementArrayFinder) => Promise<void>, editor?: string) {
        await browser.wait(EC.invisibilityOf(element(by.className('cfg-loader__spinner'))), TIMEOUT);
        await browser.sleep(1000);

        await browser.executeScript('window.scrollTo(0,0)');
        const annotationButton = element.all(by.css('.mining-toolbar--web-ui-view mn-toolbar-group')).last().all(by.tagName('button')).first();
        const monacoEditor = element(by.css('.annotation-editor-container__monaco-editor ngx-monaco-editor .overflow-guard'));
        const content = monacoEditor.element(by.css('.lines-content .view-lines')).all(by.className('view-line'));
        await content.first().all(by.className('mtk1')).first().click();

        /** Button should be disabled initially */
        await expect(annotationButton.isEnabled()).toBe(false);

        /** Select line */
        await func.call(this, content);

        /** Button should be enabled after selection */
        await expect(annotationButton.isEnabled()).toBe(true);
        await annotationButton.click();
        const annotation = element.all(by.tagName('code-annotation-editor-component')).last();
        const annotationTextArea = annotation.element(by.tagName('textarea'));
        await annotationTextArea.sendKeys(description);
        if (editor === 'cae') {
            await browser.actions().mouseMove(element(by.css('.shared-annotation-editor__scroll'))).perform();
            /** Save and cancel buttons */
            const actionButtons = element.all(by.css('nz-space .ant-space-item button'));
            /** Save description */
            await expect(actionButtons.last().isEnabled()).toBe(true);
            await actionButtons.last().click();
        } else {
            /** Save and cancel buttons */
            const actionButtons = element.all(by.css('code-annotation-editor-component .code-annotation-editor-component__actions button'));

            /** Save description */
            await actionButtons.first().click();
        }
        /** Verify if it is saved */
        expect(await element.all(by.className('code-annotation-editor-component__description')).last().getText()).toBe(description);
        const annotationContent = annotation.all(by.css('.code-annotation-editor-component__content .code-annotation-editor-component__labels'));
        expect(await annotationContent.first().getText()).toBe('Rule');
        expect(await annotationContent.last().getText()).toBe('In Analysis');
    }

    /**
     * Returns Add Annotation and Data Dictionary buttons.
     */
    getHeaderButtons() {
        return this.getToolBar().last().all(by.tagName('button'));
    }

    /**
     * Returns the annotation delete button reference of last annotation.
     * @param editor type of editor.
     */
    getAnnotationDeleteButton(editor?: string) {
        return element.all(by.className('code-annotation-editor-component__actions')).last().all(by.tagName('a')).last();
    }

    /**
     * Deletes the annotation from shared annotation editor.
     */
    async deleteAnnotation() {
        await element.all(by.className('code-annotation-editor-component__actions')).last().all(by.tagName('a')).last();
    }

    /**
     * Returns the annotation edit button reference of last annotation.
     */
    async getAnnotationEditButton() {
        await element.all(by.className('code-annotation-editor-component__actions')).last().all(by.tagName('a')).first().click();
    }

    /**
     * Checks if shared annotation editor is closed.
     */
    async checkAnnotationEditor() {
        expect(await browser.wait(EC.invisibilityOf(element(by.css('.code-viewer__drawer-annotation-editor--open'))), TIMEOUT)).toBe(true);
    }


    /**
     * Returns the Data Dictionary dialog.
     */
    getDataDictionaryDialog() {
        return element(by.css('.ant-modal-content'));
    }

    /**
     * Saves the description entered in the text area.
     * @param editor type of editor.
     */
    saveAnnotationDescription(editor?: string) {
        if (editor === 'cae') {
            const annotation = element.all(by.tagName('code-annotation-editor-component')).last();
            /** Save and cancel buttons */
            const actionButtons = element.all(by.css('.ant-space-horizontal .ant-space-item'));
            /** Save description */
            actionButtons.last().element(by.tagName('button')).click();
        } else {
            element.all(by.css('nz-space .ant-space-item button')).first().click();
        }
    }

    /**
     * Returns the annotation description.
     */
    async getAnnotationDescription() {
        return element.all(by.className('code-annotation-editor-component__description')).last().getText().then(text => {
            return text;
        }).catch(error => console.log('Error while fetching description'));
    }

    /**
     * Returns the annotation type.
     */
    getAnnotationType() {
        return element.all(by.css('code-annotation-editor-component')).last().all(by.css('.code-annotation-editor-component__content .code-annotation-editor-component__labels')).first().getText();
    }

    /**
     * Returns the annotation state.
     */
    getAnnotationState() {
        return element.all(by.css('code-annotation-editor-component')).last().all(by.css('.code-annotation-editor-component__content .code-annotation-editor-component__labels')).last().getText();
    }

    /**
     * Selects the annotation type (Database in this case).
     */
    async selectAnnotationType() {
        await this.getAnnotationActions().first().click();
        await element.all(by.tagName('nz-option-item')).first().click();
    }

    /**
     * Selects the annotation state (Candidate in this case).
     */
    async selectAnnotationState() {
        await this.getAnnotationActions().last().click();
        await element.all(by.tagName('nz-option-item')).first().click();
    }

    /**
     * Clicks on back button available in header.
     */
    async clickOnBackButton() {
        await this.getToolBar().first().element(by.tagName('button')).click();
    }

    /**
     * Adds the description to the selected annotation.
     *
     * @param text The Annotation Description.
     * @param editor type of editor.
     */
    async addTextToAnnotationDescription(text: string, editor?: string) {
        if (editor === 'cae') {
            expect(await browser.wait(EC.visibilityOf(element(by.css('.shared-side-viewer__drawer-editor--open'))), 8000));
            await browser.sleep(1000);
        }
        const annotation = element.all(by.tagName('code-annotation-editor-component')).last();
        const annotationTextArea = annotation.element(by.tagName('textarea'));

        await annotationTextArea.clear();
        await annotationTextArea.sendKeys(text);
        const actionButtons = element.all(by.css('.ant-space-horizontal .ant-space-item'));
        /** Save description */
        await actionButtons.last().element(by.tagName('button')).click();
    }

    /**
     * Check if data dictionary dialog is opened.
     */
    isDataDictionaryDialogOpened() {
        return element(by.css('.ant-modal-content')).isPresent();
    }

    /**
     * Returns the data dictionary title.
     */
    getDataDictionaryDialogTitle() {
        return this.getDataDictionaryDialog().element(by.className('ant-modal-title')).getText();
    }

    /**
     * Adds the given description to the Data Dictionary.
     *
     * @param desc The Description to be added for Data Dictionary.
     */
    async addDataDictionaryDescription(desc: string) {
        return await element(by.css('.shared-dd-editor__form-item textarea')).sendKeys(desc);
    }

    /**
     * Returns the data dictionary description.
     */
    async getDataDictionaryDescription() {
        return await this.getDataDictionaryDialog().all(by.css('data-dictionary-editor-component textarea')).first().getText();
    }

    /**
     * Clicks on data dictionary button in header.
     */
    async clickOnDataDictionaryButton() {
        await element.all(by.css('.mining-fixed-page-header')).last().all(by.tagName('button')).last().click();
    }

    /**
     * Enables the Database item in Data Dictionary dialog.
     */
    async enableDatabaseInDD() {
        await this.getDataBaseItemInDD().element(by.css('.ant-switch-small')).click();
    }

    /**
     * Returns the Database item label in the Data Dictionary dialog.
     */
    getDatabaseItemLabelInDD() {
        return this.getDataBaseItemInDD().element(by.css('.data-dictionary-modal__content-item-text label')).getText();
    }

    /**
     * Adds the given value to the Database entry in the Data Dictionary dialog.
     *
     * @param tables The tables entry for database item.
     */
    async addDataBaseValueInDD(tables: string) {
        await this.getDataBaseItemInDD().element(by.css('.data-dictionary-modal__content-item-text textarea')).clear();
        await this.getDataBaseItemInDD().element(by.css('.data-dictionary-modal__content-item-text textarea')).sendKeys(tables);
    }

    /**
     * Check if Database is enabled in the Data Dictionary dialog.
     */
    isDatabaseInDDEnabled() {
        return this.getDataBaseItemInDD().element(by.css('.ant-switch-checked')).isPresent();
    }

    /**
     * Enables the UI item in Data Dictionary dialog.
     */
    async enableUIInDD() {
        await this.getUIItemInDD().element(by.css('.ant-switch-small')).click();
    }

    /**
     * Returns the UI item label in the Data Dictionary dialog.
     */
    getUIItemLabelInDD() {
        return this.getUIItemInDD().element(by.css('.data-dictionary-modal__content-item-text label')).getText();
    }

    /**
     * Adds the given value to the UI entry in the Data Dictionary dialog.
     *
     * @param mapSet The mapSet value.
     * @param mapName The mapName value.
     */
    async addUIValueInDD(mapSet: string, mapName: string) {
        await this.getUIItemInDD().all(by.css('.data-dictionary-modal__content-item-text textarea')).first().sendKeys(mapSet);
        await this.getUIItemInDD().all(by.css('.data-dictionary-modal__content-item-text textarea')).last().sendKeys(mapName);
    }

    /**
     * Check if UI is enabled in the Data Dictionary dialog.
     */
    isUIInDDEnabled() {
        return this.getUIItemInDD().element(by.css('.ant-switch-checked')).isPresent();
    }

    /**
     * Enables the File I/O item in Data Dictionary dialog.
     */
    async enableFileIOInDD() {
        await this.getFileIOItemInDD().element(by.css('.ant-switch-small')).click();
    }

    /**
     * Returns the File I/O item label in the Data Dictionary dialog.
     */
    getFileIOLabelInDD() {
        return this.getFileIOItemInDD().element(by.css('.data-dictionary-modal__content-item-text label')).getText();
    }

    /**
     * Adds the given value to the Database entry in the Data Dictionary dialog.
     *
     * @param dataSet The dataSet value.
     */
    async addFileIOValueInDD(dataSet: string) {
        await this.getFileIOItemInDD().element(by.css('.data-dictionary-modal__content-item-text textarea')).sendKeys(dataSet);
    }

    /**
     * Check if File I/O is enabled in the Data Dictionary dialog.
     */
    isFileIOInDDEnabled() {
        return this.getFileIOItemInDD().element(by.css('.ant-switch-checked')).isPresent();
    }

    /**
     * Enables the Other item in Data Dictionary dialog.
     */
    async enableOtherInDD() {
        await this.getOtherItemInDD().element(by.css('.ant-switch-small')).click();
    }

    /**
     * Returns the Other item label in the Data Dictionary dialog.
     */
    getOtherItemLabelInDD() {
        return this.getOtherItemInDD().element(by.css('.data-dictionary-modal__content-item-text label')).getText();
    }

    /**
     * Adds the given value to the Other entry in the Data Dictionary dialog.
     *
     * @param otherScope The Other scope value.
     * @param source The source value.
     */
    async addOtherValueInDD(otherScope: string, source: string) {
        await this.getOtherItemInDD().all(by.css('.data-dictionary-modal__content-item-text textarea')).first().sendKeys(otherScope);
        await this.getOtherItemInDD().all(by.css('.data-dictionary-modal__content-item-text textarea')).last().sendKeys(source);
    }

    /**
     * Returns the Other item value(source) in the Data Dictionary dialog.
     */
    getOtherValueInDD2() {
        return this.getOtherItemInDD().all(by.css('.data-dictionary-modal__content-item-text textarea')).last().getAttribute('value');
    }

    /**
     * Check if Other field is enabled in the Data Dictionary dialog.
     */
    isOtherInDDEnabled() {
        return this.getOtherItemInDD().element(by.css('.ant-switch-checked')).isPresent();
    }

    /**
     * Enables the Parameter item in the Data Dictionary dialog.
     */
    async enableParamenterInDD() {
        await this.getParamenterItemInDD().element(by.css('.ant-switch-small')).click();
    }

    /**
     * Check if Parameter is enabled in the Data Dictionary dialog.
     */
    isParameterInDDEnabled() {
        return this.getParamenterItemInDD().element(by.css('.ant-switch-checked')).isPresent();
    }

    /**
     * Returns the Parameter item label in the Data Dictionary dialog.
     */
    getParamenterLabelInDD() {
        return this.getParamenterItemInDD().element(by.css('.data-dictionary-modal__content-item-text label')).getText();
    }

    /**
     * Saves the Data Dictionary (click on Save button  in the Data Dictionary dialog).
     */
    async saveDataDictionary() {
        await element(by.css('.shared-annotation-editor__button-bar .ant-btn-primary')).click();
    }

    /**
     * Tests if the added Data Dictionary if highlighted.
     */
    checkIfCSSAppliedForDataDictionary() {
        return element(by.className('annotation-editor-container__data-dictionary')).isPresent();
    }

    /**
     * Hover on the Data Dictionary and click on edit button, Opens the Data Dictionary dialog for editing.
     */
    async editDataDictionary() {
        await this.hoverOnDataDictionary();
        browser.manage().timeouts().implicitlyWait(5000);
        await element.all(by.css('.monaco-hover-content .hover-contents a')).first().click();
    }

    /**
     * Closes the Data Dictionary dialog box by clicking on cancel button.
     */
    async closeDataDictionary() {
        await element(by.css('.shared-annotation-editor__button-bar .cancel_editor')).click();
    }

    /**
     * Hover on the Data Dictionary and click on delete button (deletes the Data Dictionary).
     */
    async deleteDataDictionary() {
        await this.hoverOnDataDictionary();
        await browser.wait(EC.visibilityOf(element.all(by.css('.hover-row a')).first()), TIMEOUT);
        await element.all(by.css('.hover-contents a')).last().click();
    }

    /**
     * Returns the reference of header element.
     */
    private getToolBar() {
        return element.all(by.css('.mining-fixed-page-header'));
    }

    /**
     * Returns annotation actions (state & type).
     */
    private getAnnotationActions() {
        return element.all(by.css('.code-annotation-editor-component__content nz-select'));
    }

    /**
     * Returns the first annotation.
     */
    private getAnnotation() {
        return element(by.css('code-annotation-editor-component'));
    }

    /**
     * Returns the Data Dictionary dialog items.
     */
    private getDataDictionaryItems() {
        return this.getDataDictionaryDialog().all(by.css('.data-dictionary-modal .data-dictionary-modal__content-item'));
    }

    /**
     * Returns the Data Dictionary dialog Database item.
     */
    private getDataBaseItemInDD() {
        return this.getDataDictionaryItems().get(0);
    }

    /**
     * Returns the Data Dictionary dialog UI item.
     */
    private getUIItemInDD() {
        return this.getDataDictionaryItems().get(1);
    }

    /**
     * Returns the Data Dictionary dialog File I/O item.
     */
    private getFileIOItemInDD() {
        return this.getDataDictionaryItems().get(2);
    }

    /**
     * Returns the Data Dictionary dialog Other item.
     */
    private getOtherItemInDD() {
        return this.getDataDictionaryItems().get(3);
    }

    /**
     * Returns the Data Dictionary dialog Parameter item.
     */
    private getParamenterItemInDD() {
        return this.getDataDictionaryItems().get(4);
    }
}
