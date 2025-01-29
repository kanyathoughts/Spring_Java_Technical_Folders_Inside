import { browser } from 'protractor';
import { AnnotationEditorPage } from './page-objects/shared-annotation-editor.po';

describe('Annotation Editor page test', () => {
  const page = new AnnotationEditorPage;

  beforeAll(async () => {
    await page.navigateToAnnotationEditor();
  });

  xit('should load the annotation-editor page and submit ', async () => {
    const typeField  = page.getAnnotationTypeSelectBox();
    await typeField.get(0).click();
    const firstAnnotationType = page.getSelectBoxDropdownItem();
    await firstAnnotationType.get(0).click();
    await browser.sleep(1000);
    const categoryField  = page.getAnnotationTypeSelectBox();
    await categoryField.get(1).click();
    const firstAnnotationCategory = page.getSelectBoxDropdownItem();
    await firstAnnotationCategory.get(0).click();
    await browser.sleep(1000);
    const annotationField  = page.getAnnotationTypeSelectBox();
    await annotationField.get(2).sendKeys('Annotation Data');
    await browser.sleep(1000);
    const statusField  = page.getAnnotationTypeSelectBox();
    await statusField.get((await statusField).length - 1).click();
    const firstAnnotationStatus = page.getSelectBoxDropdownItem();
    await firstAnnotationStatus.get(0).click();
    await browser.sleep(1000);
    const submitButton = page.getSubmitButton();
    await submitButton.get(1).click();
    expect(await browser.getCurrentUrl()).toContain('/annotation-editor');
    await browser.sleep(1000);
    });

});
