import { browser, $, $$, element, by } from 'protractor';

/**
 * The shared-annotation-editor page object.
 */
export class AnnotationEditorPage {

  /**
   * Navidate to annotation-editor page
   */
  navigateToAnnotationEditor() {
     return browser.get(`/#/project-1/module-2002/annotation-editor?length=10&offset=10`);
  }

  /**
   * Gets the web annotation editor drawer in the annotation table.
   */
   getAnnotationEditorDialog() {
      return element(by.tagName('app-web-annotation-editor'))
   }

   /**
    * Gets the annotation-editor dialog title
    */
   getAnnotationEditorDialogTitle() {
      return element(by.tagName('nz-page-header-title')).getText();
   }
   /**
    * Gets annotation description from editor dialog
    */
   getDescriptionFromDialog() {
      return element(by.css('.ant-form-item-control-input-content textarea'))
   }

  /**
   * Gets the annotation type select box in the annotation search form.
   */
  getAnnotationTypeSelectBox() {
     return element.all(by.css('.ant-form-item.ant-row .ant-form-item-control-input-content > :first-child'));
  }

  /**
   * Gets the annotation type select box in the annotation search form.
   */
  getSelectBoxDropdownItem() {
     return $$('.ant-select-item.ant-select-item-option .ant-select-item-option-content')
  }

  /**
   * Gets the search button in the form.
   */
  getSubmitButton() {
     return $$('.ant-space .ant-space-item .ant-btn');
  }
}