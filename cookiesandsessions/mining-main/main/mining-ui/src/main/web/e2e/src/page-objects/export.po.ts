import { browser, element, by } from 'protractor';

export class ExportPage {
  /**
   * Navigates to export page
   */
  navigateTo() {
    return browser.get('/#/project-1/export');
  }

  /**
   * return a new crated anchor tag in DOM once the success response received
   * */
   getAnchorTag() {
    return element.all(by.css('.export-download'));
  }

  /**
   * Returns button for the button links present on the export page
   * @param buttonIndex The index of the button links
   */
   getButtonLink(buttonIndex: number) {
    return element.all(by.css('.ant-btn-link')).get(buttonIndex);
  }
}