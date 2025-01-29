import { browser, element, by } from 'protractor';

/**
 * The taxomony configuration page object.
 */
export class TaxonomyConfigurationPage {
  /**
   * Navigates to taxonomies page.
   * @param projectId The project ID
   */
  navigateToTaxonomies(projectId: number) {
    return browser.get('/#/project-' + projectId + '/configuration/taxonomies');
  }

  /**
   * Get Taxonomy Configuration tab buttons.
   * @param index number
   */
  getImportExportButtons(index: number) {
    return element.all(by.css('.ant-card-body button')).get(index);
  }

  /**
   * Get Taxonomy Configuration icon.
   */
  getQuestionCircleIcon() {
    return element(by.css('.ant-card-body i'));
  }
}
