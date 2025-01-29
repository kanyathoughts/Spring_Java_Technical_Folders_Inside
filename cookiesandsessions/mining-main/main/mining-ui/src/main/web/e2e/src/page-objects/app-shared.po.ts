/*
 * Use the Page Object pattern to define the page under test.
 * See docs/coding-guide/e2e-tests.md for more info.
 */
import { browser, element, by, ElementFinder } from 'protractor';
/** Waiting timeout for conditions to expire */
export const TIMEOUT = 5000;
import { protractor } from 'protractor/built/ptor';
import { MetricsPage } from './metrics-card.po';
import * as fs from 'fs';

const metricsPage: MetricsPage = new MetricsPage();
const EC = protractor.ExpectedConditions;

export class AppSharedPage {

  constructor() { }

  /**
   * Navigates to root and set default language.
   */
  async navigateAndSetLanguage() {
    // Forces default language
    await this.navigateTo();
    await browser.executeScript(() => localStorage.setItem('language', 'en-US'));
  }

  /**
   * Navigates to root.
   */
  async navigateTo() {
    await browser.get('/');
  }

  /**
   * Navigates to clients page to select from list of clients.
   */
  navigateToClients() {
    return browser.get('/#/clients');
  }

  /**
   * Selects the first client and navigates to projects page to see list of projects.
   */
  async navigateToProjects() {
    const client = await element(by.tagName('nz-spin')).all(by.css('.ant-col')).first();
    await client.element(by.css('.ant-card-cover')).click();
  }

  /**
   * Selects the project with id '1' and navigates to dashboard page in the project.
   */
  navigateToDashboard() {
    return browser.get('#/project-1/dashboard');
  }

  /**
   * Navigates to module details page in the project.
   *
   * @param id: The module id for the module details
   */
  async navigateToModuleDetails(id: number) {
    return browser.get('/#/project-1/module-' + id + '/details/overview');
  }

  /**
   * Navigates to modules(browse-modules) page from dashboard.
   */
  navigateToModules() {
    return browser.get('#/project-1/modules');
  }

  /**
   * Navigates to Application Decomposition page.
   */
  navigateToAppDecomposition() {
    return browser.get('#/project-1/metrics/decomposition');
  }

  /**
   * Navigates to Customizable Modules Page.
   */
  navigateToCustomizableModules() {
    return browser.get('#/project-1/modules');
  }

  /**
   * Navigates to Customizable Annotation Page.
   */
  navigateToCustomizableAnnotation() {
    return browser.get('#/project-1/annotations');
  }
  /**
   * Navigates to Utilities page.
   * @param projectId The project ID.
   */
  navigateToUtilities(projectId: number) {
    return browser.get('#/project-' + projectId + '/metrics/utilities');
  }

  /**
   * Navigates to specified client to display projects page.
   *
   * @param clientName the name of the client to select
   */
  async navigateToClient(clientName: string) {
    const client = await element(by.tagName('nz-spin')).element(by.cssContainingText('.ant-col', clientName));
    await client.element(by.css('.ant-card-cover')).click();
  }

  /**
   * Navigates to specified project to display dashboard page.
   *
   * @param projectName the name of the project to select
   */
  async navigateToProject(projectName: string) {
    const project = await element(by.tagName('nz-spin')).element(by.cssContainingText('.ant-col', projectName));
    await project.element(by.css('.ant-card-cover')).click();
  }

  /**
   * Navigates to sql decomposition.
   */
  navigateToMetricsSqlDecomposition() {
    return browser.get('/#/project-1/metrics/sql');
  }

  /**
   * Navigates to Interfaces.
   */
  navigateToMetricsInterfaces() {
    return browser.get('/#/project-1/metrics/interfaces');
  }

  /**
   * Navigates to Rule Candidates.
   */
  navigateToMetricsRuleCandidates() {
    return browser.get('/#/project-1/metrics/candidates');
  }

  /**
   * Navigates to Technologies.
   */
  navigateToMetricsTechnologies() {
    return browser.get('/#/project-1/metrics/technologies');
  }

  /**
   * Navigates to Code Quality.
   */
  navigateToMetricsCodeQuality() {
    return browser.get('/#/project-1/metrics/code-quality');
  }

  /**
   * Navigates to Summary.
   */
  navigateToMetricsSummary() {
    return browser.get('/#/project-1/metrics/summary');
  }

  /**
   * Navigates to dna page from dashboard.
   */
  navigateToDna() {
    return browser.get('#/project-1/dna');
  }

  /**
   * Gets all the rows in the specified mining table.
   *
   * @param table The table element
   */
  getTableRows(table: ElementFinder) {
    return table.$$('.ant-table-tbody .ant-table-row');
  }

  /**
   * Gets the row in the specified mining table.
   *
   * @param table The table element
   * @param rowPosition The row position in the table
   */
  getTableRow(table: ElementFinder, rowPosition: number) {
    return table.$$('.ant-table-tbody .ant-table-row').get(rowPosition - 1);
  }

  /**
   * Gets the column value from the table row in the specified mining table.
   *
   * @param row The row in the table from which to get the columns value
   * @param columnPos The column position in the table
   */
  async getTableColumn(row: ElementFinder, columnPos: number) {
    return await row
      .$$('.ant-table-cell')
      .get(columnPos - 1)
      .$('span')
      .getText();
  }

  /**
   * Gets the module name from the table row in the specified mining table.
   *
   * @param row The row in the table from which to get the columns value
   * @param columnPos The column position in the table
   */
  async getModuleName(row: ElementFinder, columnPos: number) {
    return await row
      .$$('.ant-table-cell')
      .get(columnPos - 1)
      .$('a')
      .getText();
  }

  /**
   * Tests the mining table rows for the exact specified value in a particular column.
   *
   * @param table The mining table to test
   * @param columnPos The column position in the table
   * @param value The value to check in every row at specified column position
   */
  async testCellValuesForValue(table: ElementFinder, columnPos: number, value: string, ignoreCase?: boolean, filter?: string) {
    (await table.$$('.ant-table-tbody .ant-table-row')).forEach(async (row) => {
      expect(browser.wait(EC.presenceOf(row.$$('.ant-table-cell').get(columnPos - 1)), TIMEOUT))
      const cellValue = await row
        .$$('.ant-table-cell')
        .get(columnPos - 1)
        .$('.ng-star-inserted')
        .getText();
      ignoreCase ? expect(cellValue.toLowerCase()).toEqual(value.toLowerCase()) : expect(cellValue).toEqual(value);
    });
  }

  /**
   * Tests the mining table rows to contain the specified value in a particular column.
   *
   * @param table The mining table to test
   * @param columnPos The column position in the table
   * @param value The value to check in every row at specified column position
   */
  async testCellValuesToContainValue(table: ElementFinder, columnPos: number, value: string, ignoreCase?: boolean) {
    (await table.$$('.ant-table-tbody .ant-table-row')).forEach(async (row) => {
      const cellValue = await row
        .$$('.ant-table-cell')
        .get(columnPos - 1)
        .$('span')
        .getText();
      ignoreCase ? expect(cellValue.toLowerCase()).toContain(value.toLowerCase()) : expect(cellValue).toContain(value);
    });
  }

  /**
   * Returns the current page header element.
   */
  currentPageHeader() {
    return element.all(by.css('.ant-page-header-heading-title')).first();
  }

  /**
   * Returns the current page sub header element.
   */
  currentPageSubHeader() {
    return element.all(by.css('.ant-page-header-heading-sub-title')).first();
  }

  /**
   * Returns the current page header element.
   */
  getPageHeader() {
    return element(by.css('.ant-page-header-heading-title')).getText();
  }

  /**
   * Returns the current open deletion modal element.
   */
  getDeleteModal() {
    return element(by.tagName('mn-confirm-delete-modal'));
  }

  /**
   * Returns the confirmation checkbox element of the deletion modal.
   */
  getDeleteModalConfirmationCheckbox() {
    return this.getDeleteModal().element(by.css('input[type="checkbox"]'));
  }

  /**
   * Returns the button elements of the current modal.
   */
  getModalActionItems() {
    return element(by.tagName('nz-modal-container')).element(by.css('.ant-modal-footer')).all(by.tagName('button'));
  }

  /**
   * Returns the element for notification.
   */
  getMessage() {
    return element(by.tagName('nz-message'));
  }

  /**
   * Returns the element for success notification.
   */
   getSuccessMessage() {
    return element(by.className('ant-message-success'));
  }

  /**
   * Returns true if the current page is modules page.
   */
  async isModulesPage() {
    return new RegExp('^.*/project-1/modules$').test(await browser.getCurrentUrl());
  }

  /**
   * Returns true if the current page is module details page.
   */
  async isModuleDetailPage(moduleId: number) {
    return new RegExp('^.*/project-1/module-' + moduleId + '/details/overview$').test(await browser.getCurrentUrl());
  }

  /**
   * Returns true if the current page is code-viewer page.
   */
  async isCodeViewerPage(moduleId: number) {
    return new RegExp('^.*/project-1/module-' + moduleId + '/code-viewer$').test(await browser.getCurrentUrl());
  }

  /**
   * Returns true if the current page is dependencies page.
   */
  async isDependencyGraphPage(moduleId: number) {
    return new RegExp('^.*/project-1/module-' + moduleId + '/dependencies$').test(await browser.getCurrentUrl());
  }


  /**
   * Method to make a POST api call to enable and disable the Feature.
   * @param state to enable and disable the state of the metricsUI by sending true or false.
   */
  async toggleFeature(featureId: string, state: boolean) {
    const token: any = await browser.executeScript('return JSON.parse(window.localStorage.getItem("oauthToken"));');
    const request = require('request');
    const options = {
      method: 'POST',
      url: browser.params.url + 'api/v1/features/' + featureId + '/toggle?state=' + state + '&access_token=' + token.access_token,
      json: true
    };
    await request(options);
  }

  async createProjectConfiguration(projectId: number, accessToken: string) {
    await require('request')({
      method: 'POST',
      url: browser.params.url + 'api/v1/projects/' + projectId + '/defaultConfiguration?access_token=' + accessToken,
      json: true
    });

    /* The project configuration creation is quite expensive. Add ugly delay therefore so test doesn't fail because 
     * the configs are not ready when the tests start */
    await browser.sleep(5000);
  }

 /**
   * Creates the project configurations for all test projects with nid 1, 2, 3 and 4.
   */
  async createProjectConfigurations() {
    const token: any = await browser.executeScript('return JSON.parse(window.localStorage.getItem("oauthToken"));');

    await this.createProjectConfiguration(1, token.access_token);
    await this.createProjectConfiguration(2, token.access_token);
    await this.createProjectConfiguration(3, token.access_token);
    await this.createProjectConfiguration(4, token.access_token);
  }

  /**
   * Checks if file is downloaded in Export page
   * Make sure to have called the method rmDownloadDir in the beforeAll/afterAll method 
   * @param fileExtension The extension of the downloaded file
   */
  checkExport(fileExtension: string, fileName?: string) {
    let filesArray = fs.readdirSync('e2e/downloads');
    filesArray = filesArray.filter(file => file.includes(fileExtension))
    expect(filesArray.length).toBeGreaterThanOrEqual(1);
    const downloadedFile = 'e2e/downloads' + '/' + filesArray[0];
    const response = fs.existsSync(downloadedFile);
    expect(response).toBeTruthy();
    expect(downloadedFile).toContain(fileExtension);
    if (fileName) {
      expect(downloadedFile).toContain(fileName);
    }
  }

  /**
   * first remove all the downloaded files
   * @param dirPath  path gives as location for downloading files
   */
  rmDownloadDir(dirPath: string) {
    try {
      const files = fs.readdirSync(dirPath);
      for (const file of files) {
        const filePath = dirPath + '/' + file;
        fs.unlinkSync(filePath);
        console.log(`${dirPath}/${file} has been removed successfully`);
      }
    } catch (err) {
      console.log(err)
    }
  }
  /**
   * Returns all mining table filters.
   */
  getTableFilters(index: number) {
    return element.all(by.css('.ant-table-row type-based-filter')).get(index).element(by.css('.ant-table-filter-trigger-container'));
  }
  /**
   * Returns search box filter for a column in mining table.
   */
  searchBox() {
    return element(by.css('.type-based-filter__number-text-search-box'));
  }
  /**
   * Returns search box input element.
   */
  getSearchBoxInput() {
    return this.searchBox().element(by.css('.type-based-filter__number-text-search-box input'));
  }
  /**
   * Returns search box filter buttons.
   */
  getSearchButtons() {
    return this.searchBox().all(by.css('.type-based-filter__filter-btn-gp button'));
  }
  /**
   * Returns mining table rows count.
   */
  async getRowsCount() {
    return await element(by.css('.ant-table-tbody')).all(by.className('ant-table-row')).count();
  }
  /**
   * Returns total count of data in mining table.
   */
  async getTableTotalCount() {
    return Number((await element(by.css('.mining-table__header-row > .ant-col')).getText()).split(':')[1]);
  }
  /**
   * Returns dropdown filter in mining table coulmns.
   */
  getFilters() {
    return element(by.css('.ant-table-filter-dropdown'));
  }
  /**
   * Returns dropdown items.
   */
  getFilterDropdown() {
    return this.getFilters().all(by.css('.type-based-filter__multi-select-list'));
  }
  /**
   * Returns buttons for drop down filter.
   */
  getFilterDropdownButtons() {
    return element(by.css('.type-based-filter__multi-select-btn-gp')).all(by.tagName('button'));
  }
  /**
   * Retuns Input filter for a column in mining table.
   */
  getFilterInput() {
    return this.getFilters().element(by.css('.ant-input-number-input'));
  }

  /**
   * Gets the Dna cards.
   */
  getDnaCard() {
    return element(by.css('.mining__content-margin')).all(by.tagName('app-dna-card'));
  }

  /**
   * Gets the Empty Dna image.
   */
  getEmptyDnaImage() {
    return element(by.css('.ant-empty .ant-empty-image'));
  }
}
