import { ExpectedConditions, browser, by, element, protractor } from 'protractor';
import { AppSharedPage } from './page-objects/app-shared.po';
import { CustomizableTable } from './page-objects/customizable-table.po';

const path = require('path');
const fs = require('fs');
const downloadsPath = path.resolve(__dirname, '../downloads');
const columnNames = ['Number of Annotations', 'File Identified / File Missing', 'In Codebase'];

describe('Customizable Modules Table Test', () => {
  const appPage: AppSharedPage = new AppSharedPage();
  const customizableModules: CustomizableTable = new CustomizableTable();

  beforeAll(async () => {
    await appPage.navigateToCustomizableModules();
    appPage.rmDownloadDir(downloadsPath);
  });

  afterAll(async () => {
    appPage.rmDownloadDir(downloadsPath);
  });

  xit('should test the filter', async () => {
    // Clicking on search filter.
    const searchFilter = await customizableModules.getFilterIcon(0);
    await searchFilter.click();
    const searchField = await customizableModules.getFilterSearchBox();
    await searchField[0].sendKeys('Basic');
    const searchButton = await customizableModules.getSearchButton(1);
    await searchButton.click();
    expect(await browser.getCurrentUrl()).toContain('&filter=%5B%7B%22key%22:%22name%22,%22value%22:%5B%22Basic%22%5D%7D%5D');
    await searchFilter.click();
    // Resetting the filter.
    const resetButton = await customizableModules.getSearchButton(0);
    await resetButton.click();
     // Clicking on multi-select filter.
    const multiSelectFilter = await customizableModules.getFilterIcon(1);
    await multiSelectFilter.click();
    const multiSelectCheckBox = await customizableModules.getMultiSelectCheckBox();
    await multiSelectCheckBox.click();
    const okButton = await customizableModules.getSearchButton(1);
    await okButton.click();
    expect(await browser.getCurrentUrl()).toContain('&filter=%5B%7B%22key%22:%22objectTypeLink.technologyLink%22,%22value%22:%5B%22BASIC%22%5D%7D%5D');
    await multiSelectFilter.click();
    // Resetting the filter.
    await resetButton.click();
  });

  xit('should check sorting', async () => {
    const sorter = await customizableModules.getSortIcon();
    await sorter[0].click();
    expect(await browser.getCurrentUrl()).toContain('sort=None');
    await sorter[0].click();
    expect(await browser.getCurrentUrl()).toContain('sort=name;DESC');
    await sorter[0].click();
    expect(await browser.getCurrentUrl()).toContain('sort=name;ASC');
  });

  xit('should test pagination', async () => {
    const paginationNextIcon = await customizableModules.getPaginationIcon('ant-pagination-next');
    await paginationNextIcon[0].click();
    expect(await browser.getCurrentUrl()).toContain('page=2');
    const paginationPrevIcon = await customizableModules.getPaginationIcon('ant-pagination-prev');
    await paginationPrevIcon[0].click();
    expect(await browser.getCurrentUrl()).toContain('page=1');
  });

  xit('should add new columns to the table', async () => {
    // Adding single column.
    const settingsIcon = await customizableModules.getHeaderButtons(0);
    await settingsIcon.click();
    const columnCheckBox = await customizableModules.getColumnCheckBox(1);
    await columnCheckBox.click();
    expect(await customizableModules.getTableHeader(1).getText()).toBe(columnNames[0]);
    // Adding multiple columns by clicking on parent checkbox.
    const parentCheckBox = await customizableModules.getColumnCheckBox(2);
    await parentCheckBox.click();
    expect(await customizableModules.getTableHeader(2).getText()).toBe(columnNames[1]);
    expect(await customizableModules.getTableHeader(3).getText()).toBe(columnNames[2]);
  });

  xit('should test CSV export', async () => {
      const loader = element(by.css('.loader-component'));
      if(loader.isPresent()) {
      browser.wait(ExpectedConditions.invisibilityOf(loader), 5000, 'Loader should disappear');
      }
      let downloadedFile = null;
      const CSVExportButton = await customizableModules.getHeaderButtons(1);
      if(CSVExportButton) {
      await CSVExportButton.click();
      await customizableModules.getAnchorTag().click();
      const filesArray = await fs.readdirSync('e2e/downloads');
      downloadedFile = (filesArray && filesArray.length) > 0 ? 'e2e/downloads' + '/' + filesArray[0] : null;
      const response = await fs.existsSync(downloadedFile);
      if (filesArray && filesArray.length > 0) {
        expect(response).toBeTruthy();
        expect(downloadedFile).toContain('.csv');
      } else {
        expect(response).toBeFalsy();
      }
    }
  });

});
