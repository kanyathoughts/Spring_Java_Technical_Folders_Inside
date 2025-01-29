import { browser } from 'protractor';
import { AppSharedPage } from './page-objects/app-shared.po';
import { CustomizableTable } from './page-objects/customizable-table.po';

const path = require('path');
const fs = require('fs');
const downloadsPath = path.resolve(__dirname, '../downloads');

describe('Customizable Annotation Table Test', () => {
  const appPage: AppSharedPage = new AppSharedPage();
  const customizableAnnotation: CustomizableTable = new CustomizableTable();

  beforeAll(async () => {
    await appPage.navigateToCustomizableAnnotation();
    appPage.rmDownloadDir(downloadsPath);
  });

  afterAll(async () => {
    appPage.rmDownloadDir(downloadsPath);
  });

  it('should test the filter', async () => {
    // Clicking on search filter.
    const searchFilter = await customizableAnnotation.getFilterIcon(0);
    await searchFilter.click();
    const searchField = await customizableAnnotation.getFilterSearchBox();
    await searchField[0].sendKeys('PRG');
    const searchButton = await customizableAnnotation.getSearchButton(1);
    await searchButton.click();
    expect(await browser.getCurrentUrl()).toContain('&filter=%5B%7B%22key%22:%22module.name%22,%22value%22:%5B%22PRG%22%5D%7D%5D');
    await searchFilter.click();
    // Resetting the filter.
    const resetButton = await customizableAnnotation.getSearchButton(0);
    await resetButton.click();
     // Clicking on multi-select filter.
    const multiSelectFilter = await customizableAnnotation.getMultiselectHeader();
    await multiSelectFilter.click();
    const multiSelectCheckBox = await customizableAnnotation.getMultiSelectCheckBox();
    await multiSelectCheckBox.click();
    const okButton = await customizableAnnotation.getSearchButton(1);
    await okButton.click();
    expect(await browser.getCurrentUrl()).toContain('page=1&sort=%7Bcontent_module_name:%20ASC%7D&filter=%5B%7B%22key%22:%22type%22');
  });

  it('should check sorting', async () => {
    const sorter = await customizableAnnotation.getSortIcon();
    await sorter[1].click();
    expect(await browser.getCurrentUrl()).toContain('DESC');
    await sorter[0].click();
    expect(await browser.getCurrentUrl()).toContain('content_module_name');
  });

  it('should test pagination', async () => {
    const paginationNextIcon = await customizableAnnotation.getPaginationIcon('ant-pagination-next');
    if (paginationNextIcon && paginationNextIcon.length) {
      await paginationNextIcon[0].click();
      expect(await browser.getCurrentUrl()).toContain('page=2');
      const paginationPrevIcon = await customizableAnnotation.getPaginationIcon('ant-pagination-prev');
      await paginationPrevIcon[0].click();
      expect(await browser.getCurrentUrl()).toContain('page=1');
    }
  });

  it('should add new columns to the table', async () => {
    // Adding single column.
    const settingsIcon = await customizableAnnotation.getHeaderButtons(0);
    await settingsIcon.click();
    const columnCheckBox = await customizableAnnotation.getColumnCheckBox(5);
    await columnCheckBox.click();
    expect(await browser.getCurrentUrl()).toContain('columns=Annotation.sourceAttachment');
    // Adding multiple columns by clicking on parent checkbox.
    const parentCheckBox = await customizableAnnotation.getColumnCheckBox(0);
    await parentCheckBox.click();
    expect(await browser.getCurrentUrl()).toContain('columns=Module.technology&columns=Module.type');
  });

  it('should test CSV export', async () => {
    let downloadedFile = null;
    const CSVExportButton = await customizableAnnotation.getHeaderButtons(1);
    await CSVExportButton.click();
    await customizableAnnotation.getAnchorTag().click();
    const filesArray = await fs.readdirSync('e2e/downloads');
    downloadedFile = (filesArray && filesArray.length) > 0 ? 'e2e/downloads' + '/' + filesArray[0] : null;
    const response = await fs.existsSync(downloadedFile);
    if (filesArray && filesArray.length > 0) {
      expect(response).toBeTruthy();
      expect(downloadedFile).toContain('.csv');
    } else {
      expect(response).toBeFalsy();
    }
  });
});
