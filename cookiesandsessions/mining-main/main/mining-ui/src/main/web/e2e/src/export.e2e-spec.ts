import { browser } from 'protractor';
import { AppSharedPage } from './page-objects/app-shared.po';
import { ExportPage } from './page-objects/export.po';

const path = require('path');
const downloadsPath = path.resolve(__dirname, '../downloads');
const EC = browser.ExpectedConditions;


xdescribe('export page test', () => {
  const exportPage: ExportPage= new ExportPage();
  const appPage: AppSharedPage = new AppSharedPage();

  const waitCondition = EC.and(
    EC.visibilityOf(appPage.getSuccessMessage()),
    // EC.not(EC.presenceOf(appPage.getSuccessMessage()))
  );

  beforeAll(async () => {
    await exportPage.navigateTo();
  });

  beforeEach(() =>{
    appPage.rmDownloadDir(downloadsPath);
  });

  afterAll(() =>{
    appPage.rmDownloadDir(downloadsPath);
  });

  it('should export the csv', () => {
    exportPage.getButtonLink(0).click().then(() => {
      browser.wait(waitCondition);
      appPage.checkExport('.csv');
    });
  });

  it('should export the excel', async () => {
    const btnLink = exportPage.getButtonLink(1);
    expect(btnLink.getText()).toBe('Download Excel');
    await btnLink.click();
    browser.wait(EC.visibilityOf(appPage.getSuccessMessage()));
    expect(appPage.getSuccessMessage().getText()).toBe('exported successfully');
    browser.wait(EC.not(EC.presenceOf(appPage.getSuccessMessage())));
    browser.sleep(10000);
    appPage.checkExport('.xlsx');
  });

  it('should export the effort summary excel', () => {
    exportPage.getButtonLink(2).click().then(() => {
      browser.wait(waitCondition);
      appPage.checkExport('.xls');
    });
  });

  it('should export the Discovery Dna', async () => {
    await exportPage.getButtonLink(3).click();
    browser.wait(waitCondition);
    appPage.checkExport('.zip', 'Dna_files');
  });

  it('should export the graphml', async () => {
    await exportPage.getButtonLink(4).click();
    browser.wait(waitCondition);
    appPage.checkExport('.graphml');
  });
});