
import { AppSharedPage } from './page-objects/app-shared.po';
import { TaxonomyConfigurationPage } from './page-objects/taxonomy-configuration.po';
import translate from '../../src/translations/en-US.json';

const path = require('path');
const fs = require('fs');
const downloadsPath = path.resolve(__dirname, '../downloads');

describe('Taxonomy Configuration Page Test', () => {
  const taxonomyConfigurationPage: TaxonomyConfigurationPage = new TaxonomyConfigurationPage();
  const appPage: AppSharedPage = new AppSharedPage();

  beforeAll(async () => {
    await taxonomyConfigurationPage.navigateToTaxonomies(1);
    appPage.rmDownloadDir(downloadsPath);
  });

  afterAll(async () => {
    appPage.rmDownloadDir(downloadsPath);
  });

  it('should test import buttons', async () => {
    const importButton = await taxonomyConfigurationPage.getImportExportButtons(0);
    expect(importButton.isPresent()).toBeTruthy();
    expect(importButton.getText()).toBe('Import Assignments');
  });

  it('should test export buttons', async () => {
    const exportButton = await taxonomyConfigurationPage.getImportExportButtons(1);
    expect(exportButton.isPresent()).toBeTruthy();
    expect(exportButton.getText()).toBe('Export All Assignments');
  });

  it('should test icon', async () => {
    const exportButton = await taxonomyConfigurationPage.getQuestionCircleIcon();
    expect(exportButton.isPresent()).toBeTruthy();
  });

  it('should test header title', async () => {
    expect(await appPage.currentPageHeader().getText()).toBe(translate.navigation.configuration);
  });
});
