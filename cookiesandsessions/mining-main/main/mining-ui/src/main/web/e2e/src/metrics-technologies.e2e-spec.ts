import { browser, ElementFinder } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { MetricsPage } from './page-objects/metrics-card.po';

const path = require('path');
const downloadsPath = path.resolve(__dirname, '../downloads');

describe('Metrics - Technologies page test', () => {
  const cardTitle = ['BASIC', 'COBOL', 'JCL', 'NATURAL'];
  const appPage: AppSharedPage = new AppSharedPage();
  const metricsPage: MetricsPage = new MetricsPage();

  beforeAll(async () => {
    appPage.rmDownloadDir(downloadsPath);
    await appPage.navigateToMetricsTechnologies();
  });

  afterAll(async() => {
    appPage.rmDownloadDir(downloadsPath);
  });

  it('should test the header name', async () => {
    expect(await metricsPage.getHeaderTitle().getText()).toBe(translate.metrics.technologies.menuItem);
    browser.sleep(1000);
  });

  it('should have a Technologies item sidebar', async () => {
    expect(metricsPage.getSidebarMetricsNavigationItemTitle(1)).toBe(translate.metrics.technologies.menuItem);
  });

  it('should have technology cards', async () => {
    const technologyCards = metricsPage.getMetricsCard();
    if (technologyCards) {
      await technologyCards.each((card: ElementFinder, index: number) => {
        expect(card.getText()).toBe(cardTitle[index]);
      });
    }
  });

  xit('should check chart data', async () => {
    const technologyChartData = metricsPage.getMetricsChartData();
    if (technologyChartData) {
      await technologyChartData.each(async(card: ElementFinder) => {
        await card.click();
        const header = await metricsPage.getHeaderTitle().getText();
        const metricsCardTitle = cardTitle[0].charAt(0) + cardTitle[0].substr(1).toLowerCase();
        expect(await metricsPage.getMetricsDrawer().getText()).toBe(header + ': ' + metricsCardTitle);
        const filterDataArray = (await metricsPage.getMetricsChartFilters().getText()).split('\n');
        expect(filterDataArray[1].concat(filterDataArray[2])).toBe('Technology:' + cardTitle[0]);
        const resultArray = (await metricsPage.getMetricsChartResults().getText()).split(':');
        expect(resultArray[1]).toBeGreaterThan(0);
        await appPage.checkExport('.csv');
      });
      await metricsPage.getMetricsDrawerBackButton().click();
    } else {
        expect( await metricsPage.getMetricsNoData()).toBeTruthy();
    }
  });

  it('should export single chart as PNG', async () => {
    await metricsPage.getMetricsChartExport();
    const fileName = (await metricsPage.getHeaderTitle().getText()).replace(/\s+/g, '-') + '_' +
      (await metricsPage.getMetricsCardPageTitle(0).getText()).replace(/\s+/g, '-');
    await appPage.checkExport('.png', fileName);
  });

  it('should have Metrics Filter', async () => {
    const metricsFilterSearch =  metricsPage.getMetricsFilter();
    await metricsFilterSearch.click();
    const firstAnnotationType = metricsPage.getSelectBoxDropdownItem();
    if ((firstAnnotationType).length > 0) {
      await firstAnnotationType.get(1).click();
      browser.sleep(1000);
    }
    expect(metricsFilterSearch).toBeDefined();
  });
});
