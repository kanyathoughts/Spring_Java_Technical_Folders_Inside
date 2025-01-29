import { browser, ElementFinder } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { MetricsPage } from './page-objects/metrics-card.po';

const path = require('path');
const downloadsPath = path.resolve(__dirname, '../downloads');

describe('Metrics - Sql Decomposition page test', () => {
    const appPage: AppSharedPage = new AppSharedPage();
    const metricsPage: MetricsPage = new MetricsPage();

    beforeAll(async () => {
        appPage.rmDownloadDir(downloadsPath);
        await appPage.navigateToMetricsSqlDecomposition();
    });

    afterAll(async () => {
        appPage.rmDownloadDir(downloadsPath);
    });

    it('should test the header name', async () => {
        expect(await metricsPage.getHeaderTitle().getText()).toBe(translate.metrics.sqlDecomposition.menuItem);
    });

    it('should have a sql decomposition sidebar', async () => {
        expect(metricsPage.getSidebarMetricsNavigationItemTitle(4)).toBe(translate.metrics.sqlDecomposition.menuItem);    
    });

    it('should have sql-decomposition cards', async () => {
        const sqlCards = metricsPage.getMetricsCard();
        await sqlCards.each((card: ElementFinder) => {
            expect(card.getText()).toBe(translate.metrics.sqlDecomposition.card1Title);
        });
    });

    it('should check chart data', async () => {
      const noData = metricsPage.getMetricsNoData().isPresent();
        if (noData) {
          expect(noData).toBeTruthy();
        } else {
          const SQLChartData = metricsPage.getMetricsChartData();
          await SQLChartData.each(async(card: ElementFinder) => {
            await card.click();
            const header = await metricsPage.getHeaderTitle().getText();
            expect(await metricsPage.getMetricsDrawer().getText()).toBe(header + ': ' + translate.metrics.sqlDecomposition.card1Title);
            const resultArray = (await metricsPage.getMetricsChartResults().getText()).split(':');
            expect(resultArray[1]).toBeGreaterThanOrEqual(0);
            await appPage.checkExport('.csv');
          });
          await metricsPage.getMetricsDrawerBackButton().click();
        }
    });

    it('should have Metrics Filter', async () => {
        const metricsFilterSearch = metricsPage.getMetricsFilter();
        expect(metricsFilterSearch.isPresent()).toBeTruthy();
        await metricsFilterSearch.click();
        const firstAnnotationType = metricsPage.getSelectBoxDropdownItem();
        if ((firstAnnotationType).length > 0) {
            await firstAnnotationType.get(1).click();
            browser.sleep(1000);
        }
        expect(metricsFilterSearch).toBeDefined();
    });

    it('should export single chart as PNG', async () => {
        const noData = metricsPage.getMetricsNoData().isPresent();
        if (noData) {
          expect(noData).toBeTruthy();
        } else {
          await metricsPage.getMetricsChartExport();
          const fileName = (await metricsPage.getHeaderTitle().getText()).replace(/\s+/g, '-') + '_' +
            (await metricsPage.getMetricsCardPageTitle(0).getText()).replace(/\s+/g, '-');
          await appPage.checkExport('.png', fileName);
        }
    });
});
