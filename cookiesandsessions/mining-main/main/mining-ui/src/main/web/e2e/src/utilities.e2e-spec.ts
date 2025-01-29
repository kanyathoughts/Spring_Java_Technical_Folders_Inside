import { browser, ElementFinder } from 'protractor';
import { AppSharedPage } from './page-objects/app-shared.po';
import translate from '../../src/translations/en-US.json';
import { MetricsPage } from './page-objects/metrics-card.po';

const path = require('path');
const downloadsPath = path.resolve(__dirname, '../downloads');

describe('Utilities page test', () => {
    const appPage: AppSharedPage = new AppSharedPage();
    const metricsPage: MetricsPage = new MetricsPage();

    beforeAll(async () => {
        appPage.rmDownloadDir(downloadsPath);
        await appPage.createProjectConfigurations();
        await appPage.navigateToUtilities(1);
    });

    afterAll(async () => {
        appPage.rmDownloadDir(downloadsPath);
    });

    it('should test the header name', async () => {
        expect(await metricsPage.getHeaderTitle().getText()).toBe(translate.metrics.utilities.menuItem);
    });

    it('should have Utilities cards', async () => {
        if (expect((await metricsPage.getMetricsCard()).length).toBeGreaterThanOrEqual(0)) {
            expect(await metricsPage.getMetricsCardPageTitle(0, '.utilities-metrics').getText()).toBe(translate.metrics.utilities.utilityInvocation);
            expect(await metricsPage.getMetricsCardPageTitle(1, '.utilities-metrics').getText()).toBe(translate.metrics.utilities.utilityCategories);
        }
    });

    it('should export single chart as PNG', async () => {
        if (expect((await metricsPage.getMetricsCard()).length).toBeGreaterThanOrEqual(0)) {
            await metricsPage.getMetricsChartExport();
            const fileName = (await metricsPage.getHeaderTitle().getText()).replace(/\s+/g, '-') + '_' +
                (await metricsPage.getMetricsCardPageTitle(0).getText()).replace(/\s+/g, '-');
            await appPage.checkExport('.png', fileName);
        }
    });

    xit('should have Metrics Filter', async () => {
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

    it('should check chart data', async () => {
        const noData = metricsPage.getMetricsNoData();
        if ( ! noData) {
            const utilitiesChartData = metricsPage.getMetricsChartData();
            await utilitiesChartData.each(async (card: ElementFinder) => {
                await card.click();
                const header = await metricsPage.getHeaderTitle().getText();
                expect(await metricsPage.getMetricsDrawer().getText()).toBe(header + ': ' + translate.metrics.utilities.utilityInvocation);
                const filterText = 'Category:File Manipulation';
                const filterDataArray = (await metricsPage.getMetricsChartFilters().getText()).split('\n');
                expect(filterDataArray[1].concat(filterDataArray[2])).toBe(filterText);
                const resultArray = (await metricsPage.getMetricsChartResults().getText()).split(':');
                expect(resultArray[1]).toContain('CSV');
            });
            await metricsPage.getMetricsDrawerBackButton().click();
        } else {
            expect(noData.isPresent()).toBeFalsy();
        }
    });

    it('should check for empty data', async () => {
      await appPage.navigateToUtilities(2);
      const noData = metricsPage.getMetricsNoData().isPresent();
      expect(noData).toBeTruthy();
    });
});
