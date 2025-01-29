import { browser, ElementFinder } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { MetricsPage } from './page-objects/metrics-card.po';

const path = require('path');
const downloadsPath = path.resolve(__dirname, '../downloads');

describe('Metrics - Code Quality page test', () => {
    const appPage: AppSharedPage = new AppSharedPage();
    const metricsPage: MetricsPage = new MetricsPage();

    beforeAll(async () => {
        appPage.rmDownloadDir(downloadsPath);
        await appPage.navigateToMetricsCodeQuality();
    });

    afterAll(async() => {
        appPage.rmDownloadDir(downloadsPath);
    });

    it('should test the header name', () => {
        expect(metricsPage.getHeaderTitle().getText()).toBe(translate.metrics.codeQuality.menuItem);
    });

    it('should have a code quality item sidebar', async () => {
        expect(metricsPage.getSidebarMetricsNavigationItemTitle(7)).toBe(translate.metrics.codeQuality.menuItem);
    });

    xit('should have code quality cards', async () => {
        for (let i = 0; i < 8; i++) {
            const j = i > 4 ? i + 1 : i; // Currently one of the chart is not rendering(No data), so added this to skip that. 
            expect(await metricsPage.getMetricsCardPageTitle(i, '.code-quality-metrics').getText()).toBe(translate.metrics.codeQuality[`cardTitle${j}`]);
        }
    });

    it('should check chart data', async () => {
        const technologyChartData = metricsPage.getMetricsChartData();
        if (technologyChartData) {
            await technologyChartData.each(async(card: ElementFinder) => {
                await card.click();
                const header = await metricsPage.getHeaderTitle().getText();
                expect(await metricsPage.getMetricsDrawer().getText()).toBe(header + ': ' + translate.metrics.codeQuality.cardTitle4);
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
        const metricsFilterSearch = metricsPage.getMetricsFilter();
        if (metricsFilterSearch) {
            await metricsFilterSearch.click();
            const firstAnnotationType = metricsPage.getSelectBoxDropdownItem();
            if ((firstAnnotationType).length > 0) {
                await firstAnnotationType.get(1).click();
                browser.sleep(1000);
            }
            expect(metricsFilterSearch).toBeDefined();
        }
    });
});
