import { AppSharedPage } from './page-objects/app-shared.po';
import translate from '../../src/translations/en-US.json';
import { MetricsPage } from './page-objects/metrics-card.po';
import { ElementFinder } from 'protractor';

describe('Application Decomposition page test', () => {
    const appPage: AppSharedPage = new AppSharedPage();
    const metricsPage: MetricsPage = new MetricsPage();

    beforeAll(async() => {
        await appPage.navigateToAppDecomposition();
    });

    it('should test the header name', async() => {
        expect(await metricsPage.getHeaderTitle().getText()).toBe(translate.metrics.applicationDecomposition.menuItem);
    });

    it('should have a application decomposition sidebar', async () => {
        expect(metricsPage.getSidebarMetricsNavigationItemTitle(6)).toBe(translate.metrics.applicationDecomposition.menuItem)    });

    it('should check chart data', async () => {
        const applicationChartData = metricsPage.getMetricsChartData();
        if (applicationChartData) {
            await applicationChartData.each(async (card: ElementFinder) => {
                await card.click();
                const header = await metricsPage.getHeaderTitle().getText();
                const cardTitle = await metricsPage.getMetricsCard().first().getText();
                expect(await metricsPage.getMetricsDrawer().getText()).toBe(header + ':' + cardTitle);
                const resultArray = (await metricsPage.getMetricsChartResults().getText()).split(':');
                expect(resultArray[1]).toBeGreaterThan(0);
                await appPage.checkExport('.csv');
            });
        } else {
            expect(await metricsPage.getMetricsNoData()).toBeTruthy();
        }
    });
});
