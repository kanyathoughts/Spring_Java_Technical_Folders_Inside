import { browser, ElementFinder } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { MetricsPage } from './page-objects/metrics-card.po';

const path = require('path');
const downloadsPath = path.resolve(__dirname, '../downloads');

describe('Metrics - Interfaces page test', () => {
    const appPage: AppSharedPage = new AppSharedPage();
    const metricsPage: MetricsPage = new MetricsPage();

    beforeAll(async () => {
        appPage.rmDownloadDir(downloadsPath);
        await appPage.navigateToMetricsInterfaces();
    });

    afterAll(async() => {
        appPage.rmDownloadDir(downloadsPath);
    });

    it('should test the header name', async () => {
        expect(await metricsPage.getHeaderTitle().getText()).toBe(translate.metrics.interfaces.menuItem);
    });

    it('should have a interfaces sidebar', async () => {
        expect(metricsPage.getSidebarMetricsNavigationItemTitle(3)).toBe(translate.metrics.interfaces.menuItem);
    });

    it('should have interface card', async () => {
       if (expect((await metricsPage.getMetricsCard()).length).toBeGreaterThanOrEqual(0)) {
           expect(await metricsPage.getMetricsCardTitle(0).getText()).toBe(translate.metrics.interfaces.cardTitle);
           expect(await metricsPage.getMetricsCardDescription(0).getText()).toBe(translate.metrics.interfaces.cardDescription);
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
        const metricsFilterSearch =  metricsPage.getMetricsFilter();
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
        if(!noData) {
            const interfaceChartData = metricsPage.getMetricsChartData();
            if (interfaceChartData) {
                await interfaceChartData.each(async(card: ElementFinder) => {
                    await card.click();
                    const header = await metricsPage.getHeaderTitle().getText();
                    expect(await metricsPage.getMetricsDrawer().getText()).toBe(header + ':' + translate.metrics.interfaces.cardTitle);
                    const filterText = 'Category: IMS region controller';
                    const filterDataArray = (await metricsPage.getMetricsChartFilters().getText()).split('\n');
                    expect(filterDataArray[1].concat(filterDataArray[2])).toBe(filterText);
                    const resultArray = (await metricsPage.getMetricsChartResults().getText()).split(':');
                    expect(resultArray[1]).toBeGreaterThan(0);
                });
            } else {
                expect(interfaceChartData.isPresent()).toBeFalsy();
            }
            await metricsPage.getMetricsDrawerBackButton().click();
        } else {
            expect(noData.isPresent()).toBeFalsy();
        }
    });
});
