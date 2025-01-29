import { browser } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { MetricsPage } from './page-objects/metrics-card.po';

const path = require('path');
const fs = require('fs');
const downloadsPath = path.resolve(__dirname, '../downloads');

describe('Metrics - Rule Candidates page test', () => {
    const appPage: AppSharedPage = new AppSharedPage();
    const metricsPage: MetricsPage = new MetricsPage();

    beforeAll(async () => {
        appPage.rmDownloadDir(downloadsPath);
        await appPage.navigateToMetricsRuleCandidates();
    });

    afterAll(async () => {
        appPage.rmDownloadDir(downloadsPath);
    });

    it('should test the header name', async () => {
        expect(await metricsPage.getHeaderTitle().getText()).toBe(translate.metrics.ruleCandidates.menuItem);
    });

    it('should have a rule candidates sidebar', async () => {
        expect(metricsPage.getSidebarMetricsNavigationItemTitle(8)).toBe(translate.metrics.ruleCandidates.menuItem);
    });

    it('should export single chart as PNG', async () => {
        await metricsPage.getMetricsChartExport();
        const fileName = (await metricsPage.getHeaderTitle().getText()).replace(/\s+/g, '-') + '_' +
            (await metricsPage.getMetricsCardPageTitle(0).getText()).replace(/\s+/g, '-');
        await appPage.checkExport('.png', fileName);
    });

    it('should have rule candidates card', async () => {
        if (expect((await metricsPage.getMetricsCard()).length).toBeGreaterThanOrEqual(0)) {
            expect(metricsPage.getMetricsCardTitle(0).getText()).toBe(translate.metrics.ruleCandidates.cardTitle1);
            expect(metricsPage.getMetricsCardDescription(0).getText()).toBe(translate.metrics.ruleCandidates.cardDescription1);
            expect(metricsPage.getMetricsCardTitle(1).getText()).toBe(translate.metrics.ruleCandidates.cardTitle2);
            expect(metricsPage.getMetricsCardDescription(1).getText()).toBe(translate.metrics.ruleCandidates.cardDescription2);
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
});
