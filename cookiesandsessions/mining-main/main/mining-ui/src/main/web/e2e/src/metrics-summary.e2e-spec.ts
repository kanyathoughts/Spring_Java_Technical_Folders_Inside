import { browser, ElementFinder } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { MetricsPage } from './page-objects/metrics-card.po';

describe('Metrics - Summary page test', () => {
  const appPage: AppSharedPage = new AppSharedPage();
  const metricsPage: MetricsPage = new MetricsPage();

  beforeAll(async () => {
    await appPage.navigateToMetricsSummary();
  });

  it('should test the header name', () => {
    expect(metricsPage.getHeaderTitle().getText()).toBe(translate.metrics.summary.menuItem);
  });

  it('should have a  Summary item sidebar', async () => {
    expect(metricsPage.getSidebarMetricsNavigationItemTitle(0)).toBe(translate.metrics.summary.menuItem);
  });

  it('should have summary cards', async () => {
    const summaryCards = metricsPage.getMetricsCard();
    await summaryCards.each((card: ElementFinder) => {
      expect(card.getText()).toBe(translate.metrics.summary.cardTitle);
    });
  });

  it('should have summary table', async () => {
    expect(metricsPage.getTable().isPresent()).toBeTruthy();
  });

  it('should check for the data in tables', async() => {
    expect(await metricsPage.getSummaryTableRows().isPresent()).toBeTruthy;
    expect(await metricsPage.getSummaryTableNoData().isPresent()).toBeFalsy;
  });

  it('should check if data is sorted using Lines of Code', async() => {
    const data = await metricsPage.getSummaryTableRows();
    if (data) {
      expect(await metricsPage.getSummaryTableHeader(0).get(4).getAttribute('class')).toContain("ant-table-column-has-sorters ant-table-column-sort ant-table-cell ant-table-cell-ellipsis ng-star-inserted");
    }
  });

});
