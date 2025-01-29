import { $, $$, browser, by, element, ElementFinder } from 'protractor';

/* The dashboard page object. */
export class DashboardPage {

    public static URL ='/dashboard';

    /**
     * Gets the header title element.
     */
    getHeaderTitle() {
        const header = element(by.tagName('nz-page-header'));
        return header.element(by.css('.ant-page-header-heading-title'));
    }

    /**
     * Gets the sidebar navigation element title.
     *
     * @param index index of the navigation item.
     */
    getSidebarNavigationItemTitle(index: number) {
        return this.getSidebarItems().get(index).element(by.tagName('span'));
    }

    /**
     * Gets the sidebar navigation element Link.
     *
     * @param index index of the navigation item.
     */
    getSidebarNavigationItemLink(index: number) {
        return this.getSidebarItems().get(index).element(by.tagName('a'));
    }
    
    /**
     * Gets the metrics sidebar navigation element Link.
     * @param index index of the navigation item.
     */
    getSidebarMetricsNavigationLink(index: number) {
        return this.getMetricsSidebarItems().get(index).element(by.tagName('a'));
    }

    /**
     * Gets the sidebar trigger element.
     */
    getSidebarTrigger() {
        return element(by.css('.mining-sider__trigger'));
    }

    /**
     * Gets the dashboard cards title element.
     *
     * @param index index of the card.
     */
    getDashboardCardsTitle(index: number) {
        return this.getDashboardCards().get(index).element(by.css('.ant-card-head-title'));
    }

    /**
     * Selects the taxonomy card dropdown.
     * @param index index of the card.
     */
    async selectTaxonomyDropdown(index: number) {
        await this.getDashboardCards().get(index).element(by.tagName('nz-select')).click();
        browser.sleep(1000);
        return element.all(by.css('.ant-select-item')).get(2).getText();
    }

    /**
     * Gets the dashboard cards table rows.
     *
     * @param index index of the card.
     */
    getDashboardCardsTableRows(index: number) {
        return this.getDashboardCards().get(index).all(by.css('.ant-card-body .ant-table-tbody .ant-table-row'));
    }

    /**
     * Gets the dashboard cards table filters.
     * @param index index of the card.
     */
    getDashboardCardsTableFilters(index: number) {
        return this.getDashboardCards().get(index).element(by.css('.ant-table-row')).all(by.css('.ant-table-filter-trigger-container'));
    }

    /**
     * Gets the column for table row.
     *
     * @param ele table row element.
     * @param index index of the column.
     */
    getTableRowCol(ele: ElementFinder, index: number) {
        return ele.$$('.ant-table-cell').get(index).$('span');
    }

    /**
     * Gets the dashboard cards table No Data Message.
     *
     * @param index index of the card.
     */
    getDashboardCardsTableNoData(index: number) {
        return this.getDashboardCards().get(index).element(by.css('.ant-card-body > app-message > .mining-msg > .mining-msg__noDataPlaceholder > p'));
    }

    /**
     * Gets the dashboard basic cards details.
     *
     * @param index index of the card.
     */
    getBasicCardsTitle(index: number) {
        return this.getBasicCards().get(index).element(by.css('.ant-statistic-title'));
    }

    private getDashboardCards() {
        return element(by.tagName('nz-list')).all(by.css('.ant-list-item nz-card'));
    }

    private getSidebarItems() {
        return element(by.tagName('nz-sider')).all(by.css('.project-shell__nav-sider__content > li'));
    }

    private getMetricsSidebarItems() {
        this.getSidebarItems().get(1).click();
        return this.getSidebarItems().get(1).all(by.css('.ng-star-inserted > li'));
    }

    private getBasicCards() {
        return element.all(by.css('.basic-fact__margin > nz-col'));
    }
    
}
