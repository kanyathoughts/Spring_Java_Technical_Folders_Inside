import { $$, by, element } from 'protractor';

/* The Metrics page object. */
export class MetricsPage {

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
    getSidebarMetricsNavigationItemTitle(testIndex: number) {
        let subMenuItem = element.all(by.css('nz-sider .project-shell__nav-sider__content .ant-menu-submenu-selected'));
        subMenuItem.click();
        const allSubMenus = element.all(by.css('nz-sider .project-shell__nav-sider__content .ant-menu-submenu .ant-menu-item'));
        let filteredElements = allSubMenus.filter(async (element, index) => {
            return index === testIndex;
        });
        return filteredElements.getText();
    }

    /**
     * Gets the Metrics cards title element.
     *
     * @param index index of the card.
     */
    getMetricsCardTitle(index: number) {
        return this.getMetricsCard().get(index).element(by.css('.ant-card-head-title'));
    }

    /**
     * Get the metrics card title present on the metrics page.
     *
     * @param index index of the card.
     * @param className class for particular page card
     */
    getMetricsCardPageTitle(index: number, className?: string) {
        if (className) {
            return this.getMetricsPageCard(className).get(index).element(by.css('.ant-card-head-title'));
        } else {
            return element.all(by.css('app-metrics-card')).get(index).element(by.css('.ant-card-head-title'));
        }
    }

    /**
     * Exports the single metrics chart.
     */
    async getMetricsChartExport() {
        await element.all(by.css('app-metrics-card')).first().element(by.css('.ant-card-head-wrapper .ant-card-extra a')).click();
    }

    /**
     * method to select the filter;
     */
    getMetricsFilter() {
        return element(by.css('.taxonomy-filter__content-width > .ant-select-selector'));
    }

    /**
     * Gets the filter options and select.
     */
    getSelectBoxDropdownItem() {
        return $$('.ant-select-tree-list > div .ant-select-tree-list-holder-inner nz-tree-node-title');
    }

    /**
     * Gets the no data message in the metrics interface page
     */
    getMetricsInterfaceMessage() {
        return element(by.css('app-interfaces app-message'));
    }

    /**
     * Gets the Metrics cards title element.
     *
     * @param index index of the card.
     */
    getMetricsCardDescription(index: number) {
        return this.getMetricsCardBody().get(index).element(by.css('.kpi-metrics__description'));
    }

    getNoDataFound() {
        const message =  element(by.css('.mining__content-margin')).all(by.tagName('app-message'));
        return message.element(by.css('.mining-msg__noDataPlaceholder'));
    }

    /**
     * Gets the Metrics cards.
     */
    getMetricsCard() {
        return element(by.css('.mining__content-margin')).all(by.tagName('app-metrics-card'));
    }

    /**
     * Gets the Metrics chart data.
     */
    getMetricsChartData() {
        return element(by.tagName('app-metrics-card')).all(by.css('.ant-card-hoverable canvas'));
    }

    /**
     * Gets the Metrics drawer.
     */
    getMetricsDrawer() {
        return element(by.css('div .ant-drawer-open .ant-drawer-title .ant-page-header-heading-title'));
    }

    /**
     * Gets the Metrics CSV export.
     */
    async getMetricsCSV() {
        return element(by.css('div .ant-drawer-body .ant-row .ant-btn-link'));
    }

    /**
     * Gets the Chart filters.
     */
    getMetricsChartFilters() {
        return element(by.css('div .ant-drawer-open .ant-drawer-body .ant-space-vertical .ant-space-horizontal'));
    }

    /**
     * Gets the metrics card with no data card.
     */
    getMetricsNoCardData() {
        return element(by.tagName('app-metrics-card')).all(by.css('.ant-card .kpi-metrics__description'));
    }

    /**
     * Gets the metrics page without data.
     */
    getMetricsNoData() {
        return element(by.tagName('nz-content')).all(by.css('.mining-msg__noDataPlaceholder'));
    }

    /**
     * Gets the Chart Results.
     */
    getMetricsChartResults() {
        return element(by.css('div .ant-table .mining-table__header-row'));
    }

    /**
     * Get the metrics page card depending upon the class
     *
     * @param className class for which , card is needed.
     */

    getMetricsPageCard(className: string) {
        return element(by.css(className)).all(by.tagName('app-metrics-card'));
    }

    private getMetricsCardBody() {
        return element(by.tagName('nz-card')).all(by.css('.ant-card-body'));
    }

    getSidebarItems() {
        return element(by.tagName('nz-sider')).all(by.css('.project-shell__nav-sider__content > li'));
    }

    /**
     * Gets the Metrics table.
     */
    getTable() {
        return element(by.tagName('nz-table')).all(by.css('.ant-table table'));
    }

    /**
     * Gets the summary cards table rows.
     */
    getSummaryTableRows() {
        return this.getTable().all(by.css('.ant-card-body .ant-table-tbody .ant-table-row'));
    }

    /**
     * Gets the table when no data is present.
     */
    getSummaryTableNoData() {
        return this.getTable().all(by.css('.ant-card-body .ant-table-tbody .ant-table-placeholder'));
    }

    /**
     * Gets the summary cards table header.
     *
     * @param index index of the header.
     */
    getSummaryTableHeader(index: number) {
        return this.getTable().get(index).all(by.css('.ant-card-body .ant-table-thead th'));
    }

    /**
     * Gets the Drawer back button.
     */
    getMetricsDrawerBackButton() {
        return element(by.css('nz-page-header .ant-page-header-heading-left .ant-page-header-back i'));
    }

}
