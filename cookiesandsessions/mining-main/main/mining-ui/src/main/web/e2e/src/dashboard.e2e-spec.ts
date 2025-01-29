import { browser } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { DashboardPage } from './page-objects/dashboard.po';

describe('Dashboard page test', () => {
    const appPage: AppSharedPage = new AppSharedPage();
    const dashboardPage: DashboardPage = new DashboardPage();

    beforeAll(async() => {
        await appPage.navigateToDashboard();
    });

    it('should have dashboard cards', async() => {
        expect(await dashboardPage.getDashboardCardsTitle(2).getText()).toBe(translate.hotspots.mostReferencedPrograms);
        expect(await dashboardPage.getDashboardCardsTitle(3).getText()).toBe(translate.hotspots.mostFDs);
        expect(await dashboardPage.getDashboardCardsTitle(4).getText()).toBe(translate.hotspots.mostCandidateBusinessRules);
    });

    xit('should have basic cards', async() => {
        expect(await dashboardPage.getBasicCardsTitle(0).getText()).toBe(translate.projectDashboard.modules);
        expect(await dashboardPage.getBasicCardsTitle(1).getText()).toBe(translate.projectDashboard.totalSourceLinesOfCodes);
        expect(await dashboardPage.getBasicCardsTitle(2).getText()).toBe(translate.projectDashboard.topTechnologies);
        expect(await dashboardPage.getBasicCardsTitle(3).getText()).toBe(translate.lastScan.header);
    });

    xit('should navigate to other pages from dashboard', async() => {
        await dashboardPage.getSidebarTrigger().click();
        /** Metrics side bar navigation */
        await dashboardPage.getSidebarMetricsNavigationLink(0).click();
        expect(new RegExp('^.*\/project-1\/metrics/technologies$').test(await browser.getCurrentUrl())).toBe(true);

        await dashboardPage.getSidebarMetricsNavigationLink(1).click();
        expect(new RegExp('^.*\/project-1\/metrics/utilities$').test(await browser.getCurrentUrl())).toBe(true);
        
        await dashboardPage.getSidebarMetricsNavigationLink(2).click();
        expect(new RegExp('^.*\/project-1\/metrics/interfaces$').test(await browser.getCurrentUrl())).toBe(true);
        
        await dashboardPage.getSidebarMetricsNavigationLink(3).click();
        expect(new RegExp('^.*\/project-1\/metrics/sql$').test(await browser.getCurrentUrl())).toBe(true);
        
        await dashboardPage.getSidebarMetricsNavigationLink(4).click();
        expect(new RegExp('^.*\/project-1\/metrics/decomposition$').test(await browser.getCurrentUrl())).toBe(true);
        
        await dashboardPage.getSidebarMetricsNavigationLink(5).click();
        expect(new RegExp('^.*\/project-1\/metrics/code-quality$').test(await browser.getCurrentUrl())).toBe(true);
        
        await dashboardPage.getSidebarMetricsNavigationLink(6).click();
        expect(new RegExp('^.*\/project-1\/metrics/candidates$').test(await browser.getCurrentUrl())).toBe(true);
        
        /** Dashboard side bar navigation */
        await dashboardPage.getSidebarNavigationItemLink(2).click();
        expect(new RegExp('^.*\/project-1\/modules$').test(await browser.getCurrentUrl())).toBe(true);

        await dashboardPage.getSidebarNavigationItemLink(3).click();
        expect(new RegExp('^.*\/project-1\/annotations$').test(await browser.getCurrentUrl())).toBe(true);

        await dashboardPage.getSidebarNavigationItemLink(0).click();
        expect(new RegExp('^.*\/project-1\/dashboard$').test(await browser.getCurrentUrl())).toBe(true);
    });

    it('should check for the data in tables', async() => {
        /* Checks the first row of Program table */
        const progRow = await dashboardPage.getDashboardCardsTableRows(2);
        expect((await dashboardPage.getTableRowCol(progRow[0], 1).getText()).toLowerCase()).toMatch(/^basic|cobol$/);

        /* Checks the first row of Candidate table */
        const candidateRow = await dashboardPage.getDashboardCardsTableRows(4);
        expect((await dashboardPage.getTableRowCol(candidateRow[0], 1).getText()).toLowerCase()).toMatch(/^natural|cobol$/);

        /* Checks for No data msg for table FDs*/
        expect(dashboardPage.getDashboardCardsTableNoData(1).isPresent()).toBeFalsy();

        /* Checks for No data msg for table Data sets*/
        expect(dashboardPage.getDashboardCardsTableNoData(2).isPresent()).toBeFalsy();

        /* Checks for No data msg for table database tables */
        expect(dashboardPage.getDashboardCardsTableNoData(4).isPresent()).toBeFalsy();
    });

    xdescribe('Dashboard Tables Filter Test', () => {

        it('should test filters in program table', async () => {
            const progTable = await dashboardPage.getDashboardCardsTableRows(0);
            if (progTable.length > 0) {
                /** Count before filter */
                const noOfRows = await dashboardPage.getDashboardCardsTableRows(0).count();

                /** Filter for Module Coulumn */
                const moduleFilter = await dashboardPage.getDashboardCardsTableFilters(0).get(0);
                await moduleFilter.click();
                expect(await appPage.searchBox().isPresent()).toBeTruthy();
                await appPage.getSearchBoxInput().sendKeys('PRG');
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(0).count()).toBeLessThan(noOfRows);
                await moduleFilter.click();
                /** Reset Module filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(0).count()).toEqual(noOfRows);

                /** Filter for Language column */
                const languageFilter = await dashboardPage.getDashboardCardsTableFilters(0).get(1);
                await languageFilter.click();
                expect(await appPage.getFilterDropdown().isPresent()).toBeTruthy();
                await appPage.getFilterDropdown().get(0).click();
                await appPage.getFilterDropdownButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(0).count()).toBeLessThan(noOfRows);
                await languageFilter.click();
                /** Reset language filter */
                await appPage.getFilterDropdownButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(0).count()).toEqual(noOfRows);

                /** Filter for Value Column */
                const valueFilter = await dashboardPage.getDashboardCardsTableFilters(0).get(2);
                await valueFilter.click();
                expect(await appPage.getFilterInput().isPresent()).toBeTruthy();
                await appPage.getFilterInput().sendKeys(1);
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(0).count()).toBeLessThan(noOfRows);
                await valueFilter.click();
                /** Reset value filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(0).count()).toEqual(noOfRows);
            }
        });

        it('should test filters in FDs table', async () => {
            const fdsTable = await dashboardPage.getDashboardCardsTableRows(1);
            if (fdsTable.length > 0) {
                /** Count before filter */
                const noOfRows = await dashboardPage.getDashboardCardsTableRows(1).count();

                /** Filter for Module Coulumn */
                const moduleFilter = await dashboardPage.getDashboardCardsTableFilters(1).get(0);
                await moduleFilter.click();
                expect(await appPage.searchBox().isPresent()).toBeTruthy();
                await appPage.getSearchBoxInput().sendKeys('PRG');
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(1).count()).toBeLessThan(noOfRows);
                await moduleFilter.click();
                /** Reset Module filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(1).count()).toEqual(noOfRows);

                /** Filter for Language column */
                const languageFilter = await dashboardPage.getDashboardCardsTableFilters(1).get(1);
                await languageFilter.click();
                expect(await appPage.getFilterDropdown().isPresent()).toBeTruthy();
                await appPage.getFilterDropdown().get(0).click();
                await appPage.getFilterDropdownButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(1).count()).toBeLessThan(noOfRows);
                await languageFilter.click();
                /** Reset language filter */
                await appPage.getFilterDropdownButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(1).count()).toEqual(noOfRows);

                /** Filter for Type column */
                const typeFilter = await dashboardPage.getDashboardCardsTableFilters(1).get(2);
                await typeFilter.click();
                expect(await appPage.getFilterDropdown().isPresent()).toBeTruthy();
                await appPage.getFilterDropdown().get(0).click();
                await appPage.getFilterDropdownButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(1).count()).toBeLessThanOrEqual(noOfRows);
                await typeFilter.click();
                /** Reset type filter */
                await appPage.getFilterDropdownButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(1).count()).toEqual(noOfRows);

                /** Filter for Value Column */
                const valueFilter = await dashboardPage.getDashboardCardsTableFilters(1).get(3);
                await valueFilter.click();
                expect(await appPage.getFilterInput().isPresent()).toBeTruthy();
                await appPage.getFilterInput().sendKeys(1);
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(1).count()).toBeLessThanOrEqual(noOfRows);
                await valueFilter.click();
                /** Reset value filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(1).count()).toEqual(noOfRows);
            }
        });

        it('should test filters in data set table', async () => {
            const datasetTable = await dashboardPage.getDashboardCardsTableRows(2);
            if (datasetTable.length > 0) {
                /** Count before filter */
                const noOfRows = await dashboardPage.getDashboardCardsTableRows(2).count();
                /** Filter for dataSet Coulumn */
                const dataSetFilter = await dashboardPage.getDashboardCardsTableFilters(2).get(0);
                await dataSetFilter.click();
                expect(await appPage.searchBox().isPresent()).toBeTruthy();
                await appPage.getSearchBoxInput().sendKeys('PRG');
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(2).count()).toBeLessThan(noOfRows);
                await dataSetFilter.click();
                /** Reset dataSet filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(2).count()).toEqual(noOfRows);

                /** Filter for rwcount Column */
                const rwCountFilter = await dashboardPage.getDashboardCardsTableFilters(2).get(1);
                await rwCountFilter.click();
                expect(await appPage.getFilterInput().isPresent()).toBeTruthy();
                await appPage.getFilterInput().sendKeys(1);
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(2).count()).toBeLessThan(noOfRows);
                await rwCountFilter.click();
                /** Reset rwcount filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(2).count()).toEqual(noOfRows);
            }
        });

        it('should test filters in candidate table', async () => {
            const candidateTable = await dashboardPage.getDashboardCardsTableRows(3);
            if (candidateTable.length > 0) {
               /** Count before filter */
               const noOfRows = await dashboardPage.getDashboardCardsTableRows(3).count();

               /** Filter for Module Coulumn */
               const moduleFilter = await dashboardPage.getDashboardCardsTableFilters(3).get(0);
               await moduleFilter.click();
               expect(await appPage.searchBox().isPresent()).toBeTruthy();
               await appPage.getSearchBoxInput().sendKeys('PRG');
               await appPage.getSearchButtons().get(0).click();
               expect(await dashboardPage.getDashboardCardsTableRows(3).count()).toBeLessThan(noOfRows);
               await moduleFilter.click();
               /** Reset Module filter */
               await appPage.getSearchButtons().get(1).click();
               expect(await dashboardPage.getDashboardCardsTableRows(3).count()).toEqual(noOfRows);

                /** Filter for Language column */
                const languageFilter = await dashboardPage.getDashboardCardsTableFilters(3).get(1);
                await languageFilter.click();
                expect(await appPage.getFilterDropdown().isPresent()).toBeTruthy();
                await appPage.getFilterDropdown().get(0).click();
                await appPage.getFilterDropdownButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(3).count()).toBeLessThan(noOfRows);
                await languageFilter.click();
                /** Reset language filter */
                await appPage.getFilterDropdownButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(3).count()).toEqual(noOfRows);

                /** Filter for Type column */
                const typeFilter = await dashboardPage.getDashboardCardsTableFilters(3).get(2);
                await typeFilter.click();
                expect(await appPage.getFilterDropdown().isPresent()).toBeTruthy();
                await appPage.getFilterDropdown().get(0).click();
                await appPage.getFilterDropdownButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(3).count()).toBeLessThanOrEqual(noOfRows);
                await typeFilter.click();
                /** Reset type filter */
                await appPage.getFilterDropdownButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(3).count()).toEqual(noOfRows);

                /** Filter for Value Column */
                const valueFilter = await dashboardPage.getDashboardCardsTableFilters(3).get(3);
                await valueFilter.click();
                expect(await appPage.getFilterInput().isPresent()).toBeTruthy();
                await appPage.getFilterInput().sendKeys(3);
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(3).count()).toBeLessThanOrEqual(noOfRows);
                await valueFilter.click();
                /** Reset value filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(3).count()).toEqual(noOfRows);
            }
        });

        it('should test filters in database table', async () => {
            const databaseTable = await dashboardPage.getDashboardCardsTableRows(4);
            if (databaseTable.length > 0) {
                /** Count before filter */
                const noOfRows = await dashboardPage.getDashboardCardsTableRows(4).count();

                /** Filter for dataBase Coulumn */
                const dataBaseFilter = await dashboardPage.getDashboardCardsTableFilters(4).get(0);
                await dataBaseFilter.click();
                expect(await appPage.searchBox().isPresent()).toBeTruthy();
                await appPage.getSearchBoxInput().sendKeys('PRG');
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(4).count()).toBeLessThan(noOfRows);
                await dataBaseFilter.click();
                /** Reset dataBase filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(4).count()).toEqual(noOfRows);

                /** Filter for rwcount Column */
                const rwCountFilter = await dashboardPage.getDashboardCardsTableFilters(4).get(1);
                await rwCountFilter.click();
                expect(await appPage.getFilterInput().isPresent()).toBeTruthy();
                await appPage.getFilterInput().sendKeys(1);
                await appPage.getSearchButtons().get(0).click();
                expect(await dashboardPage.getDashboardCardsTableRows(4).count()).toBeLessThan(noOfRows);
                await rwCountFilter.click();
                /** Reset rwcount filter */
                await appPage.getSearchButtons().get(1).click();
                expect(await dashboardPage.getDashboardCardsTableRows(4).count()).toEqual(noOfRows);
            }
        });

        describe('should test Enhanced Taxonomies card', () => {
            beforeAll(async () => {
                await appPage.navigateToDashboard();
            });

            it('should test card title', async () => {
                expect(await dashboardPage.getDashboardCardsTitle(5).getText()).toBe(translate.projectDashboard.taxonomyCardTitle);
            });
            
            it('should have taxonomies dropdown', async () => {
               expect(await dashboardPage.selectTaxonomyDropdown(5)).toBe(translate.businessProcess);
            });
        });
    });
});
