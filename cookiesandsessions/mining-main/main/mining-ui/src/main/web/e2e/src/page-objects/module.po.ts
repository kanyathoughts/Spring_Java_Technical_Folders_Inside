import { element, by, browser, ElementFinder } from 'protractor';
import { protractor } from 'protractor/built/ptor';
import { TIMEOUT } from './app-shared.po';

export const MODULE_ID = 2000;
const EC = protractor.ExpectedConditions;

export class ModulesPage {

    /**
     * Returns the number of rows in the table in current page.
     */
    async getRowsCount() {
        return await element(by.css('.ant-table-tbody')).all(by.className('ant-table-row')).count();
    }

    /**
     * Returns the table headers.
     */
    getTableHeaders() {
        return element.all(by.css('.ant-table-content thead th'));
    }

    /**
     * Returns the reference of header left section.
     */
    getHeaderLeft() {
        return element(by.css('nz-page-header .ant-page-header-heading-left'));
    }

    /**
     * Returns the module image element.
     */
    getModuleImage() {
        return this.getHeaderLeft().element(by.css('nz-avatar img'));
    }

    /**
     * Returns the reference of back button in the header.
     */
    getHeaderBackButton() {
        return this.getHeaderLeft().element(by.css('.ant-page-header-back i'));
    }

    /**
     * Returns the Header title.
     */
    getHeaderTitle() {
        return this.getHeaderLeft().element(by.tagName('nz-page-header-title'));
    }

    /**
     * Returns the Header sub-title.
     */
    getHeaderSubTitle() {
        return this.getHeaderLeft().element(by.tagName('nz-page-header-subtitle'));
    }

    /**
     * Returns the reference of code-viewer button.
     */
    getCodeViewerButton() {
        return element(by.css('nz-page-header-extra button'));
    }

    /**
     * Waits for table to load.
     */
    async waitForTableToLoad() {
        await browser.wait(EC.visibilityOf(element.all(by.css('.ant-table-tbody .ant-table-row')).first()), TIMEOUT);
    }


    /**
     * Returns the reference of tabs int the module detail page.
     */
    getTabs() {
        return element.all(by.css('nz-tabs-nav .ant-tabs-tab'));
    }

    /**
     * Returns the overview tab.
     */
    getOverviewTab() {
        return element.all(by.css('.ant-tabs-content .ant-tabs-tabpane')).get(0);
    }

    /**
     * Returns the taxonomies tab.
     */
    getTaxonomiesTab() {
        return this.getTabs().get(1);
    }

    /**
     * Returns the annotations tab.
     */
    getAnnotationsTab() {
        return this.getTabs().get(2);
    }

    /**
     * Returns the dependencies tab.
     */
    getDependenciesTab() {
        return this.getTabs().get(4);
    }

    /**
     * Returns the call chain tab.
     */
    getCallChainTab() {
        return this.getTabs().get(5);
    }

    /**
     * Returns the overview tab's characteristics section.
     */
    getOverviewTabCharacteristicsSection() {
        return this.getOverviewTab().all(by.css('nz-card')).first();
    }

    /**
     * Returns the overview tab's all characteristics.
     */
    getOverviewTabCharacteristics() {
        return this.getOverviewTabCharacteristicsSection().all(by.css('.ant-descriptions-view tr'));
    }

    /**
     * Returns the overview tab's characteristics heading.
     */
    async getOverviewTabCharacteristicsHeading() {
        return await this.getOverviewTabCharacteristicsSection().element(by.css('.ant-card-head')).getText();
    }

    /**
     * Returns the overview tab's control flow section.
     */
    getOverviewTabControlFlowSection() {
        return this.getOverviewTab().all(by.css('nz-card')).get(1);
    }

    /**
     * Returns the overview tab's control flow heading.
     */
    async getOverviewTabControlFlowHeading() {
        return await this.getOverviewTabControlFlowSection().all(by.css('.ant-col')).get(0).element(by.tagName('h4')).getText();
    }

    /**
     * Returns the overview tab's dependency graph heading.
     */
    async getOverviewTabDependencyGraphHeading() {
        return await this.getOverviewTabControlFlowSection().all(by.css('.ant-col')).get(2).element(by.tagName('h4')).getText();
    }

    /**
     * Returns the overview tab's dependency graph button.
     */
    getOverviewTabDependencyGraphButton() {
        return this.getOverviewTabControlFlowSection().all(by.css('.ant-col')).get(2).element(by.tagName('button'));
    }

    /**
     * Returns the overview tab's description section.
     */
    getOverviewTabDescriptionSection() {
        return this.getOverviewTab().all(by.css('nz-card')).get(2);
    }

    /**
     * Returns the overview tab's description heading.
     */
    async getOverviewTabDescriptionHeading() {
        return await this.getOverviewTabDescriptionSection().element(by.className('ant-card-head-title')).getText();
    }

    /**
     * Returns the overview tab's description edit button.
     */
    getOverviewTabDescriptionEditButton() {
        return this.getOverviewTabDescriptionSection().element(by.css('.ant-card-extra button'));
    }

    /**
     * Returns the overview tab's description heading.
     */
    async getOverviewTabDescriptionEditHeading() {
       return await element(by.tagName('nz-modal-container')).element(by.className('ant-modal-title')).getText();
    }

    /**
     * Returns the overview tab's description text area.
     */
    getOverviewTabDescriptionEditTextArea() {
        return element(by.tagName('nz-modal-container')).element(by.css('.modal-window textarea'));
    }

    /**
     * Returns the overview tab's description save button.
     */
    getOverviewTabDescriptionEditSaveButton() {
        return element(by.tagName('nz-modal-container')).all(by.css('.ant-modal-footer button')).first();
    }

    /**
     * Returns the overview tab's description text area content.
     */
    async getOverviewTabDescriptionEditTextAreaContent() {
        return await this.getOverviewTabDescriptionSection().element(by.className('ant-card-body')).getText();
    }

    /**
     * Navigates to taxonomies tab.
     */
    async navigateToTaxonomiesTab() {
        await this.getTaxonomiesTab().click();
        await this.waitForTableToLoad();
    }

    /**
     * Returns the taxonomies tab's table headers.
     */
    getTaxonomiesTabTableHeaders() {
       return element.all(by.css('mn-module-details-taxonomies mn-table table thead th'));
    }

    /**
     * Navigates to annotation tab.
     */
    async navigateToAnnotationsTab() {
        await this.getAnnotationsTab().click();
        await this.waitForTableToLoad();
    }

    /**
     * Returns the annotation tab's table headers.
     */
    getAnnotationsTabTableHeaders() {
       return element.all(by.css('mn-module-annotations mn-table table thead th'));
    }

    /**
     * Returns annotation tab's table edit button.
     */
    getAnnotationsTabTableActions() {
        return element.all(by.css('mn-module-annotations mn-table table tbody tr td')).last().element(by.css('button'));
    }

    /**
     * Returns annotation tab's table row content.
     */
    getAnnotationTabTableContent() {
        return element.all(by.css('mn-module-annotations mn-table table tbody tr td'));
    }

    /**
   * Gets the edit annotation dialog title in the annotation table.
   *
   * @param editAnnotationDialog The edit annotation dialog to get title from
   */
  async getEditAnnotationDialogTitle(editAnnotationDialog: ElementFinder) {
    return await editAnnotationDialog.$('.ant-drawer-header').getText();
  }

    /**
     * Navigate to dependencies tab.
     */
    async navigateToDependenciesTab() {
        await this.getDependenciesTab().click();
        await this.waitForTableToLoad();
    }

    /**
     * Returns the dependencies tab's table headers.
     */
    getDependenciesTabTableHeaders() {
       return element.all(by.css('mn-module-dependencies mn-table table thead th'));
    }

    /**
     * Navigate to dependency graph from dependencies tab.
     */
    async navigateToDgFromDependencies() {
        await element(by.css('mn-module-dependencies .dependencies__btn--right')).click();
    }

    /**
     * Navigate to call chain tab.
     */
    async navigateToCallChainTab() {
        await this.getCallChainTab().click();
    }

    /**
     * Returns the call chain items.
     */
    getCallChainItems() {
        return element(by.tagName('mn-call-chain-export')).all(by.tagName('nz-form-item'));
    }

    /**
     * Returns the call chain start value.
     */
    async getCallChainStartValue() {
        return await this.getCallChainItems().first().element(by.className('ant-select-selection-item-content')).getText();
    }

    /**
     * Returns the call chain radio buttons (inbound and outbound).
     */
    getCallChainRadioButtons() {
        return this.getCallChainItems().get(1).all(by.css('nz-radio-group label'));
    }

    /**
     * Returns the call chain radio button inbound text.
     */
    async getCallChainRadioButtonINText() {
        return await this.getCallChainRadioButtons().first().getText();
    }

    /**
     * Returns the call chain radio button outbound text.
     */
    async getCallChainRadioButtonOUTText() {
        return await this.getCallChainRadioButtons().last().getText();
    }

    /**
     * Returns the call chain end value.
     */
    async getCallChainEndValue() {
        return await this.getCallChainItems().get(2)
        .element(by.css('nz-select-top-control nz-select-item .ant-select-selection-item-content')).getText();
    }

    /**
     * Returns the call chain input field reference.
     */
    getCallChainEndInput() {
        return this.getCallChainItems().get(2).element(by.css('nz-select-top-control nz-select-search input'));
    }

    /**
     * Tests the call chain tab's Select Module section.
     */
    async testCallChainModuleSelectDialogBox() {
        const moduleSelectElement = element(by.className('call-chain__module-select-dropdown'));
        /** Wait for popup to open */
        await browser.wait(EC.visibilityOf(moduleSelectElement), TIMEOUT);

        expect(moduleSelectElement.isPresent()).toBe(true);
        expect(await moduleSelectElement.all(by.tagName('span')).first().getText()).toBe('Select module');

        const  radioButtons = moduleSelectElement.all(by.css('nz-radio-group label'));
        expect(await radioButtons.get(0).getText()).toBe('All Modules');
        expect(await radioButtons.get(1).getText()).toBe('By name');
        expect(await radioButtons.get(2).getText()).toBe('By type');

        /** Close popup */
        await browser.actions().sendKeys(protractor.Key.ESCAPE).perform();
    }

    /**
     * Returns the reference of 'Hide from call chain' element in call chain tab.
     */
    hideFromCallChainElement() {
        return this.getCallChainItems().get(4).element(by.css('nz-select-search input'));
    }

    /**
     * Returns the characteristic from the overvew tab.
     */
    async getCharacteristic(index: number) {
        return await this.getOverviewTabCharacteristics().get(index).all(by.tagName('td')).get(0).getText();
    }

    /**
     * Returns the description dialog reference element.
     */
    getDescriptionDialog() {
        return element(by.tagName('nz-modal-container'));
    }

    /**
     * Returns the Hide call chain dialog reference element.
     */
    getHideCallChainDialog() {
        return element(by.tagName('nz-option-container'));
    }

    /**
     * Returns the column filter class for modules table.
     */
    async getFilterClass(index: number) {
        return await element.all(by.tagName('nz-filter-trigger')).get(index).element(by.tagName('i')).getAttribute('class');
    }

    /**
     * navigates to details page on click of module in the modules table
     */
    async navigateToDetailsPageOnModuleClick() {
        await element(by.css('.ant-table-tbody')).all(by.className('ant-table-row')).first().element(by.css('td a')).click();
    }

    /**
     * Returns the Not referenced button in the modules page.
     */
    getNotReferencedButton() {
        return element.all(by.css('.ant-card-body button')).first();
    }
}
