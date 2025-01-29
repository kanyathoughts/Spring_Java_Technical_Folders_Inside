import { browser, protractor } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { ModulesPage } from './page-objects/module.po';
import { MODULE_ID } from './page-objects/module.po';
import { TIMEOUT } from './page-objects/app-shared.po';
import { AnnotationEditorPage } from './page-objects/shared-annotation-editor.po';

const EC = protractor.ExpectedConditions;

xdescribe('Modules Details page test', () => {
    const modulesDetailsPage: ModulesPage = new ModulesPage();
    const appPage: AppSharedPage = new AppSharedPage();
    const annotationEditorPage = new AnnotationEditorPage;

    beforeAll(async() => {
        /* Navigate to module details via modules */
        await appPage.navigateToModuleDetails(MODULE_ID);
    });

    beforeEach(async() => {
        browser.manage().window().maximize();
        browser.manage().timeouts().implicitlyWait(80000);
        await appPage.navigateToModuleDetails(MODULE_ID);
    });

    it('should test the header', async () => {
        /** Test Header left */
        expect(await modulesDetailsPage.getHeaderBackButton().getAttribute('class')).toMatch('anticon-arrow-left');
        expect(await modulesDetailsPage.getModuleImage().getAttribute('src')).toContain('/assets/icons/32x32_natural_program.png');
        expect(await modulesDetailsPage.getHeaderTitle().getText()).toBe('PRG1');
        expect(await modulesDetailsPage.getHeaderSubTitle().getText()).toBe('Natural Program');

        /** Test Header right */
        expect(await modulesDetailsPage.getCodeViewerButton().getText()).toBe('Icon/openCode Code Viewer');
    });

    it('should test back click', async () => {
        /** Back button click */
        await modulesDetailsPage.getHeaderBackButton().click();
        expect(await appPage.isModulesPage()).toBe(true);

        await appPage.navigateToModuleDetails(MODULE_ID);
        expect(await appPage.isModuleDetailPage(MODULE_ID)).toBe(true);
    });

    it('should test code viewer click', async () => {
        /** Code Viewer button click */
        await modulesDetailsPage.getCodeViewerButton().click();
        expect(await appPage.isCodeViewerPage(MODULE_ID)).toBe(true);

        await appPage.navigateToModuleDetails(MODULE_ID);
        expect(await appPage.isModuleDetailPage(MODULE_ID)).toBe(true);
    });

    it('should test the tab names', async () => {
        const tabs = modulesDetailsPage.getTabs();

        /** Should have 6 tabs */
        expect(await tabs.count()).toBe(6);

        /** First one should have active */
        expect(await tabs.first().getAttribute('class')).toMatch('ant-tabs-tab-active');

        expect(await tabs.get(0).getText()).toBe(translate.overview);
        expect(await tabs.get(1).getText()).toBe(translate.taxonomies);
        expect(await tabs.get(2).getText()).toBe(translate.annotationReporting.label);
        expect(await tabs.get(3).getText()).toBe(translate.module.dataDictionary);
        expect(await tabs.get(4).getText()).toBe(translate.dependencies.label);
        expect(await tabs.get(5).getText()).toBe(translate.module.callChain);
    });

    describe('Overview tab test', () => {

        it('should test Characteristics section', async() => {
            expect(await modulesDetailsPage.getOverviewTabCharacteristicsHeading()).toBe('Characteristics');

            expect(await modulesDetailsPage.getCharacteristic(0)).toBe(translate.lastScan.header);
            expect(await modulesDetailsPage.getCharacteristic(1)).toBe(translate.technology);
            expect(await modulesDetailsPage.getCharacteristic(2)).toBe(translate.module.characteristics.type);
            expect(await modulesDetailsPage.getCharacteristic(3)).toBe(translate.module.characteristics.path);
            expect(await modulesDetailsPage.getCharacteristic(4)).toBe(translate.module.characteristics.inCodebase);
            expect(await modulesDetailsPage.getCharacteristic(5)).toBe(translate.module.characteristics.complexity);
            expect(await modulesDetailsPage.getCharacteristic(6)).toBe(translate.sourceLinesOfCode);
            expect(await modulesDetailsPage.getCharacteristic(7)).toBe(translate.commentLinesOfCode);
        });

        it('should test control flow section', async() => {
            expect(await modulesDetailsPage.getOverviewTabControlFlowHeading()).toBe(translate.module.graphs.controlFlowGraph);

            expect(await modulesDetailsPage.getOverviewTabDependencyGraphHeading()).toBe(translate.module.graphs.dependencyGraph);

            /** Open Dependency graph */
            await modulesDetailsPage.getOverviewTabDependencyGraphButton().click();

            expect(await appPage.isDependencyGraphPage(MODULE_ID)).toBe(true);

            /** Navigate Back */
            await appPage.navigateToModuleDetails(MODULE_ID);
        });

        xit('should test Description section', async() => {
            browser.sleep(3000);
            expect(await modulesDetailsPage.getOverviewTabDescriptionHeading()).toBe(translate.module.description);
            
            /** Click on edit button */
            await modulesDetailsPage.getOverviewTabDescriptionEditButton().click();
            
            /** Wait for pop up to open */
            await browser.wait(EC.presenceOf(modulesDetailsPage.getDescriptionDialog()), TIMEOUT);
            browser.sleep(1000);
            expect(await modulesDetailsPage.getOverviewTabDescriptionEditHeading()).toBe(translate.editModuleDescLabel);

            const textArea = modulesDetailsPage.getOverviewTabDescriptionEditTextArea();

            await textArea.clear();
            await textArea.sendKeys('Hello');

            /** Save desciption */
            await modulesDetailsPage.getOverviewTabDescriptionEditSaveButton().click();

            /** Wait for pop up to close */
            await browser.wait(EC.invisibilityOf(modulesDetailsPage.getDescriptionDialog()), TIMEOUT);
            browser.sleep(1000);
    
            await expect(modulesDetailsPage.getOverviewTabDescriptionEditTextAreaContent()).toBe('Hello');
        });
    });

    describe('Annotations tab test', () => {

        beforeAll(async() => {
            await appPage.toggleFeature('sharedAnnotationEditor',true);
          });

        it('should have table with 6 columns', async() => {

            /** click on annotations */
            await modulesDetailsPage.navigateToAnnotationsTab();
            const headers = modulesDetailsPage.getAnnotationsTabTableHeaders();
            expect(await headers.get(0).getText()).toBe(translate.category);
            expect(await headers.get(1).getText()).toBe(translate.state);
            expect(await headers.get(2).getText()).toBe(translate.description);
            expect(await headers.get(3).getText()).toBe(translate.sourceCode);
            expect(await headers.get(4).getText()).toBe(translate.createdBy);
            expect(await headers.get(5).getText()).toBe(translate.updatedBy);
        });

         it('Should open web-annotation-editor on edit', async() => {
            /** click on annotations */
            await modulesDetailsPage.navigateToAnnotationsTab();
            const action = modulesDetailsPage.getAnnotationsTabTableActions();
            await action.click();
            const editAnnotationDialog = annotationEditorPage.getAnnotationEditorDialog();
            await browser.wait(EC.presenceOf(editAnnotationDialog), 5000);
            expect(editAnnotationDialog.isPresent()).toBeTruthy();
            const descriptionValue = modulesDetailsPage.getAnnotationTabTableContent().get(2).getText();
            expect(modulesDetailsPage.getEditAnnotationDialogTitle(editAnnotationDialog)).toEqual(descriptionValue+': Annotation');
            const textArea = annotationEditorPage.getDescriptionFromDialog();
            await textArea.clear();
            await textArea.sendKeys('Test Annotation');
            const saveBtn = annotationEditorPage.getSubmitButton().get(1);
            expect(saveBtn.isPresent()).toBeTruthy();
            const cancelBtn = annotationEditorPage.getSubmitButton().get(0);
            expect(cancelBtn.isPresent()).toBeTruthy();
            await cancelBtn.click();
            expect(editAnnotationDialog.isPresent()).toBeFalsy();
        });

        afterAll(async() => {
            await appPage.toggleFeature('sharedAnnotationEditor',false);
        });
    });

    describe('Dependencies tab test', () => {

        it('should have table with 3 columns and test dependency graph navigation', async() => {

            /** click on dependencies */
            await modulesDetailsPage.navigateToDependenciesTab();

            const headers = modulesDetailsPage.getDependenciesTabTableHeaders();
            expect(await headers.get(0).getText()).toBe(translate.dependencies.target);
            expect(await headers.get(1).getText()).toBe(translate.dependencies.type);
            expect(await headers.get(2).getText()).toBe(translate.dependencies.direction);

            /** Navigate to dependecy graph */
            await modulesDetailsPage.navigateToDgFromDependencies();

            expect(await appPage.isDependencyGraphPage(MODULE_ID)).toBe(true);

            /** Navigate Back */
            await appPage.navigateToModuleDetails(MODULE_ID);
        });
    });

    describe('Call chain tab test', () => {

        it('should test default values and drop downs', async() => {
            /** click on call chain */
            await modulesDetailsPage.navigateToCallChainTab();


            expect(await modulesDetailsPage.getCallChainStartValue()).toBe('PRG1');

            expect(await modulesDetailsPage.getCallChainRadioButtonINText()).toBe('inbound');
            expect(await modulesDetailsPage.getCallChainRadioButtonOUTText()).toBe('outbound');

            expect(await modulesDetailsPage.getCallChainEndValue()).toBe('All Modules');

            await modulesDetailsPage.getCallChainEndInput().click();

            await modulesDetailsPage.testCallChainModuleSelectDialogBox();

            await modulesDetailsPage.hideFromCallChainElement().click();

            /** Wait for popup for open */
            expect(await browser.wait(EC.visibilityOf(modulesDetailsPage.getHideCallChainDialog()), TIMEOUT));

            expect(modulesDetailsPage.getHideCallChainDialog().isPresent()).toBe(true);

            /** Close popup */
            await browser.actions().sendKeys(protractor.Key.ESCAPE).perform();
        });
    });
});
