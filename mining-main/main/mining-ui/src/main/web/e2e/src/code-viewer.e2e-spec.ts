import { browser, protractor} from 'protractor';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { CodeViewerPage } from './page-objects/code-viewer.po';

describe('Code Viewer page test', () => {
    const codeViewerPage: CodeViewerPage = new CodeViewerPage();
    const appPage: AppSharedPage = new AppSharedPage();
    const EC = protractor.ExpectedConditions;

    beforeAll(async () => {
        await codeViewerPage.navigateTo();
    });

    it('should test the header', async () => {

        /** Should have 2 buttons disabled */
        const buttons = codeViewerPage.getHeaderButtons();

        expect((await buttons).length).toBe(2);
        expect(await buttons.first().getText()).toBe(translate.codeViewer.annotation);
        expect(await buttons.last().getText()).toBe(translate.codeViewer.dataDictionary);

        expect(await buttons.first().isEnabled()).toBe(false);
        /** The below line should be enabled after fixing the issue WMIN-1734 */
        // expect(await buttons.last().isEnabled()).toBe(false);
    });

    xdescribe('Annotation test ', () => {

        it('should add annotation', () => {
            /** All the tests are in addAnnotation method */
            codeViewerPage.addAnnotation('Testing', codeViewerPage.selectLineForAnnotation1);

            /** Delete it */
            codeViewerPage.getAnnotationDeleteButton().click();
        });

        it('should edit annotation', () => {
            codeViewerPage.addAnnotation('Testing', codeViewerPage.selectLineForAnnotation1);

            /** Edit Annotation */
            codeViewerPage.getAnnotationEditButton();

            /** Edit description */
            codeViewerPage.addTextToAnnotationDescription('Testing 2');

            /** Select annotation type 'Database' */
            codeViewerPage.selectAnnotationType();

            /** Save description */
            codeViewerPage.saveAnnotationDescription();

            /** Verify if it is saved */
            expect(codeViewerPage.getAnnotationDescription()).toBe('Testing 2');

            /** Should be changed from RULE to DATABASE */
            expect(codeViewerPage.getAnnotationType()).toBe('Database');

            /** Delete it */
            codeViewerPage.getAnnotationDeleteButton().click();
        });

        it('should add annotations (two annotations for two different lines)', () => {
            codeViewerPage.addAnnotation('Testing 3', codeViewerPage.selectLineForAnnotation1);
            codeViewerPage.addAnnotation('Testing 4', codeViewerPage.selectLineForAnnotation2);

            /** Delete first one */
            codeViewerPage.getAnnotationDeleteButton().click();

            /** Delete last one */
            codeViewerPage.getAnnotationDeleteButton().click();
        });
    });

    xdescribe('Shared Annotation Editor Test', () => {

        it('should create annotation', async () => {
            await codeViewerPage.addAnnotation('Testing', codeViewerPage.selectLineForAnnotation1, 'cae');
            /** Verify if annotation is created */
            expect(codeViewerPage.getAnnotationDescription()).toBe('Testing');
            /** Delete it */
           await codeViewerPage.deleteAnnotation();
        });
    
        it('should update annotation', async () => {
                await codeViewerPage.addAnnotation('Testing', codeViewerPage.selectLineForAnnotation1, 'cae');
                await codeViewerPage.getAnnotationEditButton().catch(error => console.log('Error in getAnnotationEditButton'));
                codeViewerPage.addTextToAnnotationDescription('Testing 2', 'cae').catch(error => console.error('Error in addTextToAnnotationDescription'));
            
                /** Verify if it is saved */
                expect(codeViewerPage.getAnnotationDescription()).toBe('Testing 2');
        });
    
        it('should close the previous editor when opening a new', async () => {
            browser.sleep(1000);
            /** Edit existing annotation */
            await codeViewerPage.getAnnotationEditButton().catch(error => console.log('Error in getAnnotationEditButton'));
            /** Add new which opens another editor */
            await codeViewerPage.addAnnotation('Testing 3', codeViewerPage.selectLineForAnnotation1, 'cae');
            expect(codeViewerPage.getAnnotationDescription()).toBe('Testing 3');
            /** check if previous editor is closed after adding new annotation */
            codeViewerPage.checkAnnotationEditor();
        });
    
        it('should delete Annotation', async () => {
            await codeViewerPage.deleteAnnotation().catch(error => console.log('Error in deleteAnnotation'));
            expect(codeViewerPage.getAnnotationDescription()).toBe('Testing 2');
        });
    });

    describe('Data Dictionary test ', () => {

        beforeEach(async() => await codeViewerPage.navigateTo());

        xit('should add Data dictionary', async() => {

           await codeViewerPage.selectDataDictionaryKeyword();

           /** ----------------------------------------------- */
           /** STEP 1: Open DataDictionary */
           await codeViewerPage.clickOnDataDictionaryButton();
           await codeViewerPage.addDataDictionaryDescription('Test Description');

           /** ----------------------------------------------- */
           /** STEP 2: Save DD  */
           await codeViewerPage.saveDataDictionary();
            /** CSS should be applied on Data Dictionary */
           expect(await codeViewerPage.checkIfCSSAppliedForDataDictionary()).toBe(true);
        });

        xit('should edit Data dictionary', async() => {
           await codeViewerPage.editDataDictionary();
           /** close the DD dialog box */
           await codeViewerPage.closeDataDictionary();
           expect(await codeViewerPage.checkIfCSSAppliedForDataDictionary()).toBe(true);
        });

        xit('should delete Data dictionary', async() => {
           await codeViewerPage.deleteDataDictionary();
        });
    });
});
