import { browser } from 'protractor';
import * as translate from '../../src/translations/en-US.json';
import { AppSharedPage } from './page-objects/app-shared.po';
import { ElementFinder } from 'protractor';

describe('Dna page test', () => {
    const appPage: AppSharedPage = new AppSharedPage();


    beforeAll(async() => {
        browser.driver.manage().window().maximize();
        await appPage.navigateToDna();
    });

    it('should validate page header', async () => {
        expect(await appPage.currentPageHeader().getText()).toBe(translate.navigation.dna);
        expect(await appPage.currentPageSubHeader().getText()).toBe(translate.dnaAnalysis.subTitle);
    });

    it('should have a Dna card if not empty', async () => {
        const emptyDNA = await appPage.getEmptyDnaImage();
        if (emptyDNA) {
          expect(emptyDNA).toBeDefined();
        } else {
          const dnaCardData = await appPage.getDnaCard();
          expect(dnaCardData).toBeTruthy();
          dnaCardData.forEach((card: ElementFinder) => {
            expect(card.getText()).toBe(translate.metrics.summary.cardTitle);
          });
        }
    });
});
