import { browser, protractor, Key } from 'protractor';
import * as path from 'path';
import translate from '../../src/translations/en-US.json';
import { AppSharedPage, TIMEOUT } from './page-objects/app-shared.po';
import { ProjectsPage } from './page-objects/projects.po';
import { ClientsPage } from './page-objects/clients.po';

const EC = protractor.ExpectedConditions;

describe('Clients page test', () => {
    let clientsPage: ClientsPage;
    let appShared: AppSharedPage;

    beforeEach(async () => {
        appShared = new AppSharedPage();
        clientsPage = new ClientsPage();
        await appShared.navigateToClients();
    });

    it('should have the title and add client button in header' , async() => {
        expect(await appShared.getPageHeader()).toBe(translate.selectClient.pageHeaderTitle);
        const addButton = clientsPage.getAddClientButton();
        expect(await addButton.isEnabled()).toBeTruthy();
        expect(await addButton.getText()).toBe(translate.selectClient.createBtn);
    });

    xit('should test client options', async() => {
        /* Add Client */
        await clientsPage.addClient(clientsPage.createClientName());

        await clientsPage.getClientCardActions(clientsPage.getClientCards().last()).click();
        const actionItems = clientsPage.getClientActionItems();
        await browser.wait(EC.visibilityOf(actionItems.get(0)), TIMEOUT);
        expect(await actionItems.get(0).getText()).toBe(translate.selectClient.editBtn);
        expect(await actionItems.get(1).getText()).toBe(translate.selectClient.deleteBtn);

        /* Test if overlay is removed on ESCAPE */
        await browser.actions().sendKeys(Key.ESCAPE).perform();
        await browser.wait(EC.invisibilityOf(actionItems.get(0)), TIMEOUT);

        await clientsPage.deleteClient(appShared, clientsPage.getClientCards().last());
    });

    xit('should add new client without a logo', async () => {
        const clientName = clientsPage.createClientName();

        /** Add fst client so there is at least one for counting check */
        await clientsPage.addClient(clientName + '2');

        const clients = await clientsPage.getClientCards().count();

        /* Add Client */
        await clientsPage.addClient(clientName);

        /* Test if clients count increased by 1 */
        expect(await clientsPage.getClientCards().count()).toBe(clients + 1);

        /* Test if label and title are correct */
        const clientCreated = clientsPage.getClientCards().get(2);
        expect(clientsPage.getClientCardCover(clientCreated).getText()).toBe(clientName);
        expect(clientsPage.getClientCardName(clientCreated).getText()).toBe(clientName);
        expect(clientsPage.getClientCardDescription(clientCreated).getText()).toBe('0 ' + translate.selectClient.projects);

        /* Delete both clients */
        await clientsPage.deleteClient(appShared, clientsPage.getClientCards().last());
        await clientsPage.deleteClient(appShared, clientsPage.getClientCards().last());
    });

    xit('should add new Client with logo', async() => {
        const clientName = clientsPage.createClientName();

        /** Add fst client so there is at least one for counting check */
        await clientsPage.addClient(clientName + '2');

        const clients = await clientsPage.getClientCards().count();

        /* Add Client */
        await clientsPage.addClient(clientName, path.resolve('e2e', 'src', 'resources', 'Deloitte_Logo.png'));

        /* Test if clients count increased by 1 */
        expect(await clientsPage.getClientCards().count()).toBe(clients + 1);

        /* Test if label an title are correct */
        const clientCreated = clientsPage.getClientCards().get(2);
        expect(clientsPage.getClientCardLogo(clientCreated).isPresent()).toBe(true);
        expect(clientsPage.getClientCardName(clientCreated).getText()).toBe(clientName);
        expect(clientsPage.getClientCardDescription(clientCreated).getText()).toBe('0 ' + translate.selectClient.projects);

        /* Delete both clients */
        await clientsPage.deleteClient(appShared, clientsPage.getClientCards().last());
        await clientsPage.deleteClient(appShared, clientsPage.getClientCards().last());
    });

    xit('should edit client name', async () => {
        const clientName = clientsPage.createClientName();
        const newName = clientName + '_New';

        /* Add Client */
        await clientsPage.addClient(clientName);

        const clientCard = clientsPage.getClientCards().get(2);

        /* Close any modal's opened */
        await browser.actions().sendKeys(Key.ESCAPE).perform();

        /* Click on options */
        await clientsPage.getClientCardActions(clientCard).click();

        /* Click on edit */
        await clientsPage.getClientActionItems().first().click();

        const inputBox = clientsPage.getClientNameField();
        
        await inputBox.clear();

        /* Enter new client name */
        await inputBox.sendKeys(newName);
        await clientsPage.getSubmitButton().click();
        await browser.wait(EC.invisibilityOf(clientsPage.getSubmitButton()), TIMEOUT);

        expect(await clientsPage.getClientCardName(clientCard).getText()).toBe(newName);

        /* Delete Client */
        await clientsPage.deleteClient(appShared, clientCard);
    });

    it('should delete a client', async () => {
        /* Add Client */
        await clientsPage.addClient(clientsPage.createClientName());
        const clientCreated = clientsPage.getClientCards().last();
        const clientName = await clientsPage.getClientCardName(clientCreated).getText();

        /* Delete Client */
        await clientsPage.deleteClient(appShared, clientCreated);
        const lastCardAfterDeletion = clientsPage.getClientCards().last();

         /* Test if the client is deleted */
         expect(lastCardAfterDeletion).not.toEqual(clientCreated);
         expect(await clientsPage.getClientCardName(lastCardAfterDeletion).getText()).not.toBe(clientName);
    });

    it('should navigate to projects after click on client', async () => {
        /* Add Client */
        const clientName = clientsPage.createClientName();
        await clientsPage.addClient(clientName);

        /* Navigate to projects page */
        await appShared.navigateToClient(clientName);

        expect(await browser.getCurrentUrl()).toContain(ProjectsPage.URL);

        /* Navigate back to clients page */
        await appShared.navigateToClients();
        await clientsPage.deleteClient(appShared, clientsPage.getClientCards().last());
    });
});
