import { browser, protractor, Key } from 'protractor';
import translate from '../../src/translations/en-US.json';
import { ClientsPage } from './page-objects/clients.po';
import { AppSharedPage, TIMEOUT } from './page-objects/app-shared.po';
import { DashboardPage } from './page-objects/dashboard.po';
import { ProjectsPage } from './page-objects/projects.po';

const EC = protractor.ExpectedConditions;

describe('Projects page test', () => {
    /* For all client operations */
    let clientsPage: ClientsPage;
    /* For all project operations */
    let projectsPage: ProjectsPage;
    let appShared: AppSharedPage;
    /* Name of test client */
    let clientName: string;

    beforeAll(async () => {
        appShared = new AppSharedPage();
        clientsPage = new ClientsPage();
        projectsPage = new ProjectsPage();
        clientName = clientsPage.createClientName();

        await appShared.navigateToClients();

        await clientsPage.addClient(clientName);

        /* Navigate to projects page */
        await appShared.navigateToClient(clientName);
        expect(await browser.getCurrentUrl()).toContain(ProjectsPage.URL);
    });

    afterAll(async() => {
        await appShared.navigateToClients();

        /* Delete client */
        await clientsPage.deleteClient(appShared, clientsPage.getClientCards().last());
    });

    it('should have the title and add project button in header', async () => {
        expect(await appShared.getPageHeader()).toBe(clientName + ': Select Project');
        const addButton = projectsPage.getAddProjectButton();
        expect(await addButton.isEnabled()).toBe(true);
        expect(await addButton.getText()).toBe(translate.selectProject.createBtn);
    });

    it('should add a new project to client', async() => {
        const projectName = projectsPage.createProjectName();

        /** Add fst project so there is at least one for counting check */
        await projectsPage.addProject(projectName + '2');

        const projects = await projectsPage.getProjectCards().count();

        /** Add Project */
        await projectsPage.addProject(projectName);

        /* Test if projects count increased by 1 */
        expect(await projectsPage.getProjectCards().count()).toBe(projects + 1);

         /* Test if label an title are correct */
        const projectCreated = projectsPage.getProjectCards().first();
        expect(await projectsPage.getProjectCardCover(projectCreated).getText()).toBe(projectName);
        expect(await projectsPage.getProjectCardName(projectCreated).getText()).toBe(projectName);
        expect(await projectsPage.getProjectCardDescription(projectCreated).getText()).toBe('0 ' + translate.selectProject.members);
    });

    it('should test project options', async() => {
        /** Add Project */
        await projectsPage.addProject(projectsPage.createProjectName());

        await projectsPage.getProjectCardActions(projectsPage.getProjectCards().last()).click();
        const actionItems = projectsPage.getProjectActionItems();
        await browser.wait(EC.visibilityOf(actionItems.get(0)), TIMEOUT);
        expect(await actionItems.get(0).getText()).toBe(translate.selectProject.editBtn);
        expect(await actionItems.get(1).getText()).toBe(translate.selectProject.deleteBtn);

        /* Test if overlay is removed on ESCAPE */
        await browser.actions().sendKeys(Key.ESCAPE).perform();
        await browser.wait(EC.invisibilityOf(actionItems.get(0)), TIMEOUT);
    });

    it('should edit project name', async() => {
        const projectName = projectsPage.createProjectName();
        const newName = projectName + '_New';

        /** Add Project */
        await projectsPage.addProject(projectName);

        const projectCard = projectsPage.getProjectCards().last();

        /** Close any modal's opened */
        await browser.actions().sendKeys(Key.ESCAPE).perform();

        /* Click on options */
        await projectsPage.getProjectCardActions(projectCard).click();

        /* Click on edit */
        await projectsPage.getProjectActionItems().first().click();

        const inputBox = projectsPage.getProjectNameField();
        
        await inputBox.clear();

        /** Enter new project name */
        await inputBox.sendKeys(newName);
        await projectsPage.getSubmitButton().click();
        await browser.wait(EC.invisibilityOf(projectsPage.getSubmitButton()), TIMEOUT);

        expect(projectsPage.getProjectCardName(projectCard).getText()).toBe(newName);
        expect(projectsPage.getProjectCardCover(projectCard).getText()).toBe(newName);
    });

    it('should delete the project', async () => {
        const projectName = projectsPage.createProjectName();

        /** Add Project */
        await projectsPage.addProject(projectName);

        /* Close any modal's opened */
        await browser.actions().sendKeys(Key.ESCAPE).perform();

        const projectCard = projectsPage.getProjectCards().last();
        const projectCardName = await projectsPage.getProjectCardName(projectCard).getText();

        /** Click on options */
        await projectsPage.getProjectCardActions(projectCard).click();
        await projectsPage.getProjectActionItems().last().click();

        const confirmModal = appShared.getDeleteModal();
        await browser.wait(EC.visibilityOf(confirmModal), TIMEOUT);

        const modalActionItems = appShared.getModalActionItems();
        expect(await modalActionItems.last().isEnabled()).toBe(false);

        /** Click on yes (checkbox) to confirm delete */
        await appShared.getDeleteModalConfirmationCheckbox().click();
        expect(await modalActionItems.last().isEnabled()).toBe(true);

        /** Click on delete */
        await modalActionItems.last().click();
        await browser.wait(EC.invisibilityOf(appShared.getMessage()), TIMEOUT);

        /* Test if project is deleted */
        expect(projectsPage.getProjectCards().last()).not.toEqual(projectCard);
        expect(await projectsPage.getProjectCardName(projectsPage.getProjectCards().last()).getText()).not.toBe(projectCardName);
    });

    it('should navigate to dashboard after click on project', async () => {
        /** Add Project */
        await projectsPage.addProject(projectsPage.createProjectName());

        /* Navigate to dashboard page */
        await projectsPage.getProjectCardCover(projectsPage.getProjectCards().last()).click();

        expect(await browser.getCurrentUrl()).toContain(DashboardPage.URL);
    });
});
