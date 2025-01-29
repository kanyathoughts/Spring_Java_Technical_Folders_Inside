import { browser, element, by, protractor, Key } from 'protractor';
import  translate from '../../src/translations/en-US.json';
import { TIMEOUT } from './page-objects/app-shared.po';
import { ClientsProjectsPage } from './page-objects/clients-projects.po';
import { ClientsPage } from './page-objects/clients.po';
import { ProjectsPage } from './page-objects/projects.po';

const EC = protractor.ExpectedConditions;

describe('Mining client-projects page test', () => {
    const clientsProjectsPage = new ClientsProjectsPage();
    const clientsPage = new ClientsPage();
    const projectsPage = new ProjectsPage();
    const CLIENT_IS_NOT_CREATED = 'Client is not created';

    beforeAll(async () => await clientsProjectsPage.navigateTo());

    it('should test the header name', async() => {
        const header = clientsProjectsPage.getHeader();
        expect(await header.element(by.css('.ant-page-header-heading-title')).getText()).toBe(translate.manageClientAndProject.pageTitle);

        const buttons = clientsProjectsPage.getHeaderButtons();

        expect(await buttons.first().getText()).toBe(translate.manageClientAndProject.addClient);
        expect(await buttons.last().getText()).toBe(translate.manageClientAndProject.addProject);
    });

    it('should have a table with required columns', async() => {
        const tableHeaders = element(by.css('.ant-table')).element(by.tagName('table')).element(by.tagName('thead')).all(by.tagName('th'));

        expect(await tableHeaders.get(0).getText()).toBe('Client Projects');
        expect(await tableHeaders.get(1).getText()).toBe('Client Admin');
        expect(await tableHeaders.get(2).getText()).toBe(translate.updateProjectMember.projectNature);
        expect(await tableHeaders.get(3).getText()).toBe(translate.manageClientAndProject.members);
    });

    /**
     * Test includes
     * Add Client, Delete client
     */
    it('should add client and project in it', async() => {
        const buttons = clientsProjectsPage.getHeaderButtons();
        const clientName = clientsPage.createClientName();

        await browser.wait(EC.elementToBeClickable(buttons.first()), TIMEOUT);

        const noOfClients = await clientsProjectsPage.getCount();

        /** Add Client */
        const isClientCreated = await clientsProjectsPage.addClient(clientName);
        expect(isClientCreated).toBe(true, CLIENT_IS_NOT_CREATED);

        if (isClientCreated) {
            const createdClient = clientsProjectsPage.getLastClient();
            /** Test if no of clients count increased by 1 */
            expect(await clientsProjectsPage.getCount()).toBe(noOfClients + 1);

            const columns = createdClient.all(by.tagName('td'));
            expect(await columns.first().getText()).toBe(clientName);
            expect(await columns.get(3).getText()).toBe('0 ' + translate.manageClientAndProject.members);

            /** Delete Client */
            await clientsProjectsPage.deleteClient(createdClient, clientName);

            /** Test if no of clients count decreased by 1 */
            expect(await clientsProjectsPage.getCount()).toBe(noOfClients);
        }
    });

    it('should test the options available for client ', async() => {
        const buttons = clientsProjectsPage.getHeaderButtons();
        const clientName = clientsPage.createClientName();

        await browser.wait(EC.elementToBeClickable(buttons.first()), TIMEOUT);

        /** Add Client */
        const isClientCreated = await clientsProjectsPage.addClient(clientName);
        expect(isClientCreated).toBe(true, CLIENT_IS_NOT_CREATED);

        if (isClientCreated) {
            const createdClient = clientsProjectsPage.getLastClient();
            const lastColumn = createdClient.all(by.tagName('td')).last();

            /** Click on options */
            await lastColumn.element(by.tagName('button')).click();
            const options = await clientsProjectsPage.getActionItems();
            expect(await options[0].getText()).toBe(translate.manageClientAndProject.addProject);
            expect(await options[1].getText()).toBe(translate.manageClientAndProject.editClientDetails);
            expect(await options[2].getText()).toBe(translate.manageClientAndProject.editClientAdmins);
            expect(await options[3].getText()).toBe(translate.manageClientAndProject.deleteClientAndProjects);

            /** Escape to close options */
            await browser.actions().sendKeys(Key.ESCAPE).perform();

            /** Delete the created client */
            await clientsProjectsPage.deleteClient(createdClient, clientName);
        }
    });

    it('should add project using option available client row', async () => {
        const clientName = clientsPage.createClientName();
        const projectName = projectsPage.createProjectName();

        /** Add Client */
        const isClientCreated = await clientsProjectsPage.addClient(clientName);
        expect(isClientCreated).toBe(true, CLIENT_IS_NOT_CREATED);

        if (isClientCreated) {
            const createdClient = clientsProjectsPage.getLastClient();
            /** Add project */
            await clientsProjectsPage.addProjectToClient(createdClient, projectName);

            const createdProject = clientsProjectsPage.getLastProject();
            expect(await createdProject.all(by.tagName('td')).first().getText()).toBe(projectName);

            /** collapse project */
            await clientsProjectsPage.expandOrCollapseProject();

            /** Delete the created client */
            await clientsProjectsPage.deleteClient(createdClient, clientName, [projectName]);
        }
    });

    it('should add project using add project button in header', async () => {
        const buttons = clientsProjectsPage.getHeaderButtons();
        const clientName = clientsPage.createClientName();
        const projectName = projectsPage.createProjectName();

        await browser.wait(EC.elementToBeClickable(buttons.first()), TIMEOUT);

        /** Add Client */
        const isClientCreated = await clientsProjectsPage.addClient(clientName);
        expect(isClientCreated).toBe(true, CLIENT_IS_NOT_CREATED);

        if (isClientCreated) {
            const createdClient = clientsProjectsPage.getLastProject();
            /** Add Project */
            await clientsProjectsPage.addProject(clientName, projectName);

            /** collapse project */
            await clientsProjectsPage.expandOrCollapseProject();

            /** Delete the created client */
            await clientsProjectsPage.deleteClient(createdClient, clientName, [projectName]);
        }
    });

    it('should test view project option navigation to dashboard', async() => {
        const buttons = clientsProjectsPage.getHeaderButtons();
        const clientName = clientsPage.createClientName();
        const projectName = projectsPage.createProjectName();

        await browser.wait(EC.elementToBeClickable(buttons.first()), TIMEOUT);

        /** Add Client */
        const isClientCreated = await clientsProjectsPage.addClient(clientName);
        expect(isClientCreated).toBe(true, CLIENT_IS_NOT_CREATED);

        if (isClientCreated) {
            const createdClient = clientsProjectsPage.getLastProject();
            /** Add Project */
            await clientsProjectsPage.addProject(clientName, projectName);
            const projectLastCol = await clientsProjectsPage.getProjectOptionColumn();

            /** Click on options */
            await projectLastCol.element(by.tagName('button')).click();
            const actionItems = await clientsProjectsPage.getActionItems();

            /** Should navigate to home page */
            await actionItems[0].click();

            expect(await browser.getCurrentUrl()).toContain('/dashboard');

            /** Navigate back to /clients-projects page */
            await clientsProjectsPage.navigateTo();

            await clientsProjectsPage.deleteClient(createdClient, clientName);
        }
    });

    it('should edit the client name', async() => {
        const buttons = clientsProjectsPage.getHeaderButtons();
        const clientName = clientsPage.createClientName();
        const newClientName = clientsPage.createClientName();

        await browser.wait(EC.elementToBeClickable(buttons.first()), TIMEOUT);

        /** Add Client */
        const isClientCreated = await clientsProjectsPage.addClient(clientName);
        expect(isClientCreated).toBe(true, CLIENT_IS_NOT_CREATED);

        if (isClientCreated) {
            const client = clientsProjectsPage.getLastClient();
            await clientsProjectsPage.editClient(client, clientName, newClientName);
            await clientsProjectsPage.deleteClient(client, newClientName);
        }
    });

    it('should test options available for project', async () => {
        const buttons = clientsProjectsPage.getHeaderButtons();
        const clientName = clientsPage.createClientName();
        const projectName = projectsPage.createProjectName();

        await browser.wait(EC.elementToBeClickable(buttons.first()), TIMEOUT);

        /** Add Client */
        const isClientCreated = await clientsProjectsPage.addClient(clientName);
        expect(isClientCreated).toBe(true, CLIENT_IS_NOT_CREATED);

        if (isClientCreated) {
            const createdClient = clientsProjectsPage.getLastClient();
            /** Add Project */
            await clientsProjectsPage.addProject(clientName, projectName);

            /** Click on options */
            await clientsProjectsPage.getProjectOptionColumn().element(by.tagName('button')).click();
            const actionItems = await clientsProjectsPage.getActionItems();

            expect(await actionItems[0].getText()).toBe(translate.manageClientAndProject.viewProject);
            expect(await actionItems[1].getText()).toBe(translate.manageClientAndProject.editProjectDetails);
            expect(await actionItems[2].getText()).toBe(translate.manageClientAndProject.editMembers);
            expect(await actionItems[3].getText()).toBe(translate.manageClientAndProject.deleteProject);

            /** Escape to close options */
            await browser.actions().sendKeys(Key.ESCAPE).perform();

            /** collapse project */
            await clientsProjectsPage.expandOrCollapseProject();

            /** Delete the created client */
            await clientsProjectsPage.deleteClient(createdClient, clientName, [projectName]);
        }
    });

    it('should edit project name', async() => {
        const buttons = clientsProjectsPage.getHeaderButtons();
        const clientName = clientsPage.createClientName();
        const projectName = projectsPage.createProjectName();
        const newProjectName = projectsPage.createProjectName();

        await browser.wait(EC.elementToBeClickable(buttons.first()), TIMEOUT);

        /** Add Client */
        const isClientCreated = await clientsProjectsPage.addClient(clientName);
        expect(isClientCreated).toBe(true, CLIENT_IS_NOT_CREATED);

        if (isClientCreated) {
            const createdClient = clientsProjectsPage.getLastClient();
            /** Add Project */
            await clientsProjectsPage.addProject(clientName, projectName);
            const projectCreated = clientsProjectsPage.getLastProject();

            /** Edit project */
            await clientsProjectsPage.editProject(projectCreated, projectName, newProjectName);

            /** collapse project */
            await clientsProjectsPage.expandOrCollapseProject();

            /** Delete the created client */
            await clientsProjectsPage.deleteClient(createdClient, clientName, [newProjectName]);
        }
    });
});
