import { element, by, ElementFinder, browser, Key, protractor } from 'protractor';
import { TIMEOUT } from './app-shared.po';

export class ProjectsPage {

    /** The path of the projects page */
    public static URL ='/projects';

    /**
     * Returns the button element for adding new projects
     */
    getAddProjectButton() {
        return element(by.css('.ant-page-header-heading .ant-btn.ant-btn-primary'));
    }

    /**
     * Returns the action elements for editing and deleting projects
     */
    getProjectActionItems() {
        return element.all(by.css('.cdk-overlay-container .ant-dropdown-menu-item'));
    }

    /**
     * Returns the Submit button element from the dialog box
     */
    getSubmitButton() {
        return element(by.css('.ant-modal-footer')).all(by.tagName('button')).last();
    }

    /**
     * Returns the project card elements.
     */
    getProjectCards() {
        return element.all(by.css('.ant-spin-container .ant-row .ant-col'));
    }

    /**
     * Returns the cover element of the given project card which contains the project name.
     *
     * @param projectCard The project card; not undefined
     */
    getProjectCardCover(projectCard: ElementFinder) {
        return projectCard.element(by.css('.ant-card-cover'));
    }

    /**
     * Returns the title element of the given project card which contains the project name.
     *
     * @param projectCard The project card; not undefined
     */
    getProjectCardName(projectCard: ElementFinder) {
        return projectCard.element(by.css('.ant-card-meta-title'));
    }

    /**
     * Returns the description element of the given project card which contains the number of project projects.
     *
     * @param projectCard The project card; not undefined
     */
    getProjectCardDescription(projectCard: ElementFinder) {
        return projectCard.element(by.css('.ant-card-meta-description'));
    }

    /**
     * Returns the actions element of the given project card which is used to edit and delete projects.
     *
     * @param projectCard The project card; not undefined
     */
    getProjectCardActions(projectCard: ElementFinder) {
        return projectCard.element(by.css('.ant-card-actions li i'));
    }

    /**
     * Returns the input element for the client name.
     */
    getProjectNameField() {
        return element(by.css('input[formControlName="name"]'));
    }

    /**
     * Returns a new project name with pattern `TestProject_` followed by the current timestamp and a random number
     */
    createProjectName() {
        return 'TestProject_' + new Date().getTime() + '_' + Math.floor((Math.random() * 500) + 1);
    }

    /**
     * Helper method that adds the project.
     *
     * @param pName The project name.
     */
    async addProject(name: string) {
        /** Close any modal's opened */
        await browser.actions().sendKeys(Key.ESCAPE).perform();

        await this.getAddProjectButton().click();

        const submitButton = this.getSubmitButton();
        await browser.wait(protractor.ExpectedConditions.visibilityOf(submitButton), TIMEOUT);
        expect(await submitButton.isEnabled()).toBe(false, 'Submit button must be disabled');

        await this.getProjectNameField().sendKeys(name);
        expect(await submitButton.isEnabled()).toBe(true, 'Submit button must be enabled');

        /* Click on submit button to add project and wait until modal is invisible */
        submitButton.click();
        await browser.wait(protractor.ExpectedConditions.invisibilityOf(this.getSubmitButton()), TIMEOUT);
    }
}
