import { browser, element, by, protractor, Key } from 'protractor';
import { TIMEOUT } from './app-shared.po';

const EC = protractor.ExpectedConditions;

export class ClientsProjectsPage {

  navigateTo() {
      return browser.get('/#/admin/client-projects');
  }

  async deleteClient(client: any, clientName: string, projects?: string[]) {
    /** Close any modal's opened */
    await browser.actions().sendKeys(Key.ESCAPE).perform();

    /** Click on options */
    await this.getOptions(client).click();
    await this.getActionItems().last().click();

    const confirmText = await element(by.css('nz-modal-container .ant-modal-body mn-confirm-delete-modal p')).getText();
    expect(confirmText.includes(clientName)).toBe(true);

    if (projects && projects.length > 0) {
        const allProjects = await element(by.css('nz-modal-container .ant-modal-body mn-confirm-delete-modal ul')).all(by.tagName('li'));
        expect(allProjects.length).toBe(projects.length);
        allProjects.forEach(async pro => expect(projects.includes(await pro.getText())).toBe(true));
    }

    const confirmModal = element(by.tagName('mn-confirm-delete-modal'));
    await browser.wait(EC.visibilityOf(confirmModal), TIMEOUT);

    const confirmModalActionItems = element(by.tagName('nz-modal-container')).element(by.css('.ant-modal-footer')).all(by.tagName('button'));
    expect(await confirmModalActionItems.last().isEnabled()).toBe(false);

    /** Click on yes (checkbox) to confirm delete */
    await confirmModal.element(by.css('input[type="checkbox"]')).click();
    expect(await confirmModalActionItems.last().isEnabled()).toBe(true);

    /** Click on delete */
    await confirmModalActionItems.last().click();
    await browser.wait(EC.invisibilityOf(confirmModal), TIMEOUT);
  }

  /**
   * Helper method to edit the client name.
   *
   * @param client The client element.
   * @param oldName old client name.
   * @param newName new client name.
   */
  async editClient(client: any, oldName: string, newName: string) {
      /** Close any modal's opened */
      await browser.actions().sendKeys(Key.ESCAPE).perform();

      await this.getOptions(client).click();
      await this.getActionItems().get(1).click();

      const inputBox = this.getNameInput();
      expect(this.getLastProject().all(by.tagName('td')).first().getText()).toBe(oldName);
      await inputBox.clear();

      await inputBox.sendKeys(newName);
      await this.getSubmitButton().click();

      const clientEdited = this.getLastProject();
      const editedClientName = await clientEdited.all(by.tagName('td')).first().getText();
      expect(editedClientName.includes(newName)).toBe(true);
  }

  /**
   * Helper method to edit the project name.
   *
   * @param project The project element.
   * @param oldName old project name.
   * @param newName new project name.
   */
  async editProject(project: any, oldName: string, newName: string) {

      /** Close any modal's opened */
      await browser.actions().sendKeys(Key.ESCAPE).perform();

      /** Click on options */
      await this.getOptions(project).click();
      await this.getActionItems().get(1).click();

      const inputBox = this.getNameInput();

      expect(this.getLastProject().all(by.tagName('td')).first().getText()).toBe(oldName);
      await inputBox.clear();

      await inputBox.sendKeys(newName);
      await this.getSubmitButton().click();

      const projectEdited = this.getLastProject();
      const editedProjectName = await projectEdited.all(by.tagName('td')).first().getText();

      expect(editedProjectName.includes(newName)).toBe(true);
  }

  /**
   * Helper method that adds the client and returns true if client is created.
   *
   * @param clientName The client name.
   */
  async addClient(clientName: string) {
      /** Close any modal's opened */
      await browser.actions().sendKeys(Key.ESCAPE).perform();

      await this.getHeaderButtons().first().click();

      const submitButton = this.getSubmitButton();
      expect(await submitButton.isEnabled()).toBe(false);
      const inputBox = this.getNameInput();
      await inputBox.sendKeys(clientName);

      expect(await submitButton.isEnabled()).toBe(true);

      /** Click on submit button to add client */
      await submitButton.click();
      const clientCreated = this.getLastProject();
      const createdClientName = await clientCreated.all(by.tagName('td')).first().getText();
      expect(createdClientName.includes(clientName)).toBe(true);
      return createdClientName.includes(clientName);
  }

  /**
   * Helper method that adds the project (using the option available on client)
   *
   * @param client The client element.
   * @param pName The project name.
   */
  async addProjectToClient(client: any, pName: string) {
      /** Close any modal's opened */
      await browser.actions().sendKeys(Key.ESCAPE).perform();

      /** Click on options */
      await this.getOptions(client).click();
      await this.getActionItems().first().click();

      const projectSubmitButton = this.getSubmitButton();
      expect(await projectSubmitButton.isEnabled()).toBe(false);

      const projectinputBoxName = this.getNameInput();
      await projectinputBoxName.sendKeys(pName);

      expect(await projectSubmitButton.isEnabled()).toBe(true);

      /** Click to add project */
      await projectSubmitButton.click();

      const newRow = element(by.css('.ant-table'))
                          .element(by.tagName('table'))
                          .element(by.tagName('tbody'))
                          .all(by.tagName('tr'))
                          .last();

      const expandButton = newRow.element(by.tagName('button'));
      expect(expandButton.isPresent()).toBe(true);

      /** Expand client */
      await expandButton.click();
  }

  /**
   * Helper method that adds the project.
   *
   * @param cName The client name.
   * @param pName The project name.
   */
  async addProject(cName: string, pName: string) {

      /** Close any modal's opened */
      await browser.actions().sendKeys(Key.ESCAPE).perform();

      await this.getHeaderButtons().last().click();
      const projectSubmitButton = this.getSubmitButton();
      expect(await projectSubmitButton.isEnabled()).toBe(false);

      const projectinputBoxName = this.getNameInput();
      const clientSelectBox = element(by.css('.ant-select-selection-search-input'));
      await projectinputBoxName.sendKeys(pName);
      await clientSelectBox.sendKeys(cName);
      await clientSelectBox.sendKeys(protractor.Key.ENTER);
      expect(await projectSubmitButton.isEnabled()).toBe(true);

      /** Click to add project */
      await projectSubmitButton.click();


      const newRow = element(by.css('.ant-table'))
                          .element(by.tagName('table'))
                          .element(by.tagName('tbody'))
                          .all(by.tagName('tr'))
                          .last();

      const expandButton = newRow.element(by.tagName('button'));
      expect(await newRow.all(by.tagName('td')).first().getText()).toBe(cName);
      expect(expandButton.isPresent()).toBe(true);

      /** Expand client */
      await expandButton.click();
  }

  /** Get options for client/project
   *
   * @param element The Client element or the Project element
   */
  getOptions(element: any) {
      return element.all(by.tagName('td')).last().element(by.tagName('button'));
  }

  /**
   * Returns the header of the page.
   */
  getHeader() {
      return element(by.tagName('nz-page-header'));
  }

  /**
   * Returns the Add Client and Add Project button.
   */
  getHeaderButtons() {
      return this.getHeader().element(by.tagName('nz-page-header-extra')).all(by.tagName('button'));
  }

  /**
   * Returns the name Input text field.
   */
  getNameInput() {
      return element(by.css('input[formControlName="name"]'));
  }

  /**
   * Returns the action items available for clients and projects.
   */
  getActionItems() {
      return element.all(by.className('ant-dropdown')).first().element(by.tagName('ul')).all(by.tagName('li'));
  }

  /**
   * Return the reference of the Submit button on the modal.
   */
  getSubmitButton() {
     return element(by.css('.ant-modal-footer')).all(by.tagName('button')).last();
  }

  /**
   * Returns the options column of the project.
   */
  getProjectOptionColumn() {
      return element(by.css('.ant-table-content')).element(by.tagName('tbody')).all(by.tagName('tr')).last().all(by.tagName('td')).last();
  }

  /**
   * Returns the last client in the list.
   */
  getLastClient() {
      return element(by.css('.ant-table-content tbody')).all(by.tagName('tr')).last();
  }

  /**
   * Returns the last client in the list.
   */
  getLastProject() {
      return element(by.css('.ant-table-content')).element(by.tagName('tbody')).all(by.tagName('tr')).last();
  }

  /**
   * Returns the number of clients/projects.
   */
  async getCount() {
      return await element(by.css('.ant-table-content')).element(by.tagName('tbody')).all(by.tagName('tr')).count();
  }

  /**
   * Allows to to expand or collapse the client (show/hide the list of projects).
   */
  async expandOrCollapseProject() {
      await element(by.css('.ant-table-content'))
               .element(by.tagName('tbody'))
               .all(by.tagName('tr')).get(await this.getCount() - 2)
               .element(by.tagName('button'))
               .click();
  }
}
