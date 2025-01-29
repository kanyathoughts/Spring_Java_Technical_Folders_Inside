import { browser, element, by, protractor, Key, ElementFinder } from 'protractor';
import { AppSharedPage, TIMEOUT } from './app-shared.po';

const EC = protractor.ExpectedConditions;

export class ClientsPage {

  /**
   * Returns the button element for adding new clients
   */
  getAddClientButton() {
      return element(by.css('.ant-page-header-heading .ant-btn.ant-btn-primary'));
  }

  /**
   * Returns the action elements for editing and deleting clients
   */
  getClientActionItems() {
      return element.all(by.css('.cdk-overlay-container .ant-dropdown-menu-item'));
  }

  /** Returns the Submit button element from the dialog box */
  getSubmitButton() {
      return element(by.css('.ant-modal-footer')).all(by.tagName('button')).last();
  }

  /**
   * Returns the client card elements.
   */
  getClientCards() {
      return element.all(by.css('.ant-spin-container .ant-row .ant-col'));
  }

  /**
   * Returns the cover element of the given client card which contains the client logo or client name.
   *
   * @param clientCard the client card; not undefined
   */
  getClientCardCover(clientCard: ElementFinder) {
      return clientCard.element(by.css('.ant-card-cover'));
  }

  /**
   * Returns the title element of the given client card which contains the client name.
   *
   * @param clientCard the client card; not undefined
   */
  getClientCardName(clientCard: ElementFinder) {
      return clientCard.element(by.css('.ant-card-meta-title'));
  }

  /**
   * Returns the description element of the given client card which contains the number of client projects.
   *
   * @param clientCard the client card; not undefined
   */
  getClientCardDescription(clientCard: ElementFinder) {
      return clientCard.element(by.css('.ant-card-meta-description'));
  }

  /**
   * Returns the logo element of the given client card which contains the client logo.
   *
   * @param clientCard the client card; not undefined
   */
  getClientCardLogo(clientCard: ElementFinder) {
      return clientCard.element(by.className('client-card__logo'));
  }

  /**
   * Returns the actions element of the given client card which is used to edit and delete clients.
   *
   * @param clientCard the client card; not undefined
   */
  getClientCardActions(clientCard: ElementFinder) {
      return clientCard.element(by.css('.ant-card-actions li i'));
  }

  /**
   * Returns the input element for the client name.
   */
  getClientNameField() {
      return element(by.css('input[formControlName="name"]'));
  }

  /**
   * Returns the input element for the client logo.
   */
  getClientLogoField() {
      return element(by.css('input[type="file"]'));
  }

  /**
   * Helper method to create a new client for the given name and logo. The method also checks if the client was created successfully, if
   * the name and logo, if present, were set correctly and if all elements are available.
   *
   * @param name The client name.
   * @param logo The path to the client logo, optional parameter
   */
  async addClient(name: string, logo?: string) {
      /* Close any modal's opened */
      await browser.actions().sendKeys(Key.ESCAPE).perform();

      await this.getAddClientButton().click();

      const submitButton = this.getSubmitButton();
      await browser.wait(EC.visibilityOf(submitButton), TIMEOUT);
      expect(await submitButton.isEnabled()).toBe(false, 'Submit button must be disabled');

      await this.getClientNameField().sendKeys(name);
      if (logo) {
          await this.getClientLogoField().sendKeys(logo);
      }
      expect(await submitButton.isEnabled()).toBe(true, 'Submit button must be enabled');

      /* Click on submit button to add client and wait until modal is invisible */
      submitButton.click();
      await browser.wait(EC.invisibilityOf(this.getSubmitButton()), TIMEOUT);
  }

  /**
   * Helper method to delete client. The method also checks if the client was deleted successfully
   *
   * @param appShared The AppSharedPage helper.
   * @param clientCard The client card element.
   */
  async deleteClient(appShared: AppSharedPage, clientCard: ElementFinder) {
      /* Close any modal's opened */
      await browser.actions().sendKeys(Key.ESCAPE).perform();

      /* Click on client options */
      await this.getClientCardActions(clientCard).click();
      await this.getClientActionItems().last().click();

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
  }

  /**
   * Returns a new client name with pattern `TestClient_` followed by the current timestamp and a random number
   */
  createClientName() {
      return 'TestClient_' + new Date().getTime() + '_' + Math.floor((Math.random() * 500) + 1);
  }
}
