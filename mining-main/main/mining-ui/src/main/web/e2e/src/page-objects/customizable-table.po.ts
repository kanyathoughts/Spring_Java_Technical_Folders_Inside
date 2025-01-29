import { browser, by, element } from "protractor";

/**
 * The customizable Table Page Object.
 */
export class CustomizableTable {

  /**
   * Returns the filter icons for the default columns.
   * @param index index of the navigation item.
   */
  async getFilterIcon(index: number) {
    return element.all(by.css('.mining-table__type-based-filter i.type-based-filter__filter-icon')).get(index);
  }

  /**
   * Returns the filter search input.
   */
  getFilterSearchBox() {
    return element.all(by.css('.ant-table-filter-dropdown .type-based-filter__number-text-search-box input'));
  }

  /**
   * Returns the filter search button.
   * @param index index of the navigation item.
   */
  async getSearchButton(index: number) {
    return element.all(by.css('.ant-dropdown .type-based-filter__btn-gp')).all(by.tagName('button')).get(index);
  }

  /**
   * Returns the multi-select header.
   */
  async getMultiselectHeader() {
    return element.all(by.css('.ant-table-row')).get(0).all(by.className('ant-table-column-has-sorters')).get(1).all(by.className('type-based-filter__filter-icon')).get(0);
  }

  /**
   * Returns the multi-select checkbox.
   */
  async getMultiSelectCheckBox() {
    return element.all(by.css('.type-based-filter__multi-select-container .type-based-filter__multi-select-list .ant-checkbox-input')).first();
  }

  /**
   * Returns the sort icon.
   */
  async getSortIcon() {
    return element.all(by.css('.ant-table-filter-column .ant-table-column-sorters'));
  }

  /**
   * Returns the pagination icon.
   * @param className name of the class.
   */
  async getPaginationIcon(className: string) {
    const css = 'nz-pagination .' + className + ' button';
    return element.all(by.css(css));
  }

  /**
   * Returns the column check box.
   * @param index index of the navigation item.
   */
  async getColumnCheckBox(index: number) {
    return element.all(by.css('.mining-setting-popover nz-tree-node-checkbox')).get(index);
  }

  /**
   * Returns the buttons present in the table header.
   * @param index index of the navigation item.
   */
  async getHeaderButtons(index: number) {
    return element.all(by.css('.mining-table__header-column .ant-btn-link')).get(index);
  }

  /**
   * Returns newly created anchor tag in DOM once the success response is received.
   */
  getAnchorTag() {
    return element.all(by.css('.export-download'));
  }

   /**
   * Returns table header.
   * @param index index of the header.
   */
    getTableHeader(index: number) {
      return element.all(by.css('thead th')).get(index);
    }
}
