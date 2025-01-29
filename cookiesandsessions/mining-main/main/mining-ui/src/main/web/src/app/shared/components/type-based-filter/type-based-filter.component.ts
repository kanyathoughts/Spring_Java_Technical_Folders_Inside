import { Component, Input, Output, EventEmitter, OnChanges, ElementRef, ViewChild } from '@angular/core';
import { FilterProperties, FilterType } from '../mining-table/mining-table-config.interface';
import { NzFormatEmitEvent, NzTreeNodeOptions } from 'ng-zorro-antd/core/tree';

export enum Operators {
  EQUALS = 'eq',
  GTE = 'gte',
  LTE = 'lte'
}

interface MultiSelectFilter {
  text: string;
  value: string;
  byDefault?: boolean;
}

@Component({
  selector: 'type-based-filter',
  templateUrl: './type-based-filter.component.html',
})
export class TypeBasedFilterComponent implements OnChanges {


  @Input() getLabel: (value: string) => string;
  @Input() filterProperties: FilterProperties;
  @Output() selectedValue: EventEmitter<any> = new EventEmitter();
  @Output() checkBoxSelection: EventEmitter<boolean> = new EventEmitter();
  @Output() validateNumberEvent: EventEmitter<any> = new EventEmitter();
  @Input() textSearchTooltip: string;
  // Input property for number type
  @Input() error = '';
  @Input() nzErrorTip = '';
  @Input() nzPlaceHolder = '';
  @ViewChild('searchField') searchField: ElementRef;

  filterType = FilterType;
  isShowFilter = false;
  numberFilter = 0;
  labels: { [key: string]: { [key: string]: string } };
  operator = Operators.EQUALS;
  selectedMultiSelectFilters: MultiSelectFilter[] = [];
  filteredValue: string[] = [];
  searchedValue: string | number = '';
  selectedOperator = Operators.EQUALS;

  ngOnChanges(): void {
      if (this.filterProperties?.filterType === FilterType.numberValue && this.filterProperties?.filterValue) {
        this.operator = this.filterProperties?.filterValue[0].operator ?? Operators.EQUALS;
        this.numberFilter = this.filterProperties.filterValue[0].value;
      } else if (this.filterProperties?.filterType === FilterType.multiSelect) {
        const initialFilter = this.filterProperties?.listOfFilter?.filter((filterItem: any) => filterItem['byDefault']);
        initialFilter.forEach((value: MultiSelectFilter) => {
          if (value.byDefault) {
            this.filterProperties.isFilterActive = true;
          }
          this.filteredValue.push(value['text']);
        });
      } else if (this.filterProperties?.filterType === FilterType.treeSelect) {
        const initialFilter = this.filterProperties?.listOfFilter?.filter((filterItem: any) => filterItem['byDefault']);
        initialFilter.forEach((value: MultiSelectFilter) => {
          if (value.byDefault) {
            this.filterProperties.isFilterActive = true;
            this.filteredValue.push(value['value']);
          }
        });
      } else if (this.filterProperties?.filterType === FilterType.freeText) {
        this.searchedValue = this.filterProperties?.filterValue;
      }
  }

  parserNumber = (value: string): string => value.replace(/\D/g, '');

  /**
   * Apply mapping to value
   * @param filter includes text and value of the filter to map.
   * @returns the mapped value
   */
  applyMapping(filter: { text: string, value: string }): string {
    if (typeof this.getLabel !== 'function') {
      return filter.text;
    }
    return this.getLabel(filter.value);
  }

  /**
   * method to check the status of  multi select check box
   * @param filter applied filter
   */
  toggleCheckBox(filter: MultiSelectFilter): void {
    this.selectedMultiSelectFilters.push(filter);
  }

  /**
   * method to reset filter
   */
  onResettingFilter(): void {
    this.filterProperties.filterValue = '';
    this.searchedValue = '';
    this.numberFilter = 0;
    this.operator = Operators.EQUALS;
    this.filterProperties.listOfFilter?.forEach((filterItem: any) => {
      filterItem['byDefault'] = false;
    });
    this.filteredValue = [];
    this.isShowFilter = false;
    this.filterProperties.isFilterActive = false;
    this.selectedValue.emit([]);
    this.filterProperties.defaultKeys = [];
  }

  /**
   * method to validate entered  value
   * @param selectedNumber selected number
   */
  validateFilterInput(selectedNumber: number): void {
    this.filterProperties.filterValue = [{ value: selectedNumber, operator: this.operator }];
    this.validateNumberEvent.next(null);
  }

  /**
   * method to change listOfFilter.byDefault to checked/unchecked based on selection
   * @param event Taxonomy tree node data
   */
  taxonomySelection(event: NzFormatEmitEvent): void {
    if (event.node.origin) {
      if (event.node.origin.children) {
        event.node.origin.children.forEach((childNode: NzTreeNodeOptions) => {
          const filterItem = this.filterProperties.listOfFilter.find((item: MultiSelectFilter) => item.value === childNode.id);
          if (filterItem) {
            filterItem.byDefault = childNode.checked;
          }
        });
      } else {
        const filterItem = this.filterProperties.listOfFilter.find((item: MultiSelectFilter) => item.value === event.node.origin.id);
        if (filterItem) {
          filterItem.byDefault = event.node.origin.checked;
        }
      }
    }
  }

  /**
   * calls the appropriate method for filtering data on the basis of filter type.
   */
  filterRecords(): void {
    switch (this.filterProperties.filterType) {
      case this.filterType.freeText:
        this.search(this.filterProperties.filterValue as string | number);
        break;
      case this.filterType.numberValue:
        this.search(this.numberFilter);
        break;
      case this.filterType.multiSelect:
        this.onApplyingFilter();
        break;
      case this.filterType.treeSelect:
        this.onApplyingFilter();
        break;
    }
  }

  /**
   * Revert the changes when dropdown closes by clicking elsewhere.
   * @param status status of the dropdown.
   */
  removeUnappliedFilter(status: boolean): void {
    setTimeout(() => {
      this.searchField.nativeElement.focus();
    }, 0);
    if ( ! status) {
      if (this.filterProperties?.filterType === FilterType.multiSelect) {
        this.filterProperties?.listOfFilter?.forEach((filterItem: MultiSelectFilter) => {
          this.selectedMultiSelectFilters.forEach((filter: MultiSelectFilter) => {
            const index = this.filteredValue?.indexOf(filter['text']);
            if (filterItem['text'] === filter['text']) {
              filterItem['byDefault'] = (index || index === 0) && index !== -1;
            }
          });
        });
        this.filterProperties.isFilterActive = this.filterProperties?.listOfFilter?.some((filterItem: MultiSelectFilter) => filterItem['byDefault'] === true);
      } else if (this.filterProperties?.filterType === FilterType.treeSelect) {
        this.filterProperties.isFilterActive = this.filterProperties?.listOfFilter?.some((filterItem: MultiSelectFilter) => filterItem['byDefault'] === true);
        this.filterProperties.defaultKeys = this.filterProperties?.listOfFilter.filter((filterItem: MultiSelectFilter) =>
          this.filteredValue.indexOf(filterItem.value) > -1).map((filterItem: MultiSelectFilter) => filterItem['key']);
        this.filterProperties.listOfFilter.forEach((filterItem: MultiSelectFilter) => {
          if (this.filteredValue.indexOf(filterItem.value) > -1) {
            filterItem.byDefault = true;
          } else {
            filterItem.byDefault = false;
          }
        });
      } else if (this.filterProperties?.filterType === FilterType.freeText) {
        this.filterProperties.filterValue = this.searchedValue ? this.searchedValue : '';
        this.filterProperties.isFilterActive = this.filterProperties.filterValue?.trim() !== '';
      } else if (this.filterProperties.filterType === FilterType.numberValue) {
        this.numberFilter = this.searchedValue ? this.searchedValue as number : 0;
        this.operator = this.selectedOperator;
      }
    }
  }

  private search(filterValue: string | number): void {
    // We can't use (! filterValue) on the below condition because it prevents us from filtering on 0
    if (filterValue === null || filterValue === undefined) {
      return;
    }
    this.filterProperties.isFilterActive = true;
    const selectedFilterValue = this.filterProperties.filterType === 'number' ? [{ value: filterValue, operator: this.operator }] :
      [{ value: filterValue }];
    this.searchedValue = selectedFilterValue[0].value as string;
    this.selectedOperator = selectedFilterValue[0]?.['operator'];
    this.selectedValue.emit(selectedFilterValue);
    this.isShowFilter = false;
  }

  private onApplyingFilter(): void {
    this.filteredValue = [];
    const selectedFilterValue = this.filterProperties.listOfFilter.filter((filterItem: MultiSelectFilter) => filterItem['byDefault']);
    if( this.filterProperties?.filterType === FilterType.multiSelect ) {
      selectedFilterValue.forEach((value: MultiSelectFilter) => this.filteredValue.push(value['text']));
    } else {
      selectedFilterValue.forEach((value: MultiSelectFilter) => this.filteredValue.push(value['value']));
    }
    this.filterProperties.isFilterActive = this.filterProperties?.listOfFilter?.some((filterItem: any) => filterItem['byDefault'] === true);
    this.selectedValue.emit(selectedFilterValue);
    this.isShowFilter = false;
  }
}
