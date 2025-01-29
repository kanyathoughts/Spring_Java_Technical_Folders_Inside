import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { TypeBasedFilterComponent } from './type-based-filter.component';
import { TranslateModule } from '@ngx-translate/core';
import { NzDropDownModule } from 'ng-zorro-antd/dropdown';
import { FilterType } from '../mining-table/mining-table-config.interface';
import { NzFormatEmitEvent } from 'ng-zorro-antd/core/tree';

describe('TypeBasedFilterComponent', () => {
  let component: TypeBasedFilterComponent;
  let fixture: ComponentFixture<TypeBasedFilterComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [TypeBasedFilterComponent],
      imports: [
        TranslateModule.forRoot({}),
        NzDropDownModule
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TypeBasedFilterComponent);
    component = fixture.componentInstance;
    component.filterProperties = {
      filterType : 'multiSelect',
      listOfFilter : [{
        text: 'test', 
        value : 1234,
        byDefault: true,
      }]
    } as any
    fixture.detectChanges();
  });

  it('can load instance', () => {
    expect(component).toBeTruthy();
  });

  it('should test onResettingFilter', () => {
    component.onResettingFilter()
    expect(component.isShowFilter).toBeFalsy();
    expect(component.filterProperties.defaultKeys.length).toBe(0);
  });

  it('should test validateFilterInput when input is number', () => {
    component.validateFilterInput(3)
    expect(component.filterProperties.filterValue[0].value).toBeLessThanOrEqual(3);
  });

  it('should test toggleCheckBox', () => {
    const filter = {byDefault: true, text: "NATURAL", value: "NATURAL"};
    component.toggleCheckBox(filter);
    const findIndex = component.selectedMultiSelectFilters.indexOf(filter);
    expect(findIndex).toBe(component.selectedMultiSelectFilters.length - 1);
  });

  it('should test onApplyingFilter', () => {
    (component as any).onApplyingFilter()
    expect(component.isShowFilter).toBeFalse();
  });

  it('should test search', () => {
    (component as any).search(3)
    expect(component.isShowFilter).toBeFalse();
  });

  it('should test filterRecords', () => {
    component.filterRecords();
  });
  
  
  it('should test validateFilterInput when input is string', () => {
    component.parserNumber('test');
    (component as any).search(undefined);
    (component as any).search('abc');
    expect(component.isShowFilter).toBeLessThanOrEqual(3);
  });

  it('should test removeUnappliedFilter method', () => {
    component.filterProperties = {
      "filterType": FilterType.freeText,
      "filterValue": "test"
    };
    component.searchedValue = 'test';
    component.removeUnappliedFilter(false);
    expect(component.filterProperties.filterValue).toBe('test');

    component.filterProperties = {
      "filterType": FilterType.numberValue,
      "filterValue": [
        {
          "value": 1,
          "operator": "eq"
        }
      ]
    };
    component.searchedValue = 0;
    component.removeUnappliedFilter(false);
    expect(component.numberFilter).toBe(0);

    component.filterProperties = {
      "filterType": FilterType.multiSelect,
      "listOfFilter": [
        {
          "text": "COBOL",
          "value": "COBOL",
          "byDefault": false
        },
        {
          "text": "NATURAL",
          "value": "NATURAL",
          "byDefault": true
        }
      ],
      "isFilterActive": true
    };
    component.selectedMultiSelectFilters = [{
      "text": "NATURAL",
      "value": "NATURAL",
      "byDefault": true
    }];
    component.filteredValue = ['COBOL'];
    component.removeUnappliedFilter(false);
    expect(component.filterProperties.listOfFilter[0].byDefault).toBeFalsy();

    component.filterProperties = {
      "filterType": FilterType.treeSelect,
      "listOfFilter": [
        {
          "text": "COBOL",
          "value": "COBOL",
          "byDefault": false
        },
        {
          "text": "NATURAL",
          "value": "NATURAL",
          "byDefault": true
        }
      ],
      "isFilterActive": true
    };
    component.filteredValue = ['COBOL'];
    component.removeUnappliedFilter(false);
    expect(component.filterProperties.isFilterActive).toBeTruthy();
    expect(component.filterProperties.listOfFilter[0].byDefault).toBeTruthy();
    expect(component.filterProperties.listOfFilter[1].byDefault).toBeFalsy();
  });

  it('should test taxonomySelection', () => {
    const event: any = {
      node: {
        origin: {
          id: 'COBOL',
          checked: true
        }
      }
    };
    component.filterProperties = {
      filterType: FilterType.treeSelect,
      listOfFilter: [
        {
          text: 'COBOL',
          value: 'COBOL',
          byDefault: false
        },
        {
          text: 'NATURAL',
          value: 'NATURAL',
          byDefault: true
        }
      ],
      isFilterActive: true
    };
    component.filteredValue = ['COBOL'];
    component.taxonomySelection(event);
    expect(component.filterProperties.listOfFilter[0].byDefault).toBeTruthy();
    expect(component.filterProperties.listOfFilter[1].byDefault).toBeTruthy();
  });
});
