import { Component, HostListener, ViewChild, ElementRef, AfterViewChecked, ContentChildren, QueryList, ChangeDetectorRef, Input } from '@angular/core';
import { NzButtonSize } from 'ng-zorro-antd/button';
import { MiningToolbarButtonDirective } from './mining-toolbar-button.directive';

@Component({
  selector: 'mn-toolbar-button',
  templateUrl: './mining-toolbar-button.component.html'
})

export class MiningToolbarButtonComponent implements AfterViewChecked {
  @ViewChild('toolbar', { static: true }) toolbar: ElementRef;
  @ContentChildren(MiningToolbarButtonDirective) toolbarButtons: QueryList<MiningToolbarButtonDirective>;
  @Input() moreBtnSize: NzButtonSize = 'default';
  visibleButtons: MiningToolbarButtonDirective[] = [];
  hiddenButtons: MiningToolbarButtonDirective[] = [];
  isDropdownNeeded = false;
  childrenDetails: MiningToolbarButtonDirective[] = [];
  toolbarDetail: HTMLElement[] = [];
  buttonWidthArray: number[] = [];
  screenWidth: number;
  constructor(private cdk: ChangeDetectorRef) { }

  @HostListener('window:resize', ['$event'])

  onResize(): void {
    (window.innerWidth > this.screenWidth) ? this.onWindowExpand() : this.checkDropdownNeeded();
    this.screenWidth = window.innerWidth;
  }

  ngAfterViewChecked(): void {
    if (this.toolbar.nativeElement.children.length > 0 && this.buttonWidthArray.length < 1) {
      this.screenWidth = window.innerWidth;
      const detail = [...this.toolbar.nativeElement.children];
      this.buttonWidthArray = detail.filter((el: HTMLElement) => el.classList.contains('buttonTruncTemplate') && el.offsetWidth > 0).map(el => el.offsetWidth);
      this.checkDropdownNeeded();
    }
  }

  /**
   * Method to segregate the templates of buttons into visible and hidden states(in Dropdown).
   */
  checkDropdownNeeded(): void {
    const visibleButtonDetails: MiningToolbarButtonDirective[] = [];
    const hiddenButtonDetails: MiningToolbarButtonDirective[] = [];
    // Increased the toolbar width with 2px because the offsetWidth of the buttons present inside the toolbar is giving an absolute or Round off values instead
    // of Decimal values. Due to which when we calculate whether the button should be inside the menu or not the calculation is legging behind because of 1px.
    // Hence increasing the whole width.
    const toolbarWidth = (this.toolbar.nativeElement as HTMLElement).offsetWidth - (this.isDropdownNeeded ? 24 : 0) + 2;
    let buttonsWidth = 0;
    this.childrenDetails = this.toolbarButtons.toArray();
    this.childrenDetails.forEach((element: MiningToolbarButtonDirective, index) => {
      const width = this.buttonWidthArray[index] + 8;
      if (buttonsWidth + width <= toolbarWidth && ! element.alwaysHidden) {
        visibleButtonDetails.push(element);
      } else {
        hiddenButtonDetails.push(element);
      }
      buttonsWidth += width;
    });
    this.visibleButtons = visibleButtonDetails;
    this.hiddenButtons = hiddenButtonDetails;
    this.isDropdownNeeded = this.hiddenButtons.length > 0 ? true : false;
  }

  private onWindowExpand(): void {
    if (this.hiddenButtons.length > 0 && this.visibleButtons.length !== this.buttonWidthArray.length) {
      this.visibleButtons.push(this.hiddenButtons.shift());
      this.cdk.detectChanges();
      if ((this.toolbar.nativeElement as HTMLElement).offsetHeight > 40 && this.visibleButtons.length) {
        this.hiddenButtons.unshift(this.visibleButtons.pop());
      }
    }
    this.isDropdownNeeded = this.hiddenButtons.length > 0 ? true : false;
  }
}
