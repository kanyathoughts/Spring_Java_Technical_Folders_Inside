import { Component, Input, Output, EventEmitter } from '@angular/core';
import { FunctionalBlock } from '../functional-block.interface';

@Component({
  selector: 'app-functional-block-card',
  templateUrl: './functional-block-card.component.html',
  styleUrls: ['./functional-block-card.component.less']
})
export class FunctionalBlockCardComponent {
  @Input() cardData: FunctionalBlock;
  @Input() isRoot?: boolean;
  @Output() childCard = new EventEmitter();
  @Output() parentCard = new EventEmitter();

  /**
   * Method to emit user selected child data to parent.
   * @param cardChild user clicked card.
   */
  openChildCard(cardChild: FunctionalBlock): void {
    this.childCard.emit({
      parent: this.cardData,
      child: cardChild.uid
    });
  }

  /**
   * Emits selected card's uid
   * @param id closing card id
   */
  closeCard(id: string): void {
    this.parentCard.emit(id);
  }
}
