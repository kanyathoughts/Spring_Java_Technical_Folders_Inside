import { Injectable } from '@angular/core';
import { fromEvent, Observable } from 'rxjs';
import { map, filter, distinctUntilChanged, pairwise } from 'rxjs/operators';

export enum ScrollDirection {
  UP = 'up',
  DOWN = 'down'
}

@Injectable({
  providedIn: 'root'
})
export class ScrollEventService {

  private headerHeight = 48;

  private onScrollEvent = fromEvent(window, 'scroll').pipe(
    map(v => this.getYPosition(v)),
    filter(v => v > this.headerHeight || v === 0), // allow emitting 0 to show header when DOM are modified and scroll disapear
    distinctUntilChanged(),
    pairwise(),
    map(v => v[0] > v[1] ? ScrollDirection.UP : ScrollDirection.DOWN)
  );

  /**
   * Get an observable tracking user's scrolling direction
   * @returns  observable emitting scrolling direction
   */
  getScrollObservable(): Observable<ScrollDirection> {
    return this.onScrollEvent;
  }

  private getYPosition(e: Event): number {
    return (e.target as any).scrollingElement.scrollTop;
  }
}
