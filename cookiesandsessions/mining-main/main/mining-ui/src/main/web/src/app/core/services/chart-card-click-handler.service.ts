import { Injectable } from '@angular/core';
import { Plot } from '@antv/g2plot/lib/core/plot';
import { ChartEvent } from '@app/modules/metrics/shared/components/metrics-card/metrics-card.interface';
import { Observable, Subject } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class ChartCardClickHandlerService {
  private finalClick: Subject<{event: ChartEvent, cardTitle: string}> = new Subject<{event: ChartEvent, cardTitle: string}>();
  private elementArgs: any;
  private chartElementClicked = false;

  /**
   * Register the different even listener on the Chart
   * @param chart G2Plot chart
   * @param cardTitle Title of the clicked card, this is needed to prevent all the cards to open if the page contains several cards
   */
  addListenerToChart(chart: Plot<any>, cardTitle: string): void {
    chart.on('legend-item:click', () => {
      this.finalClick.next({event: ChartEvent.LEGEND, cardTitle});
      this.chartElementClicked = true;
    });
    chart.on('element:click', (args: any) => {
      this.elementArgs = args;
      this.finalClick.next({event: ChartEvent.ELEMENT, cardTitle});
      this.chartElementClicked = true;
    });
  }

  /**
   * Method to call when the card is clicked
   * @param event click event
   * @param cardTitle Title of the clicked card, this is needed to prevent all the cards to open if the page contains several cards
   */
  clickOnCard(event: Event, cardTitle: string): void {
    event.stopPropagation();
    if (!this.chartElementClicked) {
      this.finalClick.next({event: ChartEvent.CHART, cardTitle});
    }
    this.chartElementClicked = false; // reset for next click
  }

  /**
   * Final click observable, card click is filtered if chart element is already clicked
   * @returns Final Click Observable
   */
  getFinalClick(): Observable<{event: ChartEvent, cardTitle: string}> {
    return this.finalClick;
  }

  /**
   * Get the arguments passed by the event listener in case of click on chart element
   * @returns Chart Element Arguments
   */
  getElementArgs(): any {
    return this.elementArgs;
  }
}
