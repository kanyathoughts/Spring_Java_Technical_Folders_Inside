import { Component, OnInit } from '@angular/core';
import { EclipseService } from '@app/core/authentication/eclipse.service';

@Component({
  templateUrl: './base-header-shell.component.html'
})
export class BaseHeaderShellComponent implements OnInit {
  header = false;
  constructor(
    private eclipseService: EclipseService
  ) { }

  ngOnInit(): void {
    this.header = ! this.eclipseService.isEclipseView;
  }

  public setFullScreen(value: boolean): void {
    this.header = value;
  }
}
