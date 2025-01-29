import {
  Directive,
  ComponentRef,
  ComponentFactoryResolver,
  ViewContainerRef,
  TemplateRef,
  Input,
  ComponentFactory
} from '@angular/core';
import { LoaderComponent } from '@app/shared/loader/loader.component';
import { MessageComponent } from '@app/shared/components/message/message.component';
import { TranslateService } from '@ngx-translate/core';

export enum LoaderState {
  loading = 'loading',
  nocontent = 'nocontent',
  error = 'error',
  success = 'success'
}

@Directive({
  selector: '[appLoader]'
})
export class LoaderDirective {
  @Input() appLoaderNoContentMessage: string = this.translateService.instant('noDataFound');
  loadingFactory: ComponentFactory<LoaderComponent>;
  loaderComponent: ComponentRef<LoaderComponent>;
  messageFactory: ComponentFactory<MessageComponent>;
  messageComponentRef: ComponentRef<MessageComponent>;
  loading: boolean;
  inputAppLoaderErrorMessage: string;
  messageComponent: MessageComponent;

  constructor(
    private templateRef: TemplateRef<any>,
    private vcRef: ViewContainerRef,
    private componentFactoryResolver: ComponentFactoryResolver,
    private translateService: TranslateService
  ) {
    this.loadingFactory = this.componentFactoryResolver.resolveComponentFactory(LoaderComponent);
    this.messageFactory = this.componentFactoryResolver.resolveComponentFactory(MessageComponent);
  }

  @Input()
  set appLoader(state: LoaderState) {
    this.vcRef.clear();

    if (state) {
      if (state === LoaderState.loading) {
        // create and embed an instance of the loading component
        this.loaderComponent = this.vcRef.createComponent(this.loadingFactory);
      } else if (state === LoaderState.success) {
        // embed the contents of the host template
        this.vcRef.createEmbeddedView(this.templateRef);
      } else if (state === LoaderState.nocontent) {
        this.messageComponent = this.vcRef.createComponent(this.messageFactory).instance;
        this.messageComponent.message = this.translateService.instant(this.appLoaderNoContentMessage);
        this.messageComponent.severity = 'info';
      } else if (state === LoaderState.error) {
        this.messageComponent = this.vcRef.createComponent(this.messageFactory).instance;
        if (this.inputAppLoaderErrorMessage) {
          this.messageComponent.message = this.translateService.instant(this.inputAppLoaderErrorMessage);
        } else {
          /*
          * Setting it as null as the message is being displayed directly from the component
          * and we don't want to override that message.
          */
          this.messageComponent.message = null;
          this.messageComponent.hrefLink = null;
        }
        this.messageComponent.severity = 'error';
      }
    }
  }

  @Input() set appLoaderErrorMessage(value: string) {
    this.inputAppLoaderErrorMessage = value;
    if (this.messageComponent && ! this.messageComponent.message) {
      this.messageComponent.message = value;
    }
  }

  @Input() set appLoaderNavigationUrl(value: string) {
    if (value && this.messageComponent) {
      this.messageComponent.hrefLink = value;
    }
  }

  @Input() set appLoaderLinkName(value: string) {
    if (value && this.messageComponent) {
      this.messageComponent.linkName = value;
    }
  }
}
