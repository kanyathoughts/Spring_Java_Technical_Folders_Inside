/**
 * MockDialogService is a mock service of {@link DialogService} for unit test cases.
 */
export class MockDialogService {
    private config: any;

    public open(component: any, config: any) {
      this.config = config;
      return this;
    }
    public close() {
      // dummy method
    }
    public getConfig(): any {
      return this.config;
    }
}

