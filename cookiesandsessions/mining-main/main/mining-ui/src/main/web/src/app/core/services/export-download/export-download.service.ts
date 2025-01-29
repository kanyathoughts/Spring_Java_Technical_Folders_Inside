import { NzMessageService } from 'ng-zorro-antd/message';
import { TranslateService } from '@ngx-translate/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';

@Injectable({
  providedIn : 'root'
})
export class ExportDownloadService {
  constructor(
    private messageService: NzMessageService,
    private translateService: TranslateService,
    private httpClient: HttpClient
  ) {}
  /**
   * method to download file for given file
   * @param   uri indicates end point for downloading the file
   * @param   progressMessage progress message
   * @param   successMessage sucess message
   * @param   failureMessage failure message
   */
  downloadUrl(
    url: string,
    progressMessage: string,
    successMessage: string,
    failureMessage: string,
    filename?: string,
    queryParams?: HttpParams
  ): void {
    const loadingMessageId = this.messageService.loading(`${this.translateService.instant(progressMessage)}`, {
      nzDuration: 0,
    }).messageId;
    this.httpClient
      .get(url, {
        observe: 'response',
        responseType: 'blob',
        params: queryParams ? queryParams : null,
      })
      .subscribe(
        (blob: any) => {
            this.messageService.remove(loadingMessageId);
            this.messageService.success(`${this.translateService.instant(successMessage)}`);
            const downloadLink = document.createElement('a');
            downloadLink.className = 'export-download';
            const objectUrl = URL.createObjectURL(blob.body as Blob | MediaSource);
            downloadLink.href = objectUrl;
            downloadLink.download = filename? filename : blob.headers.get('content-disposition').split(';')[1].split('filename')[1].split('=')[1].trim();
            downloadLink.click();
            URL.revokeObjectURL(objectUrl);
        },
        () => {
          this.messageService.remove(loadingMessageId);
          this.messageService.error(`${this.translateService.instant(failureMessage)}`);
        }
      );
  }
}
