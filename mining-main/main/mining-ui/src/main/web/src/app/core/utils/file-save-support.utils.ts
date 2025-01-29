import { Injectable } from '@angular/core';

const fileSavedMssg = 'File saved successfully';

/**
 * Provides helper methods for file saving.
 */
@Injectable()
export default class FileSaveSupport {

  static save(fileContent: string, fileName: string): Promise<string> {
    return new Promise((resolve, reject) => {
      // extract file format
      const format = fileName.split('.')[1].toLowerCase();

      if (FileSaveSupport.isFileConstructorAvailable()) {
        if (
          format === 'txt' ||
          format === 'json' ||
          format === 'svg' ||
          format === 'graphml' ||
          format === 'pdf' ||
          format === 'png' ||
          format === 'csv'
        ) {
          let mimeType = '';
          switch (format) {
            case 'txt':
            case 'json':
            default:
              mimeType = 'text/plain';
              break;
            case 'svg':
              mimeType = 'image/svg+xml';
              break;
            case 'png':
              mimeType = 'image/png';
              break;
            case 'graphml':
              mimeType = 'application/xml';
              break;
            case 'pdf':
              mimeType = 'text/plain; charset=x-user-defined';
              break;
            case 'csv':
              mimeType = 'text/csv';
              break;
          }

          let blob = null;
          if (format === 'pdf') {
            // encode content to make transparent images work correctly
            const uint8Array = new Uint8Array(fileContent.length);
            for (let i = 0; i < fileContent.length; i++) {
              uint8Array[i] = fileContent.charCodeAt(i);
            }
            blob = new Blob([uint8Array], { type: mimeType });
          } else if (format === 'png') {
            // save as binary data
            const dataUrlParts = fileContent.split(',');
            const bString = window.atob(dataUrlParts[1]);
            const byteArray = [];
            for (let i = 0; i < bString.length; i++) {
              byteArray.push(bString.charCodeAt(i));
            }
            blob = new Blob([new Uint8Array(byteArray)], { type: mimeType });
          } else {
            blob = new Blob([fileContent], { type: mimeType });
          }
          fileContent = URL.createObjectURL(blob);
        }

        const aElement = document.createElement('a');
        aElement.setAttribute('href', fileContent);
        aElement.setAttribute('download', fileName);
        aElement.style.display = 'none';
        document.body.appendChild(aElement);
        aElement.click();
        document.body.removeChild(aElement);
        resolve(fileSavedMssg);
        return;
      }
      reject(new Error('File save failed: Save operation is not supported by the browser.'));
    });
  }

  static isFileConstructorAvailable(): boolean {
    // Test whether required functions exist
    if (typeof window.URL !== 'function' ||
      typeof window.Blob !== 'function') {
      return false;
    }
    // Test whether the constructor works as expected
    try {
       new File(['Content'], 'fileName', {
        type: 'image/png',
        lastModified: Date.now()
      });
    } catch (ignored) {
      return false;
    }
    // Everything is available
    return true;
  }
}
