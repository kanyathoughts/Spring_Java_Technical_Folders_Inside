import { IGrammar, INITIAL, parseRawGrammar, Registry, StackElement } from 'vscode-textmate';
import { createOnigScanner, createOnigString, loadWASM } from 'vscode-oniguruma';
import vsLightPlusTheme from '../../../../theme/vs-light-plus-theme';
import { LanguageInfo } from './language-info.interface';
import { Injectable } from '@angular/core';
import { generateTokensCSSForColorMap } from 'monaco-editor/esm/vs/editor/common/languages/supports/tokenization.js';
import { TokenizationRegistry} from 'monaco-editor/esm/vs/editor/common/languages.js';
import { Color } from 'monaco-editor/esm/vs/base/common/color.js';
import { Logger } from '@app/core';
import { languages  } from 'monaco-editor/esm/vs/editor/editor.api';
import { catchError, from, Observable, of, tap } from 'rxjs';
import { getBasePath } from '@app/core/utils/base-path.utils';

const logger = new Logger('LanguageProviderService');

@Injectable()
export class LanguageProviderService {
  grammars: { [scopeName: string]: LanguageInfo } = {
    'source.natural': {
      languageId: 'natural',
      pathTmFile: 'natural.tmLanguage.xml',
      fileType: 'xml',
    },
    'source.cobol': {
      languageId: 'cobol',
      pathTmFile: 'cobol.tmLanguage.json',
      fileType: 'json',
    },
    'source.pli': {
      languageId: 'pl1',
      pathTmFile: 'pli.tmLanguage.json',
      fileType: 'json',
    },
    'source.jcl': {
      languageId: 'jcl',
      pathTmFile: 'jcl.tmLanguage.json',
      fileType: 'json',
    },
    'source.bms': {
      languageId: 'bms',
      pathTmFile: 'bms.tmLanguage.json',
      fileType: 'json',
    },
    'source.assembler': {
      languageId: 'assembler',
      pathTmFile: 'hlasm.tmLanguage.json',
      fileType: 'json',
    },
    'source.java': {
      languageId: 'java',
      pathTmFile: 'java.plist',
      fileType: 'plist',
    },
    'source.basic': {
      languageId: 'basic',
      pathTmFile: 'basic.tmLanguage.json',
      fileType: 'json',
    }
  };
  loadedWASM: Response;
  private registry: Registry;

  init(): Observable<any> {
    return from(this.loadVSCodeOnigurumWASM()).pipe(
      tap((data: ArrayBuffer | Response) => {
      void loadWASM(data);
      this.registry = new Registry({
        onigLib: Promise.resolve({
          createOnigScanner,
          createOnigString,
        }),
        loadGrammar: async (scopeName: string) => {
          const scopeNameInfo = this.grammars[scopeName];
          if (scopeNameInfo == null) {
            return null;
          }
          const response = await fetch(getBasePath() + '/assets/textmate-grammar/' + scopeNameInfo.pathTmFile);
          const grammarContent = await response.text();
          /* setting the optional parameter 'example.json or example.xml' is important to let it interpret the content as JSON */
          return parseRawGrammar(grammarContent, 'example.' + scopeNameInfo.fileType);
        },
        theme: vsLightPlusTheme,
      });
    }), catchError(error => of(logger.error(error))));
  }

  /**
   * CSS is injected to override the default style
   */
  injectCSS(): void {
    const cssColors = this.registry ? this.registry.getColorMap() : null;
    /* eslint-disable @typescript-eslint/no-unsafe-argument */
    const colorMap = cssColors ? cssColors.map(Color.Format.CSS.parseHex) : [];
    TokenizationRegistry.setColorMap(colorMap);
    const css = generateTokensCSSForColorMap(colorMap);
    const style = createStyleElementForColorsCSS();
    style.innerHTML = css;
  }

  /**
   * Must be called after monaco.language.register
   * @param languageId supported language
   * @param monaco global object configuration
   * @param sourceCode language identifier
   * @returns Promise
   */
  async getTokensProviderForLanguage(
    languageId: string,
    sourceCode: string
  ): Promise<languages.EncodedTokensProvider> {
    const encodedLanguageId = languages.getEncodedLanguageId(languageId);
    const grammar: IGrammar = await this.registry
      .loadGrammarWithConfiguration(sourceCode, encodedLanguageId, {})
      .then((grammar) => grammar);
    return {
      getInitialState: () =>  INITIAL,
      tokenizeEncoded : (line: string, state: StackElement): languages.IEncodedLineTokens => {
        const tokenizeLineResult2 = grammar.tokenizeLine2(line, state);
        const { tokens, ruleStack: endState } = tokenizeLineResult2;
        return { tokens, endState };
      },
    };
  }

  /**
   * Pre loads the WASM file, Should be used when we are loading multiple editors in the same page
   * to avoid unnecessary loading of WASM file multiple times.
   */
  async loadMonacoWASM(): Promise<void> {
    this.loadedWASM = await fetch(getBasePath() + '/assets/monaco/onig.wasm');
  }

  private async loadVSCodeOnigurumWASM(): Promise<Response | ArrayBuffer> {
    if ( ! this.loadedWASM) {
      this.loadedWASM = await fetch(getBasePath() + '/assets/monaco/onig.wasm');
    }
    const contentType = this.loadedWASM.headers.get('content-type');
    if (contentType === 'application/wasm') {
      return this.loadedWASM;
    }

    // Using the response directly only works if the server sets the MIME type 'application/wasm'.
    // Otherwise, a TypeError is thrown when using the streaming compiler.
    // We therefore use the non-streaming compiler :(.
    return await this.loadedWASM.arrayBuffer();
  }
}

/**
 * Creates style element for overriding the default style
 * @returns HTMLStyleElement
 */
const createStyleElementForColorsCSS =(): HTMLStyleElement => {
  const styleElement = document.createElement('style');
  const monacoColors = document.getElementsByClassName('monaco-colors')[0];
  if (monacoColors) {
    monacoColors.parentElement?.insertBefore(styleElement, monacoColors.nextSibling);
  } else {
    let {head} = document;
    if ( ! head) {
      head = document.getElementsByTagName('head')[0];
    }
    head?.appendChild(styleElement);
  }
  return styleElement;
};
