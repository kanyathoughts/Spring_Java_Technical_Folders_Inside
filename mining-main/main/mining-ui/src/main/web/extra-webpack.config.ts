const YWorksOptimizerPlugin = require('@yworks/optimizer/webpack-plugin');
const MonacoEditorWebpackPlugin = require('monaco-editor-webpack-plugin');
import * as path from 'path';
import * as webpack from 'webpack';

module.exports = function(config : webpack.Configuration) {
  
  config?.plugins?.push(new MonacoEditorWebpackPlugin({
    // a ton of languages are lazily loaded by default, but we dont use any of them
    languages: [],
    // we can disable features that we end up not needing/using
    features: [
      'accessibilityHelp',
      'anchorSelect',
      'bracketMatching',
      // 'browser',
      'caretOperations',
      'clipboard',
      'codeAction',
      'codelens',
      // 'colorPicker',
      // 'comment',
      'contextmenu',
      'copyPaste',
      'cursorUndo',
      // 'dnd',
      // 'documentSymbols',
      // 'dropIntoEditor',
      'find',
      // 'folding',
      // 'fontZoom',
      'format',
      // 'gotoError',
      'gotoLine',
      'gotoSymbol',
      'hover',
      // 'iPadShowKeyboard',
      // 'inPlaceReplace',
      'indentation',
      // 'inlayHints',
      'inlineCompletions',
      // 'inspectTokens',
      'lineSelection',
      'linesOperations',
      // 'linkedEditing',
      'links',
      // 'multicursor',
      // 'parameterHints',
      'quickCommand',
      // 'quickHelp',
      // 'quickOutline',
      // 'readOnlyMessage',
      'referenceSearch',
      // 'rename',
      'smartSelect',
      // 'snippet',
      'stickyScroll',
      // 'suggest',
      // 'toggleHighContrast',
      'toggleTabFocusMode',
      'tokenization',
      'unicodeHighlighter',
      // 'unusualLineTerminators',
      // 'viewportSemanticTokens',
      'wordHighlighter',
      'wordOperations',
      'wordPartOperations',
    ],
  }));
  // Remove the existing css loader rule
  const cssRuleIdx = config?.module?.rules?.findIndex((rule: any) =>
    rule.test?.toString().includes(':css')
  );
  if (cssRuleIdx !== -1) {
    config?.module?.rules?.splice(cssRuleIdx!, 1);
  }
  config?.module?.rules?.push(
    {
      test: /\.css$/,
      include: path.resolve(__dirname, 'node_modules'),
      use: ['style-loader', 'css-loader'],
    },
    { 
      test: /\.ttf$/,
      include: path.resolve(__dirname, 'node_modules/monaco-editor'),
      type: 'asset/resource'
    }
  );
  
  if (config.mode === 'production') {
   config.plugins.push(
      new YWorksOptimizerPlugin({
        shouldOptimize(module: any) {
          return (
            /* add the corresponding regex to include the files that should be obfuscated here */
            /yfiles/.test(module.resource)
            || /module-control-flow/.test(module.resource)
            || /mining-graphs-export/.test(module.resource)
            || /dependency-explorer/.test(module.resource)
            || /dependency-graph/.test(module.resource)
            || /dependency-graph-utility/.test(module.resource)
            || /yfiles-util/.test(module.resource)
            || /graph-global-styles.ts/.test(module.resource)
            || /graph-node-label-style.model.ts/.test(module.resource)
            || /yfile-graph-info/.test(module.resource)
            || /graph-layouts/.test(module.resource)
            || /node-configurations/.test(module.resource)
            || /node-label-configuration.ts/.test(module.resource)
            || /code-viewer-button.utils.ts/.test(module.resource)
            || /graph.util/.test(module.resource)
            || /data-flow-graph/.test(module.resource)
            || /module-data-lineage/.test(module.resource)
            || /functional-analysis-graph/.test(module.resource)
            || /reachability-explorer/.test(module.resource)
            || /reachability-graph/.test(module.resource)
          )},
        logLevel: 'info',
        blacklist: ['update', 'template']
      })
    )
  }

  return config
}
