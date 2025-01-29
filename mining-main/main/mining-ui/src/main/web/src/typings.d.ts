/*
 * Extra typings definitions
 */

// Allow .json files imports
declare module '*.json';

// SystemJS module definition
declare var module: NodeModule;
interface NodeModule {
  id: string;
}

// Allow .js file imports from node_modules for monaco editor
declare module 'monaco-editor/esm/vs/editor/common/languages/supports/tokenization.js';
declare module 'monaco-editor/esm/vs/editor/common/languages.js';
declare module 'monaco-editor/esm/vs/base/common/color.js';
