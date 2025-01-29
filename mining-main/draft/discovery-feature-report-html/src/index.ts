
import { Command } from 'commander';
import { readFile, writeFile } from 'fs/promises';
import { resolve } from 'path';
import { makeDocument } from './make-document.js';

import './async-operations-hack.js';

export interface Options {
  outputDir: string;
}

function main(argv: string[]) {

  const app = new Command('discovery-feature-model');

  app.argument('<modelFile>', 'JSON file containing discovery feature model');
  app.option('-o --output-dir', 'output directory', '.')

  app.action(action);

  app.parse(argv);
}

async function action(modelFile: string, options: Options) {
  const content = await readFile(resolve(modelFile), 'utf8');
  const model = JSON.parse(content);

  console.log('rendering ...');
  const dom = await makeDocument(model.featureMatrix);

  /* HACK: wait for async operations that modify the DOM to complete, before serializing the DOM to file ... */
  await Promise.all(asyncOperations);

  console.log('writing output file ...');
  await writeFile(resolve(options.outputDir ?? '.', 'index.html'), dom.serialize());
}

main(process.argv);
