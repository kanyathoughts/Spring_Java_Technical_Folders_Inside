import { createReadStream, writeFile } from 'fs-extra';
import { dirname, basename, extname, sep } from 'path';
import * as csv from 'csv';
import _ from 'lodash';

const CALL_CHAIN_ENTRY_PATTERN = /([^\(\)\[\]]+)\((\w+)\)\[(\w*)\]/;

interface CallChainEntry {
  program: string;
  type: string;
}

interface ConsolidatedCallChain {
  [name: string]: {
    [type: string]: Set<string>;
  };
}

async function main() {
  if (process.argv.length < 3) {
    console.log('Usage: mining-call-chain-consolidator <call chain csv files...>');
    process.exit(1);
  }

  const fileNames = process.argv.slice(2);
  const consolidated: ConsolidatedCallChain = {};
  let mainType = '';
  for (const fileName of fileNames) {
    mainType = await parseAndConsolidateCsv(fileName, consolidated);
  }
  const table = consolidatedToTable(mainType, consolidated);
  exportAsCsv(dirname(fileNames[0]) + sep + basename(fileNames[0], extname(fileNames[0])) + '_consolidated.csv', table);
}

async function parseAndConsolidateCsv(fileName: string, consolidated: ConsolidatedCallChain): Promise<string> {
  return new Promise((resolve, reject) => {
    let first = true;
    let callChainDirection: string;
    let mainType: string;
    createReadStream(fileName, 'utf8')
      .pipe(csv.parse({
        relax_column_count: true
      }))
      .pipe(csv.transform(record => {
        if (first) {
          callChainDirection = parseCallChainDirection(record);
          if (callChainDirection !== 'IN' && callChainDirection !== 'OUT') {
            console.error('Call chain direction is', callChainDirection, 'Please import the inbound and outboud call chain separately');
            process.exit(1);
          }
          first = false;
        } else if (record.length === 0 || record.length === 1 && record[0] === '') {
          /* skip empty lines */
          return;
        } else {
          const entries: CallChainEntry[] = record.map(parseCallChainEntry);
          const mainEntry = entries[callChainDirection === 'IN' ? entries.length - 1 : 0];
          const group = consolidated[mainEntry.program] = consolidated[mainEntry.program] ?? {};
          mainType = mainEntry.type;
          for (const entry of entries) {
            if (entry === mainEntry) {
              continue;
            }
            const set = group[entry.type] = group[entry.type] ?? new Set();
            set.add(entry.program);
          }
        }
      }, (err) => {
        if (err) {
          console.error('Error', err);
          reject(err);
        }
        resolve(mainType);
      }));
  });
}

function parseCallChainDirection(record: string[]): string {
  const start = record[0].indexOf('directions=[') + 'directions=['.length;
  const end = record[0].indexOf(']', start);
  return record[0].substring(start, end);
}

function parseCallChainEntry(s: string): CallChainEntry {
  const parsed = CALL_CHAIN_ENTRY_PATTERN.exec(s);

  if (parsed?.length !== 4) {
    console.error('invalid call chain entry', s);
    process.exit(1);
  }
  return {
    program: parsed[1],
    type: parsed[2]
  };
}

function consolidatedToTable(mainType: string, consolidated: ConsolidatedCallChain): string[][] {
  const table: string[][] = [];

  const columns = _.uniq(_.flatMap(consolidated, _.keys));
  table.push([mainType, ... columns]);
  _.forEach(consolidated, (entry, mainEntry) => {
    const asArrays = _.mapValues(entry, value => Array.from(value));
    const rows = _.max(_.map(asArrays, a => a.length)) ?? 0;
    for (let i = 0; i < rows; i++) {
      table.push([mainEntry, ... _.map(columns, col => asArrays[col]?.[i] ?? '')]);
    }
  });

  return table;
}

function exportAsCsv(fileName: string, table: string[][]) {
  const stringifier = csv.stringify(table, { quoted: true }, async (err, output) => {
    await writeFile(fileName, Buffer.from(output, 'utf8'));
    console.log('wrote', fileName);
  });
  table.forEach(row => stringifier.write(row));
  stringifier.end();
}

main();
