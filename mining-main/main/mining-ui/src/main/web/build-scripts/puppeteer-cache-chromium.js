/* the "puppeteer" package downloads a local version of Chromium and stores it in node_modules
 * this script copies it to the global npm cache directory, so we can restore it later
 * without having to download again, in case node_modules is deleted */

const exec = require('child_process').exec;
const fs = require('fs');
const path = require('path');

const PUPPETEER_CACHE_DIR = 'puppeteer-cache';

/* retrieve path to the global npm cache directory */
exec('npm config get cache', (err, stdout) => {
  if (err) {
    throw err;
  }

  const npmCachePath = stdout.trim();
  const puppeteerCachePath = path.join(npmCachePath, PUPPETEER_CACHE_DIR);
  const puppeteerDownloadPath = path.join('node_modules', 'puppeteer', '.local-chromium');

  /* create cache dir in global npm cache, if not exists */
  fs.mkdirSync(puppeteerCachePath, { recursive: true });

  /* copy all downloaded chromiums to the cache dir */
  if (fs.existsSync(puppeteerDownloadPath)) {
    fs.readdirSync(puppeteerDownloadPath).forEach(folder => {
      const dest = path.join(puppeteerCachePath, folder);
      if (!fs.existsSync(dest)) {
        copyRecursiveSync(path.join(puppeteerDownloadPath, folder), dest);
        console.log('puppeteer-cache-chromium: cached', dest);
      }
    });
  }
});

/**
 * node.js doesn't come with cp -R
 *
 * @param {string} src The path to the thing to copy.
 * @param {string} dest The path to the new copy.
 */
function copyRecursiveSync(src, dest) {
  const exists = fs.existsSync(src);
  const stats = exists && fs.lstatSync(src);
  const isDirectory = exists && stats.isDirectory();
  const isLink = exists && stats.isSymbolicLink();
  if (isDirectory) {
    fs.mkdirSync(dest);
    fs.readdirSync(src).forEach(function(childItemName) {
      copyRecursiveSync(path.join(src, childItemName), path.join(dest, childItemName));
    });
  } else if (isLink) {
    const linkTarget = fs.readlinkSync(src);
    fs.symlinkSync(linkTarget, dest);
  } else {
    fs.copyFileSync(src, dest);
  }
}
