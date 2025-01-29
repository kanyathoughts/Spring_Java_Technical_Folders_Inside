/* This script restores cached Chromium versions from the global npm cache directory
 * and puts them in the location as expected by the "puppeteer" package.
 * It then runs the puppeteer package's "install" hook in order to verify the restored version
 * and download another version if required.
 */
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

  if (!fs.existsSync(puppeteerCachePath)) {
    console.log('puppeteer-retrieve-cached-chromium: no cached chromiums');
  } else {
    /* create download dir, if puppeteer was not yet installed */
    fs.mkdirSync(puppeteerDownloadPath, { recursive: true });

    /* copy all cached chromiums to the download dir,
     * so puppeteer will skip the download */
    fs.readdirSync(puppeteerCachePath).forEach(folder => {
      const dest = path.join(puppeteerDownloadPath, folder);
      if (!fs.existsSync(dest)) {
        const src = path.join(puppeteerCachePath, folder);
        copyRecursiveSync(src, dest);
        console.log('puppeteer-retrieve-cached-chromium: restored', dest, 'from', src);
      }
    });
  }

  /* rerun the "install" script of puppeteer to download missing version, in case it was not restored from cache
   * important: _unset_ the PUPPETEER_SKIP_CHROMIUM_DOWNLOAD environment variable to allow puppeteer to download missing version */
  exec(
    'npm explore puppeteer -- npm install',
    { env: { ...process.env, PUPPETEER_SKIP_CHROMIUM_DOWNLOAD: undefined } },
    (err, stdout) => {
      if (err) {
        throw err;
      }
      console.log(stdout);
    }
  );
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
