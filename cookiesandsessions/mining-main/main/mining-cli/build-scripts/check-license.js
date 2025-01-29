const checker     = require('license-checker');
const fs          = require('fs-extra');
const _           = require('lodash');

/* where to write dependency and license information, so the maven build process can pick it up */
const LICENSE_FILE = 'npm-licenses.txt';

const options = {
  start: 'packages/mining-cli',
  production: true, /* examine only production dependencies (not build-time only) */
  customFormat: {
    path: null /* include dependency path in result */
  }
}

checker.init(options, (err, dependencies) => {
  if (err) {
    console.log("Error collecting dependency information", err);
    return;
  }

  let packagesSorted = _.sortedUniq(_.keys(dependencies).sort())
  let licenseFile = fs.createWriteStream(LICENSE_FILE)
  _.forEach(packagesSorted, (package) => {
    let licenses = dependencies[package].licenses

    /*
     * Workarounds for individual packages
     */

    /* do not include our own packages in dependencies */
    if (package.startsWith('iw-ioc') || package.startsWith('mining-api-client')) {
      return; // continue
    }

    /* convert SPDX "(licenseA AND licenseB)" format to array */
    if (typeof licenses === 'string' && licenses.startsWith('(')
        && licenses.indexOf(' AND ') > 0) {
      licenses = licenses.substring(1, licenses.length - 1).split(' AND ')
    }
    /* convert SPDX "(licenseA OR licenseB)" format to array */
    if (typeof licenses === 'string' && licenses.startsWith('(')
        && licenses.indexOf(' OR ') > 0) {
      licenses = licenses.substring(1, licenses.length - 1).split(' OR ')
    }

    licenseFile.write(package)
    licenseFile.write('\n')
    licenseFile.write(JSON.stringify(licenses))
    licenseFile.write('\n')
    if (dependencies[package].licenseFile) {
      licenseFile.write(JSON.stringify(dependencies[package].licenseFile))
    } else {
      licenseFile.write('null')
    }
    licenseFile.write('\n')
  })
  licenseFile.end()
})
