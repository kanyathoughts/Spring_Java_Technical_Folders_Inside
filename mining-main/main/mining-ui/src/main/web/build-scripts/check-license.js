const checker     = require('license-checker');
const fs          = require('fs-extra');
const _           = require('lodash');

/* where to write dependency and license information, so the maven build process can pick it up */
const LICENSE_FILE = '../../../npm-licenses.txt';

const options = {
  start: '.',
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
  _.forEach(packagesSorted, (packageIdentifier) => {
    let licenses = dependencies[packageIdentifier].licenses;
    /*
     * Workarounds for individual packages
     */
    /* do not include packages for which license is not available */
    if (packageIdentifier.startsWith('mining-web-ui-frontend@') || packageIdentifier.startsWith('yfiles@26.0.2')) {
      return; // continue
    }
    /* convert SPDX "(licenseA AND licenseB)" format to array */
    if (typeof licenses === 'string' && licenses.indexOf(' AND ') > 0) {
      licenses = licenses.split(' AND ');
    }
    /* convert SPDX "(licenseA OR licenseB)" format to array */
    if (typeof licenses === 'string' && licenses.indexOf(' OR ') > 0) {
      licenses = licenses.split(' OR ');
    }

    licenseFile.write(packageIdentifier)
    licenseFile.write('\n')
    licenseFile.write(JSON.stringify(licenses))
    licenseFile.write('\n')
    if (dependencies[packageIdentifier].licenseFile) {
      licenseFile.write(JSON.stringify(dependencies[packageIdentifier].licenseFile))
    } else {
      licenseFile.write('null')
    }
    licenseFile.write('\n')
  })
  licenseFile.end()
})
