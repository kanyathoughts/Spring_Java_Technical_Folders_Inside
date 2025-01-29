/* remove all @innowake/* dependencies from package-lock.json, so the latest snapshot is installed when running npm install */
const fs = require('fs');
const exec = require('child_process').exec;

/* require path is relative to script location, therefore '../' */
const packageLockJson = require('../package-lock.json');


delete packageLockJson.packages['node_modules/@innowake/mining-api-angular-client']

/* file path is relative to working directory, therefore NO '../' */
fs.writeFileSync('package-lock.json', JSON.stringify(packageLockJson, null, 2));

/**re-install manually the @innovake package to avoid differences between package.json and package-lock.json*/
exec('npm install --save @innowake/mining-api-angular-client')