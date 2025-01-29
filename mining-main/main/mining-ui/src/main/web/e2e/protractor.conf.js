// Protractor configuration file, see link for more information
// https://github.com/angular/protractor/blob/master/lib/config.ts

const { SpecReporter } = require('jasmine-spec-reporter');
const fs = require('fs');
const path = require('path');
const downloadsPath = path.resolve(__dirname, './downloads');
exports.config = {
  SELENIUM_PROMISE_MANAGER: false,
  allScriptsTimeout: 60000,
  specs: ['./src/**/*.e2e-spec.ts'],
  capabilities: {
    browserName: process.env.PROTRACTOR_BROWSER || 'chrome',
    chromeOptions: {
      binary: require('puppeteer').executablePath(),
      args: ['headless', 'disable-gpu', 'window-size=1920,1080', 'lang=en-US'],
      prefs: {
        intl: { accept_languages: 'en-US' },
        download: {
          prompt_for_download: false,
          directory_upgrade: true,
          default_directory: downloadsPath,
        },
      },
    },
  },
  params: { 
    url: 'http://localhost:8080/'
  },
  // Only works with Chrome and Firefox
  directConnect: true,
  baseUrl: 'http://localhost:4224/',
  framework: 'jasmine',
  jasmineNodeOpts: {
    showColors: true,
    defaultTimeoutInterval: 600000,
    print: function () {},
  },
  onPrepare: async function () {
    browser.manage().timeouts().implicitlyWait(5000);
    // Better console spec reporter
    jasmine.getEnv().addReporter(new SpecReporter({ spec: { displayStacktrace: 'raw' } }));
    await browser.get('/#/login');
    await browser.findElement(by.css('input[formControlName="username"]')).sendKeys('admin');
    await browser.findElement(by.css('input[formControlName="password"]')).sendKeys('Worx2000');
    await browser.findElement(by.css('button[type="submit"]')).click();

    require('ts-node').register({ project: require('path').join(__dirname, './tsconfig.e2e.json') });

    return browser.wait(function () {
      return browser.getCurrentUrl().then(function (url) {
        return /clients/.test(url);
      });
    }, 600000);
  },
};

