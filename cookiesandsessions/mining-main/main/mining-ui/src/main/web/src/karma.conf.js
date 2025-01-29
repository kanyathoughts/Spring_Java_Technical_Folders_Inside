// Karma configuration file, see link for more information
// https://karma-runner.github.io/1.0/config/configuration-file.html
const process = require('process');
process.env.CHROME_BIN = require('puppeteer').executablePath();
const path = require('path');

module.exports = function(config) {
  config.set({
    basePath: '..',
    frameworks: ['jasmine', '@angular-devkit/build-angular'],
    plugins: [
      require('karma-jasmine'),
      require('karma-chrome-launcher'),
      require('karma-junit-reporter'),
      require('karma-coverage'),
      require('karma-spec-reporter'),
      require('@angular-devkit/build-angular/plugins/karma'),
      require('karma-jasmine-html-reporter')
    ],
    client: {
      clearContext: false, // leave Jasmine Spec Runner output visible in browser
      captureConsole: Boolean(process.env.KARMA_ENABLE_CONSOLE),
      jasmine: {
        random: false, // disable the random running order
        DEFAULT_TIMEOUT_INTERVAL: 10000
      }
    },
    junitReporter: {
      outputDir: path.join(__dirname, '../reports/junit/'),
      outputFile: 'TESTS-xunit.xml',
      useBrowserName: false,
      suite: '' // Will become the package name attribute in xml testsuite element
    },
    coverageReporter: {
      dir: path.join(__dirname, '../reports/coverage'),
      subdir: '.',
      reporters: [
        { type: 'html'},
      ],
      fixWebpackSourcePaths: true,
      check: {
        global: {
          statements: 70,
          branches: 50,
          functions:65,
          lines: 71
        }
      }
    },
    htmlReporter: {
      outputFile: 'tests/units.html'
    },
    angularCli: {
      environment: 'dev'
    },
    reporters: ['spec', 'junit', 'kjhtml'],
    port: 9875,
    colors: true,
    // Level of logging, can be: LOG_DISABLE || LOG_ERROR || LOG_WARN || LOG_INFO || LOG_DEBUG
    logLevel: config.LOG_DEBUG,
    autoWatch: true,
    browsers: ['Chrome'],
    singleRun: false,
    browserNoActivityTimeout: 400000,
    browserDisconnectTimeout: 40000,
    browserDisconnectTolerance: 4,
    files: [
      { pattern: './src/assets/**', watched: false, included: false, nocache: false, served: true }
    ],
    proxies: {
      '/assets/': '/base/src/assets/'
    }
  });
};
