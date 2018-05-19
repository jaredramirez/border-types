var binwrap = require('binwrap');
var path = require('path');

var packageInfo = require(path.join(__dirname, 'package.json'));
var binVersion = packageInfo.version.replace(/^(\d+\.\d+\.\d+).*$/, '$1');
var root =
  'https://github.com/jaredramirez/border-types/releases/download/v' +
  binVersion +
  '/border-types';

module.exports = binwrap({
  binaries: ['border-types'],
  urls: {
    'darwin-x64': root + '-macos-x64.tar.gz',
    'linux-x64': root + '-linux-x64.tar.gz',
  },
});
