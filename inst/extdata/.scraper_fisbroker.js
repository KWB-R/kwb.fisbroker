// scraper_fisbroker.js
// Include the File System module for writing to files
var fs = require('fs');
var path = 'fisbroker.html'
var url = 'https://fbinter.stadt-berlin.de/fb/gisbroker.do;jsessionid=873981F6EB43A1C35CA50C4D38738949?cmd=navigation_Result&checkLoginKey=true&getresult=true'
var page = require('webpage').create();  
page.open(url, function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});
