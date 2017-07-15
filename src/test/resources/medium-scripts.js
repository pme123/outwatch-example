/**
 * Created by pascal.mengelt on 13.07.2017.
 */
/*
 Get JSON from a Path, e.g. "...../afHtml3Example.json"
 */
function getJson(jsonPath) {
  var Httpreq = new XMLHttpRequest(); // a new request
  Httpreq.open("GET", jsonPath, false);
  Httpreq.send(null);
  console.log("result: " + Httpreq.responseText);
  return Httpreq.responseText;
}

/*
 Get the name of the JSON
 */
function getJsonName() {
  // get path, e.g. "...../afHtml3Example_POSTFIX12MD5/index.html"
  var loc = window.location.pathname;
  // cut off file, e.g. "...../afHtml3Example_POSTFIX12MD5"
  loc = loc.substring(0, loc.lastIndexOf('/'));
  // cut off md5, e.g. "...../afHtml3Example"
  loc = loc.substring(0, loc.length - 13);
  return loc + ".json";
}

/*
 Function called by the AF to get its configuration
 */
var af_config = JSON.parse(getJson(getJsonName()));
