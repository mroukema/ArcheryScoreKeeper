(function () {
  var app = Elm.Main.init({
      node: document.getElementById('elmMain'),
      flags:
          [ "Galactic Empire"
          , "http://localhost:7500/ui/multi-site/dashboard/map"
          , "http://localhost:7500/ui/multi-site/dashboard/map"
          ]

  app.ports.getClientBoundingBox.subscribe(function(elementId) {
    var queriedElement = document.getElementById(elementId);
    console.log("getClientBoundingBox()");
    if (!queriedElement) {
      console.error("Can't find " + elementId)
      return;
    }
    var boundingBox = queriedElement.getBoundingClientRect();

    if (!boundingBox) {
      console.error('No bBox for ' + elementId)
      return;
    }
    app.ports.boundingBoxResult.send(boundingBox);
  });
})();
