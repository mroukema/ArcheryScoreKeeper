(function () {
  var mainNode = document.getElementById('elmMain')
  var app = Elm.Main.embed(mainNode);


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
