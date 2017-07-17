(function () {
  var mainNode = document.getElementById('elmMain')
  var app = Elm.Main.embed(mainNode);


  app.ports.getClientBoundingBox.subscribe(function(elementId) {
    var queriedElement = document.getElementById(elementId);
    console.log("getClientBoundingBox");
    if (!queriedElement) {
      console.log("Can't find " + elementId)
      return;
    }
    var boundingBox = queriedElement.getBoundingClientRect();

    if (!boundingBox) {
      console.log('No bBox for ' + elementId)
      return;
    }
    console.log("foundBoundingBox");
    console.log(boundingBox);
    app.ports.boundingBoxResult.send(boundingBox);
  });
})();
