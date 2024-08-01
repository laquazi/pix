
// function point(x, y) {
//   return { x: x, y: y };
// }

function triggerDownload(imgURI, filename) {
  const a = document.createElement('a');
  a.download = filename + '.png';
  a.target = '_blank';
  a.href = imgURI;

  a.dispatchEvent(new MouseEvent('click', {
    view: window,
    bubbles: false,
    cancelable: true
  }));
}

function downloadSvg(svgNode, filename, width, height) {
  const svgString = (new XMLSerializer()).serializeToString(svgNode);
  const svgBlob = new Blob([svgString], {
    type: 'image/svg+xml;charset=utf-8'
  });

  const DOM_URL = window.URL || window.webkitURL || window;
  const url = DOM_URL.createObjectURL(svgBlob);

  const image = new Image();
  image.width = width;
  image.height = height;
  image.src = url;
  image.onload = function () {
    const canvas = document.createElement('canvas');
    canvas.width = image.width;
    canvas.height = image.height;

    const ctx = canvas.getContext('2d');
    ctx.drawImage(image, 0, 0);
    DOM_URL.revokeObjectURL(url);

    const imgURI = canvas
      .toDataURL('image/png')
      .replace('image/png', 'image/octet-stream');
    triggerDownload(imgURI, filename);
  };
}

// MAIN
var shell = document.getElementById('shell');

var app = window.Elm.Main.init({
  node: shell
});

// app.ports.canvasRulerPressed.send(point(x, y));

//var canvasRuler = document.getElementById('canvasRuler');

//canvasRuler.addEventListener('click', function (event) {
//  const target = event.target;
//  const rect = target.getBoundingClientRect();
//  let x = Math.floor(event.clientX - rect.left);
//  let y = Math.floor(event.clientY - rect.top);
//  app.ports.canvasRulerPressed.send(point(x, y));
//});


//app.ports.downloadSvgAsPng.subscribe(function (message) {
//  const element = document.getElementById(message.elementId);
//  downloadSvg(element, message.filename, message.size.x, message.size.y);
//});
