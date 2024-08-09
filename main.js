
// function point(x, y) {
//   return { x: x, y: y };
// }

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

app.ports.capturePointer.subscribe(event => {
	event.target.setPointerCapture(event.pointerId);
});