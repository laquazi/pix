
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

app.ports.pointerSetCaptureById.subscribe(data => {
    const element = document.getElementById(data.elementId);
    element.setPointerCapture(data.pointerId);
});

app.ports.pointerReleaseCaptureById.subscribe(data => {
    const element = document.getElementById(data.elementId);
    element.releasePointerCapture(data.pointerId);
});

// app.ports.capturePointer.subscribe(pointerEvent => {
//     pointerEvent.target.setPointerCapture(pointerEvent.pointerId);

//     var targetId = pointerEvent.target.id;
//     pointerEvent.target.addEventListener('change', function (changedEvent) {
//         if (changedEvent.target.id != targetId) {
//             changedEvent.target.releasePointerCapture(pointerEvent.pointerId);
//             var target = document.getElementById(targetId);
//             target.setPointerCapture(pointerEvent.pointerId);
//             targetId = target.id;
//         }
//     });


// });
