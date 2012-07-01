window.onload = function() {
  var canvas=document.getElementById("opticvx_logo_canvas");
  initialize(canvas.width, canvas.height);
  canvas.startTime = (new Date()).getTime();
  canvas.stop_animation = false;
  animate();
}
window.requestAnimFrame = (function(callback){
  return window.requestAnimationFrame ||
  window.webkitRequestAnimationFrame ||
  window.mozRequestAnimationFrame ||
  window.oRequestAnimationFrame ||
  window.msRequestAnimationFrame ||
  function(callback){
      window.setTimeout(callback, 1000 / 60);
  };
})();
function animate(){
  var canvas=document.getElementById("opticvx_logo_canvas");
  var context=canvas.getContext("2d");

  //get the time
  var time_seconds = ((new Date()).getTime() - canvas.startTime)*0.001;

  //update the state
  update(time_seconds, canvas.width, canvas.height)

  // clear the canvas
  context.clearRect(0, 0, canvas.width, canvas.height);

  // draw the frame
  draw(context, time_seconds, canvas.width, canvas.height);

  // request new frame
  if(!canvas.stop_animation) {
    window.requestAnimFrame(function(){
        animate();
    });
  }
}
function initialize(cw,ch){

}
function parabola(x,cw) {
  return (1-Math.pow((x-(cw/2))/(cw/2),2)) * (75/200)*cw;
}
function draw(context, time_seconds, cw, ch){
  var anim_stage_1_time = 0.8;
  var anim_stage_2_time = 0.6;
  var anim_stage_3_time = 0.6;
  var anim_stage_4_time = 0.2;
  var anim_stage_5_time = 0.5;

  var x0 = (50/200)*cw;
  var x1 = (180/200)*cw;
  var y0 = parabola(x0,cw);
  var y1 = parabola(x1,cw);
  var anim5 = 0;

  if(time_seconds > anim_stage_1_time + anim_stage_2_time + anim_stage_3_time + anim_stage_4_time + anim_stage_5_time) {
    var canvas=document.getElementById("opticvx_logo_canvas");
    canvas.stop_animation = true;
  }
  if(time_seconds >= anim_stage_1_time + anim_stage_2_time + anim_stage_3_time + anim_stage_4_time) {
    var anim5 = (time_seconds - anim_stage_1_time - anim_stage_2_time - anim_stage_3_time - anim_stage_4_time) / anim_stage_5_time;
    if(anim5 > 1) {
      anim5 = 1;
    }
    context.beginPath();
    for(x=x0; x<=x1; x+=0.05*cw) {
      var y = parabola(x,cw);
      context.lineTo(x,y);
    }
    context.closePath();
    context.globalAlpha = anim5;
    context.fillStyle="#9932CC"
    context.fill();
    context.globalAlpha = 1;
  }
  if(time_seconds >= 0.0) {
    var anim = time_seconds / anim_stage_1_time;
    if(anim > 1) {
      anim = 1;
    }
    anim = (anim*2-1);
    anim = 4*Math.atan(anim)/Math.PI;
    anim = (anim+1)/2;
    context.beginPath();
    for(x=0; x<=cw*anim; x+=0.05*cw) {
      var y = parabola(x,cw);
      context.lineTo(x,y);
    }
    context.lineWidth = Math.ceil(cw/100);
    context.strokeStyle="#000000"
    context.stroke();
  }
  if(time_seconds >= anim_stage_1_time) {
    var anim = (time_seconds - anim_stage_1_time) / anim_stage_2_time;
    if(anim > 1) {
      anim = 1;
    }
    context.beginPath();
    context.lineTo(0,y0+((y1-y0)/(x1-x0))*(0-x0));
    context.lineTo(cw*anim,y0+((y1-y0)/(x1-x0))*(cw*anim-x0));
    context.lineWidth = Math.ceil(cw/100);
    context.strokeStyle="#000000"
    context.stroke();
  }
  context.shadowColor="rgb(" + Math.floor(255*(1-anim5)) + "," + Math.floor(255*(1-anim5)) + "," + Math.floor(255*(1-anim5)) + ")";
  context.shadowOffsetX = Math.ceil(cw/150);
  context.shadowOffsetY = Math.ceil(cw/150);
  context.shadowBlur= Math.ceil(cw/75);
  if(time_seconds >= anim_stage_1_time + anim_stage_2_time) {
    var anim = (time_seconds - anim_stage_1_time - anim_stage_2_time) / anim_stage_3_time;
    if(anim > 1) {
      anim = 1;
    }
    var grd=context.createLinearGradient(0,0,cw,0);
    grd.addColorStop(anim * 0.9,"black");
    grd.addColorStop(anim * 0.9 + 0.1,"white");
    context.fillStyle=grd;
    context.textBaseline="top";
    context.font="italic bold " + Math.ceil(cw/5) + "px Georgia";
    context.textAlign="right";
    if(anim5 > 0) {
      context.fillStyle="rgb(0," + Math.floor(90*anim5) + "," + Math.floor(70*anim5) + ")"; //"#005A46"
    }
    context.fillText("opti", cw/2, (77/200)*cw);
    context.textAlign="left";
    context.font="bolder " + Math.ceil(cw/5) + "px Georgia";
    if(anim5 > 0) {
      context.fillStyle="rgb(" + Math.floor(164*anim5) + ",0," + Math.floor(29*anim5) + ")"; //"#A4001D"
    }
    context.fillText("CVX", cw/2, (77/200)*cw);
  }
  context.shadowBlur=0
  context.shadowOffsetX = 0;
  context.shadowOffsetY = 0;
}
function update(time_seconds, cw, ch){

}