function Point(_x, _y){
  this.x = _x | 0;
  this.y = _y | 0;
}

var points;
var time;

// Graphics options
var inWidth = undefined;
var inHeight = undefined;
var lerpPointSize = 0.5;
var iter = 400;
var radius = undefined;
var rotationOffset = 2*Math.PI;
var rings = 1000/5;
var timestep = 100;
var randomDistribution = false;
var fuzzyRandom = false;
var stripedFade = false;
var colors = 0;
var opacity = 255*0.5;

var colorMask = [false, false, false];
var rVariance = 255;
var gVariance = 255;
var bVariance = 255;
var rRatio = Math.ceil(Math.random()*5);
var gRatio = Math.ceil(Math.random()*5);
var bRatio = Math.ceil(Math.random()*5);
var colorValues = [0, 0, 0];

var started = false;
var optionsConfirmed = false;

Array.prototype.numTrue = function(){
	var c = 0;
	for(var i = 0; i < this.length; i++){
		if(this[i]) c++;
	}
	return c;
}

function setColors(n){
	for(var i = 0; i < colorMask.length; i++) colorMask[i] = false;
	var prevVals = [];
	for(var i = 0; i < n; i++){
		var val = Math.floor(Math.random()*3);
		while(prevVals.indexOf(val) > -1){
			val = Math.floor(Math.random()*3);
		}
		colorMask[val] = true;
		prevVals.push(val);
	}
}

function getColorValues(t){
	if(colorMask[0]){
		colorValues[0] = (255-rVariance)+rVariance*sin(t/(1000+rings)*rRatio);
	} else colorValues[0] = 255;
	if(colorMask[1]){
		colorValues[1] = (255-gVariance)+gVariance*sin(t/(1000+rings)*gRatio);
	} else colorValues[1] = 255;
	if(colorMask[2]){
		colorValues[2] = (255-bVariance)+bVariance*sin(t/(1000+rings)*bRatio);
	} else colorValues[2] = 255;
}

function setup(){
	points = [new Point(), new Point(), new Point()];
	time = 0;

	createCanvas(window.innerWidth | inWidth, window.innerHeight | inHeight);
	if(radius == undefined){
		if(width < height){
			radius = width/2.718;
		} else {
			radius = height/2.718;
		}
	}

	// Set up p5 rendering
	smooth();
	fill(255, 255, 255, 200);
	//stroke(0, 0, 0);
	noStroke();
	background(0);
	textSize(12);
	textAlign(CENTER);
	textStyle(ITALIC);
}

function draw(){
	if(started && optionsConfirmed){
		for(var i = 0; i < 2; i++){
			time += timestep;

			points[0].x = (width/2)+cos(time/1000+rotationOffset)*radius;
			points[0].y = (height/2)+sin(time/1000+rotationOffset)*radius;
			points[1].x = (width/2)+cos(time/1000+PI/3)*(radius+radius*sin(time/(1000+rings)));
			points[1].y = (height/2)+sin(time/1000+PI/3)*(radius+radius*sin(time/(1000+rings)));
			points[2].x = (width/2)+cos(time/1000+PI)*radius;
			points[2].y = (height/2)+sin(time/1000+PI)*radius;

			getColorValues(time);

			for(var i = 0; i < points.length-2; i++){
				for(var j = 0; j < iter; j++){
					if(randomDistribution){
						var k;
						if(fuzzyRandom){
							k = random()*iter;
						} else {
							j = random()*iter;
							k = j;
						}
					} else k = j;
					var p1 = new Point(points[i].x+(points[i+1].x-points[i].x)*(j+1)/iter,
					points[i].y+(points[i+1].y-points[i].y)*(j+1)/iter);
					var p2 = new Point(points[i+1].x+(points[i+2].x-points[i+1].x)*(j+1)/iter,
					points[i+1].y+(points[i+2].y-points[i+1].y)*(j+1)/iter);
					fill(colorValues[0], colorValues[1], colorValues[2], opacity*gaussian((k/iter)/0.5-1));
					ellipse(p1.x+(p2.x-p1.x)*(k+1)/iter, p1.y+(p2.y-p1.y)*(k+1)/iter, lerpPointSize, lerpPointSize);
				}
			}
		}
	}
}

function distance(x1, y1, x2, y2){
	return sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
}

function gaussian(x, time){
	var s = 0.3;
	var e = 2.71828182845;
	var o = 1/(sqrt(TWO_PI)*s)*Math.pow(e, -(x*x)/(2*s*s));
	if(stripedFade){
		o += (sin(25*PI*x)*0.5);
	}
	return o;
}

function getColor(time){

}

window.addEventListener("keydown", function(e){
	if(e.keyCode == "32" && optionsConfirmed) started = !started;
});

window.addEventListener("keydown", function(e){
	if(e.keyCode == "82"){
		time = 0;
		background(0);
	}
});

window.addEventListener("keydown", function(e){
	if(e.keyCode == "8"){
		e.preventDefault();
		started = false;
		optionsConfirmed = false;
		time = 0;
		background(0);
		document.getElementById("controlPanelHolder").style.display = "";
	}
});