function distance(x1, y1, x2, y2){
	return Math.sqrt((y2-y1)*(y2-y1)+(x2-x1)*(x2-x1));
}

function Particle(_x, _y){
	this.x = _x | 0;
	this.y = _y | 0;
	this.v = 0;
	this.dx = 0;
	this.dy = 0;
	this.r = 8;
	
	this.draw = function(){
		ellipse(this.x, this.y, this.r, this.r);
	}

	this.update = function(t){
		if(Math.abs(this.dx) > maxDelta){
			if(this.dx > 0) this.dx = maxDelta;
			else this.dx = -maxDelta;
		}
		if(Math.abs(this.dy) > maxDelta){
			if(this.dy > 0) this.dy = maxDelta;
			else this.dy = -maxDelta;
		}
		if(this.x > width && this.dx > 0) this.dx *= -1;
		if(this.x < 0 && this.dx < 0) this.dx *= -1;
		if(this.y > height && this.dy > 0) this.dy *= -1;
		if(this.y < 0 && this.dy < 0) this.dy *= -1;
		this.x += this.dx*t;
		this.y += this.dy*t;
		this.v = Math.sqrt(this.dx*this.dx + this.dy*this.dy);
	}
}

function GravityWell(_x, _y, _s){
	this.x = _x | 0;
	this.y = _y | 0;
	this.s = _s | 1;
	
	this.getForce = function(o){
		var dist = distance(this.x, this.y, o.x, o.y);
		var angle = Math.atan2(this.y-o.y, this.x-o.x);
		var force = this.s*4/dist*dist;
		return {
			//x: Math.cos(angle)*force,
			x: 0,
			y: Math.sin(angle)*force
		}
	}
}

var particles = [];
var gravityWells = [];
var maxDelta = 500;
var renderCircles = true;

var spacing = 40;

var currentTime = window.performance.now();
var prevTime = window.performance.now();
var fps = 0;
var frames = 0;
var drawFps = false;

var fft;

var bass;
var mid;
var high;

var bassPeak = 0;
var midPeak = 0;
var highPeak = 0;

var input;

function setup(){
	input = new p5.AudioIn();
	input.start();
	fft = new p5.FFT();
	fft.setInput(input);

	var can = createCanvas(window.innerWidth, window.innerHeight);
	for(var i = 0; i < width/spacing; i++){
		for(var j = 0; j < height/spacing; j++){
			particles.push(new Particle(i*spacing+10, j*spacing+10));
		}
	}
	for(var i = 0; i < particles.length; i++){
		particles[i].x += (Math.random()*2-1)*spacing;
		particles[i].y += (Math.random()*2-1)*spacing;
		if(particles[i].x >= width || particles[i].x <= 0
			|| particles[i].y >= height || particles[i].y <= 0){
			particles.splice(i, 1);
		}
	}
	gravityWells.push(new GravityWell(width/2-width/3, height/2));
	gravityWells.push(new GravityWell(width/2, height/2));
	gravityWells.push(new GravityWell(width/2+width/3, height/2));
	noStroke();
	smooth();
	background(0);
}

function draw(){
	background(0);
	currentTime = window.performance.now();
	var dTime = (currentTime-prevTime)/1000;
	fps = Math.round(1/dTime);
	frames++;

	var spectrum = fft.analyze();
	bass = 0;
	mid = 0;
	high = 0;

	for(var i = 0; i < Math.round(spectrum.length/13)+100; i++){
		bass += map(spectrum[i], 0, 255, 0, 10);
	}
	for(var i = Math.round(spectrum.length/13); i < Math.round(spectrum.length*3/13); i++){
		mid += map(spectrum[i], 0, 255, 0, 10);
	}
	for(var i = Math.round(spectrum.length*3/13)-70; i < Math.round(spectrum.length*6/13); i++){
		high += map(spectrum[i], 0, 255, 0, 10);
	}

	if(bass == 0 && mid == 0 && high == 0 || frames%50 == 0){
		bassPeak = 0;
		midPeak = 0;
		highPeak = 0;
	}

	if(bass > bassPeak) bassPeak = bass;
	if(mid > midPeak) midPeak = mid;
	if(high > highPeak) highPeak = high;

	gravityWells[0].s = bass-bassPeak/3;
	gravityWells[1].s = mid-midPeak/3;
	gravityWells[2].s = high-highPeak/3;

	gravityWells[0].y = height/2+sin(millis()/500)*height/12;
	gravityWells[1].y = height/2+sin(millis()/500+PI/3)*height/12;
	gravityWells[2].y = height/2+sin(millis()/500+TWO_PI/3)*height/12;

	if(renderCircles){
		for(var i = 0; i < gravityWells.length; i++){
			fill(255, 255, 255, 255*90/gravityWells[i].s);
			ellipse(gravityWells[i].x, gravityWells[i].y, gravityWells[i].s/1.5, gravityWells[i].s/1.5, 30);
		}
	}
	
	fill(255, 255, 255, 200);
	for(var i = 0; i < particles.length; i++){
		var f;
		for(var j = 0; j < gravityWells.length; j++){
			f = gravityWells[j].getForce(particles[i]);
			particles[i].dx += f.x*dTime*0.007;
			particles[i].dy += f.y*dTime;
		}
		particles[i].r = 2+(1-particles[i].v/maxDelta)*6;
		particles[i].update(dTime);
		fill(255, 255, 255, 255*(particles[i].v/maxDelta));
		particles[i].draw();
		if(frames%70 == 0){
			if(particles[i].x > width || particles[i].x < 0
			|| particles[i].y > height || particles[i].y < 0){
				particles[i].x = Math.random()*width;
				particles[i].y = Math.random()*height;
				particles[i].dx = 0;
				particles[i].dy = 0;
			}
		}
	}

	if(drawFps){
		fill(255, 255, 255);
		text(fps+"fps", 10, 20);
	}
	prevTime = currentTime;
}

function normalize(){
	bassPeak = 0;
	midPeak = 0;
	highPeak = 0;
}

window.addEventListener('keydown', function(e){
	if(e.keyCode == 67) renderCircles = !renderCircles;
	else if(e.keyCode == 70) drawFps = !drawFps;
});

window.addEventListener('resize', function(e){
	resizeCanvas(window.innerWidth, window.innerHeight);
	gravityWells[0].x = width/2-width/3;
	gravityWells[0].y = height/2;

	gravityWells[1].x = width/2;
	gravityWells[1].y = height/2;

	gravityWells[2].x = width/2+width/3;
	gravityWells[2].y = height/2;
	console.log("resized");
});