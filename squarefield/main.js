var round = Math.round;
var pow = Math.pow;
var max = Math.max;
var abs = Math.abs;
var floor = Math.floor;

function rainbow(t){
	t = t%1;
	return {
		r: floor((max(-abs(3*(t-5/6))+1, 0)+max(-abs(3*(t+1/6))+1, 0))*255),
		g: floor((max(-abs(3*(t-1/6))+1, 0)+max(-abs(3*(t-7/6))+1, 0))*255),
		b: floor(max(-abs(3*(t-0.5))+1, 0)*255),
	};
}

function Segment(_start, _end){
	this.segment = true;
	this.start = _start;
	this.end = _end;
	this.dashed = false;
}

function intersects(a, b){
	// o: other line/segment/ray
	var m1 = (a.start.y-a.end.y)/(a.start.x-a.end.x);
	var m2 = (b.start.y-b.end.y)/(b.start.x-b.end.x);
	if(m1 != m2){
		var _x;
		var _y;
		if(Math.abs(m1) == Infinity){
			_x = a.start.x;
			_y = b.start.y+m2*(_x-b.start.x);
		} else if(Math.abs(m2) == Infinity){
			_x = b.start.x;
			_y = a.start.y+m1*(_x-a.start.x);
		} else {
			_x = (a.end.x*m1-a.end.y-b.start.x*m2+b.start.y)/(m1-m2);
			_y = b.start.y+m2*(_x-b.start.x);
		}
		if(_x >= Math.min(a.start.x, a.end.x) && _x <= Math.max(a.start.x, a.end.x)
		&& _x >= Math.min(b.start.x, b.end.x) && _x <= Math.max(b.start.x, b.end.x)
		&& _y >= Math.min(a.start.y, a.end.y) && _y <= Math.max(a.start.y, a.end.y)
		&& _y >= Math.min(b.start.y, b.end.y) && _y <= Math.max(b.start.y, b.end.y)){
			return true;
		}
	}
	return false;
}

//http://stackoverflow.com/questions/1255512/how-to-draw-a-rounded-rectangle-on-html-canvas
CanvasRenderingContext2D.prototype.roundRect = function (x, y, w, h, r) {
  if (w < 2 * r) r = w / 2;
  if (h < 2 * r) r = h / 2;
  this.beginPath();
  this.moveTo(x+r, y);
  this.arcTo(x+w, y,   x+w, y+h, r);
  this.arcTo(x+w, y+h, x,   y+h, r);
  this.arcTo(x,   y+h, x,   y,   r);
  this.arcTo(x,   y,   x+w, y,   r);
  this.closePath();
  return this;
}

function dshm(x){
	return -Math.pow(x-1, 2)*Math.cos(Math.pow(x, 2)/0.1)+1;
}

var can = document.getElementsByTagName("canvas")[0];
can.style.transform = "";
var width = window.innerWidth;
var height = window.innerHeight;
can.width = width;
can.height = height;
var ctx = can.getContext("2d");
ctx.scale(1, -1);
ctx.translate(0, -height);

function Vector2(_x, _y){
	this.x = _x;
	this.y = _y;

	this.add = function(o){
		this.x += o.x;
		this.y += o.y;
	}

	this.add = function(_x, _y){
		this.x = _x;
		this.y = _y;
	}

	this.multiplied = function(s){
		return new Vector2(this.x*s, this.y*s);
	}

	this.normalize = function(){
		var length = Math.sqrt(this.x*this.x + this.y*this.y);
		this.x /= length;
		this.x /= length;
	}

	this.rotate = function(a){
		var length = Math.sqrt(this.x*this.x + this.y*this.y);
		var angle = Math.atan2(this.y, this.x);
		this.x = Math.cos(angle+a)*length;
		this.y = Math.sin(angle+a)*length;
	}
}

var mousePos = new Vector2(width/2, height/2);
var mouseDown = false;

var playerSpeed = 100;

var left = false;
var right = false;
var friction = 0.9;

var boxStartSpeed = 500;
var boxSpeed = boxStartSpeed;

function Player(x, y){
	this.pos = new Vector2(x, y);
	this.vel = 0;
	this.size = 30;
	this.maxVel = 1000;
	this.accel = 0;
	
	this.invincibilityTimer = 0;
	
	this.invincible = false;

	this.collisionBounds = [new Segment({x: 0,y: 0}, {x: 0,y: 0}),
							new Segment({x: 0,y: 0}, {x: 0,y: 0}),
							new Segment({x: 0,y: 0}, {x: 0,y: 0})];

	this.update = function(dt){
		this.vel += this.accel;
		if(this.vel > this.maxVel) this.vel = this.maxVel;
		if(this.vel < -this.maxVel) this.vel = -this.maxVel;
		if(this.accel === 0){
			this.vel *= friction;
		}
		this.pos.x += this.vel*dt;
		if(this.invincibilityTimer > 0){
			this.invincible = true;
			this.invincibilityTimer -= dt;
		} else this.invincible = false;
		if(this.invincibilityTimer < 0) this.invincibilityTimer = 0;

		// NOTE These formulas must match the ones in the render function,
		//     otherwise what's rendered won't be the same as the collision volume
		this.collisionBounds[0].start.x = this.pos.x-this.size/2;
		this.collisionBounds[0].start.y = this.pos.y+(this.vel/this.maxVel)*this.size/4;
		this.collisionBounds[0].end.x = this.pos.x+(this.vel/this.maxVel)*this.size/2;
		this.collisionBounds[0].end.y = this.pos.y+this.size;

		this.collisionBounds[1].start.x = this.pos.x+(this.vel/this.maxVel)*this.size/2;
		this.collisionBounds[1].start.y = this.pos.y+this.size;
		this.collisionBounds[1].end.x = this.pos.x+this.size/2;
		this.collisionBounds[1].end.y = this.pos.y-(this.vel/this.maxVel)*this.size/4;

		this.collisionBounds[1].start.x = this.pos.x+this.size/2;
		this.collisionBounds[1].start.y = this.pos.y-(this.vel/this.maxVel)*this.size/4;
		this.collisionBounds[1].end.x = this.pos.x-this.size/2;
		this.collisionBounds[1].end.y = this.pos.y+(this.vel/this.maxVel)*this.size/4;
	};
}

function Box(x, y, powerup){
	this.pos = new Vector2(x, y);
	this.size = 20;
	this.collisionBounds = [new Segment({x: 0,y: 0}, {x: 0,y: 0}),
							new Segment({x: 0,y: 0}, {x: 0,y: 0}),
							new Segment({x: 0,y: 0}, {x: 0,y: 0}),
							new Segment({x: 0,y: 0}, {x: 0,y: 0})];
	this.powerup = powerup;
	this.hidden = Math.random()>0.9;

	this.update = function(dt){
		this.pos.y -= boxSpeed*dt;
		// Update collision bounds
		this.collisionBounds[0].start.x = this.pos.x-this.size/2;
		this.collisionBounds[0].start.y = this.pos.y-this.size/2;
		this.collisionBounds[0].end.x = this.pos.x-this.size/2;
		this.collisionBounds[0].end.y = this.pos.y+this.size/2;

		this.collisionBounds[1].start.x = this.pos.x-this.size/2;
		this.collisionBounds[1].start.y = this.pos.y+this.size/2;
		this.collisionBounds[1].end.x = this.pos.x+this.size/2;
		this.collisionBounds[1].end.y = this.pos.y-this.size/2;

		this.collisionBounds[2].start.x = this.pos.x+this.size/2;
		this.collisionBounds[2].start.y = this.pos.y-this.size/2;
		this.collisionBounds[2].end.x = this.pos.x+this.size/2;
		this.collisionBounds[2].end.y = this.pos.y+this.size/2;

		this.collisionBounds[3].start.x = this.pos.x+this.size/2;
		this.collisionBounds[3].start.y = this.pos.y+this.size/2;
		this.collisionBounds[3].end.x = this.pos.x-this.size/2;
		this.collisionBounds[3].end.y = this.pos.y-this.size/2;
	};
}

function RandomEvents(){
    this.events = ["disableLeft", "flipControls", "flipScreen", "sineShift", "none"];
    this.prevEvent = "none";
    this.currentEvent = "none";
    
    this.active = function(event){
        if(this.currentEvent == event && this.events.indexOf(event) != -1) return true;
        return false;
    }
    
    this.newEvent = function(){
		this.prevEvent = this.currentEvent;
        this.currentEvent = "none";
        if(Math.random() > 0.2){
            this.currentEvent = this.events[Math.floor(Math.random()*this.events.length)];
        }
    }
}

var player = new Player(width/2, 100);

var boxes = [];
var powerUpColor = "10,127,245";

var rPrevTime = window.performance.now();
var rCurrTime = window.performance.now();

var minDifficulty = 80;
var maxDifficulty = 20;
var difficultyTime = 60;
var difficulty = minDifficulty;
var score = 0;
var scorePhase = 0;
var gameOver = false;
var paused = false;

var rEvent = new RandomEvents();
var prevLevel = Math.floor(window.performance.now()/10000);
var currLevel = Math.floor(window.performance.now()/10000);

var controls = "keyboard";

var cameraOffset = 0;
var cameraSlowness = 25;

var mouseControlDeadzone = 70;

function renderLoop(){
	rCurrTime = window.performance.now();
	var dt = (rCurrTime-rPrevTime)/1000;
	rPrevTime = window.performance.now();
	var color = rainbow(rCurrTime/1000);
	//ctx.fillStyle = "rgba(" + color.r + ", " + color.g + ", " + color.b + ", " + Math.max((5000-rCurrTime)/5000, 0) + ")";
	//ctx.fillStyle = "rgba(255, 255, 255, 0.1)";
	ctx.fillStyle = "#fff";
	ctx.fillRect(0, 0, width, height);
	ctx.fillStyle = "#000";
	var phase = Math.sin(rCurrTime/500)*400;
	if(!rEvent.active("sineShift")) phase = 0;
	for(var i = 0; i < boxes.length; i++){
		if(boxes[i].pos.x-boxes[i].size/2-cameraOffset-phase < -boxes[i].size){
			boxes[i].pos.x += width+boxes[i].size;
		}
		if(boxes[i].pos.x-boxes[i].size/2-cameraOffset-phase > width+boxes[i].size/2){
			boxes[i].pos.x -= width+boxes[i].size;
		}
		var boxColor = "rgba(";
		if(boxes[i].powerup) boxColor += powerUpColor;
		else boxColor += "0, 0, 0";
		if(boxes[i].hidden) boxColor += ", 0.2)";
		else boxColor += ", 1.0)";
		ctx.fillStyle = boxColor;
		ctx.fillRect(boxes[i].pos.x-boxes[i].size/2-cameraOffset-phase, boxes[i].pos.y-boxes[i].size/2, boxes[i].size, boxes[i].size);
	}
	ctx.fillStyle = "#000";
	ctx.beginPath();
	ctx.moveTo(player.pos.x-player.size/2-cameraOffset-phase, player.pos.y+(player.vel/player.maxVel)*player.size/4);
	ctx.lineTo(player.pos.x+(player.vel/player.maxVel)*player.size/2-cameraOffset-phase, player.pos.y+player.size);
	ctx.lineTo(player.pos.x+player.size/2-cameraOffset-phase, player.pos.y-(player.vel/player.maxVel)*player.size/4);
	ctx.closePath();
	if(player.invincible){
		ctx.stroke();
	} else ctx.fill();
	ctx.scale(1, -1);
	ctx.textAlign = "left";
	ctx.font = "12pt Verdana";
	ctx.fillText(Math.round(1/dt)+"fps", 10, -height+20);
	ctx.fillText("Score: " + score, 10, -20);
	ctx.fillText("Level " + currLevel, 10, -40);
	
	ctx.fillStyle = "#000";
	ctx.textAlign = "center";
	ctx.font = "36pt Verdana";
	
	//["disableLeft", "flipControls", "flipScreen", "sineShift", "none"];
	if(rEvent.active("disableLeft")){
        ctx.fillText("Left key disabled!", width/2, -height+80);
	} else if(rEvent.active("flipControls")){
        ctx.fillText("Controls flipped!", width/2, -height+80);
	} else if(rEvent.active("flipScreen")){
		ctx.scale(1, -1);
        ctx.fillText("Screen flipped!", width/2, height-80);
        ctx.scale(1, -1);
	} else if(rEvent.active("sineShift")){
        //ctx.fillText("Feeling woozy?", width/2, -height+80);
	}
	
	cameraOffset -= (cameraOffset-(player.pos.x-width/2))/cameraSlowness;
	if(player.invincible){
		ctx.textAlign = "center";
		ctx.font = "14pt Verdana";
		ctx.fillText("Invincibility: "+player.invincibilityTimer.toFixed(2)+"s", 
							player.pos.x-cameraOffset-phase, -70);
	}
	if(gameOver){
        if(can.style.transform !== ""){
            can.style.transform = "";
        }
		ctx.fillStyle = "rgba(255, 255, 255, 0.8)";
		ctx.fillRect(0, -height, width, height);
		ctx.textAlign = "center";
		ctx.fillStyle = "#000";
		ctx.font = "24pt Verdana";
		ctx.fillText("You have lost with a score of " + score + ".", width/2, -height/2);
		ctx.roundRect(width/2-60, -height/2+30, 120, 40, 10);
		ctx.lineWidth = 2;
		ctx.strokeStyle = "#000";
		ctx.stroke();
		if(mousePos.x < width/2+60 && mousePos.x > width/2-60
		&& height-mousePos.y > height/2-70 && height-mousePos.y < height/2-30){
			if(mouseDown){
				ctx.fillStyle = "rgba(100, 100, 100, 0.8)";
				ctx.fill();
				ctx.fillStyle = "#fff";
			} else {
				ctx.fillStyle = "rgba(200, 200, 200, 0.8)";
				ctx.fill();
				ctx.fillStyle = "#000";
			}
		} else {
			ctx.fillStyle = "rgba(255, 255, 255, 0.8)";
			ctx.fill();
			ctx.fillStyle = "#000";
		}
		ctx.font = "18pt Verdana";
		ctx.fillText("Restart?", width/2, -height/2+60);
		if(ctx.fillStyle == "#ffffff" && mouseDown) restart();
	}
	ctx.scale(1, -1);
	if(!paused){
		requestAnimationFrame(renderLoop);
	}
}

requestAnimationFrame(renderLoop);

var uPrevTime = window.performance.now();
var uCurrTime = window.performance.now();

var canvasFlipId = 0;
var canvasFlipStartTime = 0;
var canvasFlipTime = 0.5;
function flipCanvas(){
	canvasFlipStartTime = window.performance.now();
	function flipCanvasLoop(){
		if(window.performance.now()/1000-canvasFlipStartTime/1000 > canvasFlipTime){
			clearInterval(canvasFlipId);
			can.style.transform = "rotate(180deg)";
		} else {
			can.style.transform = "rotate(" + dshm((window.performance.now()/1000-canvasFlipStartTime/1000)/canvasFlipTime)*180 + "deg)";
		}
	}
	canvasFlipId = setInterval(flipCanvasLoop, 10);
}

function straightenCanvas(){
	canvasFlipStartTime = window.performance.now();
	function flipCanvasLoop(){
		if(window.performance.now()/1000-canvasFlipStartTime/1000 > canvasFlipTime){
			clearInterval(canvasFlipId);
			can.style.transform = "0";
		} else {
			can.style.transform = "rotate(" + 180-(dshm((window.performance.now()/1000-canvasFlipStartTime/1000)/canvasFlipTime)*180) + "deg)";
		}
	}
	canvasFlipId = setInterval(flipCanvasLoop, 10);
}

function update(){
	uCurrTime = window.performance.now();
	currLevel = Math.floor((window.performance.now()-scorePhase*10)/10000);
	if(currLevel != prevLevel && !gameOver){
        if(rEvent.active("flipScreen")){
            straightenCanvas();
        }
        rEvent.newEvent();
        if(rEvent.currentEvent !== "none") player.invincibilityTimer += 1.4;
        console.log(rEvent.currentEvent);
        if(rEvent.active("flipScreen")){
            if(rEvent.prevEvent != "flipScreen") flipCanvas();
        }
	}
	var dt = (uCurrTime-uPrevTime)/1000;
	prevLevel = Math.floor((window.performance.now()-scorePhase*10)/10000);
	uPrevTime = window.performance.now();
	if(!gameOver && !paused){
		for(var i = 0; i < boxes.length; i++){
			boxes[i].update(dt);
			for(var j = 0; j < boxes[i].collisionBounds.length; j++){
				for(var m = 0; m < player.collisionBounds.length; m++){
					if(intersects(boxes[i].collisionBounds[j], player.collisionBounds[m])){
						if(boxes[i].powerup){
							player.invincibilityTimer += Math.random()*2+1;
							boxes.splice(i, 1);
						} else if(!player.invincible) gameOver = true;
					}
				}
			}
			if(boxes[i].pos.y < -boxes[i].size) boxes.splice(i, 1);
		}
		player.update(dt);
		difficulty = Math.round(Math.max(minDifficulty+(maxDifficulty-minDifficulty)*(uCurrTime-scorePhase*10)/(1000*difficultyTime), maxDifficulty));
		if(uCurrTime%difficulty < 10){
			var boxIsPowerup = Math.random() > 0.98;
			boxes.push(new Box(Math.random()*width, height+100, boxIsPowerup));
		}
		if(left || right){
			if(!rEvent.active("flipControls") && !rEvent.active("flipScreen")){
				if(left && !rEvent.active("disableLeft")) player.accel = -playerSpeed;
				if(right) player.accel = playerSpeed;
			} else {
				if(left) player.accel = playerSpeed;
				if(right) player.accel = -playerSpeed;
			}
		} else player.accel = 0;
		if(left && right){
			player.accel = 0;
		}
		boxSpeed += 10*dt;
		score = Math.round(uCurrTime/10-scorePhase);
	}
	
	if(mouseDown){
		controls = "mouse";
		if(mousePos.x > player.pos.x-cameraOffset+mouseControlDeadzone){
			right = true;
			left = false;
		} else if(mousePos.x < player.pos.x-cameraOffset-mouseControlDeadzone){
			right = false;
			left = true;
		} else {
            left = false;
            right = false;
		}
	} else {
		if(controls == "mouse"){
			right = false;
			left = false;
		}
	}
}

setInterval(update, 0);

function restart(){
	gameOver = false;
	rEvent.currentEvent = "none";
	currLevel = 0;
	prevLevel = 0;
	boxes = [];
	scorePhase = window.performance.now()/10;
	player.vel.x = 0;
	player.pos.x = width/2;
	boxSpeed = boxStartSpeed;
}

function togglePause(){
	paused = !paused;
	if(!paused){
		requestAnimationFrame(renderLoop);
		scorePhase = window.performance.now()/10-score;
	}
}

function resize(){
	var width = window.innerWidth;
	var height = window.innerHeight;
	can.width = width;
	can.height = height;
}

window.addEventListener("keydown", function(e){
	if(e.keyCode == "37" || e.keyCode == "65"){
		controls = "keyboard";
		left = true;
	}
	if(e.keyCode == "39" || e.keyCode == "68"){
		controls = "keyboard";
		right = true;
	}
	if(gameOver
	&& e.keyCode != "37"
	&& e.keyCode != "38"
	&& e.keyCode != "39"
	&& e.keyCode != "40"
	&& e.keyCode != "27"
	&& e.keyCode != "32") restart();
});

window.addEventListener("keyup", function(e){
	if(e.keyCode == "37" || e.keyCode == "65") left = false;
	if(e.keyCode == "39" || e.keyCode == "68") right = false;
	if((e.keyCode == "27" || e.keyCode == "32") && !gameOver){
		togglePause();
	}
});

window.addEventListener("onblur", function(e){
	if(!paused) togglePause();
	alert("blur");
});

window.addEventListener("onfocus", function(e){
	if(paused) togglePause();
});

window.addEventListener("mousemove", function(e){
	mousePos.x = e.x;
	mousePos.y = e.y;
});

window.addEventListener("mousedown", function(e){
	mouseDown = true;
});

window.addEventListener("mouseup", function(e){
	mouseDown = false;
});

window.addEventListener("touchstart", function(e){
    e.preventDefault();
    controls = "mobile";
    var touchX = e.changedTouches[e.changedTouches.length-1].pageX;
    if(touchX > width/2){
        right = true;
        left = false;
    } else if(touchX < width/2){
        left = true;
        right = false;
    }
    if(gameOver){
        var touchY = e.changedTouches[e.changedTouches.length-1].pageY;
        if(touchX < width/2+60 && touchX > width/2-60
		&& height-touchY > height/2-70 && height-touchY < height/2-30){
            restart();
		}
    }
});

window.addEventListener("touchend", function(e){
    e.preventDefault();
    left = false;
    right = false;
});