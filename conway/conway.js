function setCookie(cname, cvalue, exdays){
    var d = new Date();
    d.setTime(d.getTime() + (exdays*24*60*60*1000));
    var expires = "expires="+d.toUTCString();
    document.cookie = cname + "=" + cvalue + "; " + expires;
}

function getCookie(cname){
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++){
        var c = ca[i];
        while(c.charAt(0)==' ') c = c.substring(1);
        if(c.indexOf(name) == 0) return c.substring(name.length,c.length);
    }
    return "";
}

function fallsIn(min, max, val){
    if(val < min || val > max) return false;
    return true;
}

function Grid(_w, _h, _el){
    this.w = _w;
    this.h = _h;
    this.el = _el;
    this.s = new Array(_h);
    for(var i = 0; i < _h; i++){
        this.s[i] = new Array(_w);
        for(var j = 0; j < _w; j++){
            this.s[i][j] = false;
        }
    }
    
    this.isEmpty = function(){
    	for(var i = 0; i < this.h; i++){
    		for(var j = 0; j < this.w; j++){
    			if(this.s[i][j]) return false;
    		}
    	}
    	return true;
    }
    
    this.initialize = function(){
        while(this.el.firstChild){
            this.el.removeChild(this.el.firstChild);
        }
        this.s = new Array(this.h);
        for(var i = 0; i < this.h; i++){
            this.s[i] = new Array(this.w);
            for(var j = 0; j < this.w; j++){
                this.s[i][j] = false;
            }
        }
        for(var i = 0; i < this.h; i++){
            for(var j = 0; j < this.w; j++){
                var div = document.createElement("div");
                div.id = "cell";
                if(this.s[i][j]) div.className = "true" + " " + i + "d" + j;
                else div.className = "false" + " " + i + "d" + j;
                div.setAttribute("onclick", "clicked(" + i + ", " + j + ");");
                this.el.appendChild(div);
            }
            this.el.appendChild(document.createElement("br"));
            this.el.appendChild(document.createElement("br"));
        }
    }

    this.loadState = function(input){
        var split = input.split(":");
        var width = parseInt(split[0]);
        var height = parseInt(split[1]);
        this.w = width;
        this.h = height;
        this.initialize();
        debug("Loading " + width + " by " + height + " state.", 2);
        // reinitialize this.s to array of correct dimensions
        this.s = new Array(height);
        for(var i = 0; i < height; i++){
            this.s[i] = new Array(width);
            for(var j = 0; j < width; j++){
                this.s[i][j] = false;
            }
        }
        for(var i = 0; i < height; i++){
            for(var j = 0; j < width; j++){
                if(split[2][(i*width)+j] == "t"){
                    this.s[i][j] = true;
                } else this.s[i][j] = false;
            }
        }
        this.update();
    }

    this.saveState = function(){
        var state = this.w+":"+this.h+":";
        for(var i = 0; i < this.h; i++){
            for(var j = 0; j < this.w; j++){
                if(this.s[i][j]) state += "t";
                else state += "f";
            }
        }
        debug("Saving state; " + (state.length*2) + "bytes", 2);
        setCookie("state", encodeURI(state), 10);
    }
    
    this.getNeighbors = function(l, x, y){
        var n = 0;
        for(var j = -1; j < 2; j++){
            for(var i = -1; i < 2; i++){
                if(fallsIn(0, this.h-1, x+j) && fallsIn(0, this.w-1, y+i) && !(i == 0 && j == 0)){
                    if(l[x+j][y+i]) n++;
                }
            }
        }
        return n;
    }
    
    this.update = function(){
        for(var i = 0; i < this.h; i++){
            for(var j = 0; j < this.w; j++){
                if(this.s[i][j]){
                    $("." + i + "d" + j).removeClass("false");
                    $("." + i + "d" + j).addClass("true");
                } else {
                    $("." + i + "d" + j).removeClass("true");
                    $("." + i + "d" + j).addClass("false");
                }
            }
        }
    }
    
    this.advance = function(){
        var sTime = performance.now();
        var tState = new Array(this.h);
        for(var i = 0; i < this.h; i++){
            tState[i] = new Array(this.w);
            for(var j = 0; j < this.w; j++){
                tState[i][j] = this.s[i][j];
            }
        }
        for(var i = 0; i < this.h; i++){
            for(var j = 0; j < this.w; j++){
                if(tState[i][j]){
                    if(this.getNeighbors(tState, i, j) < 2){
                        this.s[i][j] = false;
                        debug("Cell " + i + " " + j + " died of starvation.", 3);
                    } else if(this.getNeighbors(tState, i, j) > 3){
                        this.s[i][j] = false;
                        debug("Cell " + i + " " + j + " died of overpopulation.", 3);
                    }
                } else {
                    if(this.getNeighbors(tState, i, j) == 3){
                        this.s[i][j] = true;
                        debug("Cell " + i + " " + j + " sprung to life.", 3);
                    }
                }
            }
        }
        this.update();
        this.saveState();
        var eTime = performance.now();
        return (eTime-sTime).toFixed(2);
    }

    this.clear = function(){
        for(var i = 0; i < this.h; i++){
            for(var j = 0; j < this.w; j++){
                this.s[i][j] = false;
            }
        }
        this.update();
    }

    this.randomStart = function(p){
        this.clear();
        for(var i = 0; i < this.h; i++){
            for(var j = 0; j < this.w; j++){
                if(Math.random() > 1-p) this.s[i][j] = true;
            }
        }
        this.update();
        this.saveState();
    }

    this.toString = function(){

    }
}

function clicked(x, y){
    if(grid.s[x][y]){
        $("." + x + "d" + y).toggleClass("true");
        $("." + x + "d" + y).toggleClass("false");
        grid.s[x][y] = false;
    }
    else if(!grid.s[x][y]){
        $("." + x + "d" + y).toggleClass("true");
        $("." + x + "d" + y).toggleClass("false");
        grid.s[x][y] = true;
    }
    grid.saveState();
}

var debugLevel = 2;
function debug(s, l, w){
    if(!w){
        if(debugLevel >= l) console.log(s);
    } else {
        if(debugLevel >= l) console.warn(s);
    }
}
var frameTime;
var warnFrameTime = 100;

var grid = new Grid(60, 30, document.getElementById("container"));
if(getCookie("state") != ""){
    debug("Found stored state", 2);
    grid.loadState(getCookie("state"));
} else grid.initialize();

var loopId = -1;
var rsPercent = 0.2;
var speed = 60;

document.getElementById("stopButton").disabled = true;

window.addEventListener("keydown", function(e){
    if(e.keyCode == "32"){
        if(loopId == -1) play();
        else stop();
    } else if(e.keyCode == "65"){
        step();
    } else if(e.keyCode == "67"){
        grid.clear();
    } else if(e.keyCode == "82"){
        grid.randomStart(rsPercent);
    }
}, false);

function step(){
    frameTime = grid.advance();
    if(grid.isEmpty()) stop();
    if(frameTime > warnFrameTime){
        $("#playButton").css("background-color", "#fbb");
        debug("Update is taking longer than " + warnFrameTime + "ms.", 1, true);
    } else $("#playButton").css("background-color", "");
    debug("Update took " + frameTime + "ms.", 1);
}

function play(){
    if(loopId == -1){
        document.getElementById("playButton").disabled = true;
        document.getElementById("stopButton").disabled = false;
        loopId = setInterval(step, speed);
    }
}

function stop(){
    clearInterval(loopId);
    document.getElementById("playButton").disabled = false;
    document.getElementById("stopButton").disabled = true;
    loopId=-1;
}

function setSpeed(s){
    speed = s;
    if(loopId != -1){
        clearInterval(loopId);
        setInterval(step, speed);
    }
}

$("#resizePanel").hide();
$("#resizeButton").click(function(){
    $("#widthField").val(grid.w);
    $("#heightField").val(grid.h);
    $("#resizePanel").show();
});

var memorySize = grid.w*grid.h*2+grid.w.toString().length*2+grid.h.toString().length*2+2;

$("#memoryAmount").text(memorySize + " bytes");

function setGridDimensions(){
    grid.w = parseInt($("#widthField").val());
    grid.h = parseInt($("#heightField").val());
    debug("Setting grid dimensions to " + grid.w + " by " + grid.h, 0);
    grid.initialize();
    $("#resizePanel").hide();
    memorySize = grid.w*grid.h*2+grid.w.toString().length*2+grid.h.toString().length*2+2;
    $("#memoryAmount").text(memorySize + " bytes");
    if(memorySize > 4096){
        $("#memoryAmount").css("color", "#a00");
        document.getElementById("memoryAmount").setAttribute("title", "If this text is red, the state of the board may not be saved on exit.");
    } else {
        $("#memoryAmount").css("color", "#000");
        document.getElementById("memoryAmount").setAttribute("title", "");
    }
}

if(memorySize > 4096){
    $("#memoryAmount").css("color", "#a00");
    document.getElementById("memoryAmount").setAttribute("title", "If this text is red, the state of the board may not be saved on exit.");
} else {
    $("#memoryAmount").css("color", "#000");
    document.getElementById("memoryAmount").setAttribute("title", "");
}