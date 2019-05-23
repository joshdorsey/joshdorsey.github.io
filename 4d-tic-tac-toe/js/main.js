// http://www.w3schools.com/js/js_cookies.asp
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

function Board(vals, name){
	this.s = [
		[vals[0], vals[1], vals[2]],
		[vals[3], vals[4], vals[5]],
		[vals[6], vals[7], vals[8]],
	];
	this.pos = name;
}

function dshm(x){
	return -Math.pow(x-1, 2)*Math.cos(Math.pow(x, 2)/0.1)+1;
}

function cosine(x){
	return -0.5*Math.cos(3.14159265*x) + 0.5;
}

function rotation(_x, _y, _z){
	return {
		x: _x,
		y: _y,
		z: _z
	};
}

function setRotation(element, persp, rot){
	$(element).css("transform", "perspective("+persp+") rotateX("+rot.x+"deg) rotateY("+rot.y+"deg) rotateZ("+rot.z+"deg);");
}

function rotateTo(element, persp, _origRot, _endRot, _duration, curveRot){
	var duration = _duration;
	var startRot = _origRot;
	var deltaRot = {
		x: _endRot.x-_origRot.x,
		y: _endRot.y-_origRot.y,
		z: _endRot.z-_origRot.z
	};
	
	var startTime = window.performance.now();
	setRotation(element, persp, startRot);
	var loopId = setInterval(function(){
		var deltaTime = window.performance.now();
		setRotation(element, persp,	rotation(startRot.x+(deltaRot.x*curveRot((deltaTime-startTime)/duration)),
											 startRot.y+(deltaRot.y*curveRot((deltaTime-startTime)/duration)),
											 startRot.z+(deltaRot.z*curveRot((deltaTime-startTime)/duration))));
		if((deltaTime-startTime)/duration > 1){
			console.info("Completed animation, loop cleared.");
			clearInterval(loopId);
		}
	}, 16);
}

// http://stackoverflow.com/questions/1026069/capitalize-the-first-letter-of-string-in-javascript
String.prototype.capitalize = function(){
    return this.charAt(0).toUpperCase() + this.slice(1);
}

// Use on boolean arrays to determine if each value is true
Array.prototype.allTrue = function(){
	for(var i = 0; i < this.length; i++){
		if(!this[i]) return false;
	}
	return true;
}

// make state information global
var state = {};
state.turn = "x";
var xWinState = {
	dimensions: [false, false, false, false],
	won: false
}

var oWinState = {
	dimensions: [false, false, false, false],
	won: false
}

function gather(){ // Does all the html groundwork to get the data into a js structure
	var boards = document.getElementsByTagName("board");
	state.boards = []; // make array
	for(var i = 0; i < boards.length; i++){ // for each board
		state.boards[i] = {};

		var cellIndex = 0;
		var childCells = boards[i].childNodes;

		state.boards[i].cells = [];
		var skipCounter = 0; // used to skip over <br> tags inside boards
		var boardVals = [];
		for(var j = 0; j < childCells.length; j++){ // For each cell inside the board
			if(childCells[j].tagName == "CELL"){
				if($(childCells[j]).hasClass("x")){
					boardVals[j-skipCounter] = "x";
				} else if($(childCells[j]).hasClass("o")){
					boardVals[j-skipCounter] = "o";
				} else {
					boardVals[j-skipCounter] = "c";
				}
			} else {
				skipCounter++;
			}
		}
		state.boards[i] = new Board(boardVals, boards[i].id);
	}
	return state;
}

function check2DWins(board){	
	// check horizontally
	for(var j = 0; j < 3; j++){ // for each row
		if(board.s[j][0] == board.s[j][1] && board.s[j][0] == board.s[j][2]){
			if(board.s[j][1] == "x"){
				xWinState.dimensions[0] = true;
			}
			if(board.s[j][1] == "o"){
				oWinState.dimensions[0] = true;
			}
		}
	}
	
	// check vertically
	for(var j = 0; j < 3; j++){ // for each col
		if(board.s[0][j] == board.s[1][j] && board.s[1][j] == board.s[2][j]){
			if(board.s[1][j] == "x"){
				xWinState.dimensions[0] = true;
			}
			if(board.s[1][j] == "o"){
				oWinState.dimensions[0] = true;
			}
		}
	}
	
	// check diagonals
	if((board.s[0][0] == board.s[1][1] && board.s[1][1] == board.s[2][2]) ||
	   (board.s[0][2] == board.s[1][1] && board.s[1][1] == board.s[2][0])){
		if(board.s[1][1] == "x"){
			xWinState.dimensions[1] = true;
		}
		if(board.s[1][1] == "o"){
			oWinState.dimensions[1] = true;
		}
	}
}

function check3DWins(cube){ // cube = [board, board, board]
	// 3D ones will have to be -- loosely -- as anything that transcends all three
	// 2D grids, which also makes sense from a gamplay standpoint.
	
	// check z-axis lines
	for(var i = 0; i < 3; i++){
		for(var j = 0; j < 3; j++){ // For each z-axis column
			if(cube[0].s[i][j] == cube[1].s[i][j] && cube[1].s[i][j] == cube[2].s[i][j] && cube[0].s[i][j] != "c"){
				if(cube[1].s[i][j] == "x"){
					xWinState.dimensions[2] = true;
				}
				if(cube[1].s[i][j] == "o"){
					oWinState.dimensions[2] = true;
				}
			}
		}
	}
	
	// check xz-axis diagonals 
	for(var i = 0; i < 3; i++){
		if((cube[0].s[i][0] == cube[1].s[i][1] && cube[1].s[i][1] == cube[2].s[i][2]) ||
		   (cube[0].s[i][2] == cube[1].s[i][1] && cube[1].s[i][1] == cube[2].s[i][0])){
			if(cube[1].s[i][1] == "x"){
				xWinState.dimensions[2] = true;
			}
			if(cube[1].s[i][1] == "o"){
				oWinState.dimensions[2] = true;
			}
		}
	}
	
	// check yz-axis diagonals
	for(var i = 0; i < 3; i++){
		if((cube[0].s[0][i] == cube[1].s[1][i] && cube[1].s[1][i] == cube[2].s[2][i]) ||
		   (cube[0].s[2][i] == cube[1].s[1][i] && cube[1].s[1][i] == cube[2].s[0][i])){
			if(cube[1].s[1][i] == "x"){
				xWinState.dimensions[2] = true;
			}
			if(cube[1].s[1][i] == "o"){
				oWinState.dimensions[2] = true;
			}
		}
	}
	
	// check xyz-axis diagonals
	if((cube[0].s[0][0] == cube[1].s[1][1] && cube[1].s[1][1] == cube[2].s[2][2]) ||
	   (cube[2].s[0][0] == cube[1].s[1][1] && cube[1].s[1][1] == cube[0].s[2][2]) ||
	   (cube[0].s[0][2] == cube[1].s[1][1] && cube[1].s[1][1] == cube[2].s[2][0]) ||
	   (cube[2].s[0][2] == cube[1].s[1][1] && cube[1].s[1][1] == cube[0].s[2][0])){
		if(cube[1].s[1][1] == "x"){
			xWinState.dimensions[2] = true;
		}
		if(cube[1].s[1][1] == "o"){
			oWinState.dimensions[2] = true;
		}
	}
}

function check4DWins(hypercube){ // hypercube = [cube, cube, cube]
	// check for j-axis straight line wins
	for(var x = 0; x < 3; x++){
		for(var y = 0; y < 3; y++){
			for(var z = 0; z < 3; z++){
				if(hypercube[0][z].s[y][x] == hypercube[1][z].s[y][x] &&
				   hypercube[1][z].s[y][x] == hypercube[2][z].s[y][x]){
					if(hypercube[1][z].s[y][x] == "x"){
						xWinState.dimensions[3] = true;
					}
					if(hypercube[1][z].s[y][x] == "o"){
						oWinState.dimensions[3] = true;
					}
				}
			}
		}
	}

	// check for two-axis diagonals
	// check for jx diagonals
	for(var z = 0; z < 3; z++){
		for(var y = 0; y < 3; y++){
			if((hypercube[0][z].s[y][0] == hypercube[1][z].s[y][1] &&
				hypercube[1][z].s[y][1] == hypercube[2][z].s[y][2]) ||
			   (hypercube[0][z].s[y][2] == hypercube[1][z].s[y][1] &&
			   	hypercube[1][z].s[y][1] == hypercube[2][z].s[y][0])){
				if(hypercube[1][z].s[y][1] == "x"){
					xWinState.dimensions[3] = true;
				}
				if(hypercube[1][z].s[y][1] == "o"){
					oWinState.dimensions[3] = true;
				}
			}
		}
	}

	// check for jy diagonals
	for(var z = 0; z < 3; z++){
		for(var x = 0; x < 3; x++){
			if((hypercube[0][z].s[0][x] == hypercube[1][z].s[1][x] &&
				hypercube[1][z].s[1][x] == hypercube[2][z].s[2][x]) ||
			   (hypercube[0][z].s[2][x] == hypercube[1][z].s[1][x] &&
			   	hypercube[1][z].s[1][x] == hypercube[2][z].s[0][x])){
				if(hypercube[1][z].s[1][x] == "x"){
					xWinState.dimensions[3] = true;
				}
				if(hypercube[1][z].s[1][x] == "o"){
					oWinState.dimensions[3] = true;
				}
			}
		}
	}

	// check for jz diagonals
	for(var x = 0; x < 3; x++){
		for(var y = 0; y < 3; y++){
			if(((hypercube[0][0].s[y][x] == hypercube[1][1].s[y][x] &&
				 hypercube[1][1].s[y][x] == hypercube[2][2].s[y][x]) ||
				(hypercube[0][2].s[y][x] == hypercube[1][1].s[y][x] &&
			   	 hypercube[1][1].s[y][x] == hypercube[2][0].s[y][x])) &&
				 hypercube[1][1].s[y][x] != "c"){
				if(hypercube[1][1].s[y][x] == "x"){
					xWinState.dimensions[3] = true;
				}
				if(hypercube[1][1].s[y][x] == "o"){
					oWinState.dimensions[3] = true;
				}
			}
		}
	}

	// check for three-axis diagonals
	// check for xyj diagonals
	for(var z = 0; z < 3; z++){
		if((hypercube[0][z].s[0][0] == hypercube[1][z].s[1][1] && hypercube[1][z].s[1][1] == hypercube[2][z].s[2][2]) ||
		   (hypercube[2][z].s[0][0] == hypercube[1][z].s[1][1] && hypercube[1][z].s[1][1] == hypercube[0][z].s[2][2]) ||
		   (hypercube[0][z].s[0][2] == hypercube[1][z].s[1][1] && hypercube[1][z].s[1][1] == hypercube[2][z].s[2][0]) ||
		   (hypercube[2][z].s[0][2] == hypercube[1][z].s[1][1] && hypercube[1][z].s[1][1] == hypercube[0][z].s[2][0])){
			if(hypercube[1][z].s[1][1] == "x"){
				xWinState.dimensions[3] = true;
			}
			if(hypercube[1][z].s[1][1] == "o"){
				oWinState.dimensions[3] = true;
			}
		}
	}

	// check for xzj diagonals
	for(var y = 0; y < 3; y++){
		if((hypercube[0][0].s[y][0] == hypercube[1][1].s[y][1] && hypercube[1][1].s[y][1] == hypercube[2][2].s[y][2]) ||
		   (hypercube[2][0].s[y][0] == hypercube[1][1].s[y][1] && hypercube[1][1].s[y][1] == hypercube[0][2].s[y][2]) ||
		   (hypercube[0][0].s[y][2] == hypercube[1][1].s[y][1] && hypercube[1][1].s[y][1] == hypercube[2][2].s[y][0]) ||
		   (hypercube[2][0].s[y][2] == hypercube[1][1].s[y][1] && hypercube[1][1].s[y][1] == hypercube[0][2].s[y][0])){
			if(hypercube[1][1].s[y][1] == "x"){
				xWinState.dimensions[3] = true;
			}
			if(hypercube[1][1].s[y][1] == "o"){
				oWinState.dimensions[3] = true;
			}
		}
	}

	// check for yzj diagonals
	for(var x = 0; x < 3; x++){
		if((hypercube[0][0].s[0][x] == hypercube[1][1].s[1][x] && hypercube[1][1].s[1][x] == hypercube[2][2].s[2][x]) ||
		   (hypercube[2][0].s[0][x] == hypercube[1][1].s[1][x] && hypercube[1][1].s[1][x] == hypercube[0][2].s[2][x]) ||
		   (hypercube[0][0].s[2][x] == hypercube[1][1].s[1][x] && hypercube[1][1].s[1][x] == hypercube[2][2].s[0][x]) ||
		   (hypercube[2][0].s[2][x] == hypercube[1][1].s[1][x] && hypercube[1][1].s[1][x] == hypercube[0][2].s[0][x])){
			if(hypercube[1][1].s[1][x] == "x"){
				xWinState.dimensions[3] = true;
			}
			if(hypercube[1][1].s[1][x] == "o"){
				oWinState.dimensions[3] = true;
			}
		}
	}

	// check for four-axis diagonals
	// check for xyzj
	if((hypercube[0][0].s[0][0] == hypercube[1][1].s[1][1] && hypercube[1][1].s[1][1] == hypercube[2][2].s[2][2]) ||
	   (hypercube[0][0].s[2][2] == hypercube[1][1].s[1][1] && hypercube[1][1].s[1][1] == hypercube[2][2].s[0][0]) ||
	   (hypercube[0][0].s[0][2] == hypercube[1][1].s[1][1] && hypercube[1][1].s[1][1] == hypercube[2][2].s[2][0]) ||
	   (hypercube[0][0].s[2][0] == hypercube[1][1].s[1][1] && hypercube[1][1].s[1][1] == hypercube[2][2].s[0][2]) ||

	   (hypercube[0][2].s[0][0] == hypercube[1][1].s[1][1] && hypercube[1][1].s[1][1] == hypercube[2][0].s[2][2]) ||
	   (hypercube[0][2].s[2][2] == hypercube[1][1].s[1][1] && hypercube[1][1].s[1][1] == hypercube[2][0].s[0][0]) ||
	   (hypercube[0][2].s[0][2] == hypercube[1][1].s[1][1] && hypercube[1][1].s[1][1] == hypercube[2][0].s[2][0]) ||
	   (hypercube[0][2].s[2][0] == hypercube[1][1].s[1][1] && hypercube[1][1].s[1][1] == hypercube[2][0].s[0][2])){
		if(hypercube[1][1].s[1][1] == "x"){
			xWinState.dimensions[3] = true;
		}
		if(hypercube[1][1].s[1][1] == "o"){
			oWinState.dimensions[3] = true;
		}
	}
}

function getBoard(state, cube, layer){
	for(var i = 0; i < state.boards.length; i++){
		if(state.boards[i].pos[0] == cube+"" && state.boards[i].pos[2] == layer+""){
			return state.boards[i];
		}
	}
}

function getCube(state, cube){
	var resultCube = [];
	for(var i = 0; i < state.boards.length; i++){
		if(state.boards[i].pos[0] == cube+""){
			resultCube.push(state.boards[i]);
		}
	}
	return resultCube;
}

function getHypercube(state){
	var resultHypercube = [
		[state.boards[0], // Cube 1
		state.boards[1],
		state.boards[2]],
		
		[state.boards[3], // Cube 2
		state.boards[4],
		state.boards[5]],
		
		[state.boards[6], // Cube 3
		state.boards[7],
		state.boards[8]]
	];
	return resultHypercube;
}

var collapsed = [false, false, false];
var collapsedRot = rotation(60, 0, 73);
var collapsedPerspective = "0px";

function toggleCollapsed(cube, dur){
	var _boards = document.getElementsByTagName("board");
	var boards = [];
	for(var i = 0; i < _boards.length; i++){
		if(Number(_boards[i].id[0]) == cube){
			boards.push(_boards[i]);
		}
	}
	if(collapsed[cube-1]){
		for(var i = 0; i < 3; i++){
			rotateTo($(boards[i]).parent(), collapsedPerspective, collapsedRot, rotation(0, 0, 0), dur | 400, dshm);
		}
		collapsed[cube-1] = false;
	} else {
		for(var i = 0; i < 3; i++){
			rotateTo($(boards[i]).parent(), collapsedPerspective, rotation(0, 0, 0), collapsedRot, dur | 400, dshm);
		}
		collapsed[cube-1] = true;
	}
}

function updateCells(){
	var updateStart = window.performance.now();
	// Update each cell in DOM
	$("cell").each(function(i){
		if($(this).hasClass("x")){
			this.style.backgroundImage = "url(\"img/x.png\")";
			this.style.backgroundSize = "100% 100%";
		} else if($(this).hasClass("o")){
			this.style.backgroundImage = "url(\"img/o.png\")";
			this.style.backgroundSize = "100% 100%";
		} else {
			this.style.backgroundImage = "";
		}
	});

	// Gather state from DOM
	state = gather();

	// Collect win data
	for(var cube = 1; cube < 4; cube++){
		for(var layer = 1; layer < 4; layer++){
			check2DWins(getBoard(state, cube, layer));
		}
	}
	for(var cube = 1; cube < 4; cube++){
		check3DWins(getCube(state, cube));
	}
	check4DWins(getHypercube(state));

	// Update winState display at the bottom of the board
	for(var i = 0; i < 4; i++){
		if(xWinState.dimensions[i]){
			$(document.getElementById("x"+(i+1)+"d")).addClass("full");
		} else if(!xWinState.dimensions[i]){
			if($(document.getElementById("x"+(i+1)+"d")).hasClass("full")){
				$(document.getElementById("x"+(i+1)+"d")).removeClass("full");
			}
		}

		if(oWinState.dimensions[i]){
			$(document.getElementById("o"+(i+1)+"d")).addClass("full");
		} else if(!oWinState.dimensions[i]){
			if($(document.getElementById("o"+(i+1)+"d")).hasClass("full")){
				$(document.getElementById("o"+(i+1)+"d")).removeClass("full");
			}
		}
	}

	if(xWinState.dimensions.allTrue()){
		xWinState.won = true;
	} else {
		xWinState.won = false;
	}
	if(oWinState.dimensions.allTrue()){
		oWinState.won = true;
	} else {
		oWinState.won = false;
	}

	if(xWinState.won || oWinState.won) state.turn = "c";
	if(state.turn != "c"){
		document.getElementById("turnLabel").textContent = state.turn.capitalize()+"'s Turn";
		$("#winOverlay").hide();
	} else {
		$("#winOverlay").show();
		if(xWinState.won){
			document.getElementById("turnLabel").textContent = "X won!";
			document.getElementById("winOverlay").textContent = "X wins!";
		} else if(oWinState.won){
			document.getElementById("turnLabel").textContent = "O won!";
			document.getElementById("winOverlay").textContent = "O wins!";
		}
	}

	// Output debug info
	var debugOutput = document.getElementById("debugOutput");
	debugOutput.textContent = "State: "+JSON.stringify(state, null, 4);
	setCookie("state", encodeURI(JSON.stringify(state)), 30);
	setCookie("undoHistory", encodeURI(JSON.stringify(prevTurns)), 30);
	// setCookie("turn", encodeURI(turn), 30);
	var updateEnd = window.performance.now();
	console.info("Board update took " + Math.round(updateEnd-updateStart, 2) + " milliseconds.");
}

function getCellElement(board, x, y){
	var currBoard = document.getElementsByTagName("board");
	for(var i = 0; i < currBoard.length; i++){
		if(currBoard[i].id == board){
			currBoard = currBoard[i];
			break;
		}
	}

	var currCell = currBoard.children;
	for(var i = 0; i < currCell.length; i++){
		if(currCell[i].id == y+"."+x){
			return currCell[i];
		}
	}
}

function setState(object){
	for(var i = 0; i < object.boards.length; i++){
		for(var x = 0; x < 3; x++){
			for(var y = 0; y < 3; y++){
				var cell = getCellElement(object.boards[i].pos.substring(0, 4), x+1, y+1);
				if(object.boards[i].s[y][x] == "x"){
					$(cell).removeClass("c");
					$(cell).removeClass("o");
					$(cell).addClass("x");
				} else if(object.boards[i].s[y][x] == "o"){
					$(cell).removeClass("c");
					$(cell).addClass("o");
					$(cell).removeClass("x");
				} else if(object.boards[i].s[y][x] == "c"){
					$(cell).addClass("c");
					$(cell).removeClass("o");
					$(cell).removeClass("x");
				}
			}
		}
	}
	state.turn = object.turn;
}

function restart(){
	$("cell").each(function(i){
		this.style.backgroundImage = "";
		$(this).removeClass("x");
		$(this).removeClass("o");
		$(this).addClass("c");
	});
	state.turn = "x";
	xWinState.won = false;
	oWinState.won = false;
	for(var i = 0; i < 4; i++){
		xWinState.dimensions[i] = false;
	}
	for(var i = 0; i < 4; i++){
		oWinState.dimensions[i] = false;
	}
	$("#winOverlay").hide();
	prevTurns = [];
	updateCells();
}

var prevTurns = [];

function undo(){
	if(prevTurns.length > 0){
		xWinState.won = false;
		oWinState.won = false;
		for(var i = 0; i < 4; i++){
			xWinState.dimensions[i] = false;
		}
		for(var i = 0; i < 4; i++){
			oWinState.dimensions[i] = false;
		}
		var boards = $("board");
		var board;
		for(var i = 0; i < boards.length; i++){
			if($(boards[i]).attr('id') == prevTurns[prevTurns.length-1].boardId){
				board = i;
			}
		}

		var cells = $(boards[board]).children();
		var cell;
		for(var i = 0; i < cells.length; i++){
			if($(cells[i]).attr('id') == prevTurns[prevTurns.length-1].cellId){
				cell = i;
			}
		}

		cell = cells[cell];
		if($(cell).hasClass("x") || $(cell).hasClass("o")){
			$(cell).removeClass("o");
			$(cell).removeClass("x");
			if(state.turn == "x"){
				state.turn = "o";
			} else if(state.turn == "o"){
				state.turn = "x";
			} else if(state.turn == "c"){
				state.turn = "x";
			}
		}
		$(cell).addClass("c");
		prevTurns.pop();
		updateCells();
	} else {
		console.info("Nothing to undo");
	}
}

$(document).ready(function(){
	// GAME STUFF
	$("#winOverlay").hide();
	$("board").each(function(n){ // Populate boards with cells
		for(var i = 0; i < 3; i++){
			var lbreak = document.createElement("br");
			for(var j = 0; j < 3; j++){
				var cell = document.createElement("cell");
				cell.id = (i+1)+"."+(j+1);
				$(cell).addClass("c");
				if(i == 2){
					$(cell).addClass("bottom");
				}
				this.appendChild(cell);
			}
			if(i < 2)this.appendChild(lbreak);
		}
	});

	$("cell").each(function(i){ // Add event listeners to all the cells
		$(this).click(function(){
			var currTurn = {boardId: "0.0", cellId: "0.0", piece: "c"};
			if($(this).hasClass("c") && (!xWinState.won && !oWinState.won) && state.turn == "x"){
				$(this).addClass("x");
				state.turn = "o";
				currTurn.piece = "x";
				currTurn.boardId = $(this).parent().attr('id');
				currTurn.cellId = $(this).attr('id');
			} else if($(this).hasClass("c") && (!xWinState.won && !oWinState.won) && state.turn == "o"){
				$(this).addClass("o");
				state.turn = "x";
				currTurn.piece = "o";
				currTurn.boardId = $(this).parent().attr('id');
				currTurn.cellId = $(this).attr('id');
			}
			$(this).removeClass("c");
			prevTurns.push(currTurn);
			updateCells();
		});
	});
	if(getCookie("state") != ""){
		console.info("Found stored state");
		setState(JSON.parse(decodeURI(getCookie("state"))));
		// turn = decodeURI(getCookie("turn"));
	} else {
		console.info("Found no stored state");
	}
	if(getCookie("undoHistory") != ""){
		console.info("Found stored undo history");
		prevTurns = JSON.parse(decodeURI(getCookie("undoHistory")));
	} else {
		console.info("Found no stored undo history");
	}
	updateCells();

	// PAGE STUFF
	$("#debugOutputToggle").click(function(){
		if($("#debugOutputToggle").hasClass("hidden")){
			$("#debugOutputToggle").removeClass("hidden");
			$("#debugOutput").removeClass("hidden");
			document.getElementById("debugOutputToggle").innerHTML = "&laquo;";
		} else {
			$("#debugOutputToggle").addClass("hidden");
			$("#debugOutput").addClass("hidden");
			document.getElementById("debugOutputToggle").innerHTML = "&raquo;";
		}
	});
});

window.addEventListener("keydown", function(e){
	if(e.keyCode == "82"){ // R Key
		restart();
	}
	if(e.keyCode == "85"){ // U Key
		undo();
	}
	if(e.keyCode == "90" && e.ctrlKey){ // Ctrl+Z
		undo();
	}
}, false);
