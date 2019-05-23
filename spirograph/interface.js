/*
document.getElementById("widthField").addEventListener('input', function(e){
	resizeCanvas(parseInt(e.target.value) | window.innerWidth, document.getElementsByTagName("canvas")[0].height);
}, false);

document.getElementById("heightField").addEventListener('input', function(e){
	resizeCanvas(document.getElementsByTagName("canvas")[0].width, parseInt(e.target.value) | window.innerHeight);
}, false);
*/

document.getElementById("timeStepField").addEventListener('input', function(e){
	document.getElementById("timeStepLabel").textContent = e.target.value;
	 timestep = parseInt(e.target.value);
}, false);

document.getElementById("pointRadiusField").addEventListener('input', function(e){
	document.getElementById("pointRadiusLabel").textContent = e.target.value;
	lerpPointSize = parseInt(e.target.value);
}, false);

document.getElementById("radiusField").addEventListener('input', function(e){
	if(e.target.value === "0"){
		document.getElementById("radiusLabel").textContent = "Auto";
		radius = undefined;
	} else {
		document.getElementById("radiusLabel").textContent = e.target.value;
		radius = parseInt(e.target.value);
	}
}, false);

document.getElementById("iterationsField").addEventListener('input', function(e){
	document.getElementById("iterationsLabel").textContent = e.target.value;
	iter = parseInt(e.target.value);
}, false);

document.getElementById("opacityField").addEventListener('input', function(e){
	document.getElementById("opacityLabel").textContent = parseFloat(e.target.value).toFixed(2);
	opacity = 255*parseFloat(e.target.value);
}, false);

document.getElementById("colorSelect").addEventListener('input', function(e){
	setColors(e.target.selectedIndex);
}, false);

document.getElementById("ringsField").addEventListener('input', function(e){
	rings = 1000/parseFloat(e.target.value);
}, false);

document.getElementById("rotationOffsetField").addEventListener('input', function(e){
	rotationOffset = parseFloat(e.target.value)*Math.PI;
}, false);

document.getElementById("pointDistSelect").addEventListener('input', function(e){
	switch(e.target.selectedIndex){
		case 0:
			randomDistribution = false;
			break;

		case 1:
			fuzzyRandom = false;
			randomDistribution = true;
			break;

		case 2:
			fuzzyRandom = true;
			randomDistribution = true;
	}
}, false);

document.getElementById("stripeField").addEventListener('click', function(e){
	stripedFade = e.target.checked;
}, false);

document.getElementById("startButton").addEventListener('click', function(e){
	console.log("start button clicked");
	optionsConfirmed = true;
	started = true;
	document.getElementById("controlPanelHolder").style.display = "none";
});