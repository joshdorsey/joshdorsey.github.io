function screenDist(distFromCamera, travelDist, viewPlaneDist){
	return (travelDist/distFromCamera)*viewPlaneDist;
}

function object(element, position, dist){
	return {el: element, off: position, pos: 0, d: dist};
}

var viewPlaneDist = 1;
var objects = [];

function updateObjects(e){
	var posX = window.innerWidth-e.clientX-(window.innerWidth/2);
	var posY = e.clientY-(window.innerHeight/2);
	for(var i = 0; i < objects.length; i++){
		objects[i].pos = posX+objects[i].off;
		$(objects[i].el).css("left", (window.innerWidth/2)+screenDist(objects[i].d, posX, viewPlaneDist)-($(objects[i].el).width()/2)+"px");
		$(objects[i].el).css("bottom", (window.innerHeight/2)+screenDist(objects[i].d, posY, viewPlaneDist)-($(objects[i].el).height()/2)+"px");
	}
}

window.addEventListener("mousemove", updateObjects, true);