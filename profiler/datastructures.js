
//====================================================
// Global variables
//====================================================

var ticTocRegionId = 0
var tNodeId = 0;

//====================================================
// Data structures
//====================================================

function TNode(name, threadId, start, duration, dNode, config) {
	this.name = name;
	this.lane = threadId;
	this.start = start;
	this.duration = duration;
	this.end = start + duration;
	this.node = dNode;
	//this.id = (dNode) ? dNode.id : undefined;
	this.id = tNodeId++;
	this.level = getTNodeLevel(this);
	this.displayText = getDisplayTextForTimelineNode(this, config.syncNodeRegex);
	this.childNodes = [];
	this.syncNodes = [];
	this.parentId = -1;
	this.dep_thread = ""; // important for sync nodes - specifies the thread the sync was expecting a result from
	this.dep_kernel = ""; // important for sync nodes - specifies the kernel the sync was expecting to complete
	this.execTime = new Time(0,0);
	this.syncTime = new Time(0,0);
	this.ticTocRegions = [];
	this.type = config.syncNodeRegex.test(this.name) ? "sync" : "execution";
}

function TicTocRegion(name, start, duration, numThreads) {
	this.id = ticTocRegionId++;
	this.name = name;
	this.start = start;
	this.totalTime = new Time(duration,0);
	this.end = start + duration;
	this.parent = null;
	this.childNodes = [];
	this.childToPerf = {};
	this.execTimeStats = createThreadLevelTimeStats(numThreads);
	this.syncTimeStats = createThreadLevelTimeStats(numThreads);
}

//====================================================
// Helper functions (used only within this file)
//====================================================

function createThreadLevelTimeStats(numThreads) {
    var tidToTime = {};
    for (var tid = 0; tid < numThreads; tid++) {
        tidToTime[tid] = new Time(0, 0);
    }

    return tidToTime;
}