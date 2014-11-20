
// =========================================
//  Constants
// =========================================

var MINOR_GC = 0
var MAJOR_GC = 1
var UNDEFINED = -1

// =========================================
//  Static values
// =========================================

var sizeStatsStrDelimiters = /[\->\(\)]+/

// =========================================
//  GC Event Class
// =========================================

function GCEvent(gcEntry, start) {
	this.type = UNDEFINED
	switch(gcEntry[1]) {
		case "Full": this.type = MAJOR_GC; break
		case "GC"  : this.type = MINOR_GC; break
		default    : console.error("Unexpected type of GC entry found: " + line); return
	}

	this.start = start // in milliseconds
	this.duration = getDurationInMillis(gcEntry, this.type)
	this.youngGenStats = getYoungGenStats(gcEntry, this.type)
	this.oldGenStats = getOldGenStats(gcEntry, this.type)
	this.heapStats = getHeapStats(gcEntry, this.type)
}

// Function to parse the entire GC dump and create GCEvent objects
//   Sample lines from gcStats dump:
//     1.101: [GC [PSYoungGen: 33792K->4424K(38912K)] 33792K->4432K(125952K), 0.0083380 secs] [Times: user=0.01 sys=0.01, real=0.00 secs]
//     28.416: [Full GC [PSYoungGen: 2039K->0K(192512K)] [ParOldGen: 73593K->60274K(141312K)] 75633K->60274K(333824K) [PSPermGen: 30245K->30203K(60928K)], 0.5467010 secs] [Times: user=    1.22 sys=0.01, real=0.55 secs]
function parseGCStatsDump(dump, jvmpUpTimeAtAppStart) {
	var gcEvents = []
	var delimiters = /[\s\[\]:,]+/
	var regexForGCEntry = /^\d.*/

	dump.split("\n").forEach(function (line) {
		if (regexForGCEntry.test(line)) {
			var gcEntry = line.split(delimiters)
			var start = getStartTimeInMillis(gcEntry)
			if (start > 0) gcEvents.push(new GCEvent(gcEntry, start))
		}
	})

	return gcEvents
}

// =========================================
//  Helper functions to parse each GC entry
// =========================================

function getStartTimeInMillis(gcEntry) {
	return (parseFloat(gcEntry[0]) * 1000)
}

function getDurationInMillis(gcEntry, gcType) {
	switch(gcType) {
		case MAJOR_GC: return parseFloat(gcEntry[10]) * 1e+03
		case MINOR_GC: return parseFloat(gcEntry[5]) * 1e+03
	}
}

function getYoungGenStats(gcEntry, gcType) {
	var offset = 0
	switch(gcType) {
		case MAJOR_GC: offset = 3; break
		case MINOR_GC: offset = 2; break
		default      : console.error("Unexpected value for gcType")
	}

	return {
		collectorType: getCollectorType(gcEntry[offset]),
		sizeStats    : getSizeStats(gcEntry[offset + 1])
	}
}

function getOldGenStats(gcEntry, gcType) {
	var offset = 0
	switch(gcType) {
		case MAJOR_GC: offset = 5; break
		case MINOR_GC: return null // OldGen is not collected during Minor GC
		default      : console.error("Unexpected value for gcType")
	}

	return {
		collectorType: getCollectorType(gcEntry[offset]),
		sizeStats    : getSizeStats(gcEntry[offset + 1])
	}
}

function getHeapStats(gcEntry, gcType) {
	var offset = 0
	switch(gcType) {
		case MAJOR_GC: offset = 7; break
		case MINOR_GC: offset = 4; break
		default      : console.error("Unexpected value for gcType")
	}

	return getSizeStats(gcEntry[offset])
}

function getCollectorType(str) {
	switch(str) {
		case "PSYoungGen": return "Parallel Scavenger"
		case "ParOldGen" : return "PARALLEL_OLD_COMPACTING"
		default 		 : return "Unknown"
	}
}

function getSizeStats(sizeStatsStr) {
	var a = sizeStatsStr.split(sizeStatsStrDelimiters)
	return {
		beforeGC : a[0],
		afterGC  : a[1],
		committed: a[2]
	}
}

