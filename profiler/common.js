
// Convert 'name' to HTML class selector string
function toClassSelector(name) {
	return '.' + name;
}

// Convert 'name' to HTML id selector string
function toIdSelector(name) {
	return '#' + name;
}

function getDisplayTextForTimeAbsPctPair(abs, pct) {
	var displayInMs = (abs < 1000) ? true : false;
	var timeAbs = displayInMs ? abs : (abs/1000).toFixed(0);
	var timeUnit = displayInMs ? "ms" : "s";
	return timeAbs + timeUnit + " : " + pct.toFixed(0) + "%";
}

function memUsageValueToStr(memUsage) {
	if (memUsage > 1) {
		var labels = ["B", "KB", "MB", "GB"];
		var i = 0;
		while ((i < labels.length) && (memUsage > 1)) {
			memUsage = memUsage / 1024;
			i++;
		}

		return ((memUsage * 1024).toFixed(0)) + labels[i - 1];
	}

	return "0B";
}