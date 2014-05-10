
var aceRange = ace.require('ace/range').Range;

function createEditor(editorId) {
	var editor = ace.edit(editorId);
    editor.setTheme("ace/theme/twilight");
    editor.getSession().setMode("ace/mode/scala");
    editor.setReadOnly(true)
    editor.setAnimatedScroll(true)

    return editor
}

function highlightLine(line) {
    var lineIndex = line - 1
    var r = new aceRange(lineIndex,0,lineIndex,10000)
    var res = editor.addSelectionMarker(r)
    editor.scrollToLine(line,true)

    return res
}

function unhighlightLine(r) {
    editor.removeSelectionMarker(r)
}