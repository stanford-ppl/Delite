
// ==============================
//  Read app source code file(s)
// ==============================

var editor = null
var fileNameToFile = {}
var sourceFileReader = new FileReader()
sourceFileReader.onload = (function() {
  return function(e) {
      editor.setValue(e.target.result, -1)
  };
})();

function getFiles(evt) {
  var files = evt.target.files
  var numFiles = files.length
  for (var i = 0; i < numFiles; i++) {
    var file = files[i]
    fileNameToFile[file.name] = file
  }
}

function addAppSourceFileHandler(inputButtonId) {
  document.getElementById(inputButtonId).addEventListener('change', getFiles, false);
}

function readFile(sourceFile) {
  sourceFileReader.readAsText(fileNameToFile[sourceFile])
}

// ===========================
//  Read DEG file
// ===========================

var degOps = {}

function readDegFile(evt) {
  var reader = new FileReader()
  reader.onload = (function() {
    return function(e) {
      degOps = JSON.parse(e.target.result).DEG.ops
    };
  })();

  if (evt.target.files.length > 0) {
    var degFile = evt.target.files[0]
    reader.readAsText(degFile)
    viewState.degFile = degFile.name

    $("#degFileName").text(viewState.degFile)
    if (viewState.profileDataFile != "") {
      $("#startButton").css("border", "2px solid green")
    }
  }
}

function addDegFileHandler(inputButtonId) {
  document.getElementById(inputButtonId).addEventListener('change', readDegFile, false);
}

// =====================================================
// Read profileData.js (the performance profile data)
// =====================================================

var profileData = {}
function readProfileDataFile(evt) {
  var reader = new FileReader()
  reader.onload = (function() {
    return function(e) {
      profileData = JSON.parse(e.target.result)
    };
  })();

  if (evt.target.files.length > 0) {
    reader.readAsText(evt.target.files[0])
    viewState.profileDataFile = evt.target.files[0].name

    $("#profDataFileName").text(viewState.profileDataFile)
    if (viewState.degFile != "") {
      $("#startButton").css("border", "2px solid green")
    }
  }
}

function addProfileDataFileHandler(inputButtonId) {
  document.getElementById(inputButtonId).addEventListener('change', readProfileDataFile, false);
}