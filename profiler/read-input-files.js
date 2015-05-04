
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

/*
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
    if ((viewState.profileDataFile != "") && (viewState.gcStatsFile != "")) {
      $("#startButton").css("border", "2px solid green")
    }
  }
}

function addDegFileHandler(inputButtonId) {
  document.getElementById(inputButtonId).addEventListener('change', readDegFile, false);
}
*/

/*
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
    if ((viewState.degFile != "") && (viewState.gcStatsFile != "")) {
      $("#startButton").css("border", "2px solid green")
    }
  }
}

function addProfileDataFileHandler(inputButtonId) {
  document.getElementById(inputButtonId).addEventListener('change', readProfileDataFile, false);
}
*/

// ====================
//  Read GC Stats file
// ====================

var gcEvents = []
function readGCStatsFile(evt) {
  var reader = new FileReader();
  reader.onload = (function() {
    return function(e) {
      //var t = parseInt(profileData.Profile.Init.JVMUpTimeAtAppStart)
      var t = parseInt(postProcessedProfile.AppData.jvmUpTimeAtAppStart);
      gcEvents = parseGCStatsDump(e.target.result, t);
    };
  })();

  if (evt.target.files.length > 0) {
    var file = evt.target.files[0]
    reader.readAsText(file)
    viewState.gcStatsFile = file.name

    $("#gcStatsFileName").text(viewState.gcStatsFile)
    if ((viewState.degFile != "") && (viewState.profileDataFile != "")) {
      $("#startButton").css("border", "2px solid green")
    }
  }
}

function addGCStatsFileHandler(inputButtonId) {
  $("#" + inputButtonId).on("change", readGCStatsFile)
}

// ===================================
//  Read post-processed profile data
// ===================================

var postProcessedProfile = {}

function readPostProcessedProfile(evt) {
  var reader = new FileReader()
  reader.onload = (function() {
    return function(e) {
      postProcessedProfile = JSON.parse(e.target.result).Profile;
    };
  })();

  if (evt.target.files.length > 0) {
    var f = evt.target.files[0];
    reader.readAsText(f);
    viewState.degFile = f.name;

    $("#degFileName").text(viewState.degFile)
    if ((viewState.profileDataFile != "") && (viewState.gcStatsFile != "")) {
      $("#startButton").css("border", "2px solid green");
    }
  }
}

function addDegFileHandler(inputButtonId) {
  document.getElementById(inputButtonId).addEventListener('change', readPostProcessedProfile, false);
}

// =====================================================
// Read profile.db (the performance profile data)
// =====================================================

var profileDB = undefined
function readProfileDB(evt) {
  var reader = new FileReader();
  reader.onload = function() {
    var Uints = new Uint8Array(reader.result);
    profileDB = new SQL.Database(Uints);
  }

  if (evt.target.files.length > 0) {
    var dbFile = evt.target.files[0];
    reader.readAsArrayBuffer(dbFile);
    viewState.profileDataFile = dbFile.name

    $("#profDataFileName").text(viewState.profileDataFile)
    if ((viewState.degFile != "") && (viewState.gcStatsFile != "")) {
      $("#startButton").css("border", "2px solid green")
    }
  }   
}

function addProfileDataFileHandler(inputButtonId) {
  document.getElementById(inputButtonId).addEventListener('change', readProfileDB, false);
}