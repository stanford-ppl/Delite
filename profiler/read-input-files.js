
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

function readProfileDB(evt) {
  var reader = new FileReader();
  reader.onload = function() {
    var Uints = new Uint8Array(reader.result);
    config.profileDB = new ProfileDB( new SQL.Database(Uints) );
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

/*
// =====================================================
//  Read the info file
// =====================================================

var postProcessedProfile = undefined;
var profileDB = undefined;

function readAttributesFile(evt) {
  var attributeFileObj = undefined;
  var reader = new FileReader();

  reader.onload = ( function() {
    return function(e) {
      parseAttributesFile( attributeFileObj, e.target.result );
    };
  } )();

  var files = evt.target.files;
  if ( files.length > 0 ) {
    var attributeFileObj = files[0];
    attributeFileObj.name = "profile.db";
    attributeFileObj.size = 41984;
    readProfileDBFile( attributeFileObj );
  }
}

function readUIDataFile( file ) {
  var reader = new FileReader()
  reader.onload = ( function() {
    return function(e) {
      postProcessedProfile = JSON.parse(e.target.result).Profile;
    };
  } )();

  reader.readAsText( file );
};

function readProfileDBFile( file ) {
  var reader = new FileReader();
  reader.onload = function() {
    var Uints = new Uint8Array( reader.result );
    profileDB = new SQL.Database( Uints );
  }

  reader.readAsArrayBuffer( file ); 
}

function parseAttributesFile( attributesFileObj, contents ) {
  var arr = contents.split("\n");

  var uiDataFileAttrs = arr[0].split(",")
  attributesFileObj.name = uiDataFileAttrs[0];
  attributesFileObj.size = parseInt( uiDataFileAttrs[1] );
  readUIDataFile( attributesFileObj );

  var profileDBFileAttrs = arr[1].split(",")
  attributesFileObj.name = profileDBFileAttrs[0];
  attributesFileObj.size = parseInt( profileDBFileAttrs[1] );
  readUIDataFile( attributesFileObj );
};

function addDegFileHandler(inputButtonId) {
  document.getElementById(inputButtonId).addEventListener('change', readAttributesFile, false);
}
*/