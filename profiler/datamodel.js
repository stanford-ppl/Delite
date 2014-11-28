
function getProfileData(degFileNodes, rawProfileData, config) {
    var dependencyData = getDependencyData(degFileNodes);
    var executionProfile = getExecutionProfile(rawProfileData, dependencyData, config);

    return {
        "dependencyData": dependencyData, 
        "executionProfile": executionProfile
   };
}

function convertToStreamGraphFormat(rawProfileData) {
    function addSampleToAggrRes(sample, timeStamp) {
        sample.forEach(function (stat, i) {
            data.push({key: statNames[i], value: stat, time: timeStamp})
        })
    }

    var data = []
    var rawSamples = rawProfileData.MemUsageSamples
    var statNames = rawSamples["0"]
    var jvmUpTimeAtAppStart = rawProfileData.Init.JVMUpTimeAtAppStart
    var appStartTimeInMillis = rawProfileData.Init.AppStartTimeInMillis

    for (timeStamp in rawSamples) {
        var t = parseInt(timeStamp)
        if (t > 0) {
            var sample = rawSamples[timeStamp]
            // All 3 sets of data (ie, timeline, GCStats and MemUsage) are aligned to timestamps on this scale
            correctedTimeStamp = (t - appStartTimeInMillis) + jvmUpTimeAtAppStart 
            addSampleToAggrRes(sample, correctedTimeStamp)
        }
    }

    return data
}

function updateMemUsageOfDNodes(memProfile, dependencyData, executionProfile, config) {
    for(nodeName in memProfile) {
        if (nodeName != "dummy") {
            var totMemUsage = memProfile[nodeName]
            executionProfile.incrementMemUsage(nodeName, totMemUsage);

            if (isPartitionNode(nodeName, config)) {
                var parentName = getNameOfParentLoop(nodeName, config);
                executionProfile.incrementMemUsage(parentName, totMemUsage);
            }
        }
    }
}

function getNumberOfThreads(perfProfile) {
    return perfProfile.res.length - 1;
}

function getDependencyData(degFileNodes) {
    var nodes = []
    for (i in degFileNodes) {
        var newNodes = initializeNodeDataFromDegFile(degFileNodes[i], 0)
        for (j in newNodes) {
            var node = newNodes[j]
            nodes.push(node)
        }
    }

    var nodeNameToId = assignNodeIds(nodes)
    nodes.forEach(function (n, i) {
        var id = n.id
        n.inputs = n.inputs.map(function(_in) {return nodeNameToId[_in]})
        n.inputs.forEach(function(_in) {
                        nodes[_in].outputs.push(id)
        })

        n.controlDeps = n.controlDeps.map(function(_in) {return nodeNameToId[_in]})
        n.antiDeps = n.antiDeps.map(function(_in) {return nodeNameToId[_in]})
    })

    nodes = nodes.map(function(n) {
        n.numInputs = n.inputs.length;
        n.numOutputs = n.outputs.length;
        return n;
    })

    return {"nodes": nodes, "nodeNameToId": nodeNameToId, "maxNodeLevel": getMaxNodeLevel(nodes)}
}

function isValidKernel(nodeName, nodeNameToId, config) {
    var isSyncNode = config.syncNodeRegex.test(nodeName);
    var isNonSyncKernelNode = nodeName in nodeNameToId;

    if(isPartitionNode(nodeName, config)) {
        var parentName = getNameOfParentLoop(nodeName, config);
        isNonSyncKernelNode = isNonSyncKernelNode || (parentName in nodeNameToId);
    }

    return (isSyncNode || isNonSyncKernelNode);
}

function isPartitionNode(nodeName, config) {
    var m1 = nodeName.match(config.re_partition);
    var m2 = nodeName.match(config.re_header);
    return ((m1 != undefined) || (m2 != undefined));
}

function getNameOfParentLoop(nodeName, config) {
    var match = nodeName.match(config.re_partition);
    if (match) { return match[1]; }

    match = nodeName.match(config.re_header);
    if (match) { return match[1] }

    return "";
}

function initializeDataForTimelineView(maxNodeLevel) {
    var timelineData = {};
    for (var i = 0; i <= maxNodeLevel; i++) {
        timelineData[i] = {};
    }

    return timelineData;
}

// TODO: This is too long a function! Refactor it!
// This function does two major distinct tasks: 
//  (i) Build the data required for the timeline view
//  (ii) Compute other stats (eg: aggregate time stats per node, etc.)
function getExecutionProfile(rawProfileData, dependencyData, config) {
    var perfProfile = rawProfileData.PerfProfile;
    var jvmUpTimeAtAppStart = rawProfileData.Init.JVMUpTimeAtAppStart;
    var appStartTimeInMillis = rawProfileData.Init.AppStartTimeInMillis;

    var executionProfile = new ExecutionProfile();
    executionProfile.numThreads = getNumberOfThreads(perfProfile);

    var syncNodes = [];
    var ticTocRegions = [];
    var dataForTimelineView = initializeDataForTimelineView(dependencyData.maxNodeLevel);

    for (var i in perfProfile.kernels) {
        var name = perfProfile.kernels[i];
        var duration = perfProfile.duration[i];
        var start = (perfProfile.start[i] - appStartTimeInMillis) + jvmUpTimeAtAppStart;

        executionProfile.tryAddNode(name);
        if (isPartitionNode(name, config)) {
            var parentName = getNameOfParentLoop(name, config);
            executionProfile.tryAddNode(parentName);
        }

        if (isValidKernel(name, dependencyData.nodeNameToId, config)) {
            var threadId = perfProfile.location[i];
            var correspondingDNode = getDNodeCorrespondingToTNode(name, dependencyData, config);
            var tNode = new TNode(name, threadId, start, duration, correspondingDNode, config);

            if (tNode.type == "execution") {
                addToMap(dataForTimelineView[tNode.level], name, tNode);
                executionProfile.incrementTotalTime(name, duration);
            } else {
                syncNodes.push(tNode);
            }
        } else {
            var region = new TicTocRegion(name, start, duration, executionProfile.numThreads);
            ticTocRegions.push(region);

            if (name == "all") {
                executionProfile.totalAppTime = duration;
                executionProfile.appStartTime = start;
                executionProfile.appEndTime = start + duration;
            }
        }
    }

    assignSyncNodesToParents(dataForTimelineView, dependencyData, syncNodes, config);
    updateChildNodesOfTNodes(dataForTimelineView, dependencyData.maxNodeLevel, dependencyData);

    updateTimeTakenByPartitionedKernels(dependencyData, executionProfile);
    updateSyncAndExecTimesOfKernels(dataForTimelineView, dependencyData.maxNodeLevel, executionProfile);
    updateMemUsageOfDNodes(rawProfileData.MemProfile, dependencyData, executionProfile, config);

    // Extract the performance data for tic-toc regions
    var topLevelTNodes = getTopLevelTNodes(dataForTimelineView);
    assignTNodesToTicTocRegions(topLevelTNodes, ticTocRegions);
    updateTicTocRegionsData(ticTocRegions, executionProfile.totalAppTime);
    ticTocRegions.sort(function(r1,r2) {return r2.duration - r1.duration});
    executionProfile.ticTocRegions = ticTocRegions;

    // Update the remaining fields of executionProfile
    executionProfile.jvmUpTimeAtAppStart = jvmUpTimeAtAppStart;
    executionProfile.threadLevelPerfStats = getThreadLevelPerfStats(dataForTimelineView, executionProfile);
    executionProfile.memUsageData = convertToStreamGraphFormat(rawProfileData);
    executionProfile.computePercentageTimeForAllNodes(); // Important to sanitize the percentage values.
    executionProfile.timelineData = {
        "timing"        : dataForTimelineView,
        "lanes"         : perfProfile.res,
    };
    
    return executionProfile;
}

function assignTNodesToTicTocRegions(tNodes, ticTocRegions) {   
    for (var i in ticTocRegions) {
        var region = ticTocRegions[i];
        for (var j in tNodes) {
            var tNode = tNodes[j];
            if ((region.start <= tNode.start) && (tNode.end <= region.end)) {
                tNode.ticTocRegions.push(region);
                region.childNodes.push(tNode);
            } else if ( ((region.start <= tNode.start) && (tNode.start < region.end)) ||
                        ((tNode.start <= region.start) && (region.start < tNode.end)) ) {
                assignTNodesToTicTocRegions(tNode.childNodes, [region]);
                assignTNodesToTicTocRegions(tNode.syncNodes, [region]);
            }
        }
    }
}

function getTopLevelTNodes(dataForTimelineView) {
    var res = [];
    var topLevelNodes = dataForTimelineView[0];
    for (name in topLevelNodes) {
        res = res.concat(topLevelNodes[name]);
    }

    return res;
}

function findTicTocRegionsForTNode(tNode, ticTocRegions) {
    return ticTocRegions.filter(function(r) {return ((r.start <= tNode.start) && (tNode.end <= r.end))})
}

function updateTicTocRegionsData(ticTocRegions, totalAppTime) {
    function incAbsTime(map, name, id, inc) {
        if (!(name in map)) {
            map[name] = {"id": id, "abs": 0, "pct": 0}
        }

        map[name].abs += inc
    }

    function computePct(map, durationOfRegion) {
        for (k in map) {
            var d = map[k];
            d.pct = parseFloat(((d.abs * 100) / durationOfRegion).toFixed(2));
        }
    }

    ticTocRegions.forEach(function(r) {
        var duration = r.totalTime.abs;
        r.totalTime.pct = parseFloat(((duration * 100) / totalAppTime).toFixed(2))
        r.childNodes.forEach(function(n) {
            incAbsTime(r.childToPerf, n.name, n.id, n.duration)
            if (n.type == "execution") {
                r.execTimeStats[n.lane].abs += n.execTime.abs;
                r.syncTimeStats[n.lane].abs += n.syncTime.abs;
            } else if (n.type == "sync") {
                r.syncTimeStats[n.lane].abs += n.duration;
            }
        })

        computePct(r.childToPerf, duration)

        for (tid in r.execTimeStats) {
            r.execTimeStats[tid].pct = (r.execTimeStats[tid].abs * 100) / r.totalTime.abs;
            r.syncTimeStats[tid].pct = (r.syncTimeStats[tid].abs * 100) / r.totalTime.abs;
        }
    })
}

function getMaxNodeLevel(nodes) {
    return nodes.map(function(n) {return n.level})
                .reduce(function(a,b) {if (a >= b) {return a} else {return b}})
}

function getTNodeLevel(n) {
    if (n.node) {
        return n.node.level
    }

    return 0
}

// For partition nodes like x123_1, it returns the parent loop's dNode, ie, x123 in the case of x123_1
function getDNodeCorrespondingToTNode(tNodeName, dependencyData, config) {
    var id = dependencyData.nodeNameToId[tNodeName];
    if (id != undefined) {
        return dependencyData.nodes[id];
    }

    if (isPartitionNode(tNodeName, config)) {
        var parentName = getNameOfParentLoop(tNodeName, config);
        return getDNode(dependencyData, parentName);
    }

    return undefined;
}

function getDNode(dependencyData, nodeName) {
    var id = dependencyData.nodeNameToId[nodeName];
    if (id) {
        return dependencyData.nodes[id];
    }

    console.log("[WARNING]: Could not find dNode with name '" + nodeName + "'");
    return undefined;
}

function updateChildNodesOfTNodes(dataForTimelineView, maxNodeLevel, dependencyData) {
    function getParentName(n) {
        var dNode = n.node
        if (dNode.type == "InternalNode") {
            var parentId = dNode.parentId
            dNode = dependencyData.nodes[parentId]
        }

        var parentId = dNode.parentId
        var parent = dependencyData.nodes[parentId]
        var pType = parent.type
        if ((pType == "WhileLoop") || (pType == "Conditional") || (pType == "MultiLoop")) {
            return parent.name + "_" + n.lane
        }

        return parent.name
    }

    for (var i = maxNodeLevel; i > 0; i--) {
        var childNodes = dataForTimelineView[i]
        for (cname in childNodes) {
            var childRuns = childNodes[cname]
            childRuns.forEach(function(n) {
                if (n.type == "execution") {
                    var parentName = getParentName(n)
                    var parentRuns = dataForTimelineView[i - 1][parentName]
                    for (var j in parentRuns) {
                        var p = parentRuns[j]
                        if ((p.start <= n.start) && (n.end <= p.end)) {
                            p.childNodes.push(n)
                            n.parentId = p.id
                            n.parent = p
                            break;
                        }
                    }
                }
            })
        }
    }
}

function isLoopNode(dNode) {
    return ((dNode.type == "Conditional") ||
            (dNode.type == "MultiLoop") ||
            (dNode.type == "WhileLoop"));
}



function updateTimeTakenByPartitionedKernels(dependencyData, executionProfile) {
    function getMaxTimeAmongParitions(executionProfile, nodeName) {
        var maxTime = 0;
        for (var i = 0; i < executionProfile.numThreads; i++) {
            var partitionName = nodeName + "_" + i;
            var timeTakenByPartition = executionProfile.totalTime(partitionName).abs;
            maxTime = (timeTakenByPartition > maxTime) ? timeTakenByPartition : maxTime;
        }

        return maxTime;
    }

    dependencyData.nodes.forEach(function (dNode) {
        if(isLoopNode(dNode)) {
            var headerNodeName = dNode.name + "_h";
            var headerTime = executionProfile.totalTime(headerNodeName).abs;
            var maxTimeAmongParitions = getMaxTimeAmongParitions(executionProfile, dNode.name);
            var totalTime = headerTime + maxTimeAmongParitions;
            executionProfile.setTotalTime(dNode.name, totalTime);
        }
    })
}

function getThreadLevelPerfStats(dataForTimelineView, executionProfile) {
    var numThreads = executionProfile.numThreads;
    var threadToData = []

    for (var i = 0; i < numThreads; i++) {
        threadToData[i] = {"execTime": {"abs": 0, "pct": 0},
                           "syncTime": {"abs": 0, "pct": 0}}
    }

    var topLevelRuns = dataForTimelineView[0]
    for (tNodeName in topLevelRuns) {
        if (tNodeName != "all") {
            var runs = topLevelRuns[tNodeName]
            if (runs.length > 0) {
                var tid = runs[0].lane
                var threadStats = threadToData[tid]
                runs.forEach(function(run) {
                    if (run.type == "execution") {
                        threadStats.execTime.abs += run.execTime.abs
                        threadStats.syncTime.abs += run.syncTime.abs
                    } else if (run.type == "sync") {
                        threadStats.syncTime.abs += run.duration
                    } else {
                        console.log("WARNING: Unidentified type of tNode")
                    }
                })
            }
        }
    }

    var totalAppTime = executionProfile.totalAppTime;
    for (var i = 0; i < numThreads; i++) {
        var td = threadToData[i]
        td.execTime.pct = ((td.execTime.abs * 100) / totalAppTime).toFixed(2)
        td.syncTime.pct = ((td.syncTime.abs * 100) / totalAppTime).toFixed(2)
    }

    return threadToData
}

function updateSyncAndExecTimesOfKernels(dataForTimelineView, maxNodeLevel, executionProfile) {
    for (var l = maxNodeLevel; l >= 0; l--) {
        var runs = dataForTimelineView[l]
        for (tNodeName in runs) {
            computeSyncAndExecTimes(runs[tNodeName])
        }
    }

    function computeSyncAndExecTimes(tNodes) {
        if ((tNodes.length > 0) && (tNodes[0].type == "execution")) {
            var totalSyncTime = 0
            tNodes.forEach(function(n) {
                var selfSyncTime = sum(n.syncNodes.map(function(o) {return o.duration}))
                var childrenSyncTime = sum(n.childNodes.map(function(o) {return o.syncTime.abs}))
                var syncTime = selfSyncTime + childrenSyncTime
                n.syncTime.abs = syncTime
                n.syncTime.pct = (syncTime * 100) / n.duration
                n.execTime.abs = n.duration - syncTime
                n.execTime.pct = 100 - n.syncTime.pct

                totalSyncTime += syncTime
            })

            executionProfile.setSyncTime(tNodes[0].name, totalSyncTime);
        }
    }
}

function initializeNodeDataFromDegFile(node, level) {
    var nodeType = node.type
    var res = []
    var condOpsData = [] // for 'WhileLoop' nodes
    var bodyOpsData = [] // for 'WhileLoop' nodes
    var componentNodes = [] // for 'MultiLoop' nodes
    var partitionNodes = [] // for 'WhileLoop' and 'MultiLoop' nodes
    var thenOpsData = [] // for 'Conditional' nodes
    var elseOpsData = [] // for 'Conditional' nodes

    function processChildNodes(childNodes) {    
        var res = []
        if (childNodes) {
            childNodes.forEach(function(cn) {
                res = res.concat(initializeNodeDataFromDegFile(cn, level + 1))
            })
        }

        return res
    }

    if ((nodeType != "EOP") && (nodeType != "EOG")) {
        var name = node.kernelId;
        if (!name) name = node.outputId; // its a WhileLoop|Conditional. For such nodes, the kernelId is defined as the "outputId" attribute
        if (!name) name = "undefined"

        if (nodeType == "MultiLoop") {
            componentNodes = node.outputs // the 'outputs' attr of the MultiLoop contains the list of kernels that were merged to form the MultiLoop
        } else if (nodeType == "WhileLoop") {
            condOpsData = condOpsData.concat(processChildNodes(node.condOps))
            bodyOpsData = bodyOpsData.concat(processChildNodes(node.bodyOps))
        } else if (nodeType == "Conditional") {
            condOpsData = condOpsData.concat(processChildNodes(node.condOps))
            thenOpsData = thenOpsData.concat(processChildNodes(node.thenOps))
            elseOpsData = elseOpsData.concat(processChildNodes(node.elseOps))
        }

        // creating the node
        var n =  {  id              : 0, 
                    name            : name, 
                    inputs          : getOrElse(node.inputs, []), 
                    outputs         : [], 
                    depth           : 0, 
                    controlDeps     : getOrElse(node.controlDeps, []), 
                    antiDeps        : getOrElse(node.antiDeps, []),
                    target          : getKernelTargetPlatform(node.supportedTargets, "unknown"),
                    type            : getOrElse(nodeType, "unknown"),
                    condOps         : condOpsData,
                    bodyOps         : bodyOpsData,
                    componentNodes  : componentNodes,
                    thenOps         : thenOpsData,
                    elseOps         : elseOpsData,
                    level           : level,
                    parentId        : -1,  // -1 indicates top-level node. For child nodes, this field will be overwritten in assignNodeIds function
                    sourceContext   : {},
                    numInputs       : 0,
                    numOutputs      : 0
                 }

        updateSourceContext(n, node.sourceContext)
        res.push(n)
        res = res.concat(condOpsData).concat(bodyOpsData).concat(thenOpsData).concat(elseOpsData)

        return res
    }

    return []
}

function getDisplayTextForTimelineNode(tNode, syncNodeRegex) {
    var m = tNode.name.match(syncNodeRegex)
    if (m) {
        return ""
    } else if (tNode.node) {
        return tNode.node.sourceContext.opName
    } 

    return name
}

function assignSyncNodesToParents(dataForTimelineView, dependencyData, syncNodes, config) {
    syncNodes.forEach(function(n) {
        var m = n.name.match(config.syncNodeRegex)
        var parentName = m[2]
        n.dep_kernel = m[3]
        n.dep_thread = "T" + m[4]


        if (parentName == "null") { // top-level sync barrier
            n.level = 0
        } else {
            var parentDNodeName = parentName;
            if (isPartitionNode(parentName, config)) {
                parentDNodeName = getNameOfParentLoop(parentName, config);
            }

            var parentId = dependencyData.nodeNameToId[parentDNodeName]
            var parentLevel = dependencyData.nodes[parentId].level
            var parent = dataForTimelineView[parentLevel][parentName].filter(function(p) {
                return (p.start <= n.start) && (n.end <= p.end)
            })[0]   // There should be just one element in the filtered list anyways

            parent.syncNodes.push(n)
            n.level = parent.level + 1
            n.parentId = parentId
        }

        addToMap(dataForTimelineView[n.level], n.name, n)
    })
}

function updateSourceContext(node, sc) {
    var sourceContext = {"file": "", "line": 0, "opName": ""}
    if (sc) {
        var arr = sc.fileName.split("/")
        sourceContext.file = arr[arr.length - 1]
        sourceContext.line = parseInt(sc.line)
        sourceContext.opName = sc.opName
    } else {
        console.log("WARNING: SourceContext info not available for kernel: " + ((node.name)))
    }

    node.sourceContext = sourceContext
}

// TODO: Find out the resolution strategy used in Delite when a kernel has been generated for multiple targets
function getKernelTargetPlatform(supportedTargets, defaultVal) {
    if (supportedTargets) {
        return supportedTargets[0]
    }

    return defaultVal
}

function assignNodeIds(nodes) {
    var nodeNameToId = {}
    var nextId = 0
    nodes.forEach(function(node) {
        node.id = nextId++
        nodeNameToId[node.name] = node.id

        // assign same id to the nodes that fused to form this given node.
        node.componentNodes.forEach(function(comp) {nodeNameToId[comp] = node.id}) 
    })

    nextId = assignInheritedParentAttrsToDNodes(nodes, nodeNameToId, nextId)

    return nodeNameToId
}

function assignInheritedParentAttrsToDNodes(nodes, nodeNameToId, nextId) {
    function helper(parent, childType) {
        parent[childType].forEach(function(n) {
            n.parentId = parent.id
            n.target = parent.target

            if (n.type == "InternalNode") {
                n.sourceContext = parent.sourceContext
            }
        })
    }

    nodes.filter(function(node) {return (node.type == "WhileLoop") || (node.type == "MultiLoop") || (node.type == "Conditional")})
         .forEach(function(node) {
            helper(node, "condOps")
            helper(node, "bodyOps")
            helper(node, "thenOps")
            helper(node, "elseOps")

            nextId = assignInheritedParentAttrsToDNodes(node.condOps, nodeNameToId, nextId)
            nextId = assignInheritedParentAttrsToDNodes(node.bodyOps, nodeNameToId, nextId)
    })

    return nextId
}

function addToMap(map, key, value) {
    if (!(key in map)) {
        map[key] = []
    }

    map[key].push(value)
}

function getOrElse(obj, defaultObj) {
    if (obj) {
        return obj
    }

    return defaultObj
}

function sum(arr) {
    return arr.reduce(function(a,b) {return a + b}, 0)
}

function max(a,b) {
    if (a >= b) { return a }
    else { return b }
}