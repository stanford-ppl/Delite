#ifndef _COLORING_H
#define _COLORING_H
#include <vector>
#include <iostream>
#include "PriorityQueue.h"

class Coloring {
	//GRAPH in CRS notation
	struct NodeInfo {
		size_t priority;
		size_t position;
	};
public:
	// size_t nN = number of nodes
	// size_t sz = pair of start and end pointers for each node's edge list in eg
	// size_t eg = 
	// size_t color_out = color number for each node
	// returns the number of colors used
	unsigned int run(size_t nN, size_t * sz, size_t * eg, size_t * color_out) {
		nNodes = nN;
		sizes = sz;
		edges = eg;
		nodes = new NodeInfo[nN];
		for(size_t i = 0; i < nNodes; i++) {
			nodes[i].priority = sizes[i + 1] - sizes[i];
		}
		removed.insert(removed.begin(),nNodes,false);
		
		pq.init(nodes, nodes + nNodes);
		NodeInfo * node;
		while(pq.pop(&node)) {
			removed[nodeID(node)] = true;
			for(size_t i = sizes[nodeID(node)]; i < sizes[nodeID(node)+1]; i++) {
				size_t otherNode = edges[i];
				if(!removed[otherNode]) {
					pq.prioritize(&nodes[otherNode],nodes[otherNode].priority - 1);
				}
			}
		}
		//now pq's internal array is reverse sorted in the order we removed stuff
		//now add the stuff back, coloring as we go
		for(size_t c = 0; c < nNodes; c++) {
			clearColors();
			NodeInfo * ni = pq.internal_array()[c];
			assert(removed[nodeID(ni)]);
			for(size_t i = sizes[nodeID(ni)]; i < sizes[nodeID(ni) + 1]; i++) {
				size_t otherNode = edges[i];
				if(!removed[otherNode]) {
					colors[color_out[otherNode]] = true;
				}
			}
			removed[nodeID(ni)] = false;
			color_out[nodeID(ni)] = getColor();
		}
		delete [] nodes;
		return colors.size();
	}
private:
	size_t nodeID(NodeInfo * ni) {
		return ni - nodes;
	}
	size_t getColor() {
		for(size_t i = 0; i < colors.size(); i++) {
			if(!colors[i])
				return i;
		}
		colors.push_back(false);
		return colors.size() - 1;
	}
	void clearColors() {
		for(size_t i = 0; i < colors.size(); i++)
			colors[i] = false;
	}
	size_t nNodes;
	size_t * sizes;
	size_t * edges;
	NodeInfo * nodes;
	PriorityQueue<NodeInfo *> pq;
	std::vector<bool> removed;
	std::vector<bool> colors;
};

#endif