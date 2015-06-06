#pragma once

#include <vector>

class graph {
public:
  uint32_t num_nodes;
  uint32_t num_edges;
  const uint32_t* edge_ptr;
  const uint32_t* ngbr_idx;
  const uint32_t* out_degree;

public:
  graph(uint32_t nn, uint32_t ne, const uint32_t* ep, const uint32_t* ni, const uint32_t* od): 
    num_nodes(nn), num_edges(ne), edge_ptr(ep), ngbr_idx(ni), out_degree(od) { }

  ~graph() {
    delete edge_ptr;
    delete ngbr_idx;
    delete out_degree;
  }
};

struct edge {
  uint32_t n1;
  uint32_t n2;
};


std::vector<edge> load_edgelist(const char* path);
const graph* graph_from_edgelist(std::vector<edge>& edges);
