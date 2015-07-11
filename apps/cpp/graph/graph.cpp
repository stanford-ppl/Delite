
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include "graph.hpp"
#include "timer.hpp"

using namespace std;

bool operator<(const edge& e1, const edge& e2) {
  if (e1.n1 == e2.n1) {
    return e1.n2 < e2.n2;
  }
  else {
    return e1.n1 < e2.n1;
  }
}

bool operator==(const edge& e1, const edge& e2) {
  return (e1.n1 == e2.n1) && (e1.n2 == e2.n2);
}


vector<edge> load_edgelist(const char* path) {
  autotimer tt("loading edge list file");

  fprintf(stderr, "notice: loading edge list file\n");

  // open the edge file
  FILE* fg = fopen(path, "r");
  if (fg == NULL) {
    fprintf(stderr, "error: could not open file \"%s\"\n", path);
    exit(1);
  }

  // first, read the file
  std::vector<edge> edges;
  uint32_t n1;
  uint32_t n2;

  while(fscanf(fg, "%d %d\n", &n1, &n2) > 0) {
    edges.push_back(edge { n1, n2 });
  }

  fclose(fg);

  fprintf(stderr, "notice: read edge list file with %ld entries\n", edges.size());

  return edges;
}

const graph* graph_from_edgelist(vector<edge>& edges) {
  autotimer tt("constructing graph from edge list");

  fprintf(stderr, "notice: constructing graph from edge list\n");

  // first sort, then unique the edge list
  sort(edges.begin(), edges.end());
  auto it = unique(edges.begin(), edges.end());
  edges.resize(distance(edges.begin(), it));

  uint32_t num_nodes = 0;
  uint32_t num_edges = edges.size();

  // compute the number of nodes
  for(uint32_t ie = 0; ie < edges.size(); ie++) {
    num_nodes = max(num_nodes, max(edges[ie].n1, edges[ie].n2));
  }
  num_nodes = num_nodes + 1;

  fprintf(stderr, "notice: kept %d edges\n", num_edges);
  fprintf(stderr, "notice: identified %d nodes\n", num_nodes);

  // produce the graph
  uint32_t* out_degree = new uint32_t[num_nodes];
  uint32_t* edge_ptr = new uint32_t[num_nodes + 1];
  uint32_t* ngbr_idx = new uint32_t[num_edges];

  for(uint32_t i = 0; i < num_nodes; i++) {
    out_degree[i] = 0;
  }

  edge_ptr[0] = 0;
  uint32_t in = 0;
  for(uint32_t ie = 0; ie < num_edges; ie++) {
    out_degree[edges[ie].n2]++;
    ngbr_idx[ie] = edges[ie].n2;
    while(in < edges[ie].n1) {
      in++;
      edge_ptr[in] = ie;
    }
  }
  while(in < num_nodes) {
    in++;
    edge_ptr[in] = num_edges;
  }

  return new graph(num_nodes, num_edges, edge_ptr, ngbr_idx, out_degree);
}
