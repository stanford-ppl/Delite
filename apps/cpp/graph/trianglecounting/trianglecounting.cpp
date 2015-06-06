
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include <omp.h>
#include "../timer.hpp"
#include "../graph.hpp"

using namespace std;



const graph* read_graph(const char* path) {
  autotimer tt("input loading");

  std::vector<edge> edges = load_edgelist(path);

  // map/filter the read edges to be in a particular format
  std::vector<edge> filtered_edges;
  for(int i = 0; i < edges.size(); i++) {
    edge& e = edges[i];
    if(e.n1 != e.n2) {
      filtered_edges.push_back(edge { min(e.n1, e.n2), max(e.n1, e.n2) });
    }
  }

  return graph_from_edgelist(filtered_edges);
}

inline uint32_t count_intersections(const uint32_t* a_begin, const uint32_t* a_end, const uint32_t* b_begin, const uint32_t* b_end) {
  if((a_begin - a_end) < (b_begin - b_end)) {
    swap(a_begin, b_begin);
    swap(a_end, b_end);
  }
  uint32_t rv = 0;
  const uint32_t* a = a_begin;
  const uint32_t* b = b_begin;
  while((a < a_end)&&(b < b_end)) {
    while((a < a_end)&&(*a < *b)) {
      a++;
    }
    if((a < a_end)&&(*a == *b)) {
      rv++;
      a++;
    }
    b++;
  }
  return rv;
}

void count_triangles(const graph* g) {
  autotimer tt("triangle counting", true);

  fprintf(stderr, "notice: counting triangles\n");

  uint32_t num_triangles = 0;

  #pragma omp parallel for reduction(+:num_triangles) schedule(dynamic, 64)
  for(uint32_t n = 0; n < g->num_nodes; n++) {
    uint32_t iebegin = g->edge_ptr[n];
    uint32_t ieend = g->edge_ptr[n+1];
    for(uint32_t ie = iebegin; ie < ieend; ie++) {
      uint32_t nn = g->ngbr_idx[ie];
      num_triangles += count_intersections(
        &g->ngbr_idx[ie+1], &g->ngbr_idx[ieend], 
        &g->ngbr_idx[g->edge_ptr[nn]], &g->ngbr_idx[g->edge_ptr[nn+1]]
      );
    }
  }

  fprintf(stderr, "notice: found %d triangles\n", num_triangles);
}

int main(int argc, char *argv[]) {
  if(argc != 2) {
    fprintf(stderr, "usage: %s graph\n", argv[0]);
    return 1;
  }

  // read the graph
  const graph* g = read_graph(argv[1]);

  count_triangles(g);

  delete g;

  return 0;
}
