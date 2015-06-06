
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

  // reverse the graph
  for(int i = 0; i < edges.size(); i++) {
    edge& e = edges[i];
    swap(e.n1, e.n2);
  }

  return graph_from_edgelist(edges);
}

vector<double> pagerank(const graph* g, int niters) {
  autotimer tt("pagerank", true);

  fprintf(stderr, "notice: running pagerank for %d iterations\n", niters);

  vector<double> pr(g->num_nodes, 1.0/g->num_nodes);
  vector<double> prnext(g->num_nodes);

  double damp = 0.15;

  // do pagerank
  for(int iter = 0; iter < niters; iter++) {
    #pragma omp parallel for schedule(dynamic, 1024)
    for(uint32_t n = 0; n < g->num_nodes; n++) {
      double acc = 0.0;
      uint32_t iebegin = g->edge_ptr[n];
      uint32_t ieend = g->edge_ptr[n+1];
      for(uint32_t ie = iebegin; ie < ieend; ie++) {
        uint32_t nn = g->ngbr_idx[ie];
        acc += pr[nn] / g->out_degree[nn];
      }
      prnext[n] = damp + (1.0 - damp) * acc;
    }
    pr.swap(prnext);
  }

  return pr;
}

int main(int argc, char *argv[]) {
  if(argc != 4) {
    fprintf(stderr, "usage: %s graph output niters\n", argv[0]);
    return 1;
  }

  //parse the input
  int niters;
  if(sscanf(argv[3], "%d", &niters) != 1) {
    fprintf(stderr, "error: could not parse input \"%s\" as integer\n", argv[3]);
    return 1;
  }

  // read the graph
  const graph* g = read_graph(argv[1]);

  vector<double> pr = pagerank(g, niters);

  delete g;

  // open output file
  FILE* fout = fopen(argv[2], "w");
  if(fout == NULL) {
    fprintf(stderr, "error: could not open output file \"%s\"\n", argv[2]);
  }

  for(uint32_t i = 0; i < pr.size(); i++) {
    fprintf(fout, "%d\t%f\n", i, pr[i]);
  }

  fclose(fout);

  return 0;
}
