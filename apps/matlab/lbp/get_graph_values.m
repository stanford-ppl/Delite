% Parse pertinent statistics from an edge file representing a graph (e.g. onlyedges-stanford)
function [num_nodes, num_edges, max_line_size, max_node_num] = get_graph_values(filename)
num_nodes = 0;
num_edges = 0;
max_line_size = 0;

max_node_num = 0;
cur_line_size = 0;
node_num_index = 0;

% first pass:  find number of nodes, edges, and max line size
fid = fopen(filename);
while ~feof(fid)
    line = fgetl(fid);
    
    fields = regexp(line,'\t','split');
    node = str2double(fields{1});
    edges = sscanf(fields{2}, '%f');
    
    num_nodes = num_nodes + 1;
    num_edges = num_edges + length(edges);
    
    max_node_num = max([max_node_num; node; edges]);
    max_line_size = max(max_line_size, length(line));
end
fclose(fid);
end