% Parse an entire edge file representing a graph (e.g. onlyedges-stanford).
function [read_edges, node_ids, edges] = read_graph_lr(filename, max_node_num, num_edges)

node_ids = zeros(max_node_num, 1);
edges = zeros(num_edges, 2);

% second pass: build graph
edge_index = 1;

fid = fopen(filename);
while ~feof(fid)
    line = fgetl(fid);
    
    fields = regexp(line,'\t','split');
    node = str2double(fields{1});
    f_edges = sscanf(fields{2}, '%f');

    node_ids(node) = node;
    
    for i=1:size(f_edges, 1)
        node_ids(f_edges(i)) = f_edges(i);
        edges(edge_index, :) = [node, f_edges(i)];
        edge_index = edge_index + 1;
    end
end
fclose(fid);

read_edges = edge_index - 1;
end