function [nodes, max_cat_node] = bpfactorgraph(max_node_num, edges)

CAT_BELIEF_SIZE = 8;
REL_BELIEF_SIZE = 6;
maxbeliefsize=max(CAT_BELIEF_SIZE, REL_BELIEF_SIZE);

nodes = struct;

nodes.length = max_node_num;
nodes.valid = zeros(nodes.length, 1);
nodes.ntype = zeros(nodes.length, 1);
nodes.potential = zeros(nodes.length, maxbeliefsize);
nodes.belief = zeros(nodes.length, maxbeliefsize);
nodes.belief_size = zeros(nodes.length, 1);
nodes.edges = zeros(nodes.length, 1); %Will grow as necessary in the second dimension
nodes.num_edges = zeros(nodes.length, 1);
nodes.messages = zeros(nodes.length, 1, maxbeliefsize); %Will grow as necessary in the second dimension
nodes.message_id = zeros(nodes.length, 1); %Will grow as necessary in the second dimension
nodes.message_size = zeros(nodes.length, 1); %Will grow as necessary in the second dimension

max_cat_node = 0;
for i = 1 : size(edges, 1)
    catIndex = edges(i, 1);
    relIndex = edges(i, 2);
    if (~nodes.valid(catIndex))
        % initialize category node
        nodes.valid(catIndex) = 1;
        nodes.ntype(catIndex) = 0; % 0=cat, 1=rel
        nodes.belief_size(catIndex) = CAT_BELIEF_SIZE;        
        if (catIndex > max_cat_node)
            max_cat_node = catIndex;
        end
    end
    
    if (~nodes.valid(relIndex))
        % initialize relation node
        nodes.valid(relIndex) = 1;
        nodes.ntype(relIndex) = 1;
        nodes.belief_size(relIndex) = REL_BELIEF_SIZE;
    end
    
    % fill message_id of the node in the other side
    nodes.message_id(catIndex, nodes.num_edges(catIndex) + 1) = nodes.num_edges(relIndex) + 1;
    nodes.message_id(relIndex, nodes.num_edges(relIndex) + 1) = nodes.num_edges(catIndex) + 1;
    
    % enter the edge
    nodes.edges(catIndex, nodes.num_edges(catIndex) + 1) = relIndex;
    nodes.num_edges(catIndex) = nodes.num_edges(catIndex) + 1;
    nodes.edges(relIndex, nodes.num_edges(relIndex) + 1) = catIndex;
    nodes.num_edges(relIndex) = nodes.num_edges(relIndex) + 1;
    
end

% fill in the unconnected nodes  %I don't think this is actually necessary
for i = 1 : nodes.length
    if (~nodes.valid(i))
        nodes.valid(i) = 1;
        nodes.ntype(i) = i > max_cat_node;
    end
end
