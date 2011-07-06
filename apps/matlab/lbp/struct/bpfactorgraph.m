function [nodes, max_cat_node] = bpfactorgraph(max_node_num, edges)

CAT_BELIEF_SIZE = 8;
REL_BELIEF_SIZE = 6;
maxbeliefsize=max(CAT_BELIEF_SIZE, REL_BELIEF_SIZE);

for i = 1:max_node_num
    nodes(i).valid = 0;
    nodes(i).ntype = 0;
    nodes(i).potential = zeros(maxbeliefsize, 1);
    nodes(i).belief = zeros(maxbeliefsize, 1);
    nodes(i).belief_size = 0;
    nodes(i).edges = 0; %Will grow as necessary
    nodes(i).num_edges = 0;
    nodes(i).messages = zeros(1, maxbeliefsize); %Will grow as necessary
    nodes(i).message_id = 0; %Will grow as necessary
    nodes(i).message_size = 0; %Will grow as necessary
end

max_cat_node = 0;
for i = 1 : size(edges, 1)
    catIndex = edges(i, 1);
    relIndex = edges(i, 2);
    if (~nodes(catIndex).valid)
        % initialize category node
        nodes(catIndex).valid = 1;
        nodes(catIndex).ntype = 0; % 0=cat, 1=rel
        nodes(catIndex).belief_size = CAT_BELIEF_SIZE;        
        if (catIndex > max_cat_node)
            max_cat_node = catIndex;
        end
    end
    
    if (~nodes(relIndex).valid)
        % initialize relation node
        nodes(relIndex).valid = 1;
        nodes(relIndex).ntype = 1;
        nodes(relIndex).belief_size = REL_BELIEF_SIZE;
    end
    
    % fill message_id of the node in the other side
    nodes(catIndex).message_id(nodes(catIndex).num_edges + 1) = nodes(relIndex).num_edges + 1;
    nodes(relIndex).message_id(nodes(relIndex).num_edges + 1) = nodes(catIndex).num_edges + 1;
    
    % enter the edge
    nodes(catIndex).edges(nodes(catIndex).num_edges + 1) = relIndex;
    nodes(catIndex).num_edges = nodes(catIndex).num_edges + 1;
    nodes(relIndex).edges(nodes(relIndex).num_edges + 1) = catIndex;
    nodes(relIndex).num_edges = nodes(relIndex).num_edges + 1;
    
end

% fill in the unconnected nodes  %I don't think this is actually necessary
for i = 1 : length(nodes)
    if (~nodes(i).valid)
        nodes(i).valid = 1;
        nodes(i).ntype = i > max_cat_node;
    end
end
