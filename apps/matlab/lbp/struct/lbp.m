function exectime = lbp(edgefile, printfile)

max_iter = 50;
converge_thresh = 0.001;

last_fraction_not_converged = 0.0;
last_avg_dist = 0.0;
last_max_dist = 999.0;

%%% Load data
% first pass for edges
[num_nodes, num_edges, max_line_size, max_node_num] = get_graph_values(edgefile);

fprintf('num nodes:  %d\n', num_nodes);
fprintf('num edges:  %d\n', num_edges);
fprintf('max line:  %d\n', max_line_size);
fprintf('max node:  %d\n', max_node_num);

% init variables for second pass
[read_edges, node_ids, edges] = read_graph_lr(edgefile, max_node_num, num_edges);
fprintf('Read %d edges from graph\n', read_edges);

% create graph from edges
[nodes, max_cat_node] = bpfactorgraph(max_node_num, edges);
fprintf('create graph for %d nodes\n',max_node_num);

% read beliefs
[edge_potential, nodes] = read_beliefs(printfile, nodes, max_cat_node, max_node_num);

tic

last_cat_node = max_cat_node;
last_node = max_node_num;

iter = 0;
while (iter < max_iter & last_max_dist > converge_thresh)
%     nodes = calculate_messages(last_cat_node, nodes, edge_potential);
%     [nodes, last_fraction_not_converged, last_max_dist, last_avg_dist] = calculate_beliefs(last_node, nodes, converge_thresh);
    disp 'Calculating messages'
    calculate_messages();
    disp 'Calculating beliefs'
    calculate_beliefs();

    fprintf('Iter %d: fraction not converged: %d max dist: %d avg dist: %d\n', iter, last_fraction_not_converged, last_max_dist, last_avg_dist)
    iter = iter + 1;
end
fprintf('Finished in %d iters\n', iter)

exectime = toc


function calculate_messages()

CAT_BELIEF_SIZE = 8;
REL_BELIEF_SIZE = 6;

for i = 1 : last_cat_node
    if ((nodes(i).ntype == 0) && nodes(i).valid)
        for j = 1 : nodes(i).num_edges
            to = nodes(i).edges(j);
            to_message_id = nodes(i).message_id(j);
            
            %need to initialize message size
            answer1 = sum_out_product(edge_potential, nodes(to).belief(1:nodes(to).belief_size), nodes(to).messages(to_message_id, 1:nodes(to).message_size(to_message_id)), REL_BELIEF_SIZE, CAT_BELIEF_SIZE, 1);
            answer2 = sum_out_product(edge_potential, nodes(i).belief(1:nodes(i).belief_size), nodes(i).messages(j, 1:nodes(i).message_size(j)), CAT_BELIEF_SIZE, REL_BELIEF_SIZE, 0);
            
            nodes(i).messages(j, 1:length(answer1)) = answer1;
            nodes(i).message_size(j) = length(answer1);
            nodes(to).messages(to_message_id, 1:length(answer2)) = answer2;
            nodes(to).message_size(to_message_id) = length(answer2);
        end
    end
end
end

function calculate_beliefs()

CAT_BELIEF_SIZE = 8;
REL_BELIEF_SIZE = 6;

dist_sum = 0;
num_not_converged = 0;

distances = zeros(last_node, 1);
parfor i = 1 : last_node
    distance = 0;
    if (nodes(i).valid)
        % make a copy of the current belief
        tmp_belief = nodes(i).belief;
        nodes(i).belief = nodes(i).potential;
        
        % integrate messages into belief
        for j = 1 : nodes(i).num_edges
            nodes(i).belief = nodes(i).belief .* nodes(i).messages(j, :)';
            nodes(i).belief = nodes(i).belief / sum(nodes(i).belief); %This works because the remaining elements of a belief are 0 if not used
        end
        
        % compute distance between new and previous beliefs
        distance = max(abs(nodes(i).belief - tmp_belief));
    end
    distances(i) = distance;
end

for i = 1 : length(distances)
    distance = distances(i);
    if (distance > converge_thresh)
        num_not_converged = num_not_converged + 1;
        dist_sum = dist_sum + distance;
    end
end

last_fraction_not_converged = num_not_converged / (last_node - 38);
if (num_not_converged == 0)
    last_avg_dist = 0;
else
    last_avg_dist = dist_sum / num_not_converged;
end
last_max_dist = max(distances);
end


end