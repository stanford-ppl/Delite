function [edge_potential, nodes] = read_beliefs(filename, nodes, last_cat_node, last_node)

CAT_BELIEF_SIZE = 8;
REL_BELIEF_SIZE = 6;

fid = fopen(filename);

% nothing on the first 2 lines of graphprint file
fgetl(fid);
fgetl(fid);

nodes.messages(1, max(nodes.num_edges), 1) = 0; %Resize array up-front to speed up ops.

% parse the potentials for the category nodes
disp 'Starting category nodes'
node_id = -1;
while (~feof(fid) && node_id ~= last_cat_node)
    line = fgetl(fid);
    
    fields = regexp(regexprep(line, '[\\[\\]\\=]', ''),' ','split');
    
    num_spaces = length(fields) - 1;
    if (num_spaces == 17) % a connected node
        node_id = str2double(fields{2});
        for i = 1 : CAT_BELIEF_SIZE
            dbl = str2double(fields{2*(i+1)});
            nodes.potential(node_id, i) = dbl;
            nodes.belief(node_id, i) = dbl;
        end     
        % initialize in-coming messages
        for i = 1 : nodes.num_edges(node_id)
            nodes.messages(node_id, i, 1:CAT_BELIEF_SIZE) = 1 / CAT_BELIEF_SIZE;
            nodes.message_size(node_id, i) = CAT_BELIEF_SIZE;
        end
    elseif (num_spaces == 3) % an unconnected node
        fields = regexp(regexprep(line, '[\\[\\]\\=]', ''),' ','split');
        node_id = str2double(fields{2});
    else
        fprintf('%s\n', line)
        error('error:  encountered unknown category node format\n')
        exit(-1);
    end
end
disp 'Finished category nodes'

% forward file pointer to the relations nodes
num_spaces = count_spaces(fgetl(fid));
while (num_spaces == 3)
    num_spaces = count_spaces(fgetl(fid));
end

% parse the potentials for the relations nodes
disp 'Starting relation nodes'
while (~feof(fid) && node_id ~= last_node)
    line = fgetl(fid);
        
    fields = regexp(regexprep(line, '[\\[\\]\\=]', ''),' ','split');
    
    num_spaces = length(fields) - 1;
    if (num_spaces == 13)
        node_id = str2double(fields{2});
        
        for i = 1 : REL_BELIEF_SIZE
            dbl = str2double(fields{2*(i+1)});
            nodes.potential(node_id, i) = dbl;
            nodes.belief(node_id, i) = dbl;
        end
        
        % initialize in-coming messages
        for i = 1 : nodes.num_edges(node_id)
            for j = 1 : REL_BELIEF_SIZE
                nodes.messages(node_id, i, j) = 1.0 / REL_BELIEF_SIZE;
                nodes.message_size(node_id, i) = REL_BELIEF_SIZE;
            end
        end
    else
        fprintf('%s\n', line);
        error('error:  encountered unknown relations node format\n');
        exit(-1);
    end
end
disp 'Finished relation nodes'

% forward file pointer to the edge potential
line = fgetl(fid);

% parse the edge potential
line = fgetl(fid);
fields = regexp(regexprep(line, '[\\[\\]\\=]', ''),' ','split');
num_spaces = length(fields) - 1;
if (num_spaces ~= 95)
    fprintf('%s\n', line);
    error('error:  encountered unknown edge potential format\n');
    exit(-1);
end
edge_potential = zeros(CAT_BELIEF_SIZE * REL_BELIEF_SIZE, 1);
for i = 1 : CAT_BELIEF_SIZE * REL_BELIEF_SIZE
    edge_potential(i) = str2double(fields{2*i});
end

fclose(fid);

end