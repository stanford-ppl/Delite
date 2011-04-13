function vals = sum_out_product(edge_potential, that_belief, that_message, that_size, this_size, first)

huge_value = 1e200;
min_prec = 1e-100;

if (first)
    this_mult = 6;
    that_mult = 1;
else
    this_mult = 1;
    that_mult = 6;
end

min = huge_value;
for i = 1 : that_size
    if ((that_message(i) > 0) && (that_message(i) < min))
        min = that_message(i);
    end
end

if (min < min_prec)
    min = min_prec;
end

vals = zeros(this_size, 1);
sum = 0;
for i = 1 : this_size
    num = 0;
    denum = 0;
    entry = 0;
    v = 0;
    for j = 1 : that_size
        num = that_belief(j);
        if (num > 0)
            denum = that_message(j);
            if (denum > min_prec)
                entry = num / (denum / min);
                v = v + entry * edge_potential((i-1)*this_mult + (j-1)*that_mult + 1);
            end
        end
    end
    vals(i) = v;
    sum = sum + v;
end

vals = vals / sum;
end