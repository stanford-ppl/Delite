
% A log plot may show the convergence better, as the learning curve is
% typically jagged even on convergence.
figure;
hold on;

log_tstf = log(time_steps_to_failure);
plot(log_tstf,'k');
 
% compute simple moving average
window = 50;
i = 1:window;
w = ones(1,window) ./ window;
weights = filter(w,1,log_tstf); 
    
x1 = window/2:size(log_tstf,2)-(window/2);
h = plot(x1,weights(window:size(log_tstf,2)), 'r--'); 
set(h, 'LineWidth', 2);

