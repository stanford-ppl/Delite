function exectime = gda(xfile, yfile)

x = load(xfile);
y = load(yfile);

tic

% num of training samples
m = length(y);

% dimensionality of training data
n = size(x, 2);

% phi, mu0, mu1, and sigma parameterize the GDA model, where we assume the input features are continuous-valued random variables with a multivariate normal distribution.
% phi is a scalar, mu0 and mu1 are n dimensional vectors, where n is the width of x, and sigma is an n x n matrix.


y_zeros = sum(y==0);
y_ones = sum(y==1);

x0 = x(y==0,:);
x1 = x(y==1,:);

mu0_num = sum(x0);
mu1_num = sum(x1);

% y_ones = 0;
% y_zeros = 0;
% mu0_num = zeros(1,n);
% mu1_num = zeros(1,n);
% 
% parfor i=1:m
%     if (y(i) == 0)
%         y_zeros = y_zeros + 1;
%         mu0_num = mu0_num + x(i,:);
%     else
%         y_ones = y_ones + 1;
%         mu1_num = mu1_num + x(i,:);
%     end
% end

phi = 1/m * y_ones;
mu0 = mu0_num / y_zeros;
mu1 = mu1_num / y_ones;

x0_mu0_tmp = x0-repmat(mu0,y_zeros,1);
x1_mu1_tmp = x1-repmat(mu1,y_ones,1);

sigma = (x0_mu0_tmp)'*(x0_mu0_tmp) + (x1_mu1_tmp)'*(x1_mu1_tmp);
    
% % calculate covariance matrix sigma
% 
% sigma = zeros(n, n);
% parfor i=1:m
%     if (y(i) == 0)
%         sigma = sigma + (x(i,:)-mu0)'*(x(i,:)-mu0);
%     else
%         sigma = sigma + (x(i,:)-mu1)'*(x(i,:)-mu1);
%     end
% end
exectime = toc;
