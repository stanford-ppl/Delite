function exectime = autoencoder(datafile, Ninp, Nex, Nhid)

%x = load(datafile);
x = randn(Ninp, Nex);

alpha = 0.01;

tic

W1 = randn(Nhid,Ninp);
W2 = randn(Ninp,Nhid);

b1 = randn(Nhid,1);
b2 = randn(Ninp,1);

for i = 1:1000
z2 = W1*x+repmat(b1,1,Nex);
a2 = tanh(z2);

z3 = W2*a2 + repmat(b2,1,Nex);

% Error
%error = norm(z3-x,'fro')/Nex

delta3 = -(x - z3)/Nex;
delta2 = (W2'*delta3).*(1-a2.^2);


grad_W2 = delta3*a2';
grad_W1 = delta2*x';

grad_b2 = sum(delta3,2);
grad_b1 = sum(delta2,2);


W2 = W2 - alpha*grad_W2;
W1 = W1 - alpha*grad_W1;

b2 = b2 - alpha*grad_b2;
b1 = b1 - alpha*grad_b1;
end
exectime = toc