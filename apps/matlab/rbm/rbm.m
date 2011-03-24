function exectime = rbm(datafile, numhid, numcases)
% Version 1.000 
%
% Code provided by Geoff Hinton and Ruslan Salakhutdinov 
%
% Permission is granted for anyone to copy, use, modify, or distribute this
% program and accompanying programs and documents for any purpose, provided
% this copyright notice is retained and prominently displayed, along with
% a note saying that the original programs are available from our
% web page.
% The programs and documents are distributed without any warranty, express or
% implied.  As the programs were written for research purposes only, they have
% not been tested to the degree that would be advisable in any important
% application.  All use of these programs is entirely at the user's own risk.

% This program trains Restricted Boltzmann Machine in which
% visible, binary, stochastic pixels are connected to
% hidden, binary, stochastic feature detectors using symmetrically
% weighted connections. Learning is done with 1-step Contrastive Divergence.   
% The program assumes that the following variables are set externally:
% maxepoch  -- maximum number of epochs
% numhid    -- number of hidden units 
% batchdata -- the data that is divided into batches (numcases numdims numbatches)

epsilonw      = single(0.1);   % Learning rate for weights 
epsilonvb     = single(0.1);   % Learning rate for biases of visible units 
epsilonhb     = single(0.1);   % Learning rate for biases of hidden units 
weightcost  = single(0.0002);   
initialmomentum  = single(0.5);
finalmomentum    = single(0.9);

maxepoch=10; %In the Science paper we use maxepoch=50, but it works just fine. 

trainingdata = single(load(datafile));
numdims=size(trainingdata, 2);
numbatches=size(trainingdata, 1) / numcases;

% Initializing symmetric weights and biases. 
vishid     = single(0.1)*randn(numdims, numhid, 'single');
hidbiases  = zeros(1,numhid, 'single');
visbiases  = zeros(1,numdims, 'single');

poshidprobs = zeros(numcases,numhid, 'single');
neghidprobs = zeros(numcases,numhid, 'single');
posprods    = zeros(numdims,numhid, 'single');
negprods    = zeros(numdims,numhid, 'single');
vishidinc  = zeros(numdims,numhid, 'single');
hidbiasinc = zeros(1,numhid, 'single');
visbiasinc = zeros(1,numdims, 'single');

tic
for epoch = 1:maxepoch,
 fprintf(1,'epoch %d\r',epoch); 
 errsum=single(0);
 for batch = 1:numbatches,
 fprintf(1,'epoch %d batch %d\r',epoch,batch); 

%%%%%%%%% START POSITIVE PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  data = trainingdata(((batch - 1) * numcases + 1):(batch * numcases), :);
  line3 = data*vishid;
  line2 = -line3 - repmat(hidbiases,numcases,1);
  line1 = 1 + exp(line2);
  poshidprobs = 1./(line1);    
  posprods    = data' * poshidprobs;
  poshidact   = sum(poshidprobs);
  posvisact = sum(data);

%%%%%%%%% END OF POSITIVE PHASE  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  poshidstates = poshidprobs > rand(numcases,numhid, 'single');

%%%%%%%%% START NEGATIVE PHASE  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  negdata = 1./(1 + exp(-poshidstates*vishid' - repmat(visbiases,numcases,1)));
  line3n = negdata*vishid;
  line2n = -line3n - repmat(hidbiases,numcases,1);
  line1n = 1 + exp(line2n);
  neghidprobs = 1./(line1n);    
  negprods  = negdata'*neghidprobs;
  neghidact = sum(neghidprobs);
  negvisact = sum(negdata); 

%%%%%%%%% END OF NEGATIVE PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  err= sum(sum( (data-negdata).^2 ));
  errsum = err + errsum;

   if epoch>5,
     momentum=finalmomentum;
   else
     momentum=initialmomentum;
   end;

%%%%%%%%% UPDATE WEIGHTS AND BIASES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    vishidinc = momentum*vishidinc + ...
                epsilonw*( (posprods-negprods)/numcases - weightcost*vishid);
    visbiasinc = momentum*visbiasinc + (epsilonvb/numcases)*(posvisact-negvisact);
    hidbiasinc = momentum*hidbiasinc + (epsilonhb/numcases)*(poshidact-neghidact);

    vishid = vishid + vishidinc;
    visbiases = visbiases + visbiasinc;
    hidbiases = hidbiases + hidbiasinc;

%%%%%%%%%%%%%%%% END OF UPDATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

  end
  fprintf(1, 'epoch %4i error %6.1f  \n', epoch, errsum); 
end;
exectime = toc
