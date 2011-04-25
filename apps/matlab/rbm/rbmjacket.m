function exectime = rbmjacket(datafile, numhid, numcases)
addpath /usr/local/jacket/engine

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
vishid     = gsingle(single(0.1)*randn(numdims, numhid, 'single'));
hidbiases  = zeros(1,numhid, 'single');
visbiases  = zeros(1,numdims, 'single');

poshidprobs = gsingle(zeros(numcases,numhid, 'single'));
neghidprobs = gsingle(zeros(numcases,numhid, 'single'));
posprods    = gsingle(zeros(numdims,numhid, 'single'));
negprods    = gsingle(zeros(numdims,numhid, 'single'));
vishidinc  = gsingle(zeros(numdims,numhid, 'single'));
hidbiasinc = gsingle(zeros(1,numhid, 'single'));
visbiasinc = gsingle(zeros(1,numdims, 'single'));

tic
for epoch = 1:maxepoch,
 fprintf(1,'epoch %d\r',epoch); 
 errsum=single(0);
 for batch = 1:numbatches,
 fprintf(1,'epoch %d batch %d\r',epoch,batch); 

%%%%%%%%% START POSITIVE PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  data = gsingle(trainingdata(((batch - 1) * numcases + 1):(batch * numcases), :));
  poshidprobs = 1./(1 + exp(-data*vishid - repmat(hidbiases,numcases,1)));    
  posprods    = data' * poshidprobs;
  poshidact   = sum(poshidprobs);
  posvisact = sum(data);

%%%%%%%%% END OF POSITIVE PHASE  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  poshidstates = poshidprobs > rand(numcases,numhid, 'single');

%%%%%%%%% START NEGATIVE PHASE  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  negdata = 1./(1 + exp(-poshidstates*vishid' - repmat(visbiases,numcases,1)));
  neghidprobs = 1./(1 + exp(-negdata*vishid - repmat(hidbiases,numcases,1)));    
  negprods  = negdata'*neghidprobs;
  neghidact = sum(neghidprobs);
  negvisact = sum(negdata); 

%%%%%%%%% END OF NEGATIVE PHASE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  err= sum(sum( ((data-negdata).^2) ));
  errsum = err + errsum;

   if epoch>5,
     momentum=finalmomentum;
   else
     momentum=initialmomentum;
   end;

%%%%%%%%% UPDATE WEIGHTS AND BIASES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    vishidinc = momentum*vishidinc + ...
                epsilonw*( 1/numcases * (posprods-negprods) - weightcost*vishid);
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
