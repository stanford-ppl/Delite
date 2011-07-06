function exectime = runautoencoder(gpu)
addpath autoencoder
if (gpu==1)
    exectime = autoencodergpu('../../data/ml/autoencoder/naturalimages.dat', 2000, 2000);
elseif (gpu==2)
    exectime = autoencoderjacket('../../data/ml/autoencoder/naturalimages.dat', 2000, 2000);
else
    %exectime = autoencoder('../../data/ml/autoencoder/naturalimages.dat', 2000, 2000);
	exectime = autoencoder('RAND', 100, 100, 500);
end
