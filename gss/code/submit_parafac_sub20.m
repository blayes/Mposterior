function submit_parafac_sub20(dd, nclass, nsub, nrun, nburn, nthin)

cvidx = repmat(1:10, nsub, 1);
cvidx = cvidx(:);

subidx = repmat(1:nsub, 1, 10);
subidx = subidx(:);

nrepf = cvidx(dd);
nsubf = subidx(dd);

disp(['nrep: ', num2str(nrepf) ' nsub: ', num2str(nsubf)]);

totTrain = csvread(strcat('../data/csv_train/full/train_', num2str(nrepf), '.csv'));
train = csvread(strcat('../data/csv_train/sub20/', num2str(nrepf), '/train_', num2str(nsubf), '.csv'));

lambda0 = [mean((train == 1), 1)' mean((train == 2), 1)'];

[nsample ndim] = size(train);

cats = repmat(2, 1, ndim);
[ntotal, ~] = size(totTrain);
[history, tend] = parafac_dx_sub(train, cats, nclass, ntotal, nrun, nburn, nthin);

save(strcat('/Shared/ssrivastva/gss/data/sub20/', num2str(nrepf), '/res_sub_', num2str(nsubf), '.mat'), 'history', 'tend', 'lambda0');    
csvwrite(strcat('/Shared/ssrivastva/gss/data/sub20/', num2str(nrepf), '/lambda0_', num2str(nsubf), '.csv'), lambda0);
csvwrite(strcat('/Shared/ssrivastva/gss/data/sub20/', num2str(nrepf), '/time_', num2str(nsubf), '.csv'), tend);

disp(['done with cv ' num2str(nrepf) ' ...' ' subset ... ' num2str(nsubf) ' ... ']);    
        
quit
