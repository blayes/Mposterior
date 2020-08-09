function submit_parafac_full(dd, nclass, nrun, nburn, nthin)

train = csvread(strcat('../data/csv_train/full/train_', num2str(dd), '.csv'));

lambda0 = [mean((train == 1), 1)' mean((train == 2), 1)'];

[nsample ndim] = size(train);

cats = repmat(2, 1, ndim);
[history, tend] = parafac_dx_full(train, cats, nclass, nrun, nburn, nthin);

save(strcat('/nfsscratch/Users/ssrivastva/gss/result/full/res_', num2str(dd), '.mat'), 'history', 'tend', 'lambda0');    
csvwrite(strcat('/nfsscratch/Users/ssrivastva/gss/result/full/lambda0_', num2str(dd), '.csv'), lambda0);
csvwrite(strcat('/nfsscratch/Users/ssrivastva/gss/result/full/time_', num2str(dd), '.csv'), tend);

disp(['done with cv ' num2str(dd) ' ...' ]);    
        
quit
