%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Probabilitistic Parafac Gibbs Sampler for Nonparametric Bayes Modeling of
% Multivariate Categorical Data (Tensor Factor Model)
% 
% For details of the sampler see Dunson and Xing (2009) at
% 
% http://www.tandfonline.com/doi/abs/10.1198/jasa.2009.tm08439#.Uxpc6Nww_0A
%
% Written by Sanvesh Srivastava on 03/03/14 based on a previous version
% by Jing Zhou of UNC, Biostatistics
% modified by SS 04/26/15
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [history, tend] = parafac_dx_full(dataMat, cats, nclass, nrun, nburn, nthin)
% parafac_dx_com performs Gibbs sampling according to Dunson and Xing (2009)
%
% dataMat: is N x P matrix, n-th row contains the P categorical responses of n-th
% individual; missing data is represented by -999 and baseline category
% is 0. Format is similar to the GSS database. Note 0 is changed to 1 later,
% so remember to change this if you are in a different setup
%
% cats: is the no. of max. categories in a particular variable; vector of
% length P.
%    
% nclass: is the upper bound on the total number of latent class in
% stick-breaking representation.
%    
% nrun: is the max. number of MCMC iterations.
%    
% nburn, nthins: are # burn-ins and # thining for posterior samples.
%    
% history: is a cell that contains the posterior samples misc. summaries
% from the Gibbs sampler. 
%    
% tend: is the time for Gibbs sampling.
    tic;    
    effSamp = ceil((nrun - nburn) / nthin);
    ncat    = length(cats);
    
    % -- initialize the \lambda tensor for the ndim variables
    [nsample, ndim] = size(dataMat); 
    lambda   = cell(1, ncat);  % joint distribution of categorical variables
    aLambda  = cell(1, ncat);  % parameters of Dirichlet for updating
                               % posterior of \lambda
    for ii = 1:ncat
        aLambda{ii}  = ones(cats(ii), nclass);        
        for jj = 1:cats(ii)        
            lambda{ii}(jj, :) = ones(1, nclass) * mean(dataMat(:, ii)  == jj); 
        end    
    end        
    
    % -- initialize parameters -- %
    alpha = 1;                                    % dp hyperparameters for stick breaking weights
    aal   = 1; bal = 1;                           % gamma hyperparameters for alpha
    aa     = cell(1, ncat);                        % dirichlet hyperparameter for \lambda_h^{(j)}
    for ii = 1:ncat
        aa{ii} = ones(cats(ii), 1);
    end
    % beta probs for computing stick breaking wts
    nus = betarnd(1, alpha, [nclass - 1, 1]); nu = zeros(nclass, 1); 
    % actual stick breaking wts for infinite mixture of atoms
    nu(1:(nclass - 1)) = nus .* cumprod([1; 1 - nus(1:(nclass-2))]); nu(nclass) = 1 - sum(nu(1:(nclass-1)));
    % Prob of latent variables belonging to classes; clases of obs; #obs assigned to a class
    zProbs = zeros(nsample, nclass); zClass = zeros(nsample, nclass); 
    
    dataBasedSummary = cell(1, ncat); % # obs for every category of a variable.    
    dataClassInd = cell(1, ncat); % indicator maxtrix of latent class memberships.
    for jj = 1:ncat        
        dataClassInd{jj} = zeros(nsample, cats(jj));
        for ii = 1:nsample
            if dataMat(ii, jj) > 0                    
                dataClassInd{jj}(ii, :) = dataMat(ii, jj) == 1:cats(jj);
            end    
        end
        dataBasedSummary{jj} = sum(dataClassInd{jj});
    end

    history = cell(5, effSamp);
    hhh     = 0;

    % -- start Gibbs sampler -- %
    for iter = 1:nrun    
        % -- update zs -- %
        zProbs = zeros(nsample, nclass); 
        zClass = zeros(nsample, nclass);     
        for ii = 1:nsample
            for jj = 1:ncat
                % notice the use of stochastic approx in form of ntotal/nsample
                zProbs(ii, :) = zProbs(ii, :) + log(sum(lambda{jj} .*  repmat(dataClassInd{jj}(ii, :)', 1, nclass), 1));           
            end
            zProbs(ii, :) = exp(zProbs(ii, :)) .* nu';
            if sum(zProbs(ii, :)) == 0 % will happen when zProbs = -Infs
                zProbs(ii, :) = ones(1, nclass) / nclass;
            else
                zProbs(ii, :) = zProbs(ii, :) / sum(zProbs(ii, :)); 
            end
        end
        
        % a way to avoid nan's if things are generated using mnrnd
        mat1 = [zeros(nsample, 1) cumsum(zProbs, 2)];
        rr = unifrnd(0, 1, [nsample, 1]); z = zeros(nsample, 1);
        for l = 1:nclass
            ind = rr > mat1(:, l) & rr <= mat1(:, l + 1); z(ind) = l;
        end
        
        for ii = 1:nsample
            zClass(ii, z(ii)) = 1;        
        end
        classCts = sum(zClass); % # samples assigned to each class
        
        % -- update lambda -- %
        for kk = 1:nclass        
            for jj = 1:ncat                
                memInd = sum(dataClassInd{jj} .* repmat(zClass(:, kk), 1, cats(jj)));
                aLambda{jj}(:, kk) = aa{jj}' + memInd;             
                % generate lambda from a dirichlet dist. using gamma dist.
                lambda{jj}(:, kk) = gamrnd(aLambda{jj}(:, kk), 1);
                lambda{jj}(:, kk) = lambda{jj}(:, kk) / sum(lambda{jj}(:, kk)); 
            end
        end
        
        % -- update nu -- %
        for kk = 1:(nclass - 1)
            nus(kk) = betarnd(1 + classCts(kk), alpha + sum(sum(zClass(:, (kk + 1):nclass))));
        end    
        nu(1:(nclass - 1)) = nus .* cumprod([1; 1 - nus(1:(nclass-2))]); nu(nclass) = 1 - sum(nu(1:(nclass-1)));
        
        % -- update alpha-- %
        nuss = 1 - nus(1:(nclass - 1)); nuss(nuss < 1e-6) = 1e-6; % avoids numerical errors
        alpha = gamrnd(aal + nclass - 1, 1 / (bal - sum(log(nuss))));
        
        % -- store draws across iterations -- %   
        if ((iter > nburn) & (mod(iter, 5) == 0)) 
            % -- calc marginals -- %
            margMat = eye(ncat, 2);
            for ii = 1:ncat            
                margMat(ii, :) = sum(bsxfun(@times, lambda{ii}', nu)); 
            end
            
            jointMat = zeros(ncat * (ncat - 1) / 2, 4);
            cnt = 0;
            for ii = 1:(ncat - 1) 
                for jj = (ii + 1):ncat
                    cnt = cnt + 1;
                    pp1pp2 = bsxfun(@times, lambda{ii}', sqrt(nu))' * ...
                             bsxfun(@times, lambda{jj}', sqrt(nu));
                    jointMat(cnt, :) = pp1pp2(:); % format: 11, 12, 21, 22
                end                
            end

            hhh = hhh + 1;
            history{1, hhh} = margMat;
            history{2, hhh} = nu;            
            history{3, hhh} = alpha;
            history{4, hhh} = jointMat;
            history{5, hhh} = lambda;            
        end        
        if mod(iter, 1000) == 0, disp(iter); end    
    end    
    tend = toc;
end
