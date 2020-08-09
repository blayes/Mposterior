clc; clear;

cd ~/gss/code

% overall posteriors 

for dd = 1:10
    load(strcat('/Shared/ssrivastva/gss/result/full/res_', num2str(dd), '.mat'));
    for cc = 1:9
        margMat = zeros(1000, 2);
        for ss = 1:1000
            margMat(ss, :) = history{1, ss}(cc, :);
        end
        csvwrite(strcat('/Shared/ssrivastva/gss/result/full/marg/', num2str(dd), '/marg_', num2str(cc), '.csv'), margMat);
    end
end

clear;

for dd = 1:10
    load(strcat('/Shared/ssrivastva/gss/result/full/res_', num2str(dd), '.mat'));
    for cc = 1:36
        jointMat = zeros(1000, 4);
        for ss = 1:1000
            jointMat(ss, :) = history{4, ss}(cc, :);
        end
        csvwrite(strcat('/Shared/ssrivastva/gss/result/full/joint/', num2str(dd), '/joint_', num2str(cc), '.csv'), jointMat);
    end
end

% subset posteriors, m = 10

clear;

for dd = 1:10
    for mm = 1:10         
        load(strcat('/Shared/ssrivastva/gss/data/sub10/', num2str(dd), '/res_sub_', num2str(mm), '.mat'));
        for cc = 1:9
            margMat = zeros(1000, 2);
            for ss = 1:1000
                margMat(ss, :) = history{1, ss}(cc, :);
            end
            csvwrite(strcat('/Shared/ssrivastva/gss/data/sub10/', ...
                            num2str(dd), '/marg/sub_', num2str(mm), '_marg_', num2str(cc), '.csv'), margMat); 
        end
    end
end

clear;

for dd = 1:10
    for mm = 1:10         
        load(strcat('/Shared/ssrivastva/gss/data/sub10/', num2str(dd), '/res_sub_', num2str(mm), '.mat'));
        for cc = 1:36
            jointMat = zeros(1000, 4);
            for ss = 1:1000
                jointMat(ss, :) = history{4, ss}(cc, :);
            end
            csvwrite(strcat('/Shared/ssrivastva/gss/data/sub10/', ...
                            num2str(dd), '/joint/sub_', num2str(mm), '_joint_', num2str(cc), '.csv'), jointMat); 
        end
    end
end

% subset posteriors, m = 20

clear;

for dd = 1:10
    for mm = 1:20         
        load(strcat('/Shared/ssrivastva/gss/data/sub20/', num2str(dd), '/res_sub_', num2str(mm), '.mat'));
        for cc = 1:9
            margMat = zeros(1000, 2);
            for ss = 1:1000
                margMat(ss, :) = history{1, ss}(cc, :);
            end
            csvwrite(strcat('/Shared/ssrivastva/gss/data/sub20/', ...
                            num2str(dd), '/marg/sub_', num2str(mm), '_marg_', num2str(cc), '.csv'), margMat); 
        end
    end
end

clear;

for dd = 1:10
    for mm = 1:20         
        load(strcat('/Shared/ssrivastva/gss/data/sub20/', num2str(dd), '/res_sub_', num2str(mm), '.mat'));
        for cc = 1:36
            jointMat = zeros(1000, 4);
            for ss = 1:1000
                jointMat(ss, :) = history{4, ss}(cc, :);
            end
            csvwrite(strcat('/Shared/ssrivastva/gss/data/sub20/', ...
                            num2str(dd), '/joint/sub_', num2str(mm), '_joint_', num2str(cc), '.csv'), jointMat); 
        end
    end
end






