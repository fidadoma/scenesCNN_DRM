clear all
% following code is just a sample that I tested on my ntb
% dir_pth = 'c:/Users/filip/Downloads/kata_db/natural/mountain';
file_pth = 'g:/Gauk 2018/scenesCNN_DRM/data/file_info.csv';
data_pth = 'g:/Gauk 2018/scenesCNN_DRM/data';
df = readtable(file_pth,'Delimiter',','); 

n = size(df,1);

% GIST Parameters:
clear param
param.orientationsPerScale = [8 8 8 8]; % number of orientations per scale (from HF to LF)
param.numberBlocks = 4;
param.fc_prefilt = 4;

gist = zeros(n,512);

fprintf('Computing..\n=======================\n\n');

tstart=tic;
nrows = n; complete=1;

for i=1:n
    f = df.filename{i};
    fpth = df.pth{i};
    fprintf('%d/%d - %s:', i, n,f)
    im1 = imread(fpth);
    [gist(i,:), ~] = LMgist(im1, '', param);
    
    ttime = toc(tstart);
    if (complete>0), eta = ttime/complete * (nrows - complete);
        fprintf('   time=%8.3f ETA=%8.3f s (%.1f%%).\n',...
            ttime, eta,complete/nrows*100);
    else
        fprintf('\n');
    end
    complete = complete+1;
end
  
csvwrite(fullfile(data_pth, 'gist_figrim_all.csv'), gist);

