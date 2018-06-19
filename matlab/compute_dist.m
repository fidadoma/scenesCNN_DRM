clear all
dir_pth = 'c:/Users/filip/Downloads/kata_db/natural/mountain';
fs = dir(dir_pth);
fs = fs(3:end);

n = numel(fs);

% GIST Parameters:
clear param
param.orientationsPerScale = [8 8 8 8]; % number of orientations per scale (from HF to LF)
param.numberBlocks = 4;
param.fc_prefilt = 4;

gist = zeros(n,512);

fprintf('Computing..\n=======================\n\n');

for i=1:numel(fs)
    f = fs(i).name;
    fprintf('%d/%d - %s:', i, n,f)
    im1 = imread(fullfile(dir_pth,f));
    [gist(i,:), ~] = LMgist(im1, '', param);
    fprintf('completed..\n')
end
  
csvwrite('gist_mountain.csv', gist);

for c=1:n
    for r=c:n
       
    gist{i} = g;
    end
end