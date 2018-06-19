% we have several images that should be close to each other and one distant
% from each other

im_files = {'g:/FIGRIM/SCENES_700x700/badlands/sun_bfubazsyxzcxnvtx.jpg',...
    'g:/FIGRIM/SCENES_700x700/badlands/sun_bhtjutyciamzamkv.jpg',...
    'g:/FIGRIM/SCENES_700x700/badlands/sun_bjynjpcltodfyuuo.jpg',...
    'g:/FIGRIM/SCENES_700x700/badlands/sun_bkyryspcupjqmqqx.jpg',...
    'g:/FIGRIM/SCENES_700x700/badlands/sun_blxhimheqqaprbws.jpg',...
    'g:/FIGRIM/SCENES_700x700/badlands/sun_bptnyzusewbqladq.jpg',...
    'g:/FIGRIM/SCENES_700x700/badlands/sun_bxxqkypwexedhjce.jpg',...
    'g:/FIGRIM/SCENES_700x700/badlands/sun_bzpjxqmxeuxuvpxa.jpg',
    'g:/FIGRIM/SCENES_700x700/badlands/sun_bdddqzpkaxzzxeom.jpg'};

n = length(im_files);

gist = cell(1,n);

for i=1:length(im_files)
    % Load image
    img = imread(im_files{i});

    % GIST Parameters:
    clear param
    param.orientationsPerScale = [8 8 8 8]; % number of orientations per scale (from HF to LF)
    param.numberBlocks = 4;
    param.fc_prefilt = 4;

    % Computing gist:
    [g, param] = LMgist(img, '', param);
    gist{i} = g;
end

% compute pairwise distances


pdist(gistD)
gistdist = zeros(n);

for i=1:n
    for j=i:n
        gistdist(j,i) = norm(gist{i} - gist{j});        
    end
end

csvwrite('gistdist.csv',gistdist)