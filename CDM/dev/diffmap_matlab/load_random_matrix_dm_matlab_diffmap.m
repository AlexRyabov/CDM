k_min = 10;
Laplacian = "rownorm";
Metric = "spearman";
DMvariables = 10;
mask = [];

script_dir = fileparts(mfilename("fullpath"));
dm_dir = "C:\Users\reeb1775\OneDrive\Projects\Matlab\DM";

addpath(dm_dir)

input_file = fullfile(script_dir, "random_matrix_20x10_rowsum1.csv");
output_file = fullfile(script_dir, "random_matrix_dm_matlab_diffmap.csv");
eigv_file = fullfile(script_dir, "random_matrix_dm_matlab_diffmap_eigv.csv");

data = readtable(input_file);

options = diffmap_options( ...
    "k_min", k_min, ...
    "Laplacian", Laplacian, ...
    "Metric", Metric, ...
    "DMvariables", DMvariables, ...
    "mask", mask);

simM = diffmap_similarity(data{:, :}, options);
DM = diffmap_eigv(simM, options);

writematrix(simM.S, fullfile(script_dir, "random_matrix_similarity_matlab_diffmap.csv"));

DM_table = array2table(DM.DMC, 'VariableNames', cellstr(compose("DM%d", 1:size(DM.DMC, 2))));
writetable(DM_table, output_file);

eigv_table = table((1:numel(DM.eigValues))', DM.eigValues(:), ...
    'VariableNames', {'DM', 'eigv'});
writetable(eigv_table, eigv_file);
