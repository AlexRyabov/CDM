dm_metric = "spearman";
dm_k_min = 10;
dm_variables = 10;

script_dir = fileparts(mfilename("fullpath"));
input_file = fullfile(script_dir, "random_matrix_20x10_rowsum1.csv");
output_file = fullfile(script_dir, "random_matrix_dm_matlab.csv");

X = table2array(readtable(input_file));

switch dm_metric
    case "spearman"
        S = (corr(X', "Type", "Spearman", "Rows", "pairwise") + 1) / 2;
        S(isnan(S)) = 0;
    case "jaccard"
        X = X > 0;
        intersection = double(X) * double(X)';
        row_counts = sum(X, 2);
        union_counts = row_counts + row_counts' - intersection;
        S = intersection ./ union_counts;
        S(isnan(S)) = 0;
    otherwise
        error("Metric is not implemented in this test script: %s", dm_metric)
end

n = size(S, 1);
S(1:n+1:end) = 0;

k_min = min(dm_k_min, n - 1);
S_clean = S;
for j = 1:n
    positive_values = S(S(:, j) > 0, j);
    if ~isempty(positive_values)
        sorted_values = sort(positive_values, "descend");
        threshold = sorted_values(min(k_min, numel(sorted_values)));
        S_clean(S(:, j) < threshold, j) = 0;
    end
end

S = max(S_clean, S_clean');
degrees = sum(S, 2);
L = eye(n) - S ./ degrees;

[eig_vect, eig_val_matrix] = eig(L);
eig_val = diag(eig_val_matrix)';

real_idx = abs(imag(eig_val)) < 1e-8;
eig_val(real_idx) = real(eig_val(real_idx));
eig_vect(:, real_idx) = real(eig_vect(:, real_idx));

[eig_val, order] = sort(eig_val, "ascend");
eig_vect = eig_vect(:, order);

nonzero = abs(eig_val) >= 1e-8;
eig_val = eig_val(nonzero);
eig_vect = eig_vect(:, nonzero);

keep = 1:min(dm_variables, numel(eig_val));
DM = eig_vect(:, keep) ./ eig_val(keep);

DM_table = array2table(DM, "VariableNames", compose("DM%d", 1:size(DM, 2)));
writetable(DM_table, output_file);
