% linear_regress.m
function w_learned = linear_regress(X,y)
% linear regression model 

% Plot the original data
epsilon = 0.0001;
max_iters = 5000;

% Use gradient descent to to learn a set of parameters w_learned
% initialize w_learned randomly
w_learned = randn(2,1);
% iterate for max_iters # of iterations (could use other convergence
% criteria)
for iteration = 1:max_iters
    grad = 2*sum(repmat(w_learned'*X-y,size(X,1),1).*X,2);
    w_learned=w_learned-0.0001*grad;
    err=sum((y-w_learned'*X).^2);
end