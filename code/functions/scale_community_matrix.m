function CM = scale_community_matrix(CM0, scalar0)

%     CM = CM0; %source: polymod-uk-all

%     a = triu(ones(size(CM0)));
%     a(a > 0) = scalar0;
%     scalar = (a + a')./(eye(size(CM0))+1);
    
    scalar = reshape(scalar0,size(CM0));
    
    CM = CM0 .* scalar;
    
end