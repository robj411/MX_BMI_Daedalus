function data = add_params_to_data(data, poptim)

    param_names = fieldnames(poptim);
    for i = 1:numel(param_names)
        this_name = param_names{i};
        data.(this_name) = poptim.(this_name);
    end
    data.hospA2 = data.hospA2 * poptim.consumer_scalar(1);
    data.hospA3 = data.hospA3 * poptim.consumer_scalar(2);
    data.hospA4 = data.hospA4 * poptim.consumer_scalar(3);
    data.schoolA1 = data.schoolA1 * poptim.school_scalar(1);
    data.schoolA2 = data.schoolA2 * poptim.school_scalar(2);
    data.travelA3 = data.travelA3;
    data.Cmat(data.edSector,:) = data.Cmat(data.edSector,:) * poptim.school_scalar(3);
    data.CM = scale_community_matrix(data.CM, poptim.community_scalar);


end