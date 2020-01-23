# -*- coding: utf-8 -*-
"""
Created on Wed Jan 22 14:43:08 2020

@author: jzwart

# example of merging training scheme file with synthetic data file from 
#   SNTemp output 
"""

import feather

train_scheme_file = '../out/drb_subset_synthetic_train_10_1_inSitu.feather'
synthetic_data_file = '../out/sntemp_input_output_subset.feather'

training_scheme = feather.read_dataframe(train_scheme_file)

training_data = feather.read_dataframe(synthetic_data_file)

out = training_data.merge(training_scheme, 
                          left_on = ['seg_id_nat', 'date'], 
                          right_on = ['seg_id_nat','date'],
                          how ='left' )

