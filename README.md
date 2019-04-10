# TainanHousePricePridiction
predict tainan,Taiwan house price using keras NN in R

Important: If file encoding error when you open it,change file encoding to UTF-8 (file default Encoding is CP950).

house_prediction_kernel/kernel_v1_done.R : preprocessing training data.

house_prediction_kernel/prediction_model.R : prediction with keras NN model.

house_prediction_kernel/house_main.R : Open API.

house_prediction_kernel/house_basic.R : API functional.

list.rda : Number and category reference table for each column.

normalization_25.rda : saved max & min value with each column.

pred_model_25.h5 : sample model.

pre_model_weights_25.h5 : sample model weights.

Pricedetail.rda : house price & Price per ping(i.e per 3.3 square meters)

How to use this project: 
1. use "kernel_v1_done.R" to preprocessing training data and save it.
2. open "prediction_model.R" training NN model and save model.
3. write API functional in "house_basic.R".
4. run "house_main.R" to build API in website.
