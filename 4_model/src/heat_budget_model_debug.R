





tmp_model_idx = '415'


cur = dplyr::filter(debug_heat_flux_out, model_idx == tmp_model_idx)

plot(cur$H_n)
plot(cur$H_a) # looks good
plot(cur$H_v) # looks good, always 0
plot(cur$H_w,type = 'l') # too high in some cases (~ 5000 W/m2 )
plot(cur$H_s) # looks good
plot(cur$H_e) # looks good
plot(cur$H_c, type = 'l') # too high in some cases (~ 2000 W/m2)
plot(cur$H_d, type = 'l') # too low in some cases (~ -400 W/m2)
plot(cur$H_f) # looks good

plot(cur$seg_tave_water, type ='l')
par(new= T)
plot(cur$H_d, xlim = c(0,40), type = 'l')
