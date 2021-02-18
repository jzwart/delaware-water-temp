





tmp_model_idx = '1' # 415 and 416 are headwater segments


cur = dplyr::filter(debug_heat_flux_out, model_idx == tmp_model_idx)

plot(cur$H_n,type ='l')
plot(cur$H_a,type ='l') # looks good
plot(cur$H_v, type = 'l') # looks good, always 0
plot(cur$H_w,type = 'l') # too high in some cases (~ 5000 W/m2 )
plot(cur$H_s, type = 'l') # looks good
plot(cur$H_e, type = 'l') # looks good
plot(cur$H_c, type = 'l') # too high in some cases (~ 2000 W/m2)
plot(cur$H_d, type = 'l') # too low in some cases (~ -400 W/m2)
plot(cur$H_f, type = 'l') # looks good
plot(cur$H_u, type = 'l')
plot(cur$dT, type = 'l')

plot(cur$seg_tave_water, type ='l')
# par(new= T)
# plot(cur$H_d, xlim = c(0,40), type = 'l')


plot(cur$seg_tave_water, type ='l', ylim = c(0,30))
for(i in unique(debug_heat_flux_out$model_idx)){
  cur = dplyr::filter(debug_heat_flux_out, model_idx == i)

  lines(cur$seg_tave_water, type ='l')

}

plot(cur$H_n, type ='l', ylim = range(debug_heat_flux_out$H_n))
for(i in unique(debug_heat_flux_out$model_idx)){
  cur = dplyr::filter(debug_heat_flux_out, model_idx == i)

  lines(cur$H_n, type ='l', col = i)

}

cur$temp_diff = c(NA, diff(cur$seg_tave_water))
plot(cur$H_n ~ cur$temp_diff) #, xlim = c(-1,1))
plot(cur$H_n ~ cur$seg_tave_water)
plot(cur$H_n,type ='l')
par(new=T)
plot(cur$seg_tave_water, col = 'red', type = 'l', axes = 'yaxt')
axis(4, col.axis = 'red')
