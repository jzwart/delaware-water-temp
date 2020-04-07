


# this seems to be best so far
fc = file('4_model/tmp/prms_ic.txt')
tmp = strsplit(readLines(fc, skipNul = T), ' +')
close(fc)

# should be 27 spaces after PRMS5 - update - I totally hacked the check in PRMS so this doesn't matter anymore since I'm using PRMS5 always
# tmp[[2]][13] = paste0(tmp[[2]][13], '                         ')
# paste(tmp[[2]], collapse = ' ')

# row 77 is stream temp
tmp[[77]][2:200] = '20.3232'

tmp[[80]][2:200] = '100.1'

out = lapply(X = 1:length(tmp), FUN = function(x){paste(tmp[[x]], collapse = ' ')})

lapply(out, write, '4_model/prms_ic.txt', append = T)


