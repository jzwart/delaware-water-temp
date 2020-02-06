#
#
# ### this can read and write but takes a long time to write; and the writing does not reproduce ic file exactly as produced by PRMS
# ncol <- max(count.fields('4_model/tmp/prms_ic.txt'))
#
# data <- read.table('4_model/tmp/prms_ic.txt', fill=TRUE, header = F,
#                    col.names=c(1:ncol))
#
# write.table(data, '4_model/prms_ic.txt',
#             na = '',
#             quote = F,
#             row.names = F,
#             col.names = F)
#
#
#
# # trying vroom
#
# data = vroom::vroom(file = '4_model/tmp/prms_ic.txt', col_names = F, trim_ws = F, delim = ' +')
#
# data
#
# vroom::vroom_write(x = data, path = '4_model/test_ic.txt', )
#
#
# vroom::vroom()


# this seems to be best so far
fc = file('4_model/tmp/prms_ic.txt')
tmp = strsplit(readLines(fc, skipNul = T), ' +')
close(fc)

# should be 27 spaces after PRMS5 - update - I totally hacked the check in PRMS so this doesn't matter anymore since I'm using PRMS5 always
# tmp[[2]][13] = paste0(tmp[[2]][13], '                         ')
# paste(tmp[[2]], collapse = ' ')

# row 77 is stream temp
tmp[[77]][2] = '20.3232'

out = lapply(X = 1:length(tmp), FUN = function(x){paste(tmp[[x]], collapse = ' ')})

lapply(out, write, '4_model/prms_ic.txt', append = T)






#
#
#
# data1 = readLines('4_model/tmp/prms_ic.txt', n = 77)
#
# stream = data1[77]
#
#
# data2= readr::read_lines(file = '4_model/tmp/prms_ic.txt',
#                   skip = 75,
#                   n_max = 82,
#                   skip_empty_rows = F)
#
# data1 = data.table::fread(text = '4_model/tmp/prms_ic.txt', fill = T, skip = 75, nrows = 8, header = F)
#
#
# data1[2,1]
#
# strsplit(as.character(data1[2,1]), split = '\\s+')
#
#
#
#
#
# stream_temp_loc = grep('stream_temp', data[,1])
#
# stream_temp = data[(stream_temp_loc):(stream_temp_loc+8),]
#
# n_segs = 456
# stream_temp_sub = stream_temp[, 1:n_segs]
#
# stream_temp_sub[,(ncol(stream_temp_sub)-8):ncol(stream_temp_sub)]
#
# write.table(x = stream_temp_sub, '4_model/test_ic.txt',
#             na = '',
#             quote = F,
#             col.names = F,
#             row.names = F,
#             sep = '\t')
