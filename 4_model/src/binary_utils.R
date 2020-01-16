

con = file('20191002_Delaware_streamtemp/prms_ic.out', open = 'rb')
binData = readBin(con = con, what = double(), n = 400000, endian = 'little')
close(con)

con = file('20191002_Delaware_streamtemp/prms_ic.out', open = 'rb')
binData_int = readBin(con = con, what = integer(), n = 400000, endian = 'little')
close(con)

head(binData)
tail(binData)
binData[1:100]

binData_filt = binData[binData>-5&binData<55]

hist(binData_filt)
hist(log10(binData_filt))

summary(binData)
diff(binData[56:600])

sum(grepl(binData[56], binData))
sum(grepl(binData[1], binData))

binData[1:50]
binData_int[1:50]


hist(binData_int)

192560 - sum(duplicated(binData))




con = file('20191002_Delaware_streamtemp/prms_ic.out', open = 'rb')

char = readBin(con = con, what = character(), n = 100000, endian = 'little')

char
any(char!='')

close(con)
