

# driver data:
t_air = 20 # degrees C; temperature of air
RH = 0.23 # decimal fraction; relative humidity

# sources: Caissie et al. 2007; Cole et al. 2014; energy budget doc from data_for_Xiaowei/Energy budget.docx


kelvin_const = 273.16 # 0 deg C Kelvin
r_l = 0 # longwave reflection fraction
sh = 0 # shade fraction
C_l = 0 # decimal cloud fraction
e_s = 6.11 * exp((17.27 * t_air) / (kelvin_const + t_air))
e_a = RH * e_s # vapor pressure of air
emis_air = 0.61 + 0.05*sqrt(e_a) # emissivitiy of air
sigma = 5.6697E-8 # Stefan-Boltzmann constant

H_a = (1 - r_l) * (1 - sh) * (1 + 0.17*C_l^2) * emis_air * sigma * (t_air + kelvin_const)^4









H_tot = H_s + H_l + H_e + H_c






