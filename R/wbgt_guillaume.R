
"
Various functions to calculate WBGT
Translated to R from Bruno Lemke's wbgt_calcs.xls vba by Joseph Guillaume, January 2008
Needed to be made vector-input safe, i.e. all operations in functions must be element-wise
 or function must deal with vector input as scalars separately

Ta - air temperature degC
dewpoint degC
windspeed m/s
solarrad - solar radiation W/m^2
pressure mB

!     This product includes software produced by UChicago Argonne, LLC 
!     under Contract No. DE-AC02-06CH11357 with the Department of Energy.


!                Copyright © 2008, UChicago Argonne, LLC
!                        All Rights Reserved
!
!                         WBGT, Version 1.0
!
!                        James C. Liljegren
!               Decision & Information Sciences Division
!
!                        OPEN SOURCE LICENSE
!
!  Redistribution and use in source and binary forms, with or without modification, 
!  are permitted provided that the following conditions are met:
!
!  1. Redistributions of source code must retain the above copyright notice, 
!     this list of conditions and the following disclaimer.  Software changes, 
!     modifications, or derivative works, should be noted with comments and 
!     the author and organization’s name.
!
!  2. Redistributions in binary form must reproduce the above copyright notice, 
!     this list of conditions and the following disclaimer in the documentation 
!     and/or other materials provided with the distribution.
!
!  3. Neither the names of UChicago Argonne, LLC or the Department of Energy 
!     nor the names of its contributors may be used to endorse or promote products 
!     derived from this software without specific prior written permission.
!
!  4. The software and the end-user documentation included with the 
!     redistribution, if any, must include the following acknowledgment:
!
!     This product includes software produced by UChicago Argonne, LLC 
!     under Contract No. DE-AC02-06CH11357 with the Department of Energy.”
!
!******************************************************************************************
!  DISCLAIMER
!
!  THE SOFTWARE IS SUPPLIED AS IS WITHOUT WARRANTY OF ANY KIND.
!
!  NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES DEPARTMENT OF ENERGY, 
!  NOR UCHICAGO ARGONNE, LLC, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS 
!  OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
!  COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, DATA, APPARATUS, PRODUCT, OR 
!  PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
!
!******************************************************************************************

        program wbgt
!
!  Purpose: to demonstrate the use of the subroutine calc_wbgt to calculate
!           the wet bulb-globe temperature (WBGT).  The program reads input 
!           data from a file containing meteorological measurements then 
!           calls calc_wbgt to compute the WBGT.
!
!           The inputs and outputs are fully described in calc_wbgt.
!
!  Author:  James C. Liljegren
!               Decision and Information Sciences Division
!               Argonne National Laboratory
!               
"

esat<-function(tk){
#  Purpose: calculate the saturation vapor pressure (mb) over liquid water given the temperature (K).
#
#  Reference: Buck's (1981) approximation (eqn 3) of Wexler's (1976) formulae.
#  over liquid water

        y = (tk - 273.15) / (tk - 32.18)
        es = 6.1121 * exp(17.502 * y)
        es = 1.004 * es  # correction for moist air, if pressure is not available; for pressure > 800 mb

        esat = es
        return(esat)
}

emis_atm<-function(t, rh){
#
#  Reference: Oke (2nd edition), page 373.
#
        e = rh * esat(t)
        emis_atm = 0.575 * e ^ 0.143
        return(emis_atm)
}

thermal_cond<-function(Tair){
#
#  Purpose: Compute the thermal conductivity of air, W/(m K) given temperature, K
#
#  Reference: BSL, page 257.
        Cp = 1003.5
        Rair = 8314.34 / 28.97

        thermal_cond = (Cp + 1.25 * Rair) * viscosity(Tair)
        return(thermal_cond)
}

viscosity<-function(Tair){
#
#  Purpose: Compute the viscosity of air, kg/(m s) given temperature, K
#
#  Reference: BSL, page 23.
#
        sigma = 3.617
        sigma2 = sigma ^ 2
        epsKappa = 97
        Mair = 28.97
        Tr = Tair / epsKappa
        omega = (Tr - 2.9) / 0.4 * (-0.034) + 1.048
        viscosity = 0.0000026693 * (Mair * Tair) ^ 0.5 / (sigma2 * omega)
        return(viscosity)
}


h_sphere_in_air<-function(Tair, Pair, speed, speedMin){
#
#  Purpose: to calculate the convective heat tranfer coefficient for flow around a sphere.
#
#  Reference: Bird, Stewart, and Lightfoot (BSL), page 409.
        Rair = 8314.34 / 28.97
        Pr = 1003.5 / (1003.5 + 1.25 * Rair)
        diameter = 0.15

        density = Pair * 100 / (Rair * Tair)   # kg/m3
      if(speed < speedMin) speed = speedMin
      Re = speed * density * diameter / viscosity(Tair)
        Nu = 2 + 0.6 * Re ^ 0.5 * Pr ^ 0.3333
        h_sphere_in_air = Nu * thermal_cond(Tair) / diameter # W/(m2 K)
        return(h_sphere_in_air)
}

#Modified to accept vector input & NA values. Joseph Guillaume 20080130
fTg<-function(Ta, relh, Pair, speed, solar, fdir, speedMin){
        if (length(Ta)>1) {
                if (length(fdir)==1) fdir<-rep(fdir,length(Ta))
                if (length(speedMin)==1) speedMin<-rep(speedMin,length(Ta))

                res<-rep(NA,length(Ta))
                for (i in 1:length(Ta)) res[i]<-fTg(Ta[i], relh[i], Pair[i], speed[i], solar[i], fdir[i], speedMin[i])
                return(res)
        } else {
        if (any(is.na(c(Ta,relh,Pair,speed,solar,fdir,speedMin)))) return(NA)

#
#  Purpose: to calculate the globe Ta
#  Author:  James C. Liljegren
#       Decision and Information Sciences Division
#       Argonne National Laboratory
#
# Pressure in millibar (Atm =1010 mB)
#    Direct radiation so cosZ=1
        cza = 1
        converge = 0.02
        alb_sfc = 0.45
        alb_globe = 0.05
        stefanb = 0.000000056696
        emis_globe = 0.95
        emis_sfc = 0.999
        Tair = Ta + 273.15
        rh = relh * 0.01
        Tsfc = Tair
        Tglobe_prev = Tair
        while (TRUE){
                Tref = 0.5 * (Tglobe_prev + Tair) # Evaluate properties at the average temperature
                h = h_sphere_in_air(Tref, Pair, speed, speedMin)
                Tglobe = (0.5 * (emis_atm(Tair, rh) * Tair ^ 4 + emis_sfc * Tsfc ^ 4) - h / (emis_globe * stefanb) * (Tglobe_prev - Tair) + solar / (2 * emis_globe * stefanb) * (1 - alb_globe) * (fdir * (1 / (2 * cza) - 1) + 1 + alb_sfc)) ^ 0.25
                dT = Tglobe - Tglobe_prev
                if (abs(dT) < converge) {
                        Tglobe = Tglobe - 273.15
                        break
                } else {
                        Tglobe_prev = (0.9 * Tglobe_prev + 0.1 * Tglobe)
                }
        }
    return(Tglobe)
}}

#Modified to accept vector input. Joseph Guillaume. 20090130
fTw<-function(Ta,Td){
        if (length(Ta)!=length(Td)) stop("Need same number of Ta and Td measurements")
        if (length(Ta)>1) {
                res<-rep(NA,length(Ta))
                for (i in 1:length(Ta)) res[i]<-fTw(Ta[i],Td[i])
                return(res)
        } else {
        if (Ta==0 || Td==0 || is.na(Ta) || is.na(Td)) return(NA)

        Tw = Td
        Diff = 10000
        Ed = 0.6106 * exp(17.27 * Td / (237.3 + Td))

        Diffold = Diff
        while (abs(Diff) + abs(Diffold) == abs(Diff + Diffold)){
                Diffold = Diff
                Ew = 0.6106 * exp(17.27 * Tw / (237.3 + Tw))
                Diff = 1556 * Ed + 101 * Ta - 1556 * Ew + 1.484 * Ew * Tw - 1.484 * Ed * Tw - 101 * Tw
                Tw = Tw + 0.2
                if (Tw > Ta) break
        }
        if (Tw > Td + 0.3) fTw = Tw - 0.3
        else fTw = Td
        return(fTw)
        }
}

calc_relhum<-function(Ta,dewpoint) return(100*exp(17.27*dewpoint/(237.7+dewpoint)-17.27*Ta/(237.7+Ta)))

#Either Tw or dewpoint must be provided
#If Tw is not provided, it is calculated using dewpoint
calc_Tnwb_bernard<-function(Tg,Ta,windspeed,Tw=NA,dewpoint=NA){
        if (length(Ta)>1) {
                res<-rep(NA,length(Ta))
                for (i in 1:length(Ta)) res[i]<-calc_Tnwb_bernard(Tg[i],Ta[i],windspeed[i],Tw[i],dewpoint[i])
                return(res)
        } else {
        if (any(is.na(c(Ta,Tg,windspeed)))) return(NA)

        if (is.na(Tw) && !is.na(dewpoint)) Tw=fTw(Ta,dewpoint)
        if(Tg-Ta>4) return(X(Ta,windspeed,Tw,Tg))
        else {
                if(windspeed>3) return(Tw)
                else return(Ta-(0.069*log10(windspeed+0.1)+0.96)*(Ta-Tw))
        }
}}

calc_wbgt_bom<-function(Ta,relhum=NA,e=NA){
        if (is.na(e) && !is.na(relhum)) e<-relhum/100*6.105*exp(17.27*Ta/(237.7+Ta))
        return(0.567 *Ta+0.393*e+3.94)
}

calc_wbgt_indoors_tw<-function(Tw,Ta) return(0.7*Tw+0.3*Ta+0.55) #using Tw
calc_wbgt_outdoors_tonouchi<-function(Tw,Ta,solarrad,windspeed) return(0.7*Tw + 0.3*Ta + 0.0117*solarrad - 0.205*windspeed + 0.751)

#Either Tnwb or windspeed must be provided
#if Tnwb is not provided, it is calculated using the Bernard formula
calc_wbgt_outdoors_liljegren_bernard<-function(Ta,Tg,Tnwb=NA,windspeed=NA) {
        if (is.na(Tnwb) && !is.na(windspeed)) Tnwb<-calc_Tnwb_bernard(Tg,Ta,windspeed)
        return(0.7*Tnwb+0.1*Ta+0.2*Tg)
}

X<-function(Ta,windspeed,Tw,Tg){
        if (length(Ta)>1) {
                res<-rep(NA,length(Ta))
                for (i in 1:length(Ta)) res[i]<-X(Ta[i],windspeed[i],Tw[i],Tg[i])
                return(res)
        } else {

        if(Tg-Ta>4 && windspeed>1) {
                return(Tw+0.25*(Tg-Ta)-0.1)
        } else if(Tg-Ta>4) {
                return(Tw+0.25*(Tg-Ta)+0.1/(windspeed+0.1)^1.1-0.2)
        } else { return(NA) }
}}

get_wbgt_values<-function(Ta,dewpoint,windspeed=1,solarrad=980,pressure=1001,full_excel_line=FALSE){
        Tw<-fTw(Ta,dewpoint)

        #Needs Ta,dewpoint
        wbgt_indoors_tw<-calc_wbgt_indoors_tw(Tw,Ta)

        #Needs Ta,dewpoint,solarrad,windspeed
        wbgt_outdoors_tonouchi<-calc_wbgt_outdoors_tonouchi(Tw,Ta,solarrad,windspeed)

        relhum<-calc_relhum(Ta,dewpoint)
        Tg<-fTg(Ta,relhum, pressure, windspeed, solarrad, 0.6,1)

        Tnwb<-calc_Tnwb_bernard(Tg,Ta,windspeed,Tw=Tw)
        #Needs Ta,relhum,pressure,windspeed,solarrad,dewpoint
        wbgt_outdoors_liljegren_bernard<-calc_wbgt_outdoors_liljegren_bernard(Ta,Tg,Tnwb)

        #Needs Ta, windspeed, Tw, Tg
        x=X(Ta,windspeed,Tw,Tg)

        if (full_excel_line){
        return(data.frame(
                Ta=Ta,
                dewpoint=dewpoint,
                windspeed=windspeed,
                solarrad=solarrad,
                pressure=pressure,
                relhum=relhum,
                Tw=Tw,
                Tnwb=Tnwb,
                Tg=Tg,
                wbgt_indoors_tw=wbgt_indoors_tw,
                wbgt_outdoors_tonouchi=wbgt_outdoors_tonouchi,
                wbgt_outdoors_liljegren_bernard=wbgt_outdoors_liljegren_bernard,
                X=x
        )) } else {
        return(data.frame(
                wbgt_indoors_tw=wbgt_indoors_tw,
                wbgt_outdoors_tonouchi=wbgt_outdoors_tonouchi,
                wbgt_outdoors_liljegren_bernard=wbgt_outdoors_liljegren_bernard
        )) }
}

#TESTING AGAINST ORIGINAL SPREADSHEET
if (FALSE){

#ans is columns E:Q from original spreadsheet (E:I are inputs,J:Q are calculated)
err_line<-function (ans){
        return(get_wbgt_values(ans[1],ans[2],ans[3],ans[4],ans[5],full_excel_line=TRUE)-ans)
}
e<-err_line(c(36,11,1,980,1001,22.14217223,20.1,27.49324803,66.01280514,25.42,36.882,36.04783465,27.49324803))
print(all(na.omit(e<1e-6)))
e<-err_line(c(25,20,30,300,1001,73.84576627,21.5,21.5,27.62714571,23.1,20.661,23.07542914,NA))
print(all(na.omit(e<1e-6)))
e<-err_line(c(26,15,15,1100,980,50.78141797,18.9,22.3487882,40.19515282,21.58,31.576,26.28318231,22.3487882))
print(all(na.omit(e<1e-6)))
e<-err_line(c(16,19,10,930,1200,120.8164345,19,22.24305356,29.37221424,18.65,27.682,23.04458034,22.24305356))
print(all(na.omit(e<1e-6)))

#Test for same input in vector form
#ans is data frame
err_matrix<-function(ans){
        return(get_wbgt_values(ans[,1],ans[,2],ans[,3],ans[,4],ans[,5],full_excel_line=TRUE)-ans)
}
ans<-data.frame()
ans<-rbind(ans,c(36,11,1,980,1001,22.14217223,20.1,27.49324803,66.01280514,25.42,36.882,36.04783465,27.49324803))
ans<-rbind(ans,c(25,20,30,300,1001,73.84576627,21.5,21.5,27.62714571,23.1,20.661,23.07542914,NA))
ans<-rbind(ans,c(26,15,15,1100,980,50.78141797,18.9,22.3487882,40.19515282,21.58,31.576,26.28318231,22.3487882))
ans<-rbind(ans,c(16,19,10,930,1200,120.8164345,19,22.24305356,29.37221424,18.65,27.682,23.04458034,22.24305356))

e<-err_matrix(ans)
print(all(na.omit(e<1e-6)))
}
