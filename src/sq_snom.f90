      subroutine sq_snom
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily snom melt when the average air
!!    temperature exceeds 0 degrees Celcius

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru         |none          |HRU number
!!    snocov1      |none          |1st shape parameter for snow cover equation
!!                                |This parameter is determined by solving the
!!                                |equation for 50% snow cover
!!    snocov2      |none          |2nd shape parameter for snow cover equation
!!                                |This parameter is determined by solving the
!!                                |equation for 95% snow cover
!!    snotmp       |deg C         |temperature of snow pack in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wst(:)%weat%ts(:)  |mm H2O        |precipitation for the time step during day
!!    snofall      |mm H2O        |amount of precipitation falling as freezing rain/snow on day
!!    snomlt       |mm H2O        |amount of water in snow melt for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Sin, Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      use hydrograph_module
      use hru_module, only : hru, ihru, precip_eff, snofall, snomlt, albday, gldb, hru_isgl, hru_index_map, glacc, glmlt, hru_gl_obj, gwe, albedo_dd
      use climate_module, only:  w
      use output_landscape_module 
      
      implicit none

      integer :: j          !none       |HRU number
      real :: smfac, gmfac  !           |
      real :: rto_sno  = 0. !none       |ratio of current day's snow water to minimum amount needed to
                            !           |cover ground completely 
      real :: snocov = 0.   !none       |fraction of HRU area covered with snow
      real :: snotmp = 0.   !deg C      |temperature of snow pack 
      ! Variables SWAT+GL
      real :: fac_mp = 0.   !-          |mixed precip intermediate calculation variable
      real :: icetmp = 0.   !deg C      |temperature of ice
      real :: vv1,vv2 = 0.
      real :: b = 0.
      real :: G = 0.
      real :: Tmm = 0.
      integer :: glid
      real :: albedo_snow = 0.  
      real :: Ipot = 0.
      j = ihru
        

        !! Estimate snow pack temperature
        snotmp = snotmp * (1. - hru(j)%sno%timp) + w%tave * hru(j)%sno%timp
        
        !! Calculate snow fall
        !! Standard Snowfall
        if (glcode(1)%pmix_flag>0 .AND. w%tave <= hru(j)%sno%falltmp) then             
            hru(j)%sno_mm = hru(j)%sno_mm + precip_eff
            snofall = precip_eff
            precip_eff = 0.
            albedo_dd(j) = 0 ! Degree Days Since SF set to 0 for Albedo Reset to 0.9
            !! set subdaily effective precip to zero
            if (time%step > 1) w%ts = 0.
        !! Mixed Precip
        elseif (glcode(1)%pmix_flag == 0) then            
            if (w%tave > hru(j)%sno%falltmp .AND. w%tave <= glpars(1)%tmix_ul) then ! In T range (>TSF & Tpbase)
                fac_mp = (w%tave - hru(j)%sno%falltmp)/ (glpars(1)%tmix_ul - hru(j)%sno%falltmp)
                snofall =  precip_eff/(1+exp(fac_mp)) ! Solid P amount from mixed P 
                hru(j)%sno_mm = hru(j)%sno_mm + snofall
                precip_eff = precip_eff - snofall
                if (time%step > 1) w%ts = precip_eff
                albedo_dd(j) = 0 ! Degree Days Since SF set to 0 for Albedo Reset to 0.9

            elseif (w%tave <= hru(j)%sno%falltmp) then ! Standard Precip if T > Tbase
                hru(j)%sno_mm = hru(j)%sno_mm + precip_eff
                snofall = precip_eff
                precip_eff = 0.
                !! set subdaily effective precip to zero
                if (time%step > 1) w%ts = 0.
                albedo_dd(j) = 0. ! Degree Days Since SF set to 0 for Albedo Reset to 0.9
            else
                snofall = 0
                if (w%tmax>0) albedo_dd(j) = albedo_dd(j) + w%tmax ! Degree Days Since SF 
            endif 
        endif
        
        !! Snow Redistribution
        ! TBD
        
        !! Snow Cover
        if (hru(j)%sno_mm < hru(j)%sno%covmx) then
            rto_sno = hru(j)%sno_mm / hru(j)%sno%covmx
            snocov = rto_sno / (rto_sno + Exp(hru(j)%snocov1 - hru(j)%snocov2 * rto_sno))
        elseif (hru(j)%sno_mm >= hru(j)%sno%covmx) then
            snocov = 1.
        endif
        
        !! Calculate Snowmelt
        !! Snowmelt Models: 1. Standard; 2. HTI; 3. ETI; 4. Exp; 5. Energy Balance
        if (w%tmax > hru(j)%sno%melttmp .and. hru(j)%sno_mm > 0.) then ! If Tmax > Melt Temp
            smfac = 0.
            !! Model 1: SWAT Standard
            if (glcode(1)%sm_model == 0) then 
                !! Adjust Melt Factor for time of year
                smfac = (hru(j)%sno%meltmx + hru(j)%sno%meltmn) / 2. + Sin((time%day - 81) / 58.09) * &
                        (hru(j)%sno%meltmx - hru(j)%sno%meltmn) / 2.        !! 365/2pi = 58.09
                !! Adjust for wet-day snowmelt (Rain on Snow)
                if (glcode(1)%ros_flag == 1 .and. precip_eff > glpars(1)%pthr) then
                    smfac = smfac + glpars(1)%pfac * (precip_eff - glpars(1)%pthr)
                end if
                snomlt = smfac * (((snotmp + w%tmax)/2.) - hru(j)%sno%melttmp)
            !!! Model 2: HTI    
            elseif (glcode(1)%sm_model == 1) then
                smfac = hru(j)%sno%tfac_s  !! Temp. Melt Factor [mm /d C°]
                if (smfac==0) then !! If SWAT standard degree day factor wants to be used
                    smfac = (hru(j)%sno%meltmx + hru(j)%sno%meltmn) / 2. + Sin((time%day - 81) / 58.09) * &
                          (hru(j)%sno%meltmx - hru(j)%sno%meltmn) / 2.        !! 365/2pi = 58.09
                endif
                !! Adjust for wet-day snowmelt (Rain on Snow)
                if (glcode(1)%ros_flag == 1 .and. precip_eff > glpars(1)%pthr) then
                    smfac = smfac + glpars(1)%pfac * (precip_eff - glpars(1)%pthr)
                end if
                Ipot = w%solradmx /(3600.*24./10**6) !! Potential Direct Solar Radiation [W/m²] -> Conversion from MJ/(m^2*d)
                snomlt = (smfac + hru(j)%sno%rfac_s * Ipot) * (((snotmp + w%tmax)/2.) - hru(j)%sno%melttmp)
            !! Model 2: ETI      
            elseif (glcode(1)%sm_model == 2) then
                smfac = hru(j)%sno%tfac_s !! Temp. Melt Factor [mm /d C°]
                if (smfac==0) then !! If SWAT standard degree day factor wants be used
                    smfac = (hru(j)%sno%meltmx + hru(j)%sno%meltmn) / 2. + Sin((time%day - 81) / 58.09) * &
                          (hru(j)%sno%meltmx - hru(j)%sno%meltmn) / 2.        !! 365/2pi = 58.09
                endif
                !! Adjust for wet-day snowmelt (Rain on Snow)
                if (glcode(1)%ros_flag == 1 .and. precip_eff > glpars(1)%pthr) then
                    smfac = smfac + glpars(1)%pfac * (precip_eff - glpars(1)%pthr)
                end if
                G = w%solrad /(3600.*24./10**6) !! HRU SWR [W/m²] -> Conversion from MJ/m^2
                snomlt = (smfac * (((snotmp + w%tmax) / 2.) - hru(j)%sno%melttmp)) + &
                           (hru(j)%sno%srfac_s * (1 - albday) * G)
            !! Model 3: Exponential Temperature Index
            elseif (glcode(1)%sm_model == 3) then 
                smfac = (hru(j)%sno%meltmx + hru(j)%sno%meltmn) / 2. + Sin((time%day - 81) / 58.09) * &
                       (hru(j)%sno%meltmx - hru(j)%sno%meltmn) / 2.        !! 365/2pi = 58.09
                Tmm = (w%tmax - hru(j)%sno%melttmp)/hru(j)%sno%f_exp    
                !! Adjust for wet-day snowmelt (Rain on Snow)
                if (glcode(1)%ros_flag == 1 .and. precip_eff > glpars(1)%pthr) then
                    smfac = smfac + glpars(1)%pfac * (precip_eff - glpars(1)%pthr)
                endif
                snomlt = smfac * hru(j)%sno%f_exp * (Tmm + log(1 + exp(-Tmm)))     
            
            end if
            !! Adjust for areal extent of snow cover
            snomlt = snomlt * snocov

        else ! If Tmax < Tmelt
            snomlt = 0.
        end if  
            
            
        !! Check Whether it is a glacier HRU 
        if (hru_isgl(j)==1) then
            glid = hru_index_map(j)
            
            !! Add a Glacier Melt Lag as Well
            icetmp = icetmp * (1. - gldb(glid)%gl_lag) + w%tave * gldb(glid)%gl_lag
            
            ! Accumulation (Snow Metamorphosis)
            if (glcode(1)%acc_mod == 1 .and. hru(j)%sno_mm > 0) then
            ! f_accu based on simple constant transformation value (between 0.001 and 0.003 which corresponds to 0.1% - 0.3%)
                if (gldb(glid)%f_accu>0.005) gldb(glid)%f_accu=0.005
                glacc = hru(j)%sno_mm * gldb(glid)%f_accu !! Timo Glacier Accumulation of HRU
                glacc = glacc * snocov !! Analogously to glmlt -> adapt to snow covered area
      
            !! Luo et al. 2015 Accumulation
            elseif (glcode(1)%acc_mod == 0 .and. hru(j)%sno_mm > 0) then
            !! f_accu = 0.003 [0.001 and 0.003]          
                b = gldb(glid)%f_accu * (1 + Sin((time%day - 81)/58.09))
                glacc = hru(j)%sno_mm  * b !! Timo Glacier Accumulation of HRU
                glacc = glacc * snocov !! Analogously to glmlt -> adapt to snow covered area
            else
                glacc = 0.
            endif    
            
            ! Glacier Melt
            if (w%tmax > gldb(glid)%glmtmp .and. snocov < 1) then ! If Tmx > GLMLTMP & HRU = Glacier HRU & snocov < 1
                gmfac = 0.
                gmfac = (gldb(glid)%glmfmx + gldb(glid)%glmfmn) / 2. + Sin((time%day - 81) / 58.09) * &
                        (gldb(glid)%glmfmx - gldb(glid)%glmfmx) / 2.    !! 365/2pi = 58.09
                !! Correct Gl Melt Factor if < Snow Melt Factor
                if (smfac > gmfac .and. glcode(1)%fgmlt_cor == 1) then
                    gmfac=smfac
                    !smfac=gmfac
                end if
                !! Glacier Melt Computation
                glmlt = gmfac*(((icetmp + w%tmax)/2.) - gldb(glid)%glmtmp)! With TIMP effects
                glmlt = (1 - gldb(glid)%f_frz) * glmlt ! Refreezing -> Reduction of glmlt                                     
              
                !! Check Snow Cover VS Glacier Cover
                if (glcode(1)%gm_init == 1) then 
                    glmlt = glmlt * (1-snocov)
                else
                    glmlt = glmlt * (1-snocov)
                endif

            else
                glmlt = 0.
            endif
        end if
            
                                         
        ! Check for non-zero
        if (snomlt < 0.) snomlt = 0.
        if (glmlt < 0.) glmlt = 0.
        if (glacc < 0.) glacc = 0.
          
        !! Correct smlt & glacc 
        if (snomlt>0 .and. glacc>0) then ! Correct if both occur in parallel
            vv1 = glacc/(glacc+snomlt)
            vv2 = 1 - vv1
            glacc = glacc*vv1
            snomlt = snomlt*vv2
            if (snomlt + glacc > hru(j)%sno_mm .and. hru(j)%sno_mm>0) then
                snomlt = hru(j)%sno_mm *vv2
                glacc = hru(j)%sno_mm *vv1                    
            end if
        else  !! Correct smlt OR glacc if only occurs and the other is 0
            if (snomlt > hru(j)%sno_mm .and. glacc == 0) snomlt = hru(j)%sno_mm
            if (glacc > hru(j)%sno_mm .and. snomlt == 0) glacc = hru(j)%sno_mm
        end if
        ! SWE Balance
        hru(j)%sno_mm = hru(j)%sno_mm - glacc - snomlt
        if (hru(j)%sno_mm<0) hru(j)%sno_mm=0
        
        ! GWE Balance / Mass Balance Calculation
        if (hru_isgl(j)==1) then
            if (glmlt > hru_gl_obj(glid)%hru_glww2) glmlt = hru_gl_obj(glid)%hru_glww2
            hru_gl_obj(glid)%hru_glww2 = hru_gl_obj(glid)%hru_glww2 - glmlt + glacc ! (GLWW2 = Uniform GWE across HRus of 1 ES)
            gwe = hru_gl_obj(glid)%hru_glww2/1000
        end if
        ! Calculate Peff
        precip_eff = precip_eff + snomlt + glmlt
        if (time%step > 1) then
        w%ts(:) = w%ts(:) + snomlt / time%step
        end if
        if (precip_eff < 0.) precip_eff = 0.
    !else ! If Tmax < Tmelt
    !  snomlt = 0.
    !end if
 
      return
      end subroutine sq_snom