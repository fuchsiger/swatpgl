      subroutine sp_smlt_gmlt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily snom melt and glacier melt

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
      use hru_module, only : hru, ihru, precip_eff, snofall, snomlt 
      use climate_module, only:  w
      use output_landscape_module 
      
      implicit none

      integer :: j          !none       |HRU number
      real :: smfac         !           |
      real :: rto_sno  = 0. !none       |ratio of current day's snow water to minimum amount needed to
                            !           |cover ground completely 
      real :: snocov = 0.   !none       |fraction of HRU area covered with snow
      real :: snotmp = 0.   !deg C      |temperature of snow pack

      j = ihru

        !! Estimate snow pack temperature
        snotmp = snotmp * (1. - hru(j)%sno%timp) + w%tave * hru(j)%sno%timp
        
        !! Calculate snow fall
        if (w%tave <= hru(j)%sno%falltmp) then
          hru(j)%sno_mm = hru(j)%sno_mm + precip_eff
          snofall = precip_eff
          precip_eff = 0.
          !! set subdaily effective precip to zero
          if (time%step > 1) w%ts = 0.
        endif
        
        !! Calculate Snowmelt
        if (w%tmax > hru(j)%sno%melttmp .and. hru(j)%sno_mm > 0.) then ! If Tmax > Melt Temp
          !! Adjust Melt Factor for time of year
          smfac = (hru(j)%sno%meltmx + hru(j)%sno%meltmn) / 2. + Sin((time%day - 81) / 58.09) *     &
                        (hru(j)%sno%meltmx - hru(j)%sno%meltmn) / 2.        !! 365/2pi = 58.09
          snomlt = smfac * (((snotmp + w%tmax)/2.) - hru(j)%sno%melttmp)

          !! Adjust for areal extent of snow cover
          if (hru(j)%sno_mm < hru(j)%sno%covmx) then
            rto_sno = hru(j)%sno_mm / hru(j)%sno%covmx
            snocov = rto_sno / (rto_sno + Exp(hru(j)%snocov1 - hru(j)%snocov2 * rto_sno))
          else
            snocov = 1.
          endif
          snomlt = snomlt * snocov
          if (snomlt < 0.) snomlt = 0.
          if (snomlt > hru(j)%sno_mm) snomlt = hru(j)%sno_mm
          hru(j)%sno_mm = hru(j)%sno_mm - snomlt
          precip_eff = precip_eff + snomlt
          if (time%step > 1) then
            w%ts(:) = w%ts(:) + snomlt / time%step
          end if
          if (precip_eff < 0.) precip_eff = 0.
        else ! If Tmax < Tmelt
          snomlt = 0.
        end if
 
      return
      end subroutine sp_smlt_gmlt