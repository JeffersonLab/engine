      SUBROUTINE t_hms(ABORT,err)
*--------------------------------------------------------
* $Log$
* Revision 1.1  1997/05/23 20:51:35  saw
* Initial revision
*
*
      IMPLICIT NONE
      SAVE

       character*(*) here
       parameter (here= 't_hms')

       logical ABORT
       character*(*) err
      

       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'hms_data_structures.cmn'
       INCLUDE 't20_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 't20_hms.cmn'
	include 'hms_tracking.cmn'
	include 'gen_event_info.cmn'
	include 't20_misc.cmn'
      integer*4 ihit
       
*      GPBEAM : beam momentun (GEV/C)
*      HSENERGY : Lab total energy of chosen track in GeV
*      HSDELTA : Spectrometer delta of chosen track
*      HSYP_TAR : hstheta = htheta_lab*pi/180. - hsyp_tar
*      HSTHETA : Lab Scattering angle in radians
     
	if (hntracks_fp.gt.0) then	!need at least one HMS track
          tsinhtheta = sin(hstheta/2.)
          te_v = sqrt(Gpbeam*Gpbeam + mass_electron*mass_electron)
          thms_td1 = te_v - hsenergy
          thms_td2 = te_v*(1. - 1./(1. + 2.*te_v*tsinhtheta**2/tpartmass))
          tq2 = 2.*tpartmass*thms_td1
	else
          tsinhtheta = 0.
          te_v = 0.
          thms_td1 = 0.
          thms_td2 = 0.
          tq2 = 0.
	endif
	do ihit=1,HMISC_TOT_HITS
		if(HMISC_RAW_ADDR1(ihit).eq.1) then	! hight precision tdc
			if(HMISC_RAW_ADDR2(ihit).eq.10) hr_start_hms=HMISC_RAW_DATA(ihit)
		endif
	enddo
	

        return
        end
