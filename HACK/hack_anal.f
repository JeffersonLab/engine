      subroutine hack_anal(ABORT, err)
*
*-----------------------------------------------------------------------------
*-- file: hack_anal.f
*-- USER DEVELOPMENT routine; called for each event; 
*>>-  user can communicate with setup files via the arrays
*>>    HACK_INT(MAX_USER_PAR) (integer) and HACK_REAL(MAX_USER_PAR) (real)
*>>    defined in a common block in file HACK_.CMN; max_user_par=1024.
*- If additional variables are needed to be accessible from setup files
*    (e.g. histogram definitions, calibration parameters), these should be
*    defined in HACK_.CMN and 
*    registered in file HACK_REGISTER_VARIABLES.F.
*- Before event processing begins, HACK_REGISTER_VARIABLES and HACK_INITIALIZE
*    are called. They are available to be modified by the user.
*-  After all events have been processed, hack_shutdown in file
*    HACK_SHUTDOWN.F is called to allow final manipulations, e.g.
*    printed output.
* $Log: hack_anal.f,v $
* Revision 1.4  1995/10/11 14:02:00  cdaq
* (JRA) Cleanup
*
* Revision 1.3  1995/07/28 14:21:57  cdaq
* (SAW) Use specific bit manipulation routines for f2c compatibility
*
* Revision 1.2  1994/07/26  21:03:02  cdaq
* (SAW) Remove event argument.
*
* Revision 1.1  94/07/25  18:03:25  18:03:25  cdaq (Data Acquisition Account)
* Initial revision
* 
*-----------------------------------------------------------------------------
*---   for information, the following lines are copied from file hack_.cmn:
*-- file: hack_.cmn
*-- include file for USER DEVOLOPMENT common block definitions;
*-- The parameter hack_enable must be set to .ne. 0 to enable execution of
*     hack_anal subroutine for each event.
*-- any additional arrays or variables my be added by the user
*      integer*4 hack_enable
*      common /hack_c/ hack_enable
*      integer hack_hmssc_au(16,4) !raw HMS-scintillator ADC up in fixed array
*      integer hack_hmssc_ad(16,4) !raw HMS-scintillator ADC down in fixed array
*      integer hack_hmssc_tu(16,4) !raw HMS-scintillator TDC up in fixed array
*      integer hack_hmssc_td(16,4) !raw HMS-scintillator TDC down in fixed array
*      integer hack_hmssc_go(16,4) !info about which ADC/TDC fired
*      common/hack_copyeve_c/ hack_hmssc_au,hack_hmssc_ad,
*     & hack_hmssc_tu,hack_hmssc_td,hack_hmssc_go
*-----------------------------------------------------------------------------
*
      implicit none
      logical ABORT
      character*(*) err
*
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
      include 'hack_.cmn'
      integer*4 jiand                   ! To help f2c
*
      ABORT = .FALSE.                !needed as default
      err = ' '                      !needed as default

c      if(jiand(event(2),'FFFF'X).ne.'10CC'X) return ! valid physics event?

*-----------------------------------------------------------------
*-- copy HMS scintillator data for one event into arrays hack_hmssc*(j,k)
*   *=au,ad,tu,td,go [adcup/do,tdcup/do,good data index (-1 to +2) ]
*   j=1-16 [scintillator number]
*   k=1-4  [plane number]
**      call hack_copyevt(ABORT,err)
*-----------------------------------------------------------------
*-- >>>>>>>insert user code here<<<<<<<<
*EXAMP meantime = hack_hmssc_tu(s_nr,p_nr)+hack_hmssc_td(s_nr,p_nr))
*EXAMP offset = 200
*EXAMP index = (p_nr-1)*16 + s_nr + offset               !output array index
*EXAMP *-- assuming hack_real contains user supplied calibration values
*EXAMP hack_int(index) = nint(meantime*hack_real(index)) !calibrated value
*-----------------------------------------------------------------
*
      return
      end
*
