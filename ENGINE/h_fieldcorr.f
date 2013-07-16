      SUBROUTINE H_FIELDCORR(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Correct hpcentral from wrong saturation 
*-                         calculation in fieldXX.f
*-                              
*-
*-      Required Input BANKS     HMS_FOCAL_PLANE
*-                               HMS_TARGET
*-
*-      Output BANKS             HMS_PHYSICS_R4
*-                               HMS_PHYSICS_I4
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-
*-   crated: Fri Aug 18 11:48:29 EDT 2000 B. Zihlmann
*-           from h_satcorr.f but do correction to hpcentral         
*-           and the call needs to be done at begining of 
*-           run only (not event-by-event).
*-
*-
* $Log: h_fieldcorr.f,v $
* Revision 1.2  2003/02/21 14:55:26  jones
* Added write statements
*
* Revision 1.1  2002/09/24 20:11:24  jones
* h_fieldcorr.f corrects hpcentral when genable_hms_fieldcorr.eq.0
*     and hpcentral > 3.5573 GeV/c . Correction needs to be applied
*     for experiments which used fieldxx.f before field02.f
*
*
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*9 here
      parameter (here= 'h_fieldcorr')
*
      logical ABORT
      character*(*) err
      integer ierr
*
      include 'gen_data_structures.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
*
*     local variables 
*
      REAL*4 p0corr
      
*--------------------------------------------------------
*

      ierr=0
      ABORT=.FALSE.

      p0corr=0.

      if(genable_hms_fieldcorr.eq.0) then
         write(*,*) ' ******'
         write(*,*) ' HMS field correction applied when hpcentral > 3.5573 '
         write(*,*) ' Should be enabled for data taken before Jan 1 2002'
        if (hpcentral.gt.3.5573) then
          p0corr= 1.0 + 1.0755e-3*
     >         ((hpcentral-3.5573)**2)
          
          hpcentral = hpcentral*p0corr
          
        endif                        
      else
         write(*,*) ' ******'
         write(*,*) ' HMS field correction not applied '
         write(*,*) ' Should be enabled for data taken before Jan 1 2002'
      endif

      ABORT= ierr.ne.0 .or. ABORT

      return
      end



