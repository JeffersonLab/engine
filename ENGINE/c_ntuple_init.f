      subroutine c_Ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an COIN Ntuple
*
*     Purpose : Books an COIN Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
* $Log: c_ntuple_init.f,v $
* Revision 1.10  2004/02/17 17:26:34  jones
* Changes to enable possiblity of segmenting rzdat files
*
* Revision 1.9  1999/02/23 16:40:37  csa
* Variable changes
*
* Revision 1.8  1996/09/04 15:29:57  saw
* (JRA) Modify ntuple contents
*
* Revision 1.7  1996/01/22 15:06:25  saw
* (JRA) Change ntuple contents
*
* Revision 1.6  1996/01/16 21:01:12  cdaq
* (JRA) Add HSDELTA and SSDELTA
*
* Revision 1.5  1995/08/08 16:09:40  cdaq
* (DD) Change ntuple list
*
* Revision 1.4  1995/07/27  18:59:48  cdaq
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.3  1995/05/11  13:55:27  cdaq
* (SAW) Allow %d for run number in filenames
*
* Revision 1.2  1994/06/17  02:32:24  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:11:34  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='c_Ntuple_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'c_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'COINntuple')

      character*80 file
      character*80 name
      character*1000 pat,msg
      integer ilo,fn_len,m
      character*1 ifile
*
      logical HEXIST           !CERNLIB function
*
      INCLUDE 'c_ntuple.dte'
*
*--------------------------------------------------------
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(c_Ntuple_exists) THEN
        call c_Ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(c_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(c_Ntuple_file.EQ.' ') RETURN   !do nothing
      c_Ntuple_ID= default_c_Ntuple_ID
      c_Ntuple_name= default_name
      IF(c_Ntuple_title.EQ.' ') THEN
        msg= name//' '//c_Ntuple_file
        call only_one_blank(msg)
        c_Ntuple_title= msg
      ENDIF

      file= c_Ntuple_file
      call g_sub_run_number(file,gen_run_number)


*     * only needed if using more than one file      
      if (c_Ntuple_max_segmentevents .gt. 0) then
       c_Ntuple_filesegments = 1

       ifile = char(ichar('0')+c_Ntuple_filesegments)
 
       fn_len = g_important_length(file)
       ilo=index(file,'.hbook')
       if ((ilo.le.1).or.(ilo.gt.fn_len-5)) then
         ilo=index(file,'.rzdat')
       endif  

       if ((ilo.gt.1).and.(ilo.lt.fn_len)) then
         file = file(1:ilo-1) // '.' // ifile // file(ilo:fn_len)
       else
         ABORT = .true.
        RETURN
       endif
       write(*,*) ' Using segmented COIN rzdat files first filename: ',file
       else
         write(*,*) ' Not using segmented COIN rzdat files first filename: ',file  
      endif
*
**********begin insert description of contents of COIN tuple ******
      m= 0
      m=m+1
      c_Ntuple_tag(m)= 'cointime'      ! Corrected Coincidence Time
      m= m+1
      c_Ntuple_tag(m)= 'ucointime'      ! Uncorrected Coincidence Time
      m= m+1
      c_Ntuple_tag(m)= 'bpmxmean'         ! Mean Beam X Position
      m= m+1
      c_Ntuple_tag(m)= 'bpmymean'         ! Mean Beam Y Position
      m= m+1
      c_Ntuple_tag(m)= 'bpmx'         ! Beam X Position
      m= m+1
      c_Ntuple_tag(m)= 'bpmy'         ! Beam Y Position
      m= m+1
      c_Ntuple_tag(m)= 'frx'           ! Fast Raster X
      m= m+1
      c_Ntuple_tag(m)= 'fry'           ! Fast Raster Y
      m= m+1
      c_Ntuple_tag(m)= 'gbeam_x'           ! Fast Raster X
      m= m+1
      c_Ntuple_tag(m)= 'gbeam_y'           ! Fast Raster Y
      m= m+1
      c_Ntuple_tag(m)= 'hsxfp'         ! HMS Focal Plane
      m= m+1
      c_Ntuple_tag(m)= 'hsyfp'         ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsxpfp'        !
      m= m+1
      c_Ntuple_tag(m)= 'hsypfp'        !
      m= m+1
      c_Ntuple_tag(m)= 'ssxfp'         ! SOS Focal Plane
      m= m+1
      c_Ntuple_tag(m)= 'ssyfp'         !
      m= m+1
      c_Ntuple_tag(m)= 'ssxpfp'        !
      m= m+1
      c_Ntuple_tag(m)= 'ssypfp'        !
      m= m+1
      c_Ntuple_tag(m)= 'hsytar'        ! HMS Target
      m= m+1
*  xucc added begin     
      c_Ntuple_tag(m)= 'hsztar'        !  added by volmer
      m= m+1
*  it's not very hard to accept this 
*  xucc added end

      c_Ntuple_tag(m)= 'hsp'     ! Lab momentum of chosen track in GeV/c
      m= m+1
      c_Ntuple_tag(m)= 'hsxptar'       !
      m= m+1
      c_Ntuple_tag(m)= 'hsyptar'       ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsdelta'       ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssytar'        ! SOS Target
      m= m+1
*  xucc added begin 
      c_Ntuple_tag(m)= 'ssztar'        ! SOS Target
      m= m+1
*   again add new infor on Z
* xucc added end

      c_Ntuple_tag(m)= 'ssp'	! Lab momentum of chosen track in GeV/c
      m= m+1
      c_Ntuple_tag(m)= 'ssxptar'       ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssyptar'       ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssdelta'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'hcer_npe'       ! HMS Particle Id.
      m= m+1
      c_Ntuple_tag(m)= 'hsshsum'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsshtrk'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsprtrk'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsbeta_p'   ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsbeta'         ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsdedx1'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'scer_npe'       ! SOS Particle Id.
      m= m+1
c      c_Ntuple_tag(m)= 'saer_npe'       ! 
c      m= m+1
      c_Ntuple_tag(m)= 'ssshsum'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssshtrk'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssprtrk'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssbeta_p'   ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssbeta'         ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssdedx1'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'charge'         ! Charge of last Scaler Event
      m=m+1
      c_Ntuple_tag(m)= 'eventID' ! CODA event ID#
      m=m+1
      c_Ntuple_tag(m)= 'evtype' 
      m=m+1
      c_Ntuple_tag(m)= 'Em'
      m=m+1
      c_Ntuple_tag(m)= 'missmass'
      m=m+1
* 
*    xucc added begin
*      c_Ntuple_tag(m)= 'Eexc'
*      m=m+1
*      c_Ntuple_tag(m)= 'Eexcx'
*      m=m+1
*     xucc added end

      c_Ntuple_tag(m)= 'Emx'
      m=m+1
      c_Ntuple_tag(m)= 'mmx'
      m=m+1
      c_Ntuple_tag(m)= 'krel'
      m=m+1
*  seems to add some new infor about excitation energy
*     xucc added end

      c_Ntuple_tag(m)= 'Pm'
      m=m+1
      c_Ntuple_tag(m)= 'PmPar'
      m=m+1
      c_Ntuple_tag(m)= 'PmPer'
      m=m+1
      c_Ntuple_tag(m)= 'PmOop'
      m=m+1
      c_Ntuple_tag(m)= 'omega'
      m=m+1
      c_Ntuple_tag(m)= 'Q2'
      m=m+1
*     xucc added begin
      c_Ntuple_tag(m)= 'W'
      m=m+1
*    xucc added end
      c_Ntuple_tag(m)= 'W2'
      m=m+1
      c_Ntuple_tag(m)= 'Xbj'
      m=m+1
      c_Ntuple_tag(m)= 'qabs'
      m=m+1
      c_Ntuple_tag(m)= 'Zm'
      m=m+1
      c_Ntuple_tag(m)= 'Pt2'
      m=m+1
      c_Ntuple_tag(m)= 't'
      m=m+1
*    seems to add more infor on Q2, (Q the four momentum of virtual gamma from*    electrons)etc 
*    xucc added end


      c_Ntuple_tag(m)= 'th_pq'
      m=m+1
      c_ntuple_tag(m)= 'phi_pq'

*     xucc added begin
      m=m+1
      c_Ntuple_tag(m)= 'epsilon'
      m=m+1
      c_ntuple_tag(m)= 'gamma_v'
*     xucc added end


* added more thing for fpi analysis
*     xucc added end

* on June 21,2003, xucc added following for online purpose
      m=m+1
      c_Ntuple_tag(m)= 'scal_x'
      m=m+1
      c_ntuple_tag(m)= 'scal_y'
      m=m+1
      c_ntuple_tag(m)= 'hsmass2'
      m=m+1
      c_ntuple_tag(m)= 'haero_sum'
      m=m+1
      c_ntuple_tag(m)= 'haero_pos_npe_sum'
      m=m+1
      c_ntuple_tag(m)= 'haero_neg_npe_sum'
      m=m+1
      c_ntuple_tag(m)= 'hceradc1'
      m=m+1
      c_ntuple_tag(m)= 'hceradc2'
      m=m+1
      c_ntuple_tag(m)= 'hcersx'
      m=m+1
      c_ntuple_tag(m)= 'coinpathcor'
      m=m+1
      c_ntuple_tag(m)= 'sszbeam'
      m=m+1
      c_ntuple_tag(m)= 'hszbeam'
      m=m+1
      c_ntuple_tag(m)= 'ctphix'
      m=m+1
      c_ntuple_tag(m)= 'ctphiy'
      m=m+1
      c_ntuple_tag(m)= 'hsbeta_ntrk'
      m=m+1
      c_ntuple_tag(m)= 'hcoly'
      m=m+1
      c_ntuple_tag(m)= 'hcolx'
      m=m+1
      c_ntuple_tag(m)= 'scoly'
      m=m+1
      c_ntuple_tag(m)= 'scolx'
! Th comment out for Garth replay pass2
      m= m+1
      c_Ntuple_tag(m)= 'hntrks'
      m= m+1
      c_Ntuple_tag(m)= 'sntrks'
      m= m+1
      c_Ntuple_tag(m)= 'theloss'
! TH
!      c_ntuple_tag(m)= 'altcointime'
!      m=m+1
!      c_ntuple_tag(m)= 'sctimer'
* end of xucc adding on June 21,2003

*      m=m+1
*      c_Ntuple_tag(m)= 'HmsCorsi'
*      m=m+1
*      c_Ntuple_tag(m)= 'SosCorsi'
      c_Ntuple_size= m
***********end insert description of contents of COIN tuple********
*
* Open ntuple

      call c_Ntuple_open(file,ABORT,err)      

      IF(ABORT) THEN
        err= ':unable to create Coin Ntuple'
        call G_add_path(here,err)
      ELSE
        pat= ':created Coin Ntuple'
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF
*
      RETURN
      END  
