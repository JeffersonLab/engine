      subroutine c_Ntuple_init(ABORT,err)
*  xucc comments begin
*  In this file, only some new names of NTUPLE contents
*  The actual meaning of these contents should be found 
*  It will never be wrong to put it in common blocks
*  Only beware that these new parameters may only make sense
*  for FPI events. For other events, it may be like the eyes
*  of a blind person
* 
*  So thge Users himself (herself) 
*  should be responsible for usage of these new things
*  These new things are
*  
*
*      c_Ntuple_tag(m)= 'hsztar'        !  added by volmer
*      c_Ntuple_tag(m)= 'ssztar'        ! SOS Target
*      c_Ntuple_tag(m)= 'Eexc'
*      c_Ntuple_tag(m)= 'Emx'
*      c_Ntuple_tag(m)= 'mmx'
*      c_Ntuple_tag(m)= 'Eexcx'
*      c_Ntuple_tag(m)= 'Q2'
*      c_Ntuple_tag(m)= 'W'
*      c_Ntuple_tag(m)= 't'
*      c_Ntuple_tag(m)= 'epsilon'
*      c_ntuple_tag(m)= 'gamma_v'

* on June 21,2003, xucc added following for online purpose

*      c_Ntuple_tag(m)= 'scal_x'
*      c_ntuple_tag(m)= 'scal_y'
*      c_ntuple_tag(m)= 'hsmass2'
* end of xucc adding on June 21,2003


*  Of course, we will also see the correct order of new quantities
*  filled as follows 
*      c_Ntuple_contents(m)= ztar_dummy
*      c_Ntuple_contents(m)= ztar_dummy
*      c_Ntuple_contents(m)= ce_exc ! Excitation Energy
*      c_Ntuple_contents(m)= cmex  ! missing energy
*      c_Ntuple_contents(m)= cmmx ! missing mass
*      c_Ntuple_contents(m)= ce_excx ! Excitation Energy
*      c_Ntuple_contents(m)= c_bigq2  ! q2?
*      c_Ntuple_contents(m)= c_invmass
*      c_Ntuple_contents(m)= cmin_t
*      c_Ntuple_contents(m)= c_epsilon
*      c_Ntuple_contents(m)= c_gamma_v
*  

* on June 21,2003, xucc added following for online purpose

c       c_Ntuple_contents(m)= ssx_cal
c       c_Ntuple_contents(m)= ssy_cal
c       c_ntuple_contents(m)= hsmass2
* end of xucc adding on June 21,2003


*    
*   Another one which puzzled me is the quantity cphipq
*   there is only one line like
*c        cphipq=asin(p_rot_y/p_rot_mag_check)/deg_rad
*   to tell us what it could be.
*   I find that Volmer's cphipi:
*   phipi = acos(p_new_x/sqrt(p_new_x**2+p_new_y**2))
*   could be more useful for us. So I used this instead.
*   although maybe this could be
*   phipi = acos(p_new_x/sqrt(p_new_x**2+p_new_y**2))/deg_rad
*   Of course, I also changed the tag phi_pq to phi_pi accordingly
*  
*  xucc comment end

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
* $Log$
* Revision 1.9.2.2  2003/07/03 14:06:09  cdaq
* update for fpi-2 (xu)
*
* Revision 1.9.4.1  2003/03/05 22:51:31  xu
* new variables
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
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
*
      logical HEXIST           !CERNLIB function
*
      INCLUDE 'c_ntuple.dte'
*
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
*
*- get any free IO channel
*
      call g_IO_control(io,'ANY',ABORT,err)
      c_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      c_Ntuple_IOchannel= io
*
      c_Ntuple_ID= default_c_Ntuple_ID
      id= c_Ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(c_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      c_Ntuple_name= default_name
*
      id= c_Ntuple_ID
      name= c_Ntuple_name

      file= c_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
      io= c_Ntuple_IOchannel
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      io= c_Ntuple_IOchannel
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(c_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
      c_Ntuple_file= file
*
**********begin insert description of contents of COIN tuple ******
      m= 0
      m=m+1
      c_Ntuple_tag(m)= 'cointime'      ! Corrected Coincidence Time
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
      c_Ntuple_tag(m)= 'hsbeta_notrk'   ! 
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
      c_Ntuple_tag(m)= 'ssbeta_notrk'   ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssbeta'         ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssdedx1'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'charge'         ! Charge of last Scaler Event
      m=m+1
      c_Ntuple_tag(m)= 'eventID' ! CODA event ID#
      m=m+1
      c_Ntuple_tag(m)= 'Em'
      m=m+1
      c_Ntuple_tag(m)= 'missmass'
      m=m+1

*    xucc added begin
      c_Ntuple_tag(m)= 'Eexc'
      m=m+1
      c_Ntuple_tag(m)= 'Emx'
      m=m+1
      c_Ntuple_tag(m)= 'mmx'
      m=m+1
      c_Ntuple_tag(m)= 'Eexcx'
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

*     xucc added begin
      c_Ntuple_tag(m)= 'Q2'
      m=m+1
      c_Ntuple_tag(m)= 'W'
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

* end of xucc adding on June 21,2003

*      m=m+1
*      c_Ntuple_tag(m)= 'HmsCorsi'
*      m=m+1
*      c_Ntuple_tag(m)= 'SosCorsi'
      c_Ntuple_size= m
***********end insert description of contents of COIN tuple********
*
      title= c_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//c_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        c_Ntuple_title= title
      ENDIF
*
      id= c_Ntuple_ID
      io= c_Ntuple_IOchannel
      name= c_Ntuple_name
      title= c_Ntuple_title
      size= c_Ntuple_size
      file= c_Ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,c_Ntuple_tag)      !create Ntuple
*
      call HCDIR(c_Ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      c_Ntuple_exists= HEXIST(c_Ntuple_ID)
      ABORT= .NOT.c_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // c_Ntuple_directory // '/]' // 
     &                         name // ' IO#$ "' // file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')
*
      IF(ABORT) THEN
        err= ':unable to create '//msg
        call G_add_path(here,err)
      ELSE
        pat= ':created '//msg
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF
*
      RETURN
      END  
