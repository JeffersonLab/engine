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
* $Log$
* Revision 1.10.12.1  2004/09/21 15:28:22  jones
* Version for the baryon cvs branch
*
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
      c_Ntuple_tag(m)= 'hsxptar'       !
      m= m+1
      c_Ntuple_tag(m)= 'hsyptar'       ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsdelta'       ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssytar'        ! SOS Target
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
      c_Ntuple_tag(m)= 'scer_npe'       ! SOS Particle Id.
      m= m+1
      c_Ntuple_tag(m)= 'ssshsum'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'ssshtrk'        ! 
      m= m+1
      c_Ntuple_tag(m)= 'charge'         ! Charge of last Scaler Event
      m=m+1
      c_Ntuple_tag(m)= 'eventID' ! CODA event ID#
      m=m+1
      c_Ntuple_tag(m)= 'mmass2'
      m=m+1
      c_Ntuple_tag(m)= 'hpvec0'
      m=m+1
      c_Ntuple_tag(m)= 'hpvecx'
      m=m+1
      c_Ntuple_tag(m)= 'hpvecy'
      m=m+1
      c_Ntuple_tag(m)= 'hpvecz'
      m=m+1
      c_Ntuple_tag(m)= 'spvec0'
      m=m+1
      c_Ntuple_tag(m)= 'spvecx'
      m=m+1
      c_Ntuple_tag(m)= 'spvecy'
      m=m+1
      c_Ntuple_tag(m)= 'spvecz'
      m=m+1
      c_Ntuple_tag(m)= 'W'
      m=m+1
      c_Ntuple_tag(m)= 'Q2'
      m=m+1
      c_Ntuple_tag(m)= 'cos_thet'
      m=m+1
      c_Ntuple_tag(m)= 'phicm'
      m=m+1
      c_Ntuple_tag(m)= 'ebeam'
      m=m+1
      c_Ntuple_tag(m)= 'hsp'
      m=m+1
      c_Ntuple_tag(m)= 'ssp'
      m=m+1
      c_Ntuple_tag(m)= 'haeronpe'
      m= m+1
      c_Ntuple_tag(m)= 'tdcmu_ym'
      m= m+1
      c_Ntuple_tag(m)= 'adcmu_yp'
      m= m+1
      c_Ntuple_tag(m)= 'adcmu_ym'
      m= m+1
      c_Ntuple_tag(m)= 'sceradc1'
      m= m+1
      c_Ntuple_tag(m)= 'sceradc2'
      m= m+1
      c_Ntuple_tag(m)= 'sceradc3'
      m= m+1
      c_Ntuple_tag(m)= 'sceradc4'
      m= m+1
      c_Ntuple_tag(m)= 'sshl1e'
      m= m+1
      c_Ntuple_tag(m)= 'sshl2e'
      m= m+1
      c_Ntuple_tag(m)= 'sshl3e'
      m= m+1
      c_Ntuple_tag(m)= 'sshl4e'
      m= m+1
      c_Ntuple_tag(m)= 'hshl1e'
      m= m+1
      c_Ntuple_tag(m)= 'hshl2e'
      m= m+1
      c_Ntuple_tag(m)= 'hshl3e'
      m= m+1
      c_Ntuple_tag(m)= 'hshl4e'
      m= m+1
      c_Ntuple_tag(m)= 'ssphi'
      m= m+1
      c_Ntuple_tag(m)= 'hsphi'
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
