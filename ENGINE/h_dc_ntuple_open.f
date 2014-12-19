      subroutine h_dc_Ntuple_open(file,ABORT,err)
*----------------------------------------------------------------------
*
*     Opens an HMS Ntuple file
*
*     Purpose : Books an HMS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------

      implicit none
      save

      character*13 here
      parameter (here='h_dc_Ntuple_open')

      logical ABORT
      character*(*) err

      INCLUDE 'h_dc_ntuple.cmn'

      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 4096)    !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
      integer iquest
       common/quest/iquest(100)
      logical HEXIST           !CERNLIB function

*--------------------------------------------------------

      err= ' '
      ABORT = .FALSE.
      IF(h_dc_Ntuple_exists) THEN
        call h_dc_Ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF

*- get any free IO channel

      call g_IO_control(io,'ANY',ABORT,err)
      h_dc_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      h_dc_Ntuple_IOchannel= io
      
      id= h_dc_Ntuple_ID
      name= h_dc_Ntuple_name
      title= h_dc_Ntuple_title
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(h_dc_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

      CALL HCDIR(directory,'R')       !CERNLIB read current directory

*-open New *.rzdat file-
      recL= default_recL
      iquest(10)=65000
      call HROPEN(io,name,file,'NQ',recL,status)       !CERNLIB

      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(h_dc_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
      
      size= h_dc_Ntuple_size
      bank= default_bank
      title= h_dc_Ntuple_title
      call hbset('BSIZE',8176,status)
      call HBNT(id,title,' ')
      call HBNAME(id,'GINFO',evnum,'evnum:R*4,evtype:R*4')
      call HBNAME(id,'HDCRAWINFO',hdc_rnhits,'hdc_rnhits:R*4,hdc_tnhits:R*4'//
     >',hdc_plnhits(12):R*4')
      call HBNAME(id,'RASTINFO',frx_raw_adc,'frx_raw_adc:R*4,fry_raw_adc:R*4'//
     >',frx_adc:R*4,fry_adc:R*4,frx:R*4,fry:R*4')
      call HBNAME(id,'HDCINFO',hdc_ntr,'hdc_ntr[0,20]:I*4,'//
     >'hdc_xfp(hdc_ntr):R*4,hdc_yfp(hdc_ntr):R*4,'//
     >'hdc_xpfp(hdc_ntr):R*4,hdc_ypfp(hdc_ntr):R*4,hdc_chi2(hdc_ntr):R*4,'//
     >'hdc_ytg(hdc_ntr):R*4,hdc_xptg(hdc_ntr):R*4,'//
     >'hdc_yptg(hdc_ntr):R*4,hdc_delta(hdc_ntr):R*4,hdc_ptar(hdc_ntr):R*4,'//
     >'hdc_chi2min:R*4,hdc_x2dmin:R*4,hdc_y2dmin:R*4,hdc_ngoodtr:I*4')
      call HBNAME(id,'HSCININFO',hscin_starttime,'hscin_starttime:R*4,hscin_rfptime(4):R*4,hscin_fptimedif(6):R*4')
      call HBNAME(id,'SDCINFO',sdc_ntr,'sdc_ntr[0,20]:I*4,'//
     >'sdc_xfp(sdc_ntr):R*4,sdc_yfp(sdc_ntr):R*4,'//
     >'sdc_xpfp(sdc_ntr):R*4,sdc_ypfp(sdc_ntr):R*4,sdc_chi2(sdc_ntr):R*4,'//
     >'sdc_ytg(sdc_ntr):R*4,sdc_xptg(sdc_ntr):R*4,'//
     >'sdc_yptg(sdc_ntr):R*4,sdc_delta(sdc_ntr):R*4,sdc_ptar(sdc_ntr):R*4')
      call HCDIR(h_dc_Ntuple_directory,'R')      !record Ntuple directory

      CALL HCDIR(directory,' ')       !reset CERNLIB directory

      h_dc_Ntuple_exists= HEXIST(h_dc_Ntuple_ID)
      ABORT= .NOT.h_dc_Ntuple_exists

      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_dc_Ntuple_directory // '/]' // 
     &                         name // ' IO#$ "' // file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')

      IF(ABORT) THEN
        err= ':unable to create '//msg
        call G_add_path(here,err)
      ELSE
        pat= ':created '//msg
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF

      RETURN
      END  
