      subroutine h_fpp_nt_open(file,ABORT,err)
*----------------------------------------------------------------------
*
*     Opens an HMS FPP Ntuple file
*
*     Purpose : Books an HMS FPP Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------

      implicit none
      save

      character*13 here
      parameter (here='h_fpp_nt_open')
      integer Nwpawc,h,nh
      parameter (Nwpawc=150000)
      
      logical ABORT
      character*(*) err

      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'h_fpp_ntuple.cmn'
      include 'h_fpp_ntup.cwn'

      integer iquest
      common /PAWC/ h(Nwpawc)
      common /QUEST/ iquest(100)

      integer default_recL, default_bufS
c      parameter (default_recL= 8191)    !record length
c      parameter (default_bufS= 8176)    !record length
      parameter (default_recL= 4096)    !record length
      parameter (default_bufS= 4096)    !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,recL,bufS,iv(10),m
      real rv(10)

      logical HEXIST           !CERNLIB function

*--------------------------------------------------------

      err= ' '
      ABORT = .FALSE.
      IF(h_fpp_nt_exists) THEN
        call h_fpp_nt_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF


*- get any free IO channel

      call g_IO_control(io,'ANY',ABORT,err)
      h_fpp_nt_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      h_fpp_nt_IOchannel= io

      id= h_fpp_nt_ID
      name= h_fpp_nt_name
      title= h_fpp_nt_title

      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(h_fpp_nt_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

      nh=Nwpawc
c      call HLIMIT(nh)
      
      recL = default_recL
      bufS = default_bufS
      iquest(10) = 256000

*-open New *.rzdat file-
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
      call HROPEN(io,name,file,'NQE',recL,status) 

      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(h_fpp_nt_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

      call HBSET('BSIZE',bufS,status) 
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(h_fpp_nt_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HBSET error#$ allocating storage#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

* Open COLUMNwise ntuple for variable number of entries
      call HBNT(id,title,' ')
      call HBNAME(id,' ',0,'$clear')


* need to manually apply same ranges as defined for array limits!!
*  H_FPP_MAX_TRACKS=9
*  H_FPP_MAX_WIRES=104
*  max layers per chamber pair = 6
*  H_FPP_N_PLANES=12
*  H_FPP_MAX_RAWHITS=2400
*  MAX_cwn_goodhits= 100
*  max hit1/set = N chamb * N lay * max cluster/layer * max hit/cluster = 270

      call HBNAME(id,'FPP1',cwnFPP_eventID,'eventID:U*4'
     1 //',evtcode[0,63]:U*4'
     1 //',helicite[-1,1]:I*4'
     1 //',hsdelta:R'
     1 //',hstheta:R'
     1 //',hsphi:R'
     1 //',w:R'
     1 //',hszbeam:R'
     1 //',hsxfp:R'
     1 //',hsyfp:R'
     1 //',hsxpfp:R'
     1 //',hsypfp:R'
     1 //',hsytar:R'
     1 //',hsxptar:R'
     1 //',hsyptar:R'
     1
     1 //',trig_TDC1:I*4'
     1 //',trig_TDC2:I*4'
     1 
     1// ',RawHits[0,2400]:U*4'
     1
     1// ',Nhits1[0,100]:U*4'
     1// ',h1_Pol(Nhits1)[1,2]:U*4'
     1// ',h1_Layer(Nhits1)[0,12]:U*4'
     1// ',h1_Wire(Nhits1)[0,104]:U*4'
     1// ',h1_on_trk(Nhits1)[0,18]:U*4'
     1// ',h1_time(Nhits1):R'
     1// ',h1_drift(Nhits1):R'
     1// ',h1_resid(Nhits1):R'
     1// ',h1_d_HMS(Nhits1):R'
     1
     1// ',Ntrack[0,18]:U*4'
     1// ',Pol(Ntrack)[0,2]:U*4'
     1// ',trackNo(Ntrack)[0,9]:U*4'
     1// ',trk_hits(Ntrack)[0,6]:U*4'
     1// ',trk_conet(Ntrack):I*4'
     1// ',trk_s_xp(Ntrack):R'
     1// ',trk_s_x(Ntrack):R'
     1// ',trk_s_yp(Ntrack):R'
     1// ',trk_s_y(Ntrack):R'
     1// ',trk_f_xp(Ntrack):R'
     1// ',trk_f_x(Ntrack):R'
     1// ',trk_f_yp(Ntrack):R'
     1// ',trk_f_y(Ntrack):R'
     1// ',trk_chi2(Ntrack):R'
     1// ',trk_zclos(Ntrack):R'
     1// ',trk_sclos(Ntrack):R'
     1// ',trk_theta(Ntrack):R'
     1// ',trk_phi(Ntrack):R'
     1 )

      call HCDIR(h_fpp_nt_directory,'R')      !record Ntuple directory
      CALL HCDIR(directory,' ')       !reset CERNLIB directory

      h_fpp_nt_exists= HEXIST(h_fpp_nt_ID)
      ABORT= .NOT.h_fpp_nt_exists

      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_fpp_nt_directory // '/]' // 
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

      h_fpp_nt_segmentevents = 0

      RETURN
      END  
