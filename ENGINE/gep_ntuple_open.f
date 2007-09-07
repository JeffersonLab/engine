      subroutine gep_ntuple_open(file,ABORT,err)

      implicit none
      save

      character*15 here
      parameter(here='gep_ntuple_open')

      logical ABORT
      character*(*) err

      include 'gep_ntuple.cmn'
c      include 'gep_data_structures.cmn'

      integer default_bank,default_recl

      parameter(default_bank=8000) !4 bytes/word
      parameter(default_recl=1024) !record length

      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)

      logical HEXIST          !CERNLIB function

      err=' '
      abort=.false.

      if(gep_ntuple_exists) then
         call gep_ntuple_shutdown(ABORT,err)
         if(abort)then
            call G_add_path(here,err)
            return
         endif
      endif

c     get any free IO channel

      call g_IO_control(io,'ANY',ABORT,err)
      if(abort) then
         call G_add_path(here,err)
         return
      endif

      gep_ntuple_io_channel = io

      id = gep_ntuple_id
      name = gep_ntuple_name
      title = gep_ntuple_title
      abort = HEXIST(id)
      if(abort) then
         call g_IO_control(gep_ntuple_io_channel,'FREE',abort,err)
         call g_build_note(':HBOOK id#$ already in use',
     $        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         return
      endif

      call HCDIR(directory,'R') !CERNLIB read current directory

      recL = default_recl

      call HROPEN(io,name,file,'N',recL,status)

      abort= status.ne.0

      if(abort) then

         call g_IO_control(gep_ntuple_io_channel,'FREE',abort,err)
         iv(1) = status
         iv(2) = io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         return
      endif

      size = gep_ntuple_size
      bank = default_bank
      title = gep_ntuple_title

      call HBOOKN(id,title,size,name,bank,gep_ntuple_tag) ! create ntuple
c      call HBNT(id,title,' ')
c$$$      call HBNAME(id,'GEPBLOCK',gep_evid,'gep_evid:I*4,'//
c$$$     $     'gep_ctime_hms,gep_ctime_cal,gep_Q2,gep_Q2_H,'//
c$$$     $     'gep_Q2_B,GEP_E_electron,GEP_P_proton,GEP_delta_p,'//
c$$$     $     'gep_epsilon,gep_etheta_deg,gep_ptheta_deg,gep_ephi_deg,'//
c$$$     $     'gep_pphi_deg,gep_Emiss,gep_Pmissx,gep_Pmissy,gep_Pmissz,'//
c$$$     $     'gep_Pmiss,gep_W2,gep_Mmiss')

      call HCDIR(gep_ntuple_directory,'R') ! record ntuple directory
      call HCDIR(directory,' ')         ! reset CERNLIB directory

      gep_ntuple_exists = HEXIST(gep_ntuple_id)

      abort = .not.gep_ntuple_exists

      iv(1) = id
      iv(2) = io

      pat = 'Ntuple id#$ [' // gep_ntuple_directory // '/]' // 
     $     name // ' IO#$ "' // file // '"'
      
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)

      call sub_string(msg,'/]','/]')

      if(abort) then
         err = 'unable to create '//msg
         call G_add_path(here,err)
      else
         pat=':created '//msg
         call G_add_path(here,pat)
         call G_log_message('INFO: '//pat)
      endif

      return 
      end
