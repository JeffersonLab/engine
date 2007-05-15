      subroutine b_ntuple_open(file,ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_ntuple_open')

      logical ABORT
      character*(*) err

      include 'b_ntuple.cmn'

      integer default_bank,default_recl
      parameter(default_bank=8000)     !4 bytes/word
      parameter(default_recl=1024)     !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)

      logical HEXIST        !CERNLIB function

      err=' '
      ABORT=.false.
      if(b_ntuple_exists) then
         call b_ntuple_shutdown(ABORT,err)
         if(abort) then
            call G_add_path(here,err)
            return
         endif
      endif

c     get any free IO channel

      call g_IO_control(io,'ANY',ABORT,err)
      b_ntuple_exists = .not.ABORT
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
      
      b_ntuple_iochannel = io

      id = b_ntuple_id
      name = b_ntuple_name
      title = b_ntuple_title
      ABORT = HEXIST(id)
      if(ABORT) then
         call g_IO_control(b_ntuple_iochannel,'FREE',ABORT,err)
         call g_build_note(':HBOOK id#$ already in use',
     $        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         return
      endif

      call HCDIR(directory,'R') !CERNLIB read current directory

      recL = default_recl
      call HROPEN(io,name,file,'N',recL,status)

      ABORT= status.ne.0
      if(ABORT) then
         call g_IO_control(b_ntuple_iochannel,'FREE',ABORT,err)
         iv(1) = status
         iv(2) = io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         return
      endif

      size = b_ntuple_size
      bank = default_bank
      title = b_ntuple_title
      
      call HBOOKN(id,title,size,name,bank,b_ntuple_tag)  ! create ntuple

      call HCDIR(b_ntuple_directory,'R')     ! record ntuple directory

      call HCDIR(directory,' ')          !reset CERNLIB directory

      b_ntuple_exists = HEXIST(b_ntuple_id)

      abort = .not.b_ntuple_exists

      iv(1) = id
      iv(2) = io
      pat = 'Ntuple id#$ [' // b_ntuple_directory // '/]' //
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
