      subroutine g_init_filenames(ABORT, error, env_var)
*----------------------------------------------------------------------
*-    Purpose and Methods:
*-
*-    Read a configuration file with set of filenames and options.
*-    Much of this will be handled by CTP when a string capability is added
*-    to CTP parameter files.  Allowed keywords in config file are
*-    'hist', 'test', 'parm', 'alias', 'data', 'hbook', 'map', 'nevents', 'data'
*-
*-    This routine does the booking of hist, test, and parm files.  This
*-    booking should be moved to another file.
*-
*-    Inputs:
*-
*-    env_var     Environment variable pointing to the config file.
*-
*-    Outputs:
*-
*-    ABORT
*-    error
*-
*-    Created               Steve Wood, CEBAF
*-    Modified   3-Dec-1993 Kevin Beard, Hampton U.
*-    Modified   8-Dec-1993 Kevin Beard; rewrote parsing,added 'data' type
*-    $Log$
*-    Revision 1.1  1994/02/02 20:08:15  cdaq
*-    Initial revision
*-
*----------------------------------------------------------------------
      implicit none
      SAVE
*
      character*16 here
      parameter (here= 'g_init_filenames')
*
      logical ABORT
      character*(*) error
      character*(*) env_var
*
      include 'gen_filenames.cmn'
*
      integer SSIZE
      parameter (SSIZE=160)
      character*160 s
      character*20 key
      character*80 setting
      character*1 tab
      logical blankskip,rebook_flag
      integer*2 i,ifname,ifname_end,itype  ! String indices i*2.
      integer*4 thwhalias                  ! Function def for CTP routine
      integer ierr,Nlines_read
      LOGICAL MATCH			!KBB personal function
      logical echo
      data echo/.FALSE./
*
*     The variables used for string manipulation should be i*2 or
*     else strange things may happen.
*
*     Book the histograms, tests and parameters
*
      g_hist_rebook = .false.
      g_test_rebook = .false.
      g_parm_rebook = .false.
      g_alias_filename = ' '
      g_data_source_opened = .false.     !not opened yet
      g_data_source_in_hndl= 0           !none
      g_data_source_filename= ' '        !undefined
      Nlines_read= 0      
c     
      call getenv(env_var,g_config_filename)
*
      ABORT= g_config_filename.EQ.' '
      IF(ABORT) THEN
        error= here//':blank environmental variable '//env_var
        RETURN
      ENDIF
*
      open(unit=G_LUN_CONFIG,file=g_config_filename,type='OLD',
     &                                err=999)
c
      do while(.true.)
         read(G_LUN_CONFIG,'(a)',end=901,err=901) s
         Nlines_read= Nlines_read+1
         IF(echo) type *,' '//s(1:78)
c     
*
         call NO_tabs(s)
	 call NO_blanks(s)
*
*-NO comments
         i = INDEX(s,'!')
         IF(i.GT.0) s(i:)= ' '
*
         i= INDEX(s,'=')
         IF(i.GT.1) THEN
           key= s(1:i-1)
	   setting= s(i+1:)
         ELSE
           key= s
           setting= ' '
         ENDIF	   
*
         rebook_flag = .false.
*
         if(key.eq.' ') then
*do nothing
*
         elseif(MATCH(key,'ECHO')) then
               echo= .TRUE.
*
         elseif(MATCH(key,'NOecho')) then
               echo= .FALSE.
*
*
         elseif(MATCH(key,'REBO*ok')) then
               rebook_flag = .true.
               type *,'Rebooking: '
*
         elseif(match(key,'HIST*ogram')) then
            g_ctp_hist_filename = setting
            if(rebook_flag.or..not.g_config_loaded) 
     &                             g_hist_rebook = .true.
*
         elseif(match(key,'PAR*ameter')) then
            g_ctp_parm_filename = setting
            if(rebook_flag.or..not.g_config_loaded) 
     &                             g_parm_rebook = .true.
*
         elseif(match(key,'TEST')) then
            g_ctp_test_filename = setting
            if(rebook_flag.or..not.g_config_loaded) 
     &                             g_test_rebook = .true.
*
         elseif(match(key,'ALIA*S')) then
            g_alias_filename = setting
*
         elseif(match(key,'HBO*OK')) then ! hbook= name of output file
            g_histout_filename = setting
*
         elseif(MATCH(key,'MAP')) then
            g_decode_map_filename = setting
*
         elseif(match(key,'NEV*ENTs')) then
*
	    setting(21:)= ','           !add ","
            call NO_blanks(setting)     !squeeze down
	    read(setting,'(I)',err=901) g_max_events
*
         elseif(match(key,'data*_source')) then
            g_data_source_filename= setting
*
         elseif(key.eq.'?' .or. match(key,'HE*lp')) then
            type *
            type *,' following keywords supported:'
            type *
            type *,' ECHO'
            type *,' NOECHO'
            type *,' REBOok'
            type *,' HISTogram= file'
            type *,' PARameter= file'
            type *,' TEST= file'
            type *,' ALIAs= file'
            type *,' HBOok= file'
            type *,' MAP= file'
            type *,' NEVents= #'
            type *,' DATA_source= file'
            type *,' HElp'
            type *
         else
            type *,'Unknown keyword '//key
         endif
 101     continue				! Drop here for comments
      enddo
 901  continue

*
*     Need to move this code to another routine.
*

      if(g_parm_rebook) call thload(g_ctp_parm_filename)
      if(g_test_rebook) call thload(g_ctp_test_filename)
      if(g_hist_rebook) call thload(g_ctp_hist_filename)
*
      if(g_parm_rebook .or. g_test_rebook .or. g_hist_rebook) then
         call thbook
         if(g_alias_filename.ne.' ') then
            ierr = thwhalias(g_alias_filename)
            type *,'called haliaswrite',ierr
         endif
      endif
*
      close(unit=G_LUN_CONFIG)
      g_config_loaded = Nlines_read.GT.0         !at least one line!
      ABORT= .NOT.g_config_loaded
      IF(ABORT) THEN
        error= ':opened OK, but no lines read from "'//
     &                                  g_config_filename//'"'
        call G_add_path(here,error)
      ELSE
        error= ' '
      ENDIF
      IF(echo) type *,' ......exiting '//here//'........'
      return
*
999   g_config_loaded= .FALSE.
      ABORT= .NOT.g_config_loaded
      error= ':unable to open file "'//g_config_filename//'"'
      call G_add_path(here,error)
      return
*
      end
