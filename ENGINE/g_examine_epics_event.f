      subroutine g_examine_epics_event
* $Log: g_examine_epics_event.f,v $
* Revision 1.5.12.2  2004/09/08 16:51:44  cdaq
* added counters for error messages so they will only print a couple times each
*
*
*DJG -add escape for bad epics file
* 
*Revision 1.2  1996/11/05 21:40:32  saw
* (JRA) Print out just first epics event
*
* Revision 1.1  1996/08/12 18:30:13  saw
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save

      real*4 gepics_ebeam,gepics_ibeam
      real*4 gepics_xpos_a, gepics_ypos_a     !add H00Apos
      real*4 gepics_xpos_b, gepics_ypos_b     !add H00Bpos
      real*4 gepics_xpos_c, gepics_ypos_c     !add H00Cpos
      real*4 gepics_xraw_a, gepics_yraw_a     !add H00Araw
      real*4 gepics_xraw_b, gepics_yraw_b     !add H00Braw
      real*4 gepics_xraw_c, gepics_yraw_c     !add H00Craw
      real*4 haxraw,hayraw,hbxraw,hbyraw,hcxraw,hcyraw ! nominal beam positions
      real*4 gepics_smcpos,gepics_bdspos,gepics_soltarpos
      real*4 hepics_iq1,hepics_iq2,hepics_iq3,hepics_bd
      real*4 p_q1,p_q2,p_q3,p_d
      real*4 d_q1,d_q2,d_q3,d_d
      real*4 di_q3            !current offset in q3
      real*4 di_expect        !expected offset in q3
      real*4 sepics_hp,sepics_sp,sd
      real*8 iq1,iq2,iq3,id,bd,phms

      character buffer*12000
      equivalence (craw(5), buffer)
      integer i,j,evlen
      integer g_important_length,find_char
      integer numevent
      integer epics_targ_num,epics_cryotarg_num
      integer max_mess
      logical dump_event,hallc_event

      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_filenames.cmn'
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'

*--------------------------------------------------------

      max_mess = 3
* set the nominal beam positions for use later:
* these are the nominal positions used during the 
* xem run 6/4 - 7/4   js

      haxraw = 0.37
      hayraw = -1.33
      hbxraw = 0.54
      hbyraw = -1.14
      hcxraw = 0.52
      hcyraw = -0.64
      
*-- initialize sepics_sp 
      sepics_sp = 999.99
      
*--------------------------------------------------------

      hallc_event = .false.
      numevent = numevent + 1

      if (buffer(30:49).eq.'CA.Client.Diagnostic') then
        write(6,*) 'BADEPICS: epics event #',numevent
        return
      endif

      if (g_epics_output_filename.ne.' ' .and.
     &   (gdebugdumpepics.eq.1 .or. numevent.le.2)) then  !write out event
        dump_event = .true.
      else
        dump_event = .false.
      endif

      if (dump_event) write (G_LUN_EPICS_OUTPUT,*) 'epics event #',numevent

      if (craw(3)-1.le.0) then
        write (6,*) 'EPICSLEN: bad EPICS record length, numevent=',
     &    numevent,', craw3=',craw(3)
        return
      endif

cccc      write (6,*) 'epics,evlen',evlen,numevent,craw(3)

      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
      i = 1
cccc      write (6,*) 'epics,evlen',evlen,numevent,craw(3)
      do while (i.le.evlen)

         j = find_char (buffer, i, 10) ! 10 = NewLine character
         if (i.eq.j) goto 20
         if(i.lt.j-1 .and. dump_event) write(G_LUN_EPICS_OUTPUT,'(4x,a)') buffer(i:j-1)
         if (i+11.le.j-1) then  !text line.
c ********** read out the BPMs (POS values first...) **********
            if (buffer(i:i+11).eq.'IPM3H00A.XPO') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_xpos_a
               if (numevent.le.2) write(6,*) ' ++  X(IPM3H00A.XPOS) = ',gepics_xpos_a,' mm'
            else if (buffer(i:i+11).eq.'IPM3H00A.YPO') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_ypos_a
               if (numevent.le.2) write(6,*) ' ++  Y(IPM3H00A.YPOS) = ',gepics_ypos_a,' mm'
            else if (buffer(i:i+11).eq.'IPM3H00B.XPO') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_xpos_b
               if (numevent.le.2) write(6,*) ' ++  X(IPM3H00B.XPOS) = ',gepics_xpos_b,' mm'
            else if (buffer(i:i+11).eq.'IPM3H00B.YPO') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_ypos_b
               if (numevent.le.2) write(6,*) ' ++  Y(IPM3H00B.YPOS) = ',gepics_ypos_b,' mm'
            else if (buffer(i:i+11).eq.'IPM3H00C.XPO') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_xpos_b
               if (numevent.le.2) write(6,*) ' ++  X(IPM3H00C.XPOS) = ',gepics_xpos_c,' mm'
            else if (buffer(i:i+11).eq.'IPM3H00C.YPO') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_ypos_b
               if (numevent.le.2) write(6,*) ' ++  Y(IPM3H00C.YPOS) = ',gepics_ypos_c,' mm'
c               write(6,*) '-------------------'
c *********** now read the raw values: ***************
            else if (buffer(i:i+11).eq.'IPM3H00A.XRA') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_xraw_a
               if (numevent.le.2) write(6,*) ' ++  X(IPM3H00A.XRAW) = ',gepics_xraw_a,' mm'
               if(abs(gepics_xraw_a - haxraw).gt.0.1.and.beampos(1).lt.max_mess) then
                  write(6,*) ' !!! H00AXRAW is off! delta = ', gepics_xraw_a-haxraw, ' mm'
                  beampos(1) = beampos(1) + 1
               endif
            else if (buffer(i:i+11).eq.'IPM3H00A.YRA') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_yraw_a
               if (numevent.le.2) write(6,*) ' ++  Y(IPM3H00A.YRAW) = ',gepics_yraw_a,' mm'
               if(abs(gepics_yraw_a - hayraw).gt.0.1.and.beampos(2).lt.max_mess) then
                  write(6,*) ' !!! H00AYRAW is off! delta = ', gepics_yraw_a-hayraw, ' mm'
                  beampos(2) = beampos(2) + 1
               endif
            else if (buffer(i:i+11).eq.'IPM3H00B.XRA') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_xraw_b
               if (numevent.le.2) write(6,*) ' ++  X(IPM3H00B.XRAW) = ',gepics_xraw_b,' mm'
               if(abs(gepics_xraw_b - hbxraw).gt.0.1.and.beampos(3).lt.max_mess) then
                  write(6,*) ' !!! H00BXRAW is off! delta = ', gepics_xraw_b-hbxraw, ' mm'
                  beampos(3) = beampos(3) + 1
               endif
            else if (buffer(i:i+11).eq.'IPM3H00B.YRA') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_yraw_b
               if (numevent.le.2) write(6,*) ' ++  Y(IPM3H00B.YRAW) = ',gepics_yraw_b,' mm'
               if(abs(gepics_yraw_b - hbyraw).gt.0.1.and.beampos(4).lt.max_mess) then
                  write(6,*) ' !!! H00BYRAW is off! delta = ', gepics_yraw_b-hbyraw, ' mm'
                  beampos(4) = beampos(4) + 1
               endif
            else if (buffer(i:i+11).eq.'IPM3H00C.XRA') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_xraw_c
               if (numevent.le.2) write(6,*) ' ++  X(IPM3H00C.XRAW) = ',gepics_xraw_c,' mm'
               if(abs(gepics_xraw_c - hcxraw).gt.0.1.and.beampos(5).lt.max_mess) then
                  write(6,*) ' !!! H00CXRAW is off! delta = ', gepics_xraw_c-hcxraw, ' mm'
                  beampos(5) = beampos(5) + 1
               endif
            else if (buffer(i:i+11).eq.'IPM3H00C.YRA') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_yraw_c
               if (numevent.le.2) write(6,*)' ++  Y(IPM3H00C.YRAW) = ',gepics_yraw_c,' mm'
               if(abs(gepics_yraw_c - hcyraw).gt.0.1.and.beampos(6).lt.max_mess) then
                  write(6,*) ' !!! H00CYRAW is off! delta = ', gepics_yraw_c-hcyraw, ' mm'
                  beampos(6) = beampos(6) + 1
               endif
c               write(6,*) '-------------------'
c ********** read out the energy **********
            else if (buffer(i:i+11).eq.'MBSY3C_energ') then
               read(buffer(i+13:j-1),*,ERR=30) gepics_ebeam
               if (numevent.le.2) write(6,*) ' ++  E_beam      = ',gepics_ebeam,' MeV'
            else if (buffer(i:i+8).eq.'SoltarPOS') then
               hallc_event = .true.
               read(buffer(i+9:j-1),*,ERR=30) gepics_soltarpos
               if (numevent.le.2) write(6,*) ' ++  SoltarPOS   = ',gepics_soltarpos
               
*     Check target position***********************
               
            else if (buffer(i:i+5).eq.'BDSPOS') then
               read(buffer(i+6:j-1),*,ERR=30) gepics_bdspos
               if (numevent.le.2) then 
                  write(6,*) '++  BDSPOS      = ',gepics_bdspos
                  if((abs(gepics_bdspos-4100480.).lt.10.) .and. 
     >                 (gen_run_number.lt.18206)) then
                     epics_targ_num=11 !hydrogen
                  else if((abs(gepics_bdspos-2657121.).lt.10.) .and.
     >                    (gen_run_number.lt.18742)) then
                     epics_targ_num=11 !hydrogen
                  else if(abs(gepics_bdspos-1207646.).lt.10.) then
                     epics_targ_num=15 !deuterium
                  else if((abs(gepics_bdspos-2657121.).lt.10.) .and.
     >                    (gen_run_number.gt.18742)) then
                     epics_targ_num=13 !3He
                  else if(abs(gepics_bdspos+358896).lt.10.) then
                     epics_targ_num=17 !dummy
                  else if(abs(gepics_bdspos+287318).lt.10.) then
                     epics_targ_num=23 !Cz=-6
                  else if(abs(gepics_bdspos+204682).lt.10.) then
                     epics_targ_num=22 !Cz=-3
                  else if(abs(gepics_bdspos+122659).lt.10.) then
                     epics_targ_num=21 !Cz=3
                  else if(abs(gepics_bdspos+39101).lt.10.) then
                     epics_targ_num=20 !Cz=6
                  else if((abs(gepics_bdspos-39440).lt.10.) .or.
     >                    (abs(gepics_bdspos-70155).lt.10.)) then
                     epics_targ_num=19 !Cz=0
                  endif
                  if(abs(epics_targ_num-gtarg_num).gt.0.1) then
                     write(6,*) 'BADTARGET: gtarg_num= ',gtarg_num,
     >                    'epics_targ_num= ',epics_targ_num
                  endif
               endif
            else if (buffer(i:i+5).eq.'SMCPOS') then
               read(buffer(i+6:j-1),*,ERR=30) gepics_smcpos
               if (numevent.le.2) write(6,*) '++  SMCPOS      = ',gepics_smcpos
            else if (buffer(i:i+11).eq.'beam_current') then
               read(buffer(i+14:j-1),*,ERR=30) gepics_ibeam
               if (numevent.le.2) write(6,*) '++  I_beam      = ',gepics_ibeam,' microA'

* --- HMS magnets!!! ----              

* --- now check all the currents...
* --- note that since hpcentral_set is a positive quantity, we only check
* --- absolute values for now... 
            else if (buffer(i:i+11).eq.'echmsq1i_set') then
* --- now get the desired hms magnet currents from the set momentum...
               phms = hpcentral_set
               call h_field03(phms,iq1,iq2,iq3,id,bd) ! now calculate currents and fields

               read(buffer(i+14:j-1),*,ERR=30) hepics_iq1
               d_q1=(abs(hepics_iq1)-iq1)/iq1
               if (numevent.le.2) write(6,*)
     &              '++  I_Q1(set) = ',hepics_iq1
               if (abs(d_q1).gt.0.001 .and. iq1.ne.0.0.and.hpcentral_set>0.1
     &              .and.hms_off(1)<max_mess) then 
                  write(6,*)'!!!BADQ1: I_Q1(set) = ',abs(hepics_iq1),', I_Q1(field) = ',
     &                 iq1,', delta = ',
     &                 100.0*d_q1,' %, hpcentral_set = ',hpcentral_set
                  hms_off(1) = hms_off(1) + 1
               endif
               
            else if (buffer(i:i+11).eq.'echmsq2i_set') then
               read(buffer(i+14:j-1),*,ERR=30) hepics_iq2
               d_q2=(abs(hepics_iq2)-iq2)/iq2
               if (numevent.le.2) write(6,*)
     &              '++  I_Q2(set) = ',hepics_iq2
               if (abs(d_q2).gt.0.001 .and. iq2.ne.0.0 .and.hpcentral_set>0.1
     &              .and.hms_off(2)<max_mess) then 
                  write(6,*)'!!!BADQ2: I_Q2(set) = ',abs(hepics_iq2),', I_Q2(field) = ',
     &                 iq2,', delta = ',
     &                 100.0*d_q2,' %, hpcentral_set = ',hpcentral_set
                  hms_off(2) = hms_off(2) + 1
               endif
               
            else if (buffer(i:i+11).eq.'echmsq3i_set') then
               read(buffer(i+14:j-1),*,ERR=30) hepics_iq3
               d_q3=(abs(hepics_iq3)-iq3)/iq3
               if (numevent.le.2) write(6,*)
     &              '++  I_Q3(set) = ',hepics_iq3
               if (abs(d_q3).gt.0.001 .and.iq3.ne.0.0 .and. hpcentral_set>0.1
     &              .and.hms_off(3)<max_mess) then
                  write(6,*)'!!!BADQ3: I_Q3(set) = ',abs(hepics_iq3),', I_Q3(field) = ',
     &                 iq3,', delta = ',
     &                 100.0*d_q3,' %, hpcentral_set = ',hpcentral_set
                  hms_off(3) = hms_off(3) + 1
               endif
               
            else if (buffer(i:i+13).eq.'echmsdb_true_s') then
               read(buffer(i+14:j-1),*,ERR=30) hepics_bd
               d_d=(abs(hepics_bd)-bd)/bd
               if (numevent.le.2) write(6,*)
     &              '++  B_D(true) = ',hepics_bd
               if (abs(d_d).gt.0.001.and.hepics_bd.ne.0.0.and.hpcentral_set>0.1
     &              .and.hms_off(4)<max_mess) then
                  write(6,*)'!!!BADD: B_D(set) = ',abs(hepics_bd),', B_D(field) = ',
     &                 bd,', delta = ',
     &                 100.0*bd,' %, hpcentral_set = ',hpcentral_set
                  hms_off(4) = hms_off(4) + 1
               endif
               
c************************************************************************************
c now do the SOS magnets...

C------------- SOS Q
            else if (buffer(i:i+10).eq.'ecsosqp_set') then
               read(buffer(i+11:j-1),*,ERR=30) sepics_sp
               
            else if (buffer(i:i+11).eq.'ecsosqp_hall') then
               read(buffer(i+12:j-1),*,ERR=30) sepics_hp
               sd = (sepics_hp - sepics_sp)/sepics_hp
               if (numevent.le.2) write(6,*)
     &              '++  SOS q hall momentum = ',sepics_hp
               if (abs(sd).gt.0.001 .and. sepics_hp.ne.0.0.and.spcentral_set>0.1
     &              .and.sos_set_off(1)<max_mess) then
                  write(6,*)'!!! SOS quad is off! set momentum: ',sepics_sp,
     &                 ' GeV, hall probe momentum: ',sepics_hp, 
     &                 ' GeV, delta: ',100.0*sd,'%'            
                  sos_set_off(1) = sos_set_off(1) + 1
               endif
               sd = (spcentral_set - abs(sepics_sp))/spcentral_set
               if (abs(sd).gt.0.001 .and. sepics_sp.ne.0.0.and.spcentral_set>0.1
     &              .and.sos_off(1)<max_mess) then
                  write(6,*)'!!! SOS quad setpoint is off! set momentum: ',sepics_sp,
     &                 'GeV, spcentral_set: ',spcentral_set, 
     &                 ' GeV, delta: ',100.0*sd,'%'        
                  sos_off(1) = sos_off(1) + 1
               endif
C------------- SOS D1
            else if (buffer(i:i+11).eq.'ecsosd1p_set') then
               read(buffer(i+12:j-1),*,ERR=30) sepics_sp
            else if (buffer(i:i+12).eq.'ecsosd1p_hall') then
               read(buffer(i+13:j-1),*,ERR=30) sepics_hp
               sd = (sepics_hp - sepics_sp)/sepics_hp
               if (numevent.le.2) write(6,*)
     &              '++  SOS d1 hall momentum = ',sepics_hp
               if (abs(sd).gt.0.001 .and. sepics_hp.ne.0.0.and.spcentral_set>0.1
     &              .and.sos_set_off(2)<max_mess) then
                  write(6,*)'!!! SOS d1 is off! set momentum: ',sepics_sp,
     &                 ' GeV, hall probe momentum: ',sepics_hp, 
     &                 ' GeV, delta: ',100.0*sd,'%'            
                  sos_set_off(2) = sos_set_off(2) + 1
               endif
               sd = (spcentral_set - abs(sepics_sp))/spcentral_set
               if (abs(sd).gt.0.001 .and. sepics_sp.ne.0.0.and.spcentral_set>0.1
     &              .and.sos_off(2)<max_mess) then
                  write(6,*)'!!! SOS d1 setpoint is off! set momentum: ',sepics_sp,
     &                 'GeV, spcentral_set: ',spcentral_set, 
     &                 ' GeV, delta: ',100.0*sd,'%'               
                  sos_off(2) = sos_off(2) + 1
               endif
C------------- SOS D2
            else if (buffer(i:i+11).eq.'ecsosd2p_set') then
               read(buffer(i+12:j-1),*,ERR=30) sepics_sp
            else if (buffer(i:i+12).eq.'ecsosd2p_hall') then
               read(buffer(i+13:j-1),*,ERR=30) sepics_hp
               sd = (sepics_hp - sepics_sp)/sepics_hp
               if (numevent.le.2) write(6,*)
     &              '++  SOS d2 hall momentum = ',sepics_hp
               if (abs(sd).gt.0.001 .and. sepics_hp.ne.0.0.and.spcentral_set>0.1
     &              .and.sos_set_off(3)<max_mess) then
                  write(6,*)'!!! SOS d2 is off! set momentum: ',sepics_sp,
     &                 'GeV, hall probe momentum: ',sepics_hp, 
     &                 ' GeV, delta: ',100.0*sd,'%' 
                  sos_set_off(3) = sos_set_off(3) + 1
               endif
               sd = (spcentral_set - abs(sepics_sp))/spcentral_set
               if (abs(sd).gt.0.001 .and. sepics_sp.ne.0.0 .and.spcentral_set>0.1 
     &              .and.sos_off(3)<max_mess) then
                  write(6,*)'!!! SOS d2 setpoint is off! set momentum: ',sepics_sp,
     &                 'GeV, spcentral_set: ',spcentral_set, 
     &                 ' GeV, delta: ',100.0*sd,'%'               
                  sos_off(3) = sos_off(3) + 1
               endif
C-------------done
            endif               !picking out the variables
         endif                  !IF [this is a line of text]

 20      i = j + 1
      enddo
      return
      
 30   write(6,*) 'Epics file error - outta here!'

      return

    
      end



