      SUBROUTINE h_track_tests
* 
*  Derek made this in Mar 1996
*
*  This routine delivers some handy tracking information.  It's divided
*  into three parts.  The first part looks at the chambers and their
*  efficiency.  The second part defines some scintillator tests to determine
*  whether the chambers should have fired.  The last part puts this info
*  into different files.  Also, if you want to look at the stub tests you
*  you can uncomment some lines in h_link_stubs.f to get that output.
*  A final note.  Many of these tests have similar counterparts in 
*  trackeff.test;  if you change something here, make sure it agrees with the
*  the tests there!!
*
* $Log$
* Revision 1.3  2002/09/26 14:50:10  jones
*    Add variables sweet1xscin,sweet1yscin,sweet2xscin,sweet2yscin
*    which record which scint got hit inside the defined scint region
*    Then hgoodscinhits is set to zero if front and back hodoscopes
*    are abs(sweet1xscin-sweet2xscin).gt.3 or bs(sweet1yscin-sweet2yscin).gt.2
*
* Revision 1.2  1996/09/04 13:39:02  saw
* (JRA) Treat logicals as logicals
*
* Revision 1.1  1996/05/01 20:24:29  saw
* Initial revision
*
       IMPLICIT NONE
       SAVE

       character*50 here
       parameter (here= 'H_TRACK_TESTS')

*       logical ABORT
*       character*(*) err
*       integer*4 ierr
*       character*5  line_err
*
       INCLUDE 'hms_data_structures.cmn'
       INCLUDE 'coin_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'hms_tracking.cmn'
       INCLUDE 'gen_units.par'
       INCLUDE 'gen_event_info.cmn'
       INCLUDE 'hms_scin_tof.cmn'
       INCLUDE 'hms_scin_parms.cmn'
       INCLUDE 'hms_calorimeter.cmn'
       include 'hms_bypass_switches.cmn'

       integer planetemp
       real*4 htestbeta
       integer txth,txthp,txthps,txthpss,txtft,txtct

       integer i,j,count
       integer testsum
       integer hhitsweet1x,hhitsweet1y,hhitsweet2x,hhitsweet2y
       integer sweet1xscin,sweet1yscin,sweet2xscin,sweet2yscin

       real*4 lastcointime
       real*4 thiscointime

*In c_keep_results, the cointime is updated depending on tracking information
*Because we're independent of tracking here, we have to do some tricks to
*update the cointime.  We set cointime=100.0 if the code hasn't updated it (ie,
*it's the same as the previous event...)  First, if it's not a coincidence
*event, we just set the cointime to zero.

       thiscointime=0.0
       if (gen_event_type.eq.3) then
          thiscointime=ccointime_hms
          if (thiscointime.eq.lastcointime) then
             thiscointime=100.0
          endif
          lastcointime=ccointime_hms
       endif


*this next file prints out events the fail to track and why.  You can then
*look at them with the event display to see if they're worrisome.  If you
*uncomment this line, be sure to uncomment the close statement at the end
*of this file!
       if (hbypass_track_eff_files.eq.0) then
          open(unit=12,file='scalers/htrackeff.txt',status='unknown',
     $         access='append')
       endif

*this next file outputs a huge ascii file with many tracking parameters.  It
*is intended for use with physica.  The order of the ouput is given in the write
*statement at the end of this file.  I fyou uncomment this line, be sure to
*uncomment the close statement at the end of this file!
       if (hbypass_track_eff_files.eq.0) then
          open(unit=14,file='scalers/htrack.out',status='unknown',
     $         access='append')
       endif

*we start by looking at the chambers.  First, we look to see if each plane fired

       h1hit1 = (HDC_HITS_PER_PLANE(1).GE.1)
       h1hit2 = (HDC_HITS_PER_PLANE(2).GE.1)
       h1hit3 = (HDC_HITS_PER_PLANE(3).GE.1)
       h1hit4 = (HDC_HITS_PER_PLANE(4).GE.1)
       h1hit5 = (HDC_HITS_PER_PLANE(5).GE.1)
       h1hit6 = (HDC_HITS_PER_PLANE(6).GE.1)
       h1hit7 = (HDC_HITS_PER_PLANE(7).GE.1)
       h1hit8 = (HDC_HITS_PER_PLANE(8).GE.1)
       h1hit9 = (HDC_HITS_PER_PLANE(9).GE.1)
       h1hit10 = (HDC_HITS_PER_PLANE(10).GE.1)
       h1hit11 = (HDC_HITS_PER_PLANE(11).GE.1)
       h1hit12 = (HDC_HITS_PER_PLANE(12).GE.1)

*next, we see how many hits per plane there were ...

       hnumhit1 = HDC_HITS_PER_PLANE(1)
       hnumhit2 = HDC_HITS_PER_PLANE(2)
       hnumhit3 = HDC_HITS_PER_PLANE(3)
       hnumhit4 = HDC_HITS_PER_PLANE(4)
       hnumhit5 = HDC_HITS_PER_PLANE(5)
       hnumhit6 = HDC_HITS_PER_PLANE(6)
       hnumhit7 = HDC_HITS_PER_PLANE(7)
       hnumhit8 = HDC_HITS_PER_PLANE(8)
       hnumhit9 = HDC_HITS_PER_PLANE(9)
       hnumhit10 = HDC_HITS_PER_PLANE(10)
       hnumhit11 = HDC_HITS_PER_PLANE(11)
       hnumhit12 = HDC_HITS_PER_PLANE(12)

       hnumhits1 = HDC_HITS_PER_PLANE(1) + HDC_HITS_PER_PLANE(2) +
     $      HDC_HITS_PER_PLANE(3) + HDC_HITS_PER_PLANE(4) +
     $      HDC_HITS_PER_PLANE(5) + HDC_HITS_PER_PLANE(6) 

       hnumhits2 = HDC_HITS_PER_PLANE(7) + HDC_HITS_PER_PLANE(8) +
     $      HDC_HITS_PER_PLANE(9) + HDC_HITS_PER_PLANE(10) +
     $      HDC_HITS_PER_PLANE(11) + HDC_HITS_PER_PLANE(12) 

*next we check to see if we have fewer than the max allowed hits per chamber
*this number should agree with the value in trackeff.test.

       h1hitslt = hnumhits1.LE.hmax_pr_hits(1)
       h2hitslt = hnumhits2.LE.hmax_pr_hits(2)

*next we check to see if we have the minimum number of planes per chamber
*this number should agree with the value in trackeff.test.

       planetemp = 0
       if(HDC_HITS_PER_PLANE(1).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(2).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(3).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(4).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(5).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(6).GE.1) planetemp = planetemp+1
       hnumplanes1 = planetemp
       planetemp = 0
       if(HDC_HITS_PER_PLANE(7).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(8).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(9).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(10).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(11).GE.1) planetemp = planetemp+1
       if(HDC_HITS_PER_PLANE(12).GE.1) planetemp = planetemp+1
       hnumplanes2 = planetemp

       h1planesgt = hnumplanes1.GE.hmin_hit(1)
       h2planesgt = hnumplanes2.GE.hmin_hit(2)


*we now fill in the chamber part of the track tests.

       hfoundtrack = (hntracks_fp.NE.0)
       if (hfoundtrack) then
          htestbeta=hsbeta
       else
          htestbeta=0.0
       endif
       hcleantrack = (hsnum_fptrack.NE.0)
*hhitslt is less than max allowed hits in both chambers
       hhitslt = h1hitslt.AND.h2hitslt
*hplanesgt is at least the minimum number of planes fired per chamber
       hplanesgt = h1planesgt.AND.h2planesgt
*hspacepoints is finding at least one space point in both chambers
       hspacepoints = ((hnspace_points(1).GE.1).AND.(hnspace_points(2).GE.1))
*hstublt is passing the stub criteria for at least one spacepoint in both chambers
       hstublt = (hstubtest.ne.0)
*hhitsplanes is passing not too many hits and not too few planes
       hhitsplanes = hhitslt.AND.hplanesgt
*hhitsplanes is that and finding a spacepoint
       hhitsplanessps = hhitsplanes.AND.hspacepoints
*hhitsplanesspsstubs is that and passing the stub tests
       hhitsplanesspsstubs = hhitsplanessps.AND.hstublt
*fXhspacepoints is pasisng htis and planes but failing to find a space point
       f1hspacepoints = h1hitslt.AND.h1planesgt.AND.(hnspace_points(1).EQ.0)
       f2hspacepoints = h2hitslt.AND.h2planesgt.AND.(hnspace_points(2).EQ.0)
       fhspacepoints = f1hspacepoints.OR.f2hspacepoints
       htest1 = (hhitsplanes.AND.(.not.hspacepoints))
       htest2 = (hspacepoints.AND.(.not.hstublt))

************************now look at some hodoscope tests
*second, we move the scintillators.  here we use scintillator cuts to see
*if a track should have been found.

       hnumscins1 = hscin_hits_per_plane(1)
       hnumscins2 = hscin_hits_per_plane(2)
       hnumscins3 = hscin_hits_per_plane(3)
       hnumscins4 = hscin_hits_per_plane(4)

*first, fill the arrays of which scins were hit
       do i=1,4
          do j=1,hscin_1x_nr
             hscinhit(i,j)=0
          enddo
       enddo
       do i=1,hscin_tot_hits
          hscinhit(hscin_plane_num(i),hscin_counter_num(i))=1
       enddo


*next, look for clusters of hits in a scin plane.  a cluster is a group of 
*adjacent scintillator hits separated by a non-firing scintillator.
*Wwe count the number of three adjacent scintillators too.  (A signle track 
*shouldn't fire three adjacent scintillators.
       do i=1,hnum_scin_planes
          hnclust(i)=0
          hthreescin(i)=0
       enddo

*look for clusters in x planes... (16 scins)  !this assume both x planes have same
*number of scintillators.
       do j=1,3,2
          count=0
          if (hscinhit(j,1).EQ.1) count=count+1
          do i=1,(hscin_1x_nr-1)         !look for number of clusters of 1 or more hits
             if ((hscinhit(j,i).EQ.0).AND.(hscinhit(j,i+1).EQ.1)) count=count+1
          enddo
          hnclust(j)=count
          count=0
          do i=1,(hscin_1x_nr-2)         !look for three or more adjacent hits
             if ((hscinhit(j,i).EQ.1).AND.(hscinhit(j,i+1).EQ.1).AND.
     $            (hscinhit(j,i+2).EQ.1)) count=count+1
          enddo
          if (count.GT.0) hthreescin(j)=1
       enddo
*look for clusters in y planes... (10 scins)  !this assume both y planes have same
*number of scintillators.
       do j=2,4,2
          count=0
          if (hscinhit(j,1).EQ.1) count=count+1
          do i=1,(hscin_1y_nr-1)         !look for number of clusters of 1 or more hits
             if ((hscinhit(j,i).EQ.0).AND.(hscinhit(j,i+1).EQ.1)) count=count+1
          enddo
          hnclust(j)=count
          count=0
          do i=1,(hscin_1y_nr-2)         !look for three or more adjacent hits
             if ((hscinhit(j,i).EQ.1).AND.(hscinhit(j,i+1).EQ.1).AND.
     $            (hscinhit(j,i+2).EQ.1)) count=count+1
          enddo
          if (count.GT.0) hthreescin(j)=1
       enddo

*now put some "tracking" like cuts on the hslopes, based only on scins...
*by "slope" here, I mean the difference in the position of scin hits in two
*like-planes.  For example, a track that those great straight through will 
*have a slope of zero.  If it moves one scin over from s1x to s2x it has an
*x-slope of 1...  I pick the minimum slope if there are multiple scin hits.
       hbestxpscin=100
       hbestypscin=100
       do i=1,hscin_1x_nr
          do j=1,hscin_1x_nr
             if ((hscinhit(1,i).EQ.1).AND.(hscinhit(3,j).EQ.1)) then
                hslope=abs(i-j)
                if (hslope.LT.hbestxpscin) hbestxpscin=hslope
             endif
          enddo
       enddo
       do i=1,hscin_1y_nr
          do j=1,hscin_1y_nr
             if ((hscinhit(2,i).EQ.1).AND.(hscinhit(4,j).EQ.1)) then
                hslope=abs(i-j)
                if (hslope.LT.hbestypscin) hbestypscin=hslope
             endif
          enddo
       enddo

*next we mask out the edge scintillators, and look at triggers that happened
*at the center of the acceptance.  To change which scins are in the mask
*change the values of h*loscin and h*hiscin in htracking.param
       hhitsweet1x=0
       hhitsweet1y=0
       hhitsweet2x=0
       hhitsweet2y=0
       hgoodscinhits=0
*first x plane.  first see if there are hits inside the scin region
       do i=hxloscin(1),hxhiscin(1)
          if (hscinhit(1,i).EQ.1) then
             hhitsweet1x=1
             sweet1xscin=i
          endif
       enddo
*  next make sure nothing fired outside the good region
       do i=1,hxloscin(1)-1
          if (hscinhit(1,i).EQ.1) hhitsweet1x=-1
       enddo
       do i=hxhiscin(1)+1,hscin_1x_nr
          if (hscinhit(1,i).EQ.1) hhitsweet1x=-1
       enddo
*second x plane.  first see if there are hits inside the scin region
       do i=hxloscin(2),hxhiscin(2)
          if (hscinhit(3,i).EQ.1) then
             hhitsweet2x=1
             sweet2xscin=i
          endif
       enddo
*  next make sure nothing fired outside the good region
       do i=1,hxloscin(2)-1
          if (hscinhit(3,i).EQ.1) hhitsweet2x=-1
       enddo
       do i=hxhiscin(2)+1,hscin_2x_nr
          if (hscinhit(3,i).EQ.1) hhitsweet2x=-1
       enddo

*first y plane.  first see if there are hits inside the scin region
       do i=hyloscin(1),hyhiscin(1)
          if (hscinhit(2,i).EQ.1) then
             hhitsweet1y=1
             sweet1yscin=i
          endif
       enddo
*  next make sure nothing fired outside the good region
       do i=1,hyloscin(1)-1
          if (hscinhit(2,i).EQ.1) hhitsweet1y=-1
       enddo
       do i=hyhiscin(1)+1,hscin_1y_nr
          if (hscinhit(2,i).EQ.1) hhitsweet1y=-1
       enddo
*second y plane.  first see if there are hits inside the scin region
       do i=hyloscin(2),hyhiscin(2)
          if (hscinhit(4,i).EQ.1) then
             hhitsweet2y=1
             sweet2yscin=i
          endif
       enddo
*  next make sure nothing fired outside the good region
       do i=1,hyloscin(2)-1
          if (hscinhit(4,i).EQ.1) hhitsweet2y=-1
       enddo
       do i=hyhiscin(2)+1,hscin_2y_nr
          if (hscinhit(4,i).EQ.1) hhitsweet2y=-1
       enddo

       testsum=hhitsweet1x+hhitsweet1y+hhitsweet2x+hhitsweet2y
* now define a 3/4 or 4/4 trigger of only good scintillators the value
* is specified in htracking.param...
       if (testsum.GE.htrack_eff_test_num_scin_planes) hgoodscinhits=1

* require front/back hodoscopes be close to each other
       if (hgoodscinhits.eq.1 .and. htrack_eff_test_num_scin_planes.eq.4) then
          if (abs(sweet1xscin-sweet2xscin).gt.3) hgoodscinhits=0
          if (abs(sweet1yscin-sweet2yscin).gt.2) hgoodscinhits=0
       endif

*******************************************************************************
*     Here's where we start writing to the files.  Uncomment these lines and
*     the corresponding file open and close lines at the beginning and end
*     of this file if you want this output.  the scaler report should take
*     care of most people though...

*
       if (hbypass_track_eff_files.eq.0) then
          if (hgoodscinhits.EQ.1) then
             write(12,*) 'sweet spot hit, event number           ',gen_event_ID_number
          endif
          if (.not.hhitslt) then
             write(12,*) 'too many hits, event number           ',gen_event_ID_number
          endif
          if (.not.hplanesgt) then
             write(12,*)  'too few planes event number                    ',
     $            gen_event_ID_number
          endif
          if (hhitsplanes.AND.(.not.hspacepoints)) then
             write(12,*) 'p hits/planes, f sp # = ',gen_event_ID_number
          endif
          if ((.not.hfoundtrack).AND.hhitsplanessps) then
             write(12,*) 'p hits/planes/sps, f track # = ',gen_event_ID_number
          endif
          if (hspacepoints.AND.(.not.hstublt)) then
             write(12,*) 'p sp, f stubs # = ',gen_event_ID_number
          endif
       endif

       
*the rest of this file prepares the output of htrack.out.  If you're not
*writing to that file, don't worry about this.

       if (hbypass_track_eff_files.eq.0) then
          txth=0
          if (hhitslt) txth=1
          txthp=0
          if (hhitsplanes) txthp=1
          txthps=0
          if (hhitsplanessps) txthps=1
          txthpss=0
          if (hhitsplanesspsstubs) txthpss=1        
          txtft=0
          if (hfoundtrack) txtft=1
          txtct=0
          if (hcleantrack) txtct=1
          
          write(14,902) gen_event_ID_number,hnumhits1,hnumhits2,
     $         hnumhit1,hnumhit2,hnumhit3,hnumhit4,
     $         hnumhit5,hnumhit6,hnumhit7,hnumhit8,
     $         hnumhit9,hnumhit10,hnumhit11,hnumhit12,
     $         hnumplanes1,hnumplanes2,hnumscins1,hnumscins2,
     $         hnumscins3,hnumscins4,hnclust(1),hnclust(2),
     $         hnclust(3),hnclust(4),hthreescin(1),hthreescin(2),
     $         hthreescin(3),hthreescin(4),hbestxpscin,hbestypscin,
     $         hgoodscinhits,
     $         txtft,txtct,hntracks_fp,hbeta_notrk,htestbeta,hcal_et,
     $         hsshtrk,
     $         hcer_npe_sum,hschi2perdeg,hsdelta,thiscointime
 902      format(1x,i6,i4,i4,12(i4),2(i2),4(i3),8(i2),2(i4),i2,i2,i2,i2,f10.3,f9.3,
     $         f9.3,f9.3,f9.3,f10.3,f10.3,f10.3)
          
          close(12)
          close(14)
      endif
      end
