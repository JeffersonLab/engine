      SUBROUTINE G_UTC_date(UTC_time,date,dy,mth,yr,hr,minute,sec)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Convert UTC time (seconds since 1 Jan 1970)
*-                         into GMT time & date 
*- 
*-   Inputs  : UTC_time	- UTC time in seconds
*-   Outputs : date	- time&date string
*-             dy	- day (1-31)
*-             mth	- month (1-12)
*-             yr	- year (1970-)
*-             hr	- hr (0-23)
*-             minute	- minute. (0-59)
*-             sec	- sec. (0-59)
*- 
*-   Created 27-Apr-1992   Kevin B. Beard
* $Log: g_utc_date.f,v $
* Revision 1.2  1995/03/21 15:35:10  cdaq
* (SAW) Replace variable min with minute
*
* Revision 1.1  1994/05/27  16:45:09  cdaq
* Initial revision
*
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
      INTEGER UTC_time,dy,mth,yr,hr,minute,sec
      CHARACTER*(*) date
*
      integer hr_per_day,min_per_hr,sec_per_min,sec_per_day
      parameter (hr_per_day= 24)
      parameter (min_per_hr= 60)
      parameter (sec_per_min= 60)
      parameter (sec_per_day= hr_per_day*min_per_hr*sec_per_min)
*
      integer JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC
      parameter (JAN= 1)
      parameter (FEB= 2)
      parameter (MAR= 3)
      parameter (APR= 4)
      parameter (MAY= 5)
      parameter (JUN= 6)
      parameter (JUL= 7)
      parameter (AUG= 8)
      parameter (SEP= 9)
      parameter (OCT= 10)
      parameter (NOV= 11)
      parameter (DEC= 12)
      character*3 month(JAN:DEC)
      integer days_per_month(JAN:DEC)
*
      integer mon,time
      integer sec_year,sec_mon,sec_nonleap,sec_leap,iv(6)
      real rv
      logical first_call,leap_year
      data first_call/.TRUE./
*
*----------------------------------------------------------------------
*
      IF(first_call) THEN
	first_call= .FALSE.
        month(JAN)= 'Jan'
	month(FEB)= 'Feb'
	month(MAR)= 'Mar'
	month(APR)= 'Apr'
	month(MAY)= 'May'
	month(JUN)= 'Jun'
	month(JUL)= 'Jul'
	month(AUG)= 'Aug'
	month(SEP)= 'Sep'
	month(OCT)= 'Oct'
	month(NOV)= 'Nov'
	month(DEC)= 'Dec'
        days_per_month(JAN)= 31
        days_per_month(FEB)= 28
        days_per_month(MAR)= 31
        days_per_month(APR)= 30
        days_per_month(MAY)= 31
        days_per_month(JUN)= 30
        days_per_month(JUL)= 31
        days_per_month(AUG)= 31
        days_per_month(SEP)= 30
        days_per_month(OCT)= 31
        days_per_month(NOV)= 30
        days_per_month(DEC)= 31
	sec= 0
	do mon= JAN,DEC
	  sec= sec + 
     &          days_per_month(mon)*sec_per_day
          enddo
        sec_nonleap= sec
        sec_leap= sec_nonleap + sec_per_day
      ENDIF
*
*-year
      time= UTC_time
      leap_year= .FALSE.
      yr= 1970
      IF(time.GE.0) THEN
        sec_year= sec_nonleap
        Do while(time.GE.sec_year)
          time= time-sec_year
          yr= yr+1
          leap_year= MOD(yr,4).EQ.0
          if(leap_year) then
            sec_year= sec_leap
          else
            sec_year= sec_nonleap
          endif
        EndDo
      ELSE
        Do while(time.LT.0) 
          yr= yr-1
          leap_year= MOD(yr,4).EQ.0
          if(leap_year) then
            sec_year= sec_leap
          else
            sec_year= sec_nonleap
          endif
          time= time+sec_year
        EndDo
      ENDIF
*
*-month
      mth= JAN                                !determine the month
      sec_mon= days_per_month(mth)*sec_per_day
      DO WHILE(time.GE.sec_mon .AND. mth.LT.DEC)
        time= time - sec_mon
	mth= mth+1
        sec_mon= days_per_month(mth)*sec_per_day
        If(mth.EQ.FEB .and. leap_year) Then
          sec_mon= sec_mon + sec_per_day
        EndIf
      ENDDO
*
*-day
      dy= INT(time/sec_per_day)+1
      time= time - (dy-1)*sec_per_day
*-hr
      hr= INT(time/(min_per_hr*sec_per_min))
      time= time - hr*min_per_hr*sec_per_min
*-minute
      minute= INT(time/sec_per_min)
      time= time - minute*sec_per_min
*-sec
      sec= time
*
      iv(1)= dy
      iv(2)= yr
      iv(3)= hr
      iv(4)= minute
      iv(5)= sec
      call G_build_note('#-'//month(mth)//'-# #:#:# GMT',
     &                                    '#',iv,' ',rv,' ',date)
*
      RETURN
      END
