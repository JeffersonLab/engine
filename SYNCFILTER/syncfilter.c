/* 
 *Dump out CODA event bank information
  Bad codes

  dcff0000       Not all modules converted.  Probable missing gate
  dcfe           Extra buffers at Sync event.  Each dcfe gives # extra events
                                               for a given slot
  dcee           ROC1 only missing BPM ADC data on SOS event.

  Assume for now any type 0 event is a sync event.  If that event has dcfe
  stuff in it, then all data back to the previous sync is suspect.

 */
#define DEBUG 0
#include <stdio.h>
#define OUTSYNC 1
#define INSYNC 0
#define EARLYEND 2
#define SYNCANALYSISFLAG 251
#define SCALERSYNC 0
#define TIMEDSYNC 16
#define GOEVENT 18

#define TYPE_BANK 0x10

#define MAX_EVENT_LEN 163840
#define MAX_REALLYBIGBUFFER 3000000

void list_banks(int evno, int *buffer);
void printbadstuff(int evno, int *buffer);
int problems(int evno, int *buffer);
void appendevent(int *really_big_buffer,int *buffer,int *endpointer);
void writeoutofsyncevent(int ohandle, int nev);
void writeoutthebigbuffer(int ohandle,int *really_big_buffer,int *endpointer,
			  int *evcounts);
void writeinsyncevent(int ohandle,int nev);
void writeearlyendevent(int ohandle,int nev);


int GetNextEvent(char *fname,int *handle,int *buffer) {
  int status;
  char *filename;
  char *xfilename;

  static char infileext[3]; 
  static int segno = -1;

  /*  
  If current value of HANDLE is -1, the file has not been opened yet
  so open it and start reading
  If read returns error, check segment number: if it's -1, then
  we're in single segment mode and the run has ended.  If segno
  is 0 or larger, we are in multi-segment mode and need to switch
  segments
  */
  
  if (*handle == -1) {    /* open first file */
     filename = (char *)malloc(strlen(fname)+1);
     strcpy(filename,fname);
         fprintf(stderr,"///syncfilter: filename %s\n",
	  filename);
     status = evOpen(filename,"r",handle);
     if(status==0) {
     	 segno = -1;
         fprintf(stderr,"///syncfilter: Processing singular input file %s\n",
	  filename);
     } else {
     	 segno = 0;
     	 infileext[0] = '.';
     	 infileext[1] = '0';
     	 infileext[2] = '\0';
     	 xfilename = (char *)malloc(strlen(fname)+3);
     	 strcpy(xfilename,fname);
     	 strcat(xfilename,infileext);    
     	 status=evOpen(xfilename,"r",handle);
     	 if(status!=0) {
             fprintf(stderr,"///syncfilter TROUBLE: ");
	     fprintf(stderr,"Error opening input file (%d)\n\n",status);
             fprintf(stderr,"///syncfilter: Processing input file segment %s\n\n",
	  xfilename);
	     free(xfilename);
     	     return(status);
     	 }
         fprintf(stderr,"///syncfilter: Processing input file segment %s\n\n",
	  xfilename);
	 free(xfilename);
     }
  }

  status=evRead(*handle,buffer,MAX_EVENT_LEN);
  if(status!=0 && segno>=0) {
        evClose(*handle);
     	segno++;
	infileext[1]++;
     	xfilename = (char *)malloc(strlen(fname)+3);
     	strcpy(xfilename,fname);
     	strcat(xfilename,infileext);
	status=evOpen(xfilename,"r",handle);
        if(status==0) {
           fprintf(stderr,"///syncfilter: Continuing with ");
	   fprintf(stderr,"input segment %d (%s)\n\n",segno,xfilename);
           status=evRead(*handle,buffer,MAX_EVENT_LEN);
	}
	free(xfilename);
  }
   
  if (status == 1) evClose(*handle);
	      
  return(status);
}



main(int argc, char **argv)
{
  int nevents,evtype,count,diff;
  int buffer[MAX_EVENT_LEN];
  /*  int really_big_buffer[MAX_REALLYBIGBUFFER];*/
  int *really_big_buffer;
  int endpointer;
  char err[500];
  int ihandle,ohandle,status;
  int pointer,evlen,evnum,datatype;
  int sumlen;
  int outofsync;
  int n_at_outofsync;
  int writeoutput;
  int goodevs[16],badevs[16],endevs[16];
  int i;
  int num_between_scalread;
  int num_event4;
  int nscalread;
  char *infilename;
 char *outfilename;

  really_big_buffer = (int *) malloc(MAX_REALLYBIGBUFFER*sizeof(int));
  writeoutput = 1;
  if(argc>1) {
     infilename = (char *)malloc(strlen(argv[1])+1); 
     strcpy(infilename,argv[1]);
     if(argc>2) {
        outfilename = (char *)malloc(strlen(argv[2])+1); 
        strcpy(outfilename,argv[2]);
     } else {
        outfilename = "-";
     }
  } else {
      infilename = "-";
      outfilename = "-";
  }
  ihandle=-1;
  if (DEBUG)  fprintf(stderr,"\n///syncfilter infilename %s ",infilename);
  if (DEBUG)  fprintf(stderr,"\n///syncfilter outfilename %s ",outfilename);
  
  status = evOpen(outfilename,"w",&ohandle);
  if(status!=0) {
    fprintf(stderr,"\n///syncfilter TROUBLE: couldn't open ");
    fprintf(stderr,"output file, scanning input for errors\n");
    status = evOpen("/dev/null","w",&ohandle);
  }

  for(i=0;i<16;i++) {
    goodevs[i] = badevs[i] = endevs[i] = 0; /* Initialize event counters */
  }

  nevents = 0;
  num_between_scalread=0;
  endpointer = 0;
  outofsync = 0;
  n_at_outofsync = 0;
  num_event4=0;
  nscalread=0;
  writeinsyncevent(ohandle,nevents);

    while ((status=GetNextEvent(infilename,&ihandle,buffer)) == 0) { 
    nevents++;
    sumlen = 1;
    evtype = buffer[1]>>16;
    evlen = buffer[0];
    datatype = (buffer[1] & 0xff00) >> 8;
    

    /*    if(!outofsync && problems(nevents,buffer)) {
      outofsync = 1;
      }*/
        if( problems(nevents,buffer) && !outofsync ) {
      outofsync = 1;
      } 

    if(evtype==4) {
      num_event4++;
      if(DEBUG) fprintf(stderr,"%d: %d %d\n",nevents,evtype,num_event4);
    }
    if(evtype==20) {
      fprintf(stderr,"END event found, number of events since last scaler read : %d\n",num_between_scalread);
      appendevent(really_big_buffer,buffer,&endpointer);
      break;
    }
    if(evtype==16 || evtype==0 || evtype==18 ) {
      if(nevents>1000) {
	if(DEBUG) fprintf(stderr,"%d: %d\n",nevents,evtype);
      }
      if ( num_event4 != 1000 && evtype==0 ) {
	fprintf(stderr,"Number of Scaler reads before 1000 event type 4 =%d\n",nscalread);
      }
      if (evtype==0) {
	nscalread++;
      }
      num_between_scalread=0;
      if(outofsync) {
	writeoutofsyncevent(ohandle,n_at_outofsync);
	evWrite(ohandle,&buffer[0]);
        writeoutthebigbuffer(ohandle,really_big_buffer,&endpointer,badevs);
	writeinsyncevent(ohandle,nevents);
	outofsync = 0;
      } else {
 	  if(evtype==0) {
          evWrite(ohandle,&buffer[0]);
	  writeoutthebigbuffer(ohandle,really_big_buffer,&endpointer,goodevs);
	  } else {
	  writeoutthebigbuffer(ohandle,really_big_buffer,&endpointer,goodevs);
          evWrite(ohandle,&buffer[0]);
          }
      }
      if(DEBUG) fprintf(stderr,"Events=%d\n",nevents);
      n_at_outofsync = nevents+1;
    } else {
      appendevent(really_big_buffer,buffer,&endpointer);
      num_between_scalread++;
    }
  }

  fprintf(stderr,"Done\n");
  if(endpointer) {
    if(evtype!=20) {
      writeearlyendevent(ohandle,n_at_outofsync);
      writeoutthebigbuffer(ohandle,really_big_buffer,&endpointer,endevs);
    } else if(outofsync) {
      fprintf(stderr,"Last buffer sent (out-of-sync)\n");
      writeoutofsyncevent(ohandle,n_at_outofsync);
      writeoutthebigbuffer(ohandle,really_big_buffer,&endpointer,badevs);
    } else {
      fprintf(stderr,"Last buffer sent (in-sync) \n");
      writeoutthebigbuffer(ohandle,really_big_buffer,&endpointer,goodevs);
    }

    if(DEBUG) fprintf(stderr,"Events=%d\n",nevents);
  }
  if (DEBUG) fprintf(stderr,"%d:%s\n",status,err);
  fprintf(stderr,"%d events in run\n",nevents);
  evClose(ihandle);
  evClose(ohandle); 

  for(i=0;i<16;i++) {
    fprintf(stderr,"%2d: %8d %8d %8d\n",i,goodevs[i],badevs[i],endevs[i]);
  }

  exit(0);
}
  
int problems(int evno, int *buffer)
{
  int len = buffer[0];
  int *end = buffer+len;
  int datatype = (buffer[1] & 0xff00) >> 8;
  int *pointer=buffer;
  int bankn=0;
  int result;
  int evprinted;
  
  result = 0;
  evprinted=0;
  if(datatype == TYPE_BANK){
    pointer += 2;
    while(pointer<end) {
      int roc = (*(pointer+1) >> 16)&0x1f;
      int len = *pointer;
      int i;
      bankn++;
      if(roc>0 && roc <=4) {
	for(i=2;i<=len;i++) {
	  if((pointer[i]&0xff000000) == 0xdc000000 ||
	     (pointer[i]&0xff000000) == 0xfb000000) {
	    result = 1;
	    if(!evprinted) {
	      fprintf(stderr,"Evno: %d\n",evno);
	      evprinted=0;
	    }
	    fprintf(stderr,"Roc: %d, Word: %d, %x\n",roc,i,pointer[i]);
	  }
	}
      }
      pointer += (*pointer + 1);
    }
  }
  return(result);
}
void appendevent(int *really_big_buffer,int *buffer,int *endpointer)
{
  int end;
  int i;
  int newendpointer;

  newendpointer = *endpointer + buffer[0]+1;
  if(newendpointer>MAX_REALLYBIGBUFFER) {
    fprintf(stderr,"Amount of data between sync's exceeds buffer size\n");
    exit(1);
  }
  for(i=0;i<=buffer[0];i++) {
    really_big_buffer[*endpointer+i] = buffer[i];
  }
  *endpointer = newendpointer;
}

void writeoutthebigbuffer(int ohandle,int *really_big_buffer,int *endpointer,
			  int *evcounts)
{
  int pointer;
  int evtype;
  if(DEBUG) {
    fprintf(stderr,"endpointer=%d\n",*endpointer);
    fprintf(stderr,"Writing a big bunch of stuff\n");
  }
  pointer = 0;
  while(pointer < *endpointer) {
    evtype = really_big_buffer[pointer+1]>>16;
    if(evtype < 16) {
      evcounts[evtype]++;
    }
    evWrite(ohandle,&really_big_buffer[pointer]);
    pointer += really_big_buffer[pointer]+1;
  }
  *endpointer=0;		/* Clear the buffer */
}

void writeoutofsyncevent(int ohandle,int nev)
{
  int evbuf[5];
  evbuf[0] = 4;
  evbuf[1] = (SYNCANALYSISFLAG<<16) + 0x10cc;
  evbuf[2] = 2;
  evbuf[3] = 1<<8;		/* Integer type */
  evbuf[4] = OUTSYNC;
  evWrite(ohandle,evbuf);
  fprintf(stderr,"SF: OUT OF SYNC at %d\n",nev);
}
void writeinsyncevent(int ohandle,int nev)
{
  int evbuf[5];
  evbuf[0] = 4;
  evbuf[1] = (SYNCANALYSISFLAG<<16) + 0x10cc;
  evbuf[2] = 2;
  evbuf[3] = 1<<8;		/* Integer type */
  evbuf[4] = INSYNC;
  evWrite(ohandle,evbuf);
  fprintf(stderr,"SF: IN OF SYNC at %d\n",nev);
}
void writeearlyendevent(int ohandle,int nev)
{
  int evbuf[5];
  evbuf[0] = 4;
  evbuf[1] = (SYNCANALYSISFLAG<<16) + 0x10cc;
  evbuf[2] = 2;
  evbuf[3] = 1<<8;		/* Integer type */
  evbuf[4] = EARLYEND;
  evWrite(ohandle,evbuf);
  fprintf(stderr,"END missing, adding flag at last SYNC: %d\n",nev);
}

