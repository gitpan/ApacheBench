/* ====================================================================
 * Copyright (c) 1998-1999 The Apache Group.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. All advertising materials mentioning features or use of this
 *    software must display the following acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * 4. The names "Apache Server" and "Apache Group" must not be used to
 *    endorse or promote products derived from this software without
 *    prior written permission. For written permission, please contact
 *    apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * 6. Redistributions of any form whatsoever must retain the following
 *    acknowledgment:
 *    "This product includes software developed by the Apache Group
 *    for use in the Apache HTTP server project (http://www.apache.org/)."
 *
 * THIS SOFTWARE IS PROVIDED BY THE APACHE GROUP ``AS IS'' AND ANY
 * EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE APACHE GROUP OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Group and was originally based
 * on public domain software written at the National Center for
 * Supercomputing Applications, University of Illinois, Urbana-Champaign.
 * For more information on the Apache Group and the Apache HTTP server
 * project, please see <http://www.apache.org/>.
 *
 */

/*
   ** This program is based on ZeusBench V1.0 written by Adam Twiss
   ** which is Copyright (c) 1996 by Zeus Technology Ltd. http://www.zeustech.net/
   **
   ** This software is provided "as is" and any express or implied waranties,
   ** including but not limited to, the implied warranties of merchantability and
   ** fitness for a particular purpose are disclaimed.  In no event shall
   ** Zeus Technology Ltd. be liable for any direct, indirect, incidental, special,
   ** exemplary, or consequential damaged (including, but not limited to,
   ** procurement of substitute good or services; loss of use, data, or profits;
   ** or business interruption) however caused and on theory of liability.  Whether
   ** in contract, strict liability or tort (including negligence or otherwise)
   ** arising in any way out of the use of this software, even if advised of the
   ** possibility of such damage.
   **
 */

/*
   ** HISTORY:
   **    - Originally written by Adam Twiss <adam@zeus.co.uk>, March 1996
   **      with input from Mike Belshe <mbelshe@netscape.com> and
   **      Michael Campanella <campanella@stevms.enet.dec.com>
   **    - Enhanced by Dean Gaudet <dgaudet@apache.org>, November 1997
   **    - Cleaned up by Ralf S. Engelschall <rse@apache.org>, March 1998
   **    - POST and verbosity by Kurt Sussman <kls@merlot.com>, August 1998
   **    - HTML table output added by David N. Welton <davidw@prosa.it>, January 1999
   **    - Added Cookie, Arbitrary header and auth support. <dirkx@webweaving.org>, April 1999
   **
 */

/*
 * BUGS:
 *
 * - uses strcpy/etc.
 * - has various other poor buffer attacks related to the lazy parsing of
 *   response headers from the server
 * - doesn't implement much of HTTP/1.x, only accepts certain forms of
 *   responses
 * - (performance problem) heavy use of strstr shows up top in profile
 *   only an issue for loopback usage
 */




/*  -------------------------------------------------------------------- */

/* affects include files on Solaris */
#define BSD_COMP

/* allow compilation outside an Apache build tree */
#ifdef NO_APACHE_INCLUDES
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <string.h>

#define ap_select       select
#else				/* (!)NO_APACHE_INCLUDES */
#include "ap_config.h"
#include "ap.h"
#ifdef CHARSET_EBCDIC
#include "ebcdic.h"
#endif
#include <fcntl.h>
#ifndef MPE
#include <sys/time.h>
#endif

#ifndef NO_WRITEV
#include <sys/types.h>
#include <sys/uio.h>
#endif

#endif				/* NO_APACHE_INCLUDES */
/* ------------------- DEFINITIONS -------------------------- */

#define HEADERSIZE       512
#define STATE_DONE 	  1
#define STATE_READY 	  0
#define RUN_PRIORITY	  1
#define EQUAL_OPPORTUNITY 0
#define DEPTH_FIRST	  1
#define BREADTH_FIRST	  0

 struct connection {
   int fd;
   int state;
   int which;			/* which url are we testing */
   int read;			/* amount of bytes read */
   int bread;			/* amount of body read */
   char cbuff[HEADERSIZE];	/* a buffer to store server response header */
   int cbx;			/* offset in cbuffer */
   int gotheader;		/* non-zero if we have the entire header in
   * cbuff */
   int thread;                 /* Thread number */
   int run;
   struct timeval start, connect, done;
   char *page_content;
   char *header;
 };

 struct data {
   int run;			/* which run */
   int thread; 		/* Thread number */	
   int read;			/* number of bytes read */
   int ctime;			/* time in ms to connect */
   int time;			/* time in ms for connection */
   char *page_content;
   char *header;
 };

 struct threadval {
   int run;			/* which run */
   int which;			/* which url are we testing */
   int thread; 		/* Thread number */
 };
#define ap_min(a,b) ((a)<(b))?(a):(b)
#define ap_max(a,b) ((a)>(b))?(a):(b)


/* --------------------- GLOBALS ---------------------------- */

int *posting;   		/* GET if posting[]=0 */
int concurrency = 1;		/* Number of multiple requests to make */
int *repeats;			/* Number of time to repeat for each run */
int repeat;			/* default repeat number if not specifid */
int requests = 0;		/* the max of the repeats */
int *position;			/* The position next run starts */
char **servername;		/* name that server reports */
char **hostname; 		/* host name */
char **path;			/* path name */
char **postdata, **cookie;	/* datas for post and optional cookie line */
int *postlen;   		/* length of data to be POSTed */
int *port;			/* port numbers */

int *doclen;			/* the length the document should be */
int *totalread; 		/* total number of bytes read */
int *totalbread;		/* totoal amount of entity body read */
int *totalposted;		/* total number of bytes posted, inc. headers*/
int *good, *failed;		/* number of good and bad requests */
int *started, *finished,*arranged;
				/* numbers of requests  started , */ 
				/* finished or arranged for each url*/
int **which_thread;		/* which thread is available */ 
struct threadval *ready_to_run_queue;
int head, tail = 0, done = 0, need_to_be_done = 0;
int priority;
int *order;
char warn_and_error[2048]={"\nWarning message from ab."};
		/*warn and error message returned to perl*/
int *filesize;
int number_of_urls, number_of_runs;
int total_bytes_received = 0;
char **page_content;

/* XS library */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

/* store error cases */
int err_length = 0, err_conn = 0, err_except = 0;


struct timeval start, endtime;

/* global request (and its length) */
char request[512];
int reqlen;

/* one global throw-away buffer to read stuff into */
char buffer[8192];


struct connection *con;		/* connection array */
struct data **stats;		/* date for each request */

fd_set readbits, writebits;	/* bits for select */
struct sockaddr_in server;	/* server addr structure */

#ifndef BEOS
#define ab_close(s) close(s)
#define ab_read(a,b,c) read(a,b,c)
#define ab_write(a,b,c) write(a,b,c)
#else
#define ab_close(s) closesocket(s)
#define ab_read(a,b,c) recv(a,b,c,0)
#define ab_write(a,b,c) send(a,b,c,0)
#endif

/* --------------------------------------------------------- */

/* keep warn and error massege */

void myerr(char *s)
{
    if((strlen(warn_and_error)+strlen(s))<2014){
            strcat(warn_and_error,"\n[Warn:] ");
            strcat(warn_and_error,s);}
    else if(strlen(warn_and_error)<2014)
	strcat(warn_and_error,"\nToo much warn and error message!");
}

/* --------------------------------------------------------- */

/* write out request to a connection - assumes we can write
   (small) request out in one go into our new socket buffer  */

static void write_request(struct connection * c)
{
   
#ifndef NO_WRITEV
    struct iovec out[2]; int outcnt = 1;
#endif
    gettimeofday(&c->connect, 0);
    reset_request(c->which,c->run);
#ifndef NO_WRITEV
    out[0].iov_base = request;
    out[0].iov_len = reqlen;

    if (posting[c->which] > 0) {
	out[1].iov_base = postdata[c->which];
	out[1].iov_len = postlen[c->which];
	outcnt = 2;
	totalposted[c->which] += (reqlen + postlen[c->which]);
    }
    writev(c->fd,out, outcnt);
#else
    ab_write(c->fd,request,reqlen);
    if (posting[c->which]>0) {
        ab_write(c->fd,postdata[c->which],postlen[c-.which]);
        totalposted[c->which] += (reqlen + postlen[c->which]);
    }
#endif
    c->page_content = calloc(1,filesize[c->run]);
    FD_SET(c->fd, &readbits);
    FD_CLR(c->fd, &writebits); 

}

/* --------------------------------------------------------- */

/* make an fd non blocking */

static void nonblock(int fd)
{
    int i = 1;
#ifdef BEOS
    setsockopt(fd, SOL_SOCKET, SO_NONBLOCK, &i, sizeof(i));
#else
    ioctl(fd, FIONBIO, &i);
#endif
}

/* --------------------------------------------------------- */

/* returns the time in ms between two timevals */

static int timedif(struct timeval a, struct timeval b)
{
    register int us, s;

    us = a.tv_usec - b.tv_usec;
    us /= 1000;
    s = a.tv_sec - b.tv_sec;
    s *= 1000;
    return s + us;
}


/* --------------------------------------------------------- */
/* declare close_connection to make it accessible to start_connect */

static void close_connection(struct connection * );

/* --------------------------------------------------------- */

/* start asnchronous non-blocking connection */

static void start_connect(struct connection * c)
{
    c->read = 0;
    c->bread = 0;
    c->cbx = 0;
    c->gotheader = 0;
    c->fd = socket(AF_INET, SOCK_STREAM, 0);

    if (c->fd < 0) {
	myerr("socket error");
	good[c->which]++;
	close_connection(c);
	return;
	}
    nonblock(c->fd);

    gettimeofday(&c->start, 0);

   {
	/* get server information */
	struct hostent *he;
	he = gethostbyname(hostname[c->which]);
	if (!he){
		char * warn = malloc(1024);
		sprintf(warn,"Bad:hostname: %s,the information stored for it could be wrong!",hostname[c->which]);
		myerr(warn);
		free(warn);
		/* bad hostname, yields the resourse */
		good[c->which]++;
		close_connection(c);
		return;
		}
	server.sin_family = he->h_addrtype;
	server.sin_port = htons(port[c->which]);
	server.sin_addr.s_addr = ((unsigned long *) (he->h_addr_list[0]))[0]; 
    }

    if (connect(c->fd, (struct sockaddr *) & server, sizeof(server)) < 0) {
	if (errno == EINPROGRESS) {
	    FD_SET(c->fd, &writebits);
	    started[c->which]++;
	    return;
	}
	else {
	    ab_close(c->fd);
	    err_conn++;
	    if (failed[c->which]++ > 10) {
		myerr("\nTest aborted after 10 failures\n\n");
		/* yields the resourse */
		good[c->which]++;
		close_connection(c);
		return;
	    }
	    start_connect(c);
	    return;
	}
    }

    /* connected first time */
    started[c->which]++;
    FD_SET(c->fd, &writebits);
  
}

/* --------------------------------------------------------- */

/* close down connection and save stats */

static void close_connection(struct connection * c)
{
    if(c->read >= filesize[c->run])
	myerr("Filesize is too small,only part of the page stored!");
    if(c->read == 0)
	c->page_content = c-> header = servername[c->which] = "";
    if (good[c->which] == 1) {
	/* first time here */
	doclen[c->which] = c->bread;	
	page_content[c->which] = c->page_content;
	}
    else if (c->bread != doclen[c->which]) {
	    char *warn = malloc(1024);
	    sprintf(warn,
	    "Contents from http://%s%s dosen't match the privous try.",
	    hostname[c->which],path[c->which]);
	    myerr(warn);
	    free(warn);
	    failed[c->which]++;
	    err_length++;
	    }
	 else { if(c->read)free(c->page_content);
		c->page_content = page_content[c->which];}

    /* save out time */
    {
	struct data s;
	gettimeofday(&c->done, 0);
	s.read = c->read;
	s.ctime = timedif(c->connect, c->start);
	s.time = timedif(c->done, c->start);
        s.thread = c->thread;
	s.page_content = c->page_content;
        s.header = c->header;
        stats[c->which][c->thread] = s;
    }

    total_bytes_received += c->read;
    finished[c->which]++;
    ab_close(c->fd);
    FD_CLR(c->fd, &readbits);
    FD_CLR(c->fd, &writebits); 

    if(++done >= need_to_be_done) return;

    if(priority == RUN_PRIORITY){
      if(started[position[c->run + 1] - 1] >= repeats[c->run]){
	c->run++;
	while(c->run < number_of_runs ){
	  if(started[position[c->run + 1] - 1] >= repeats[c->run] ||
	     (order[c->run] == DEPTH_FIRST && started[position[c->run]] > 0)){
		/* this run dosen't need this resource anymore */
		c->run++;
		continue;
		}
	  c->which = position[c->run];
	  if(started[c->which] < repeats[c->run]) {
		/* for breadth first, starting one more connect to the 
		the first url if  possible
		for depth_first, get started here, */
	    c->thread = which_thread[c->which][started[c->which]];
	    start_connect(c);
	    return;
		}
	while(++c->which < position[c->run + 1] && 
		started[c->which] >= repeats[c->run]);
		/* found the first one which hasn't started completely */
	if(started[c->which] < finished[c->which - 1]){
		/* if the privious urls finished, started this one */
	   c->thread = which_thread[c->which][started[c->which]];
	   start_connect(c);
	   return;
		}
		/* this run dosen't need any more resources */
	 else c->run++;
	}
	/* no one needs any more resources */
	c->state = STATE_DONE;
	return;
	}
      else { /* possible more resource needed in this group */
	/* started[position[c->run + 1] - 1] is less than repeats[c->run] */
	if(order[c->run] == DEPTH_FIRST){ 
		/* for depth_first,connect the next one */
	  if(++c->which == (position[c->run + 1])){
	     c->which = position[c->run];
	     c->thread = started[c->which];
	   }
	  start_connect(c);
	  return;
	}
	else { /* breadth_first */
	  if(c->which < (position[c->run + 1] - 1))
		which_thread[c->which+1][finished[c->which] - 1] = c->thread;
	  if(started[c->which] == repeats[c->run])c->which++;
	  if(c->which == position[c->run]){
		c->thread = which_thread[c->which][started[c->which]];
		start_connect(c);
		return;}
	  if(started[c->which] < finished[c->which - 1]){
		c->thread = started[c->which];
		start_connect(c);
		return; 
		}
	  else {
		/*  this group doesn't really need any more sources */
	    c->run++;
	    while(c->run < number_of_runs ){
	      if(started[position[c->run + 1] - 1] == repeats[c->run] ||
	      (order[c->run] == DEPTH_FIRST && started[position[c->run]] > 0)){
		 c->run++;
		 continue;
		}
      	      c->which = position[c->run];
	      if(started[c->which] < repeats[c->run]) {
		c->thread = which_thread[c->which][started[c->which]];
		start_connect(c);
		return;
		}
	      while(++c->which < position[c->run + 1] && 
		started[c->which] >= repeats[c->run]);
	      if(started[c->which] < finished[c->which - 1]){
		c->thread = which_thread[c->which][started[c->which]];
		start_connect(c);
		return;
		}
	      c->run++;
		}
	    c->state = STATE_DONE;
	    return;
		}
	}
	}
	}
    else{ /* equal_opportunity */

	if(c->which < position[c->run + 1]-1){
	     ready_to_run_queue[tail].which = c->which + 1;
	     ready_to_run_queue[tail].thread = c->thread;
	     ready_to_run_queue[tail++].run = c->run;
	     arranged[c->which + 1]++;
		}
	else if (( order[c->run] == DEPTH_FIRST ) && 
	       (arranged[position[c->run]] < repeats[c->run])){
		ready_to_run_queue[tail].which = position[c->run];
		ready_to_run_queue[tail].thread = arranged[position[c->run]]++;
		ready_to_run_queue[tail++].run = c->run;
		}

	if(head >= tail) {c->state = STATE_DONE;return;}
    	c->thread = ready_to_run_queue[head].thread;
    	c->which = ready_to_run_queue[head].which;
    	c->run = ready_to_run_queue[head++].run;
    	start_connect(c);
    	return;
	}
}

/* --------------------------------------------------------- */

/* read data from connection */

static void read_connection(struct connection * c)
{
    int r;
    r = ab_read(c->fd, buffer, sizeof(buffer));	
    if (r == 0 || (r < 0 && errno != EAGAIN)) {
	good[c->which]++;
	close_connection(c);
	return;
    }

    if (r < 0 && errno == EAGAIN)return;
    c->read += r;
    totalread[c->which] += r;
    if(c->read <filesize[c->run]-1)strncat(c->page_content,buffer,r);
    if (!c->gotheader) {
	char *s;
	int l = 4;
	int tocopy = HEADERSIZE - c->cbx - 1;	/* -1 to allow for 0
						 * terminator */
	tocopy = ap_min(tocopy,r);
#ifndef CHARSET_EBCDIC
	memcpy(c->cbuff + c->cbx, buffer, tocopy);
#else				/* CHARSET_EBCDIC */
	ascii2ebcdic(c->cbuff + c->cbx, buffer, tocopy);
#endif				/* CHARSET_EBCDIC */
	c->cbx += tocopy;
	c->cbuff[c->cbx] = 0;	/* terminate for benefit of strstr */
	s = strstr(c->cbuff, "\r\n\r\n");
	/*
	 * this next line is so that we talk to NCSA 1.5 which blatantly
	 * breaks the http specification
	 */
	if (!s) {
	    s = strstr(c->cbuff, "\n\n");
	    l = 2;
	}
	if (!s) {
	    /*read rest next time */
		c->header = "";
		servername[c->which] = "";
		return;
	}
	else {
	    /* have full header */
	    if (!good[c->which]) {
		/* this is first time, extract some interesting info */
		char *p, *q;
		p = strstr(c->cbuff, "Server:");
		q = servername[c->which] = malloc(HEADERSIZE);
		if (p) {
		    p += 8;
		    while ((*p != '\n')&&(*p!='\t')&&(*p!='\r'))
			*q++ = *p++;
		}
		*q = 0;
	    }

	    /*
	     * XXX: this parsing isn't even remotely HTTP compliant... but in
	     * the interest of speed it doesn't totally have to be, it just
	     * needs to be extended to handle whatever servers folks want to
	     * test against. -djg
	     */


	    c->gotheader = 1;
	    *s = 0;		/* terminate at end of header */
            c->header = malloc(HEADERSIZE);
   	    strcpy(c->header, c->cbuff);
	    c->bread += c->cbx - (s + l - c->cbuff) + r - tocopy;
	    totalbread[c->which] += c->bread;
	}
    }
    else {
	/* outside header, everything we have read is entity body */
	c->bread += r;
	totalbread[c->which] += r;
    }
    

}

/* --------------------------------------------------------- */

    /* setup or reset request */
int
reset_request(int i,int j)
{
    if (posting[i] <= 0) {
	sprintf(request, "%s %s HTTP/1.0\r\n"
		"User-Agent: ApacheBench/%s\r\n"
		"%s"  
		"Host: %s\r\n"
		"Accept: */*\r\n"
		 "\r\n",
		(posting[i] == 0) ? "GET" : "HEAD",
		path[i],
		VERSION,
		cookie[j], hostname[i]);
    }
    else {
	sprintf(request, "POST %s HTTP/1.0\r\n"
		"User-Agent: ApacheBench/%s\r\n"
		"%s"  
		"Host: %s\r\n"
		"Accept: */*\r\n"
		"Content-length: %d\r\n"
		"Content-type: text/plain\r\n"
		"\r\n",
		path[i],
		VERSION,
		cookie[j],
		hostname[i], postlen[i]);
    }

    reqlen = strlen(request);

#ifdef CHARSET_EBCDIC
    ebcdic2ascii(request, request, reqlen);
#endif				/* CHARSET_EBCDIC */
return 0;
}

/* --------------------------------------------------------- */

/* run the tests */

static void test()
{
    struct timeval timeout, now;
    fd_set sel_read, sel_except, sel_write;
    int i;
    con = calloc(concurrency, sizeof(struct connection));
    memset(con, 0, concurrency * sizeof(struct connection));
 
    for(i = 0; i < concurrency; i++){
	con[i].which = ready_to_run_queue[i].which;
	con[i].run = ready_to_run_queue[i].run;
	con[i].state = STATE_READY;
	con[i].thread = ready_to_run_queue[i].thread;
	}
    stats = calloc(number_of_urls, sizeof(struct data *));
    for(i = 0; i < number_of_runs; i++){int j;
	for(j = position[i]; j < position[i + 1];j++)
	stats[j] = calloc(repeats[i], sizeof(struct data));}

    FD_ZERO(&readbits);
    FD_ZERO(&writebits);

    /* ok - lets start */
    gettimeofday(&start, 0);

    /* initialise lots of requests */

    head = concurrency;
    for (i = 0; i < concurrency; i++)start_connect(&con[i]);

    while (done < need_to_be_done) {
	int n;
	/* setup bit arrays */

	memcpy(&sel_except, &readbits, sizeof(readbits));
	memcpy(&sel_read, &readbits, sizeof(readbits));
	memcpy(&sel_write, &writebits, sizeof(readbits));
       
        
	/* Timeout of 30 seconds. */
	timeout.tv_sec = 30;
	timeout.tv_usec = 0;
	n = ap_select(FD_SETSIZE, &sel_read, &sel_write, &sel_except, &timeout);
	if (!n) {
	    myerr("\nServer timed out\n\n");
	}
	if (n < 1)
	    myerr("Select error.");
	for (i = 0; i < concurrency; i++) {
	    int s = con[i].fd;
	    if(con[i].state == STATE_DONE) continue;
	    if (FD_ISSET(s, &sel_except)) {
		err_except++;
                failed[con[i].which]++;
		start_connect(&con[i]);
		continue;
	    }
	    if (FD_ISSET(s, &sel_read)){
		read_connection(&con[i]);
		continue;}
	    if (FD_ISSET(s, &sel_write)){
		write_request(&con[i]);	
		}
	}
    }
    gettimeofday(&endtime, 0);
    if (strlen(warn_and_error) == 25) myerr("None.\n");
    else myerr("Done.\n");
}



/* ------------------------------------------------------- */

/* split URL into parts */

static int parse_url(char *p, int i)
{
    char *cp;
    char *h;
    char *url = malloc(1024);
    if (strlen(p) > 7 && strncmp(p, "http://", 7) == 0)
	p += 7;
    strcpy(url, p);
    h = url;
    p = NULL;
    if ((cp = strchr(url, ':')) != NULL) {
	*cp++ = '\0';
	p = cp;
	url = cp;
    }
    if ((cp = strchr(url, '/')) == NULL){
	hostname[i] = path[i] = h;
	return 1;
	}
    path[i] = malloc((strlen(cp)+1) * sizeof(char)); 
    strcpy(path[i], cp);
    *cp = '\0';
    hostname[i] = h;
    if (p != NULL)
	port[i] = atoi(p);
    return 0;
}


/* ------------------------------------------------------- */

void
initialize()
{
     int i,j;
     cookie = malloc(number_of_runs * sizeof(char *));
     filesize = malloc(number_of_runs * sizeof(int));

     which_thread = malloc(number_of_urls * sizeof(int *));
     arranged = malloc(number_of_urls * sizeof(int));
     for(i = 0; i < number_of_urls; i++) arranged[i] = 0;
     for(i = 0; i < number_of_runs; i++){
	for(j = position[i]; j < position[i + 1]; j++)
		which_thread[j] = malloc(repeats[i] * sizeof(int));
        for(j = 0; j < repeats[i]; j++) which_thread[position[i]][j] = j;
     	need_to_be_done += repeats[i] * (position[i + 1] - position[i]);}
     ready_to_run_queue = malloc(need_to_be_done * sizeof(struct threadval));
     for(i = 0; i < number_of_runs; i++){
     	if(order[i] == DEPTH_FIRST) {
	  if((priority == EQUAL_OPPORTUNITY) || (tail < concurrency)){
		arranged[position[i]] = 1;
		ready_to_run_queue[tail].run = i;	
		ready_to_run_queue[tail].which = position[i];
		ready_to_run_queue[tail++].thread = 0;
		}}
        else for(j = 0; j < repeats[i]; j++)
	  if((priority == EQUAL_OPPORTUNITY) || (tail < concurrency)){
		arranged[position[i]] += 1;
		ready_to_run_queue[tail].run = i;
		ready_to_run_queue[tail].thread = j;
		ready_to_run_queue[tail++].which = position[i];
		}
     }
     hostname = malloc(number_of_urls * sizeof( char *));
     path = malloc(number_of_urls * sizeof( char *));
     port = malloc(number_of_urls * sizeof(int));		     
				/* default port number */     
     doclen = malloc(number_of_urls * sizeof(int));		
				/* the length the document should be */
     totalread = malloc(number_of_urls * sizeof(int));		
				/* total number of bytes read */
     totalbread = malloc(number_of_urls * sizeof(int));		
				/* totoal amount of entity body read */
     totalposted = malloc(number_of_urls * sizeof(int));		
				/* total number of bytes posted, inc. headers*/
     started = malloc(number_of_urls * sizeof(int));
     finished = malloc(number_of_urls * sizeof(int));
     failed = malloc(number_of_urls * sizeof(int));
     good = malloc(number_of_urls * sizeof(int));
     servername = malloc(number_of_urls * sizeof(char *));
     page_content = malloc(number_of_urls * sizeof(char *));
     postdata = malloc(number_of_urls * sizeof(char *));
     postlen = malloc(number_of_urls * sizeof(int));
     posting = malloc(number_of_urls * sizeof(int));
     for(i = 0; i < number_of_urls; i++){
	doclen[i] = 0;
	totalread[i] = 0;
	totalbread[i] = 0;
	totalposted[i] = 0;
	port[i] = 80;
	started[i] = 0;
	finished[i] = 0;
	failed[i] = 0;
	good[i] = 0; 
	}
     }

MODULE = ApacheBench		PACKAGE = ApacheBench		
PROTOTYPES: ENABLE



HV *
ab(data_hash)
     SV * data_hash;
     PREINIT:
     char *pt,**url_keys;
     int i,j,k;
     int def_filesize; /* default filesize for all runs */

     CODE:
     SV * runs;
     SV * urls;
     SV * post_data;
     SV * cookies;
     AV * run_group,*tmp;
     SV * tep;
     HV * h;
     STRLEN len;

     /*Get necessary initial information and initialize*/	
     h = (HV *)SvRV(data_hash);
     tep = *(hv_fetch(h,"concurrency",11,0));
     concurrency = SvIV(tep);
     tep = *(hv_fetch(h,"filesize",8,0));
     def_filesize = SvIV(tep);
     tep = *(hv_fetch(h,"repeat",6,0));
     repeat = SvIV(tep);
     tep = *(hv_fetch(h,"priority",8,0));
     pt = SvPV(tep,len);
     if(strcmp(pt, "run_priority") == 0) priority = RUN_PRIORITY;
     else {
	priority = EQUAL_OPPORTUNITY;
	if (strcmp(pt, "equal_opportunity") != 0) myerr("Unknown priority value(the only possible priorities are run_priority and equal_opportunity),use equal_opportunity instead");
	}

     runs = *(hv_fetch(h,"runs",4,0));
     run_group = (AV *)SvRV(runs);
     number_of_runs = av_len(run_group) + 1;
     order = malloc(number_of_runs * sizeof( int ));
     repeats = malloc(number_of_runs * sizeof(int));
     position = malloc((number_of_runs + 1) * sizeof(int));

     for(i = 0,j = 0; i < number_of_runs; i++){
	tep = *(av_fetch(run_group,i,0));
	h = (HV *)SvRV(tep);
        if(hv_exists(h,"repeat",6)){
	  tep = *(hv_fetch(h,"repeat",6,0));	/* Number of requests to make*/
	  repeats[i] = SvIV(tep);}
	else repeats[i] = repeat;
	requests = ap_max(requests, repeats[i]);
	urls = *(hv_fetch(h,"urls",4,0));
	tmp =(AV *) SvRV(urls);
	position[i] = number_of_urls;
        number_of_urls += av_len(tmp) + 1;

	if(hv_exists(h,"order",5)){
	  tep = *(hv_fetch(h,"order",5,0));
	  pt = SvPV(tep,len);
	  if(strcmp(pt, "depth_first") == 0) {
		order[i] = DEPTH_FIRST;
		j += 1;}
	  else if(strcmp(pt, "breadth_first") == 0){
		order[i] = BREADTH_FIRST;
		j += repeats[i];}
	       else {
		myerr("invalue order:order could only be depth_first or breadth_first,use breadth_first instead");
		order[i] = BREADTH_FIRST;
		j += repeats[i];
		}}
	else {order[i] = BREADTH_FIRST; j += repeats[i];}
	}
     if(number_of_urls <= 0) {
			myerr("No urls,");
			return;
			}
     position[number_of_runs] = number_of_urls;
     concurrency = ap_min(concurrency, j);

     initialize();
     url_keys = malloc(number_of_urls * sizeof(char *));

     for(k = 0; k < number_of_runs; k++){
         tep = *(av_fetch(run_group,k,0));
         h = (HV *)SvRV(tep);
         if(hv_exists(h,"filesize",8)){
             tep = *(hv_fetch(h,"filesize",8,0));
             filesize[k] = SvIV(tep);}
         else filesize[k] = def_filesize;

         urls = *(hv_fetch(h,"urls",4,0));
         post_data = *(hv_fetch(h,"postdata",8,0));
         cookies = *(hv_fetch(h,"cookie",6,0));
         for(i = position[k]; i < position[k + 1]; i++){
             tmp =(AV *) SvRV(urls);
             tep = *av_fetch(tmp,i - position[k],0);
             pt = SvPV(tep,len);
             url_keys[i] = pt;
             if(parse_url(pt,i)){
		char *warn = malloc(1024);
		sprintf(warn,"Invalid url:%s,the information for this url may be wrong",pt);
		myerr(warn);
		free(warn);
             }
         }
         tmp = (AV *) SvRV(post_data);
         i = ap_min(position[k + 1], position[k] + av_len(tmp) + 1);
         for(j = position[k]; j < i; j++){
             tep = *(av_fetch(tmp,j - position[k],0));  
     	     pt = SvPV((SV *)tep,len); 
     	     postdata[j] = pt;
     	     postlen[j] = posting[j] = len;
     	 }
         for(j = i; j < position[k + 1]; j++){
	     posting[j] = postlen[j] = 0;
			/*If the number of postdata strings is less than
				 that of urls,then assign empty strings*/
	 };
         tmp = (AV *) SvRV(cookies);
         if (av_len(tmp) < 0) cookie[k] = NULL;
         else {
	     tep = *(av_fetch(tmp,0,0));  
             pt = SvPV((SV *)tep,len); 
     	     if(len != 0){ 
		cookie[k] = malloc((len+10) * sizeof(char));
		strcpy(cookie[k],"Cookie:");
      		strcat(cookie[k],pt);	
        	strcat(cookie[k],"\t\n");
    		}
     	     else cookie[k] = NULL;
         }
    }

     test();

     RETVAL = newHV();/* ready to get information stored in global variables */

     for(k = 0; k < number_of_runs; k++){
     HV * run_hash = newHV();
		      /* Store value for each thread */
     tmp = newAV();   /* initial an array to keep the thread information */ 

     for(i = 0; i < repeats[k]; i++){ 
	AV *th_t = newAV();	  /* times for processing and connecting */
	AV *th_c = newAV();	  /* connecting times */
	AV *url_contents = newAV();  /* pages read from servers */
	AV *headers = newAV();
     	h = newHV();

	for(j = position[k]; j < position[k + 1]; j++){
		av_push(th_t,newSVnv(stats[j][i].time));
		av_push(th_c,newSVnv(stats[j][i].ctime));
		av_push(url_contents,newSVpv(stats[j][i].page_content,0));
		av_push(headers,newSVpv(stats[j][i].header,0));
		}

     	hv_store(h,"total_time",10,newRV_inc((SV *)th_t),0);
     	hv_store(h,"connect_time",12,newRV_inc((SV *)th_c),0);
     	hv_store(h,"page_content",12,newRV_inc((SV *)url_contents),0);
     	hv_store(h,"headers",7,newRV_inc((SV *)headers),0);
     	av_push(tmp,newRV_inc((SV*)h));
	}

     hv_store(run_hash,"threads",7,newRV_inc((SV *)tmp),0);
		/* The end of store value for key "threads" */

		/* Store information for each url now */
     for(i = position[k];i < position[k + 1]; i++){
	int totalcon = 0, total = 0;
	int mincon = 9999999, mintot = 999999;
	int maxcon = 0, maxtot = 0;
    	h=newHV();
     	for (j = 0;  j< repeats[k]; j++) {
	    struct data s = stats[i][j];
	    mincon = ap_min(mincon, s.ctime);
	    mintot = ap_min(mintot, s.time);
	    maxcon = ap_max(maxcon, s.ctime);
	    maxtot = ap_max(maxtot, s.time);
	    totalcon += s.ctime;
	    total += s.time;
		hv_store(h,"max_connect_time",16,newSVnv(maxcon),0);
		hv_store(h,"max_time",8,newSVnv(maxtot),0);
		hv_store(h,"min_connect_time",16,newSVnv(mincon),0);
		hv_store(h,"min_time",8,newSVnv(mintot),0);
		hv_store(h,"average_connect_time",20,newSVnv((float)totalcon/repeats[k]),0);
		hv_store(h,"average_time",12,newSVnv((double)total/repeats[k]),0);
		}
     	hv_store(h,"hostname",8,newSVpv(hostname[i],0),0);
     	hv_store(h,"software",8,newSVpv(servername[i],0),0);
     	hv_store(h,"port",4,newSVnv(port[0]),0);
     	hv_store(h,"doc_length",10,newSVnv(doclen[i]),0);
     	hv_store(h,"path",4,newSVpv(path[i],0),0);
     	hv_store(h,"completed_requests",18,newSVnv(finished[i]),0);
     	hv_store(h,"failed_requests",15,newSVnv(failed[i]),0); 
     	hv_store(h,"total_read",10,newSVnv(totalread[i]),0);
     	hv_store(h,"page_content",12,newSVpv(page_content[i],0),0);
     	hv_store(h,"header",6,newSVpv(stats[i][0].header,0),0);
     	if(posting[i] > 0)
        	hv_store(h,"total_posted",12,newSVnv(totalposted[i]),0);
     	hv_store(run_hash,url_keys[i],strlen(url_keys[i]),newRV_inc((SV *)h),0);
	}
	{char key[10];
	sprintf(key,"run%d",k);
     	hv_store(RETVAL,key,strlen(key),newRV_inc((SV *)run_hash),0);
	}}
     hv_store(RETVAL,"warn",4,newSVpv(warn_and_error,0),0);
     hv_store(RETVAL,"total_time",10,newSVnv(timedif(endtime,start)),0);
     hv_store(RETVAL,"bytes_received",14,newSVnv(total_bytes_received),0);
     OUTPUT:
     RETVAL
