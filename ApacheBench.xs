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




/*  ------------------ DEBUGGING --------------------------------------- */

/* comment this out to turn off debugging messages * /
#define AB_DEBUG 1
/**/

/*  -------------------------------------------------------------------- */

#ifdef AB_DEBUG
#define AB_DEBUG2 1
#else
#define AB_DEBUG2 0
#endif

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

/* XS library */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

/* ------------------- DEFINITIONS -------------------------- */

#define HEADERSIZE       512
#define STATE_DONE 	  1
#define STATE_READY 	  0
#define RUN_PRIORITY	  1
#define EQUAL_OPPORTUNITY 0
#define DEPTH_FIRST	  1
#define BREADTH_FIRST	  0

/* ------------------- MACROS -------------------------- */

#define ap_min(a,b) ((a)<(b))?(a):(b)
#define ap_max(a,b) ((a)>(b))?(a):(b)

#ifndef BEOS
#define ab_close(s) close(s)
#define ab_read(a,b,c) read(a,b,c)
#define ab_write(a,b,c) write(a,b,c)
#else
#define ab_close(s) closesocket(s)
#define ab_read(a,b,c) recv(a,b,c,0)
#define ab_write(a,b,c) send(a,b,c,0)
#endif

/* ------------------- STRUCTS -------------------------- */

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


/* --------------------- GLOBALS ---------------------------- */

struct global {
    int *posting;   		/* GET if posting[]=0 */
    int concurrency;	/* Number of multiple requests to make */
    int *repeats;			/* Number of time to repeat for each run */
    int repeat;			/* default repeat number if not specifid */
    int requests;		/* the max of the repeats */
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
    int *started, *finished, *arranged;
				/* numbers of requests  started , */
				/* finished or arranged for each url*/
    int **which_thread;		/* which thread is available */
    struct threadval *ready_to_run_queue;
    int head, tail, done, need_to_be_done;
    int priority;
    int *order;
    char warn_and_error[2048];
    /*warn and error message returned to perl*/
    int *filesize;
    int number_of_urls, number_of_runs;
    int total_bytes_received;
    char **page_content;

    /* store error cases */
    int err_length, err_conn, err_except;


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
};



/* --------------------------------------------------------- */

/* keep warn and error massege */

void
myerr(char *warn_and_error, char *s)
{
    if ((strlen(warn_and_error)+strlen(s)) < 2014) {
	strcat(warn_and_error,"\n[Warn:] ");
	strcat(warn_and_error,s);
    } else if(strlen(warn_and_error) < 2014)
	strcat(warn_and_error,"\nToo much warn and error message!");
}

/* --------------------------------------------------------- */

/* write out request to a connection - assumes we can write
   (small) request out in one go into our new socket buffer  */

static void
write_request(struct global * registry, struct connection * c)
{

#ifndef NO_WRITEV
    struct iovec out[2]; int outcnt = 1;
#endif
#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 1, registry->done = %d\n", registry->done);
#endif
    gettimeofday(&c->connect, 0);
    reset_request(registry, c->which, c->run);
#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 2, registry->done = %d\n", registry->done);
#endif
#ifndef NO_WRITEV
    out[0].iov_base = registry->request;
    out[0].iov_len = registry->reqlen;

#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 2a.1, registry->done = %d\n", registry->done);
#endif
    if (registry->posting[c->which] > 0) {
	out[1].iov_base = registry->postdata[c->which];
	out[1].iov_len = registry->postlen[c->which];
	outcnt = 2;
	registry->totalposted[c->which] += (registry->reqlen + registry->postlen[c->which]);
    }
#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 2a.2, registry->done = %d\n", registry->done);
#endif
    writev(c->fd, out, outcnt);
#else
#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 2b.1, registry->done = %d\n", registry->done);
#endif
    ab_write(c->fd, registry->request, registry->reqlen);
    if (registry->posting[c->which] > 0) {
        ab_write(c->fd, registry->postdata[c->which], registry->postlen[c->which]);
        registry->totalposted[c->which] += (registry->reqlen + registry->postlen[c->which]);
    }
#endif
#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 3, registry->done = %d\n", registry->done);
#endif
    c->page_content = calloc(1, registry->filesize[c->run]);
    FD_SET(c->fd, &registry->readbits);
    FD_CLR(c->fd, &registry->writebits);
}

/* --------------------------------------------------------- */

/* make an fd non blocking */

static void
nonblock(int fd)
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

static int
timedif(struct timeval a, struct timeval b)
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

static void
close_connection(struct global * registry, struct connection * c);

/* --------------------------------------------------------- */

/* start asnchronous non-blocking connection */

static void
start_connect(struct global * registry, struct connection * c)
{
    c->read = 0;
    c->bread = 0;
    c->cbx = 0;
    c->gotheader = 0;
    c->fd = socket(AF_INET, SOCK_STREAM, 0);

#ifdef AB_DEBUG
    printf("AB_DEBUG: start of start_connect()\n");
#endif

    if (c->fd < 0) {
	myerr(registry->warn_and_error, "socket error");
	registry->good[c->which]++;
	close_connection(registry, c);
	return;
    }
    nonblock(c->fd);

#ifdef AB_DEBUG
    printf("AB_DEBUG: start_connect() - stage 1\n");
#endif

    gettimeofday(&c->start, 0);

    {
	/* get server information */
	struct hostent *he;
#ifdef AB_DEBUG
	printf("AB_DEBUG: start_connect() - stage 2, c->which: '%d'\n", c->which);
#endif
	he = gethostbyname(registry->hostname[c->which]);
#ifdef AB_DEBUG
	printf("AB_DEBUG: start_connect() - stage 3\n");
#endif
	if (!he) {
	    char * warn = malloc(1024);
	    sprintf(warn, "Bad hostname: %s, the information stored for it could be wrong!", registry->hostname[c->which]);
	    myerr(registry->warn_and_error, warn);
	    free(warn);
	    /* bad hostname, yields the resourse */
	    registry->good[c->which]++;
	    close_connection(registry, c);
	    return;
	}
#ifdef AB_DEBUG
	printf("AB_DEBUG: start_connect() - stage 4\n");
#endif
	registry->server.sin_family = he->h_addrtype;
	registry->server.sin_port = htons(registry->port[c->which]);
	registry->server.sin_addr.s_addr = ((unsigned long *) (he->h_addr_list[0]))[0]; 
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: start_connect() - stage 5\n");
#endif

    if (connect(c->fd, (struct sockaddr *) & registry->server, sizeof(registry->server)) < 0) {
	if (errno == EINPROGRESS) {
	    FD_SET(c->fd, &registry->writebits);
	    registry->started[c->which]++;
	    return;
	} else {
	    ab_close(c->fd);
	    registry->err_conn++;
	    if (registry->failed[c->which]++ > 10) {
		myerr(registry->warn_and_error, "\nTest aborted after 10 failures\n\n");
		/* yields the resourse */
		registry->good[c->which]++;
		close_connection(registry, c);
		return;
	    }
	    start_connect(registry, c);
	    return;
	}
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: start_connect() - stage 6\n");
#endif

    /* connected first time */
    registry->started[c->which]++;
    FD_SET(c->fd, &registry->writebits);
  
}

/* --------------------------------------------------------- */

/* close down connection and save stats */

static void
close_connection(struct global * registry, struct connection * c)
{
    if (c->read >= registry->filesize[c->run])
	myerr(registry->warn_and_error, "Filesize is too small,only part of the page stored!");
    if (c->read == 0)
	c->page_content = c->header = registry->servername[c->which] = "";
    if (registry->good[c->which] == 1) {
	/* first time here */
	registry->doclen[c->which] = c->bread;	
	registry->page_content[c->which] = c->page_content;
    } else if (c->bread != registry->doclen[c->which]) {
	char *warn = malloc(1024);
	sprintf(warn,
		"Contents from http://%s%s doesn't match the previous try.",
		registry->hostname[c->which], registry->path[c->which]);
	myerr(registry->warn_and_error, warn);
	free(warn);
	registry->failed[c->which]++;
	registry->err_length++;
    } else {
	if (c->read)
	    free(c->page_content);
	c->page_content = registry->page_content[c->which];
    }

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
        registry->stats[c->which][c->thread] = s;
    }

    registry->total_bytes_received += c->read;
    registry->finished[c->which]++;
    ab_close(c->fd);
    FD_CLR(c->fd, &registry->readbits);
    FD_CLR(c->fd, &registry->writebits); 

    if (++registry->done >= registry->need_to_be_done)
	return;

    if (registry->priority == RUN_PRIORITY) {
	if (registry->started[registry->position[c->run + 1] - 1] >= registry->repeats[c->run]) {
	    c->run++;
	    while (c->run < registry->number_of_runs) {
		if (registry->started[registry->position[c->run + 1] - 1] >= registry->repeats[c->run]
		    || (registry->order[c->run] == DEPTH_FIRST
			&& registry->started[registry->position[c->run]] > 0)) {
		    /* this run dosen't need this resource anymore */
		    c->run++;
		    continue;
		}
		c->which = registry->position[c->run];
		if (registry->started[c->which] < registry->repeats[c->run]) {
		    /* for breadth first, starting one more connect to the 
		       the first url if  possible
		       for depth_first, get started here, */
		    c->thread = registry->which_thread[c->which][registry->started[c->which]];
		    start_connect(registry, c);
		    return;
		}
		while (++c->which < registry->position[c->run + 1]
		       && registry->started[c->which] >= registry->repeats[c->run]);
		/* found the first one which hasn't started completely */
		if (registry->started[c->which] < registry->finished[c->which - 1]) {
		    /* if the privious urls finished, started this one */
		    c->thread = registry->which_thread[c->which][registry->started[c->which]];
		    start_connect(registry, c);
		    return;
		}
		/* this run dosen't need any more resources */
		else
		    c->run++;
	    }
	    /* no one needs any more resources */
	    c->state = STATE_DONE;
	    return;
	} else {
	    /* possible more resource needed in this group */
	    /* started[position[c->run + 1] - 1] is less than repeats[c->run] */
	    if (registry->order[c->run] == DEPTH_FIRST) {
		/* for depth_first,connect the next one */
		if (++c->which == (registry->position[c->run + 1])) {
		    c->which = registry->position[c->run];
		    c->thread = registry->started[c->which];
		}
		start_connect(registry, c);
		return;
	    } else { /* breadth_first */
		if (c->which < (registry->position[c->run + 1] - 1))
		    registry->which_thread[c->which+1][registry->finished[c->which] - 1] = c->thread;
		if (registry->started[c->which] == registry->repeats[c->run])
		    c->which++;
		if (c->which == registry->position[c->run]) {
		    c->thread = registry->which_thread[c->which][registry->started[c->which]];
		    start_connect(registry, c);
		    return;
		}
		if (registry->started[c->which] < registry->finished[c->which - 1]) {
		    c->thread = registry->started[c->which];
		    start_connect(registry, c);
		    return; 
		} else {
		    /*  this group doesn't really need any more sources */
		    c->run++;
		    while (c->run < registry->number_of_runs) {
			if (registry->started[registry->position[c->run + 1] - 1] == registry->repeats[c->run]
			    || (registry->order[c->run] == DEPTH_FIRST
				&& registry->started[registry->position[c->run]] > 0)) {
			    c->run++;
			    continue;
			}
			c->which = registry->position[c->run];
			if (registry->started[c->which] < registry->repeats[c->run]) {
			    c->thread = registry->which_thread[c->which][registry->started[c->which]];
			    start_connect(registry, c);
			    return;
			}
			while (++c->which < registry->position[c->run + 1]
			       && registry->started[c->which] >= registry->repeats[c->run]);
			if (registry->started[c->which] < registry->finished[c->which - 1]) {
			    c->thread = registry->which_thread[c->which][registry->started[c->which]];
			    start_connect(registry, c);
			    return;
			}
			c->run++;
		    }
		    c->state = STATE_DONE;
		    return;
		}
	    }
	}
    } else { /* equal_opportunity */

	if (c->which < registry->position[c->run + 1]-1) {
	    registry->ready_to_run_queue[registry->tail].which = c->which + 1;
	    registry->ready_to_run_queue[registry->tail].thread = c->thread;
	    registry->ready_to_run_queue[registry->tail++].run = c->run;
	    registry->arranged[c->which + 1]++;
	} else if ((registry->order[c->run] == DEPTH_FIRST)
		   && (registry->arranged[registry->position[c->run]] < registry->repeats[c->run])) {
	    registry->ready_to_run_queue[registry->tail].which = registry->position[c->run];
	    registry->ready_to_run_queue[registry->tail].thread = registry->arranged[registry->position[c->run]]++;
	    registry->ready_to_run_queue[registry->tail++].run = c->run;
	}

	if (registry->head >= registry->tail) {
	    c->state = STATE_DONE;
	    return;
	}
    	c->thread = registry->ready_to_run_queue[registry->head].thread;
    	c->which = registry->ready_to_run_queue[registry->head].which;
    	c->run = registry->ready_to_run_queue[registry->head++].run;
    	start_connect(registry, c);
    	return;
    }
}

/* --------------------------------------------------------- */

/* read data from connection */

static void
read_connection(struct global * registry, struct connection * c)
{
    int r;

    r = ab_read(c->fd, registry->buffer, sizeof(registry->buffer));	
    if (r == 0 || (r < 0 && errno != EAGAIN)) {
	registry->good[c->which]++;
	close_connection(registry, c);
	return;
    }

    if (r < 0 && errno == EAGAIN)
	return;
    c->read += r;
    registry->totalread[c->which] += r;
    if (c->read < registry->filesize[c->run]-1)
	strncat(c->page_content, registry->buffer, r);
    if (!c->gotheader) {
	char *s;
	int l = 4;
	int tocopy = HEADERSIZE - c->cbx - 1;	/* -1 to allow for 0
						 * terminator */
	tocopy = ap_min(tocopy, r);
#ifndef CHARSET_EBCDIC
	memcpy(c->cbuff + c->cbx, registry->buffer, tocopy);
#else				/* CHARSET_EBCDIC */
	ascii2ebcdic(c->cbuff + c->cbx, registry->buffer, tocopy);
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
	    registry->servername[c->which] = "";
	    return;
	} else {
	    /* have full header */
	    if (!registry->good[c->which]) {
		/* this is first time, extract some interesting info */
		char *p, *q;
		p = strstr(c->cbuff, "Server:");
		q = registry->servername[c->which] = malloc(HEADERSIZE);
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
	    registry->totalbread[c->which] += c->bread;
	}
    } else {
	/* outside header, everything we have read is entity body */
	c->bread += r;
	registry->totalbread[c->which] += r;
    }
}

/* --------------------------------------------------------- */

    /* setup or reset request */
int
reset_request(struct global * registry, int i, int j)
{
    if (registry->posting[i] <= 0) {
	sprintf(registry->request, "%s %s HTTP/1.0\r\n"
		"User-Agent: ApacheBench/%s\r\n"
		"%s"
		"Host: %s\r\n"
		"Accept: */*\r\n"
		"\r\n",
		(registry->posting[i] == 0) ? "GET" : "HEAD",
		registry->path[i],
		VERSION,
		registry->cookie[j],
		registry->hostname[i]);
    } else {
	sprintf(registry->request, "POST %s HTTP/1.0\r\n"
		"User-Agent: ApacheBench/%s\r\n"
		"%s"
		"Host: %s\r\n"
		"Accept: */*\r\n"
		"Content-length: %d\r\n"
		"Content-type: application/x-www-form-urlencoded\r\n"
		"\r\n",
		registry->path[i],
		VERSION,
		registry->cookie[j],
		registry->hostname[i],
		registry->postlen[i]);
    }

    registry->reqlen = strlen(registry->request);

#ifdef CHARSET_EBCDIC
    ebcdic2ascii(registry->request, registry->request, registry->reqlen);
#endif				/* CHARSET_EBCDIC */

    return 0;
}

/* --------------------------------------------------------- */

/* run the tests */

static void
test(struct global * registry)
{
    struct timeval timeout, now;
    fd_set sel_read, sel_except, sel_write;
    int i;

    registry->con = calloc(registry->concurrency, sizeof(struct connection));
    memset(registry->con, 0, registry->concurrency * sizeof(struct connection));

#ifdef AB_DEBUG
    printf("AB_DEBUG: start of test()\n");
#endif

    for (i = 0; i < registry->concurrency; i++) {
	registry->con[i].which = registry->ready_to_run_queue[i].which;
	registry->con[i].run = registry->ready_to_run_queue[i].run;
	registry->con[i].state = STATE_READY;
	registry->con[i].thread = registry->ready_to_run_queue[i].thread;
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: test() - stage 1\n");
#endif

    registry->stats = calloc(registry->number_of_urls, sizeof(struct data *));
    for (i = 0; i < registry->number_of_runs; i++) {
	int j;
	for (j = registry->position[i]; j < registry->position[i+1]; j++)
	    registry->stats[j] = calloc(registry->repeats[i], sizeof(struct data));
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: test() - stage 2\n");
#endif

    FD_ZERO(&registry->readbits);
    FD_ZERO(&registry->writebits);

#ifdef AB_DEBUG
    printf("AB_DEBUG: test() - stage 3\n");
#endif

    /* ok - lets start */
    gettimeofday(&registry->start, 0);

#ifdef AB_DEBUG
    printf("AB_DEBUG: test() - stage 4\n");
#endif

    /* initialise lots of requests */

    registry->head = registry->concurrency;
    for (i = 0; i < registry->concurrency; i++)
	start_connect(registry, &registry->con[i]);

#ifdef AB_DEBUG
    printf("AB_DEBUG: test() - stage 5\n");
#endif

    while (registry->done < registry->need_to_be_done) {
	int n;
	/* setup bit arrays */

#ifdef AB_DEBUG
	printf("AB_DEBUG: test() - stage 5.1, registry->done = %d\n", registry->done);
#endif

	memcpy(&sel_except, &registry->readbits, sizeof(registry->readbits));
	memcpy(&sel_read, &registry->readbits, sizeof(registry->readbits));
	memcpy(&sel_write, &registry->writebits, sizeof(registry->writebits));

#ifdef AB_DEBUG
	printf("AB_DEBUG: test() - stage 5.2, registry->done = %d\n", registry->done);
#endif
	/* Timeout of 30 seconds. */
	timeout.tv_sec = 30;
	timeout.tv_usec = 0;
	n = ap_select(FD_SETSIZE, &sel_read, &sel_write, &sel_except, &timeout);
#ifdef AB_DEBUG
	printf("AB_DEBUG: test() - stage 5.3, registry->done = %d\n", registry->done);
#endif
	if (!n)
	    myerr(registry->warn_and_error, "\nServer timed out\n\n");
	if (n < 1)
	    myerr(registry->warn_and_error, "Select error.");
#ifdef AB_DEBUG
	printf("AB_DEBUG: test() - stage 5.4, registry->done = %d\n", registry->done);
#endif
	for (i = 0; i < registry->concurrency; i++) {
	    int s = registry->con[i].fd;
#ifdef AB_DEBUG
	    printf("AB_DEBUG: test() - stage 5.5, registry->done = %d, i = %d\n", registry->done, i);
#endif
	    if (registry->con[i].state == STATE_DONE)
		 continue;
#ifdef AB_DEBUG
	    printf("AB_DEBUG: test() - stage 5.6, registry->done = %d, i = %d\n", registry->done, i);
#endif
	    if (FD_ISSET(s, &sel_except)) {
		registry->err_except++;
		registry->failed[registry->con[i].which]++;
		start_connect(registry, &registry->con[i]);
		continue;
	    }
#ifdef AB_DEBUG
	    printf("AB_DEBUG: test() - stage 5.7, registry->done = %d, i = %d\n", registry->done, i);
#endif
	    if (FD_ISSET(s, &sel_read)) {
		read_connection(registry, &registry->con[i]);
		continue;
	    }
#ifdef AB_DEBUG
	    printf("AB_DEBUG: test() - stage 5.8, registry->done = %d, i = %d\n", registry->done, i);
#endif
	    if (FD_ISSET(s, &sel_write))
		write_request(registry, &registry->con[i]);	
#ifdef AB_DEBUG
	    printf("AB_DEBUG: test() - stage 5.9, registry->done = %d, i = %d\n", registry->done, i);
#endif
	}
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: test() - stage 6\n");
#endif

    gettimeofday(&registry->endtime, 0);
    if (strlen(registry->warn_and_error) == 25)
	myerr(registry->warn_and_error, "None.\n");
    else myerr(registry->warn_and_error, "Done.\n");
}



/* ------------------------------------------------------- */

/* split URL into parts */

static int
parse_url(struct global * registry, char *p, int i)
{
    char *cp;
    char *h;
    char *url = malloc(1024);

    /* remove http:// prefix if it exists */
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
	registry->hostname[i] = registry->path[i] = h;
	return 1;
    }
    registry->path[i] = malloc((strlen(cp)+1) * sizeof(char)); 
    strcpy(registry->path[i], cp);
    *cp = '\0';
    registry->hostname[i] = h;
    if (p != NULL)
	registry->port[i] = atoi(p);
    return 0;
}


/* ------------------------------------------------------- */

void
initialize(struct global * registry)
{
    int i,j;

    registry->cookie = malloc(registry->number_of_runs * sizeof(char *));
    registry->filesize = malloc(registry->number_of_runs * sizeof(int));
    registry->which_thread = malloc(registry->number_of_urls * sizeof(int *));
    registry->arranged = malloc(registry->number_of_urls * sizeof(int));

    for (i = 0; i < registry->number_of_urls; i++)
	registry->arranged[i] = 0;
    for (i = 0; i < registry->number_of_runs; i++) {
	for (j = registry->position[i]; j < registry->position[i+1]; j++)
	    registry->which_thread[j] = malloc(registry->repeats[i] * sizeof(int));
	for (j = 0; j < registry->repeats[i]; j++)
	    registry->which_thread[registry->position[i]][j] = j;
	registry->need_to_be_done += registry->repeats[i] * (registry->position[i+1] - registry->position[i]);
    }
    registry->ready_to_run_queue = malloc(registry->need_to_be_done * sizeof(struct threadval));
    for (i = 0; i < registry->number_of_runs; i++) {
	if (registry->order[i] == DEPTH_FIRST) {
	    if ((registry->priority == EQUAL_OPPORTUNITY) || (registry->tail < registry->concurrency)) {
		registry->arranged[registry->position[i]] = 1;
		registry->ready_to_run_queue[registry->tail].run = i;
		registry->ready_to_run_queue[registry->tail].which = registry->position[i];
		registry->ready_to_run_queue[registry->tail++].thread = 0;
	    }
	} else for (j = 0; j < registry->repeats[i]; j++)
	    if ((registry->priority == EQUAL_OPPORTUNITY) || (registry->tail < registry->concurrency)) {
		registry->arranged[registry->position[i]] += 1;
		registry->ready_to_run_queue[registry->tail].run = i;
		registry->ready_to_run_queue[registry->tail].thread = j;
		registry->ready_to_run_queue[registry->tail++].which = registry->position[i];
	    }
    }
    registry->hostname = malloc(registry->number_of_urls * sizeof( char *));
    registry->path = malloc(registry->number_of_urls * sizeof( char *));
    registry->port = malloc(registry->number_of_urls * sizeof(int));
				/* default port number */
    registry->doclen = malloc(registry->number_of_urls * sizeof(int));
				/* the length the document should be */
    registry->totalread = malloc(registry->number_of_urls * sizeof(int));
				/* total number of bytes read */
    registry->totalbread = malloc(registry->number_of_urls * sizeof(int));
				/* totoal amount of entity body read */
    registry->totalposted = malloc(registry->number_of_urls * sizeof(int));
				/* total number of bytes posted, inc. headers*/
    registry->started = malloc(registry->number_of_urls * sizeof(int));
    registry->finished = malloc(registry->number_of_urls * sizeof(int));
    registry->failed = malloc(registry->number_of_urls * sizeof(int));
    registry->good = malloc(registry->number_of_urls * sizeof(int));
    registry->servername = malloc(registry->number_of_urls * sizeof(char *));
    registry->page_content = malloc(registry->number_of_urls * sizeof(char *));
    registry->postdata = malloc(registry->number_of_urls * sizeof(char *));
    registry->postlen = malloc(registry->number_of_urls * sizeof(int));
    registry->posting = malloc(registry->number_of_urls * sizeof(int));
    for (i = 0; i < registry->number_of_urls; i++) {
	registry->doclen[i] = 0;
	registry->totalread[i] = 0;
	registry->totalbread[i] = 0;
	registry->totalposted[i] = 0;
	registry->port[i] = 80;
	registry->started[i] = 0;
	registry->finished[i] = 0;
	registry->failed[i] = 0;
	registry->good[i] = 0;
    }
}

MODULE = HTTPD::Bench::ApacheBench	PACKAGE = HTTPD::Bench::ApacheBench
PROTOTYPES: ENABLE


HV *
ab(data_hash)
     SV * data_hash;

     PREINIT:
     char *pt,**url_keys;
     int i,j,k;
     int def_filesize; /* default filesize for all runs */
     struct global *registry = malloc(sizeof(struct global));

     CODE:
     SV * runs;
     SV * urls;
     SV * post_data;
     SV * cookies;
     AV * run_group, *tmpav;
     SV * tmpsv;
     HV * tmphv;
     STRLEN len;

     if (AB_DEBUG2) printf("AB_DEBUG: start of ab()\n");

     registry->concurrency = 1;
     registry->requests = 0;
     registry->tail = 0;
     registry->done = 0;
     registry->need_to_be_done = 0;
     strcpy(registry->warn_and_error, "\nWarning message from ab.");
     registry->total_bytes_received = 0;
     registry->err_length = 0;
     registry->err_conn = 0;
     registry->err_except = 0;

     /*Get necessary initial information and initialize*/
     tmphv = (HV *)SvRV(data_hash);

     tmpsv = *(hv_fetch(tmphv, "concurrency", 11, 0));
     registry->concurrency = SvIV(tmpsv);

     tmpsv = *(hv_fetch(tmphv, "filesize", 8, 0));
     def_filesize = SvIV(tmpsv);

     tmpsv = *(hv_fetch(tmphv, "repeat", 6, 0));
     registry->repeat = SvIV(tmpsv);

     tmpsv = *(hv_fetch(tmphv, "priority", 8, 0));
     pt = SvPV(tmpsv, len);
     if (strcmp(pt, "run_priority") == 0)
         registry->priority = RUN_PRIORITY;
     else {
	 registry->priority = EQUAL_OPPORTUNITY;
	 if (strcmp(pt, "equal_opportunity") != 0)
	     myerr(registry->warn_and_error, "Unknown priority value (the only possible priorities are run_priority and equal_opportunity), using default: equal_opportunity");
     }

     runs = *(hv_fetch(tmphv, "runs", 4, 0));
     run_group = (AV *)SvRV(runs);
     registry->number_of_runs = av_len(run_group) + 1;

     registry->order = malloc(registry->number_of_runs * sizeof(int));
     registry->repeats = malloc(registry->number_of_runs * sizeof(int));
     registry->position = malloc((registry->number_of_runs+1) * sizeof(int));

     if (AB_DEBUG2) printf("AB_DEBUG: done with ab() initialization\n");

     for (i = 0,j = 0; i < registry->number_of_runs; i++) {
	 if (AB_DEBUG2) printf("AB_DEBUG: starting run %d setup\n", i);

	 registry->repeats[i] = registry->repeat;

	 tmpsv = *(av_fetch(run_group, i, 0));
	 if (SvROK(tmpsv)) {
	     tmphv = (HV *)SvRV(tmpsv);
	     if (hv_exists(tmphv, "repeat", 6)) {
		 /* Number of requests to make*/
		 tmpsv = *(hv_fetch(tmphv, "repeat", 6, 0));
		 registry->repeats[i] = SvIV(tmpsv);
	     }
	 }

	 registry->requests = ap_max(registry->requests, registry->repeats[i]);

	 urls = *(hv_fetch(tmphv, "urls", 4, 0));
	 tmpav = (AV *) SvRV(urls);
	 registry->position[i] = registry->number_of_urls;

	 if (AB_DEBUG2) printf("AB_DEBUG: run %d, position[%d] == %d\n", i, i, registry->position[i]);

	 registry->number_of_urls += av_len(tmpav) + 1;

	 if (hv_exists(tmphv, "order", 5)) {
	     tmpsv = *(hv_fetch(tmphv, "order", 5, 0));
	     pt = SvPV(tmpsv, len);
	     if (strcmp(pt, "depth_first") == 0) {
		 registry->order[i] = DEPTH_FIRST;
		 j += 1;
	     } else if (strcmp(pt, "breadth_first") == 0) {
		 registry->order[i] = BREADTH_FIRST;
		 j += registry->repeats[i];
	     } else {
		 myerr(registry->warn_and_error, "invalid order: order can only be depth_first or breadth_first");
		 registry->order[i] = BREADTH_FIRST;
		 j += registry->repeats[i];
	     }
	 } else {
	     registry->order[i] = BREADTH_FIRST;
	     j += registry->repeats[i];
	 }
     }
     if (registry->number_of_urls <= 0) {
	 myerr(registry->warn_and_error, "No urls,");
	 return;
     }
     registry->position[registry->number_of_runs] = registry->number_of_urls;
     registry->concurrency = ap_min(registry->concurrency, j);

     if (AB_DEBUG2) printf("AB_DEBUG: set all run info, ready to call initialize()\n");

     initialize(registry);

     url_keys = malloc(registry->number_of_urls * sizeof(char *));

     for (k = 0; k < registry->number_of_runs; k++) {
	 if (AB_DEBUG2) printf("AB_DEBUG: starting run %d setup2 - postdata + cookie\n", k);

	 registry->filesize[k] = def_filesize;
	 tmpsv = *(av_fetch(run_group, k, 0));
	 if (SvROK(tmpsv)) {
	     tmphv = (HV *)SvRV(tmpsv);
	     if (hv_exists(tmphv, "filesize", 8)) {
		 tmpsv = *(hv_fetch(tmphv, "filesize", 8, 0));
		 registry->filesize[k] = SvIV(tmpsv);
	     }
	 }

	 if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 1\n", k);

	 urls = *(hv_fetch(tmphv, "urls", 4, 0));
	 post_data = *(hv_fetch(tmphv, "postdata", 8, 0));
	 cookies = *(hv_fetch(tmphv, "cookie", 6, 0));
	 for (i = registry->position[k]; i < registry->position[k+1]; i++) {
	     tmpav =(AV *) SvRV(urls);
	     tmpsv = *(av_fetch(tmpav, i - registry->position[k], 0));
	     if (SvPOK(tmpsv)) {
		 pt = SvPV(tmpsv, len);
		 url_keys[i] = pt;

		 if (parse_url(registry, pt, i)) {
		     char *warn = malloc(1024);
		     sprintf(warn, "Invalid url: %s, the information for this url may be wrong", pt);
		     myerr(registry->warn_and_error, warn);
		     free(warn);
		 }
	     } else {
		 char *warn = malloc(1024);
		 sprintf(warn, "Undefined url in urls list");
		 myerr(registry->warn_and_error, warn);
		 free(warn);
	     }
	 }

	 if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 2\n", k);

	 tmpav = (AV *) SvRV(post_data);
	 i = ap_min(registry->position[k+1],
		    registry->position[k] + av_len(tmpav) + 1);

	 if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 2.1\n", k);

	 for (j = registry->position[k]; j < i; j++) {
	     if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 2.2, j=%d\n", k, j);
	     tmpsv = *(av_fetch(tmpav, j - registry->position[k], 0));
	     if (SvPOK(tmpsv)) {
		 if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 2.3, j=%d\n", k, j);
		 pt = SvPV((SV *)tmpsv, len);
		 if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 2.4, j=%d\n", k, j);
		 registry->postdata[j] = pt;
		 registry->postlen[j] = registry->posting[j] = len;
	     } else {
		 registry->postlen[j] = registry->posting[j] = 0;
	     }
	     if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 2.5, j=%d\n", k, j);
	 }

	 if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 3\n", k);

	 for (j = i; j < registry->position[k+1]; j++) {
	     registry->posting[j] = registry->postlen[j] = 0;
	     /*If the number of postdata strings is less than
	       that of urls,then assign empty strings*/
	 }

	 if (AB_DEBUG2) printf("AB_DEBUG: run %d setup2 - stage 4\n", k);

	 registry->cookie[k] = NULL;
	 tmpav = (AV *) SvRV(cookies);
	 if (av_len(tmpav) >= 0) {
	     tmpsv = *(av_fetch(tmpav, 0, 0));
	     if (SvPOK(tmpsv)) {
		 pt = SvPV((SV *)tmpsv, len);
		 if (len != 0) {
		     registry->cookie[k] = malloc((len+11) * sizeof(char));
		     strcpy(registry->cookie[k], "Cookie: ");
		     strcat(registry->cookie[k], pt);
		     strcat(registry->cookie[k], "\r\n");
		     if (AB_DEBUG2) printf("AB_DEBUG: cookie[%d] == '%s'\n", k, registry->cookie[k]);
		 }
	     }
	 }
     }

     if (AB_DEBUG2) printf("AB_DEBUG: ready to test()\n");

     test(registry);

     if (AB_DEBUG2) printf("AB_DEBUG: done with test()\n");

     RETVAL = newHV();/* ready to get information stored in global variables */

     for (k = 0; k < registry->number_of_runs; k++) {
	 HV * run_hash = newHV();
		          /* Store value for each thread */
	 tmpav = newAV();   /* initial an array to keep the thread information */

	 if (AB_DEBUG2) printf("AB_DEBUG: getting regression info for run %d\n", k);

	 for (i = 0; i < registry->repeats[k]; i++) {
	     AV *th_t = newAV();  /* times for processing and connecting */
	     AV *th_c = newAV();	  /* connecting times */
	     AV *url_contents = newAV();  /* pages read from servers */
	     AV *headers = newAV();
	     tmphv = newHV();

	     for (j = registry->position[k]; j < registry->position[k+1]; j++) {
		 av_push(th_t, newSVnv(registry->stats[j][i].time));
		 av_push(th_c, newSVnv(registry->stats[j][i].ctime));
		 av_push(url_contents, newSVpv(registry->stats[j][i].page_content,0));
		 av_push(headers, newSVpv(registry->stats[j][i].header,0));
	     }

	     hv_store(tmphv, "total_time", 10, newRV_inc((SV *)th_t), 0);
	     hv_store(tmphv, "connect_time", 12, newRV_inc((SV *)th_c), 0);
	     hv_store(tmphv, "page_content", 12, newRV_inc((SV *)url_contents), 0);
	     hv_store(tmphv, "headers", 7, newRV_inc((SV *)headers), 0);
	     av_push(tmpav, newRV_inc((SV*)tmphv));
	 }

	 hv_store(run_hash, "threads", 7, newRV_inc((SV *)tmpav), 0);
		/* The end of store value for key "threads" */

		/* Store information for each url now */
	 for (i = registry->position[k]; i < registry->position[k+1]; i++) {
	     int totalcon = 0, total = 0;
	     int mincon = 9999999, mintot = 999999;
	     int maxcon = 0, maxtot = 0;

	     tmphv = newHV();
	     for (j = 0; j < registry->repeats[k]; j++) {
		 struct data s = registry->stats[i][j];
		 mincon = ap_min(mincon, s.ctime);
		 mintot = ap_min(mintot, s.time);
		 maxcon = ap_max(maxcon, s.ctime);
		 maxtot = ap_max(maxtot, s.time);
		 totalcon += s.ctime;
		 total += s.time;
		 hv_store(tmphv, "max_connect_time", 16, newSVnv(maxcon), 0);
		 hv_store(tmphv, "max_time", 8, newSVnv(maxtot), 0);
		 hv_store(tmphv, "min_connect_time", 16, newSVnv(mincon), 0);
		 hv_store(tmphv, "min_time", 8, newSVnv(mintot), 0);
		 hv_store(tmphv, "average_connect_time", 20, newSVnv((float)totalcon/registry->repeats[k]), 0);
		 hv_store(tmphv, "average_time", 12, newSVnv((double)total/registry->repeats[k]), 0);
	     }
	     hv_store(tmphv, "hostname", 8, newSVpv(registry->hostname[i],0), 0);
	     hv_store(tmphv, "software", 8, newSVpv(registry->servername[i],0), 0);
	     hv_store(tmphv, "port", 4, newSVnv(registry->port[0]), 0);
	     hv_store(tmphv, "doc_length", 10, newSVnv(registry->doclen[i]), 0);
	     hv_store(tmphv, "path", 4, newSVpv(registry->path[i],0), 0);
	     hv_store(tmphv, "completed_requests", 18, newSVnv(registry->finished[i]), 0);
	     hv_store(tmphv, "failed_requests", 15, newSVnv(registry->failed[i]), 0);
	     hv_store(tmphv, "total_read", 10, newSVnv(registry->totalread[i]), 0);
	     hv_store(tmphv, "page_content", 12, newSVpv(registry->page_content[i],0), 0);
	     hv_store(tmphv, "header", 6, newSVpv(registry->stats[i][0].header,0), 0);
	     if (registry->posting[i] > 0)
		 hv_store(tmphv, "total_posted", 12, newSVnv(registry->totalposted[i]), 0);
	     hv_store(run_hash, url_keys[i], strlen(url_keys[i]), newRV_inc((SV *)tmphv), 0);
	 }
	 {
	     char key[10];
	     sprintf(key, "run%d", k);
	     hv_store(RETVAL, key, strlen(key), newRV_inc((SV *)run_hash), 0);
	 }
     }
     hv_store(RETVAL, "warn", 4, newSVpv(registry->warn_and_error,0), 0);
     hv_store(RETVAL, "total_time", 10, newSVnv(timedif(registry->endtime,registry->start)), 0);
     hv_store(RETVAL, "bytes_received", 14, newSVnv(registry->total_bytes_received), 0);

     OUTPUT:
     RETVAL
