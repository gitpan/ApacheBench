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
   **    - CODE FORK: added Perl XS interface and is now released on CPAN as
   **      HTTPD::Bench::ApacheBench, September 2000
   **    - merged code from Apache 1.3.22 ab, October-November 2001
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

// uncomment to turn on debugging messages
//#define AB_DEBUG 1

/*  -------------------------------------------------------------------- */

#ifdef AB_DEBUG
#define AB_DEBUG_XS 1
#else
#define AB_DEBUG_XS 0
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
#include <string.h>

#define ap_select       select
#else				/* (!)NO_APACHE_INCLUDES */
#include "ap_config.h"
#include "ap.h"
#ifdef CHARSET_EBCDIC
#include "ebcdic.h"
#endif
#include <fcntl.h>
#if !defined(MPE) && !defined(WIN32)
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

#define CBUFFSIZE        4096
#define WARN_BUFFSIZE   10240
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
    int url;			/* which url are we testing */
    int read;			/* amount of bytes read */
    int bread;			/* amount of body read */
    int length;			/* Content-Length value used for keep-alive */
    char cbuff[CBUFFSIZE];	/* a buffer to store server response header */
    int cbx;			/* offset in cbuffer */
    int keepalive;		/* non-zero if a keep-alive request */
    int gotheader;		/* non-zero if we have the entire header in
				 * cbuff */
    int thread;			/* Thread number */
    int run;

    struct timeval start_time, connect_time, sent_request_time, done_time;

    char *request;		/* HTTP request */
    char *request_headers;
    int reqlen;

    char *response_headers;	/* HTTP response */
    char *response;
};

struct data {
    int run;			/* which run */
    int thread; 		/* Thread number */
    int read;			/* number of bytes read */
    int bread;			/* total amount of entity body read */
    int ctime;			/* time in ms to connect */
    int rtime;			/* time in ms for http request */
    int time;			/* time in ms for full req/resp interval */

    char *request;
    char *request_headers;

    char *response_headers;
    char *response;
};

struct threadval {
    int run;			/* which run */
    int url;			/* which url are we testing */
    int thread; 		/* Thread number */
};


/* --------------------- GLOBALS ---------------------------- */

struct global {
    int concurrency;		/* Number of multiple requests to make */
    int *repeats;		/* Number of time to repeat for each run */
    int requests;		/* the max of the repeats */
    double tlimit;		/* global time limit, in seconds */
    struct timeval min_tlimit;	/* minimum of all time limits */
    int *position;		/* The position next run starts */

    char **hostname;		/* host name */
    int *port;			/* port numbers */
    char **path;		/* path name */
    char **ctypes;		/* values for Content-type: headers */
    double *url_tlimit;		/* time limit in seconds for each url */
    bool *keepalive;		/* whether to use Connection: Keep-Alive */

    int *posting;		/* GET if ==0, POST if >0, HEAD if <0 */
    char **postdata, **cookie;	/* datas for post and optional cookie line */
    char **req_headers;		/* optional arbitrary request headers to add */
    char ***auto_cookies;	/* cookies extracted from response_headers for the run, i.e. set by http server */
    bool *use_auto_cookies;	/* whether to use auto_cookie feature for the run */
    int *postlen;		/* length of data to be POSTed */
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
    int *buffersize;
    int *memory;
    int number_of_urls, number_of_runs;

    char warn_and_error[WARN_BUFFSIZE];  /* warn and error message returned to perl */

    int total_bytes_received;
    struct timeval starttime, endtime;

    /* one global throw-away buffer to read stuff into */
    char buffer[8192];

    struct connection *con;	/* connection array */

    /* regression data for each request */
    struct data **stats;

    fd_set readbits, writebits;	/* bits for select */
    struct sockaddr_in server;	/* server addr structure */
};



/* --------------------------------------------------------- */

/* keep warn and error massege */

void
myerr(char *warn_and_error, char *s) {
    if ((strlen(warn_and_error) + strlen(s)) < (WARN_BUFFSIZE - 35)) {
	strcat(warn_and_error, "\n[Warn:] ");
	strcat(warn_and_error, s);
    } else if(strlen(warn_and_error) < (WARN_BUFFSIZE - 35))
	strcat(warn_and_error, "\nToo many warn and error messages!");
}

/* --------------------------------------------------------- */

/* write out request to a connection - assumes we can write
   (small) request out in one go into our new socket buffer  */

static void
write_request(struct global * registry, struct connection * c) {

#ifndef NO_WRITEV
    struct iovec out[2];
    int outcnt = 1;
#endif
#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 1, registry->done = %d\n", registry->done);
#endif
    gettimeofday(&c->connect_time, 0);
    reset_request(registry, c);
#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 2, registry->done = %d\n", registry->done);
#endif

#ifndef NO_WRITEV
    out[0].iov_base = c->request;
    out[0].iov_len = c->reqlen;

#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 2a.1, registry->done = %d\n", registry->done);
#endif
    if (registry->posting[c->url] > 0) {
	out[1].iov_base = registry->postdata[c->url];
	out[1].iov_len = registry->postlen[c->url];
	outcnt = 2;
	registry->totalposted[c->url] = (c->reqlen + registry->postlen[c->url]);
    }
#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 2a.2, registry->done = %d\n", registry->done);
#endif
    writev(c->fd, out, outcnt);

#else /* NO_WRITEV */

#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 2b.1, registry->done = %d\n", registry->done);
#endif
    ab_write(c->fd, c->request, c->reqlen);
    if (registry->posting[c->url] > 0) {
        ab_write(c->fd, registry->postdata[c->url], registry->postlen[c->url]);
    }
#endif /* NO_WRITEV */

#ifdef AB_DEBUG
    printf("AB_DEBUG: write_request() - stage 3, registry->done = %d\n", registry->done);
#endif

    FD_SET(c->fd, &registry->readbits);
    FD_CLR(c->fd, &registry->writebits);
    gettimeofday(&c->sent_request_time, 0);

    if (registry->memory[c->run] >= 3)
	c->response = calloc(1, registry->buffersize[c->run]);
}

/* --------------------------------------------------------- */

/* make an fd non blocking */

static void
nonblock(int fd) {
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
timedif(struct timeval a, struct timeval b) {
    register int us, s;

    us = a.tv_usec - b.tv_usec;
    us /= 1000;
    s = a.tv_sec - b.tv_sec;
    s *= 1000;
    return s + us;
}


/* --------------------------------------------------------- */

/* converts a double precision number of seconds to a timeval */

static struct timeval
double2timeval(double secs) {
    register struct timeval retval;

    retval.tv_sec = (long)secs;
    retval.tv_usec = (long)((secs - (long)secs) * 1000000);
    return retval;
}

/* converts a timeval to a double precision number of seconds */

static double
timeval2double(struct timeval a) {
    return (double)a.tv_sec + (double)a.tv_usec / 1000000;
}


/* --------------------------------------------------------- */
/* declare close_connection to make it accessible to start_connect */

static void
close_connection(struct global * registry, struct connection * c);

/* --------------------------------------------------------- */

/* start asnchronous non-blocking connection */

static void
start_connect(struct global * registry, struct connection * c) {
    c->read = 0;
    c->bread = 0;
    c->keepalive = 0;
    c->cbx = 0;
    c->gotheader = 0;
    c->fd = socket(AF_INET, SOCK_STREAM, 0);

#ifdef AB_DEBUG
    printf("AB_DEBUG: start of start_connect()\n");
#endif

    if (c->fd < 0) {
	myerr(registry->warn_and_error, "socket error");
	registry->failed[c->url]++;
	close_connection(registry, c);
	return;
    }
    nonblock(c->fd);

#ifdef AB_DEBUG
    printf("AB_DEBUG: start_connect() - stage 1\n");
#endif

    c->connect_time.tv_sec = 0;
    c->connect_time.tv_usec = 0;
    c->sent_request_time.tv_sec = 0;
    c->sent_request_time.tv_usec = 0;
    gettimeofday(&c->start_time, 0);

    {
	/* get server information */
	struct hostent *he;
#ifdef AB_DEBUG
	printf("AB_DEBUG: start_connect() - stage 2, c->url: '%d'\n", c->url);
#endif
	he = gethostbyname(registry->hostname[c->url]);
#ifdef AB_DEBUG
	printf("AB_DEBUG: start_connect() - stage 3\n");
#endif
	if (!he) {
	    char * warn = malloc(256 * sizeof(char));
	    sprintf(warn, "Bad hostname: %s, the information stored for it could be wrong!", registry->hostname[c->url]);
	    myerr(registry->warn_and_error, warn);
	    free(warn);
	    /* bad hostname, yields the resource */
	    registry->failed[c->url]++;
	    close_connection(registry, c);
	    return;
	}
#ifdef AB_DEBUG
	printf("AB_DEBUG: start_connect() - stage 4\n");
#endif
	registry->server.sin_family = he->h_addrtype;
	registry->server.sin_port = htons(registry->port[c->url]);
	registry->server.sin_addr.s_addr = ((unsigned long *) (he->h_addr_list[0]))[0]; 
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: start_connect() - stage 5\n");
#endif

    if (connect(c->fd, (struct sockaddr *) & registry->server, sizeof(registry->server)) < 0) {
	if (errno == EINPROGRESS) {
	    FD_SET(c->fd, &registry->writebits);
	    registry->started[c->url]++;
	    return;
	} else {
	    ab_close(c->fd);
	    /* retry the same request 10 times if it fails to connect */
	    if (registry->failed[c->url]++ > 10) {
		myerr(registry->warn_and_error,
		      "Test aborted after 10 failures");
		/* yields the resource */
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
    registry->started[c->url]++;
    FD_SET(c->fd, &registry->writebits);
}

/* --------------------------------------------------------- */

/* move connection to the next run, because the current run either doesn't need
   or cannot use any more connection slots (resources)
   returns 1 if the next request is ready to be sent,
   returns 0 if this connection is done */

static int
schedule_request_in_next_run(struct global * registry, struct connection * c) {
    c->run++;
    while (c->run < registry->number_of_runs) {
	if (registry->started[registry->position[c->run + 1] - 1] >= registry->repeats[c->run]
	    || (registry->order[c->run] == DEPTH_FIRST
		&& registry->started[registry->position[c->run]] > 0)) {
	    /* this run has finished all repetitions of url requests
	       or is a depth_first run which only requires one slot,
	       so doesn't need this resource anymore */
	    c->run++;
	    continue;
	}
	/* start at first url in the run */
	c->url = registry->position[c->run];
	if (registry->started[c->url] < registry->repeats[c->run]) {
	    /* for breadth_first, start one more connect to 1st url if possible
	       for depth_first, get started here */
	    c->thread = registry->which_thread[c->url][registry->started[c->url]];
	    return 1;
	}
	/* look at each url in the sequence until we find one which needs
	   to be repeated more */
	while (++c->url < registry->position[c->run + 1]
	       && registry->started[c->url] >= registry->repeats[c->run]);
	/* only start another request from this run if more requests of the
	   previous url in the sequence have finished (in-order execution) */
	if (registry->started[c->url] < registry->finished[c->url - 1]) {
	    c->thread = registry->which_thread[c->url][registry->started[c->url]];
	    return 1;
	} else
	    /* this run doesn't need any more resources */
	    c->run++;
    }
    /* no one needs any more resources */
    c->state = STATE_DONE;
    return 0;
}

/* --------------------------------------------------------- */

/* setup the next request in the sequence / repetition / run to be sent
   returns 1 if the next request is ready to be sent,
   returns 0 if this connection is done,
   sets the connection values: c->run, c->url, c->thread, and c->state,
   as well as helper structures: registry->which_thread[][],
     registry->ready_to_run_queue[], and registry->arranged[]
*/

static int
schedule_next_request(struct global * registry, struct connection * c) {

    if (registry->priority == RUN_PRIORITY) {
	/* if the last url in this run has repeated enough, go to next run */
	if (registry->started[registry->position[c->run + 1] - 1] >= registry->repeats[c->run])
	    return schedule_request_in_next_run(registry, c);

	/* possible more resources needed in this group */
	/* started[position[c->run + 1] - 1] < repeats[c->run] */
	if (registry->order[c->run] == DEPTH_FIRST) {
	    /* for depth_first, connect the next one and restart the
	       sequence if we're at the last url in the run */
	    if (++c->url == registry->position[c->run + 1]) {
		c->url = registry->position[c->run];
		c->thread = registry->started[c->url];
	    }
	    return 1;
	} else { /* breadth_first */
	    if (c->url < (registry->position[c->run + 1] - 1))
		/* TODO: check if (registry->finished[c->url] > 0) ??? */
		registry->which_thread[c->url+1][registry->finished[c->url] - 1] = c->thread;
	    if (registry->started[c->url] == registry->repeats[c->run])
		/* go to next url in sequence if we repeated this one enough */
		c->url++;
	    if (c->url == registry->position[c->run]) {
		/* this is the first url in the sequence: set its repetition
		   number to the initial incremental value (0, 1, 2, 3, ...) */
		c->thread = registry->which_thread[c->url][registry->started[c->url]];
		return 1;
	    }
	    /* only start another request from this run if more requests of the
	       previous url in the sequence have finished(in-order execution)*/
	    if (registry->started[c->url] < registry->finished[c->url - 1]) {
		c->thread = registry->started[c->url];
		return 1;
	    } else {
		return schedule_request_in_next_run(registry, c);
	    }
	}

    } else { /* equal_opportunity */
	/* we use a FIFO to queue up requests to be sent */
	if (c->url < registry->position[c->run + 1]-1) {
	    /* if url is before the end of the url sequence,
	       add it to the tail of the request queue */
	    registry->ready_to_run_queue[registry->tail].url = c->url + 1;
	    registry->ready_to_run_queue[registry->tail].thread = c->thread;
	    registry->ready_to_run_queue[registry->tail++].run = c->run;
	    registry->arranged[c->url + 1]++;
	} else if (registry->order[c->run] == DEPTH_FIRST
		   && registry->arranged[registry->position[c->run]] < registry->repeats[c->run]) {
	    /* end of the url sequence in depth_first with more repetitions
	       necessary: start from the beginning of the url sequence */
	    registry->ready_to_run_queue[registry->tail].url = registry->position[c->run];
	    registry->ready_to_run_queue[registry->tail].thread = registry->arranged[registry->position[c->run]]++;
	    registry->ready_to_run_queue[registry->tail++].run = c->run;
	}

	if (registry->head >= registry->tail) {
	    c->state = STATE_DONE;
	    return 0;
	}
	c->thread = registry->ready_to_run_queue[registry->head].thread;
	c->url = registry->ready_to_run_queue[registry->head].url;
	c->run = registry->ready_to_run_queue[registry->head++].run;
	return 1;
    }
}

/* --------------------------------------------------------- */

/* save regression data and benchmark timings */

static void
store_regression_data(struct global * registry, struct connection * c) {
    struct data s;

#ifdef AB_DEBUG
    printf("AB_DEBUG: start of store_regression_data()\n");
#endif

    if (registry->failed[c->url] > 0)
        return;

    if (c->read >= registry->buffersize[c->run] &&
	registry->memory[c->run] >= 3) {
	char * warn = malloc(256 * sizeof(char));
	sprintf(warn, "[run %d, iter %d, req %d]: Buffer size of %d is too small, got response of size %d", c->run, c->thread, c->url, registry->buffersize[c->run], c->read);
	myerr(registry->warn_and_error, warn);
	free(warn);
    }

    if (c->read == 0) {
	if (registry->memory[c->run] >= 3)
	    c->response = "";
	if (registry->memory[c->run] >= 2)
	    c->response_headers = "";
    }

    if (registry->memory[c->run] >= 1) {
	gettimeofday(&c->done_time, 0);
	if (c->connect_time.tv_sec || c->connect_time.tv_usec)
	    s.ctime = timedif(c->connect_time, c->start_time);
	else
	    s.ctime = 0;
	if (c->sent_request_time.tv_sec || c->sent_request_time.tv_usec)
	    s.rtime = timedif(c->sent_request_time, c->start_time);
	else
	    s.rtime = 0;
	s.time = timedif(c->done_time, c->start_time);
	s.thread = c->thread;
	s.read = c->read;
    }
    if (registry->memory[c->run] >= 2) {
	s.bread = c->bread;
	s.request_headers = malloc((strlen(c->request_headers)+1) * sizeof(char));
	s.response_headers = malloc((strlen(c->response_headers)+1) * sizeof(char));
	strcpy(s.request_headers, c->request_headers);
	strcpy(s.response_headers, c->response_headers);
    }
    if (registry->memory[c->run] >= 3) {
	s.response = malloc((strlen(c->response)+1) * sizeof(char));
	strcpy(s.response, c->response);
	if (registry->posting[c->url] > 0) {
	    s.request = malloc((strlen(c->request) +
				registry->postlen[c->url] + 1) *
				    sizeof(char));
	    strcpy(s.request, c->request);
	    strcat(s.request, registry->postdata[c->url]);
	} else {
	    s.request = malloc((strlen(c->request)+1) * sizeof(char));
	    strcpy(s.request, c->request);
	}
    }

    registry->stats[c->url][c->thread] = s;

    registry->total_bytes_received += c->read;
}

/* --------------------------------------------------------- */

/* remove existing cookies from registry->auto_cookies[..][..] which will be set again by extract_cookies_from_response() */

static void
remove_existing_cookie_from_auto_cookies(struct global * registry, struct connection * c, char * set_cookie_hdr) {

#ifdef AB_DEBUG
    printf("AB_DEBUG: start of remove_existing_cookie_from_auto_cookies()\n");
#endif
    // first need to find the name of cookie on current "Set-Cookie: " header line
    char * cookie_name = calloc(128, sizeof(char));
    strcat(cookie_name, "Cookie: ");
    strncat(cookie_name, set_cookie_hdr+14, strstr(set_cookie_hdr+14, "=") - (set_cookie_hdr+14));

#ifdef AB_DEBUG
    printf("AB_DEBUG: remove_existing_cookie_from_auto_cookies() - stage 1\n");
#endif

    char * existing_cookie = strstr(registry->auto_cookies[c->run][c->thread], cookie_name);
    if (existing_cookie) {
        char * new_auto_cookies = calloc(strlen(registry->auto_cookies[c->run][c->thread]), sizeof(char));
        strncpy(new_auto_cookies, registry->auto_cookies[c->run][c->thread], existing_cookie - registry->auto_cookies[c->run][c->thread]);
        char * end_of_existing_cookie = strstr(existing_cookie, "\r\n");
        strcat(new_auto_cookies, end_of_existing_cookie+2);

        // overwrite auto_cookies with new version with existing_cookie removed
        strcpy(registry->auto_cookies[c->run][c->thread], new_auto_cookies);
        free(new_auto_cookies);
    }

    free(cookie_name);
}

/* extract cookies from response_data (Set-Cookie: headers) and save to auto_cookies */

static void
extract_cookies_from_response(struct global * registry, struct connection * c) {
#ifdef AB_DEBUG
    printf("AB_DEBUG: start of extract_cookies_from_response()\n");
#endif
    if (registry->failed[c->url] > 0)
        return;

    if (! registry->auto_cookies[c->run])
        registry->auto_cookies[c->run] = calloc(registry->repeats[c->run], sizeof(char));
        
    if (! registry->auto_cookies[c->run][c->thread]) {
        registry->auto_cookies[c->run][c->thread] = malloc(CBUFFSIZE * sizeof(char));
        strcat(registry->auto_cookies[c->run][c->thread], "");
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: extract_cookies_from_response() - stage 1; run %d, thread %d\n", c->run, c->thread);
#endif

    if (! c->response_headers) return;

    char * set_cookie_hdr = strstr(c->response_headers, "\r\nSet-Cookie: ");
    while (set_cookie_hdr) {
        remove_existing_cookie_from_auto_cookies(registry, c, set_cookie_hdr);

        char * eoh = strstr(set_cookie_hdr+2, "\r\n");
        if (! strnstr(set_cookie_hdr, "=; Expires=", eoh - set_cookie_hdr)) // hack: do not set expired headers
            // drop the "Set-" from beginning to just append "Cookie: ....\r\n"
            strncat(registry->auto_cookies[c->run][c->thread], set_cookie_hdr + 6, eoh - set_cookie_hdr - 4);

        set_cookie_hdr = strstr(set_cookie_hdr+1, "\r\nSet-Cookie: ");
    }
}

/* --------------------------------------------------------- */

/* close down connection and save stats */

static void
close_connection(struct global * registry, struct connection * c) {
#ifdef AB_DEBUG
    printf("AB_DEBUG: start of close_connection()\n");
#endif

    if (registry->use_auto_cookies[c->run])
        extract_cookies_from_response(registry, c);
    store_regression_data(registry, c);
    registry->finished[c->url]++;

#ifdef AB_DEBUG
    printf("AB_DEBUG: close_connection() - stage 1\n");
#endif

    ab_close(c->fd);
    FD_CLR(c->fd, &registry->readbits);
    FD_CLR(c->fd, &registry->writebits);

#ifdef AB_DEBUG
    printf("AB_DEBUG: close_connection() - stage 2\n");
#endif

    /* finish if last response has been received */
    if (++registry->done >= registry->need_to_be_done)
	return;

#ifdef AB_DEBUG
    printf("AB_DEBUG: close_connection() - stage 3\n");
#endif

    /* else continue with requests in run queues */
    if (schedule_next_request(registry, c))
	start_connect(registry, c);
}

/* --------------------------------------------------------- */

/* read data from connection */

static void
read_connection(struct global * registry, struct connection * c) {
    int r;

#ifdef AB_DEBUG
    printf("AB_DEBUG: start of read_connection()\n");
#endif

    r = ab_read(c->fd, registry->buffer, sizeof(registry->buffer));
    if (r == 0 || (r < 0 && errno != EAGAIN)) {
	if (errno == EINPROGRESS)
	    registry->good[c->url]++;
	close_connection(registry, c);
	return;
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: read_connection() - stage 1\n");
#endif

    if (r < 0 && errno == EAGAIN)
	return;
    c->read += r;
    if (c->read < registry->buffersize[c->run]-1 &&
	registry->memory[c->run] >= 3)
	strncat(c->response, registry->buffer, r);

#ifdef AB_DEBUG
    printf("AB_DEBUG: read_connection() - stage 2\n");
#endif

    if (!c->gotheader) {
	char *s;
	int wslen = 4;
	int space = CBUFFSIZE - c->cbx - 1;	/* -1 to allow for 0
						 * terminator */
	int tocopy = (space < r) ? space : r;
#ifndef CHARSET_EBCDIC
	memcpy(c->cbuff + c->cbx, registry->buffer, tocopy);
#else				/* CHARSET_EBCDIC */
	ascii2ebcdic(c->cbuff + c->cbx, registry->buffer, tocopy);
#endif				/* CHARSET_EBCDIC */
	c->cbx += tocopy;
	space -= tocopy;
	c->cbuff[c->cbx] = 0;	/* terminate for benefit of strstr */
	s = strstr(c->cbuff, "\r\n\r\n");
	/*
	 * this next line is so that we talk to NCSA 1.5 which blatantly
	 * breaks the http specification
	 */
	if (!s) {
	    s = strstr(c->cbuff, "\n\n");
	    wslen = 2;
	}
	if (!s) {
	    /* read rest next time */
	    if (registry->memory[c->run] >= 2)
		c->response_headers = "";
	    if (space)
		return;
	    else {
		/*
		 * header is in invalid or too big - close connection
		 */
		ab_close(c->fd);
		FD_CLR(c->fd, &registry->writebits);
		start_connect(registry, c);
	    }
	} else {
	    /* have full header */

	    /*
	     * XXX: this parsing isn't even remotely HTTP compliant... but in
	     * the interest of speed it doesn't totally have to be, it just
	     * needs to be extended to handle whatever servers folks want to
	     * test against. -djg
	     */

	    c->gotheader = 1;
	    *s = 0;		/* terminate at end of header */
	    if (registry->memory[c->run] >= 2) {
		c->response_headers = malloc(CBUFFSIZE * sizeof(char));
		strcpy(c->response_headers, c->cbuff);
	    }
	    if (registry->keepalive[c->url] &&
		(strstr(c->cbuff, "Keep-Alive") ||
		 strstr(c->cbuff, "keep-alive"))) { /* for benefit of MSIIS */
		char *cl;
		cl = strstr(c->cbuff, "Content-Length:");
		/* handle NCSA, which sends Content-length: */
		if (!cl)
		    cl = strstr(c->cbuff, "Content-length:");
		if (cl) {
		    c->keepalive = 1;
		    c->length = atoi(cl + 16);
		}
	    }
	    c->bread += c->cbx - (s + wslen - c->cbuff) + r - tocopy;
	}
    } else {
	/* outside header, everything we have read is entity body */
	c->bread += r;
    }

    /*
     * cater for the case where we're using keepalives and doing HEAD
     * requests
     */
    if (c->keepalive &&
	((c->bread >= c->length) || (registry->posting[c->url] < 0))) {
	/* save current url for checking for hostname/port changes */
	int prev = c->url;

	/* finished a keep-alive connection */
	registry->good[c->url]++;
	registry->finished[c->url]++;

	store_regression_data(registry, c);

	if (++registry->done >= registry->need_to_be_done)
	    return;

	if (!schedule_next_request(registry, c))
	    return;

	c->length = 0;
	c->gotheader = 0;
	c->cbx = 0;
	c->read = c->bread = 0;
	c->keepalive = 0;

	/* if new hostname/port is different from last hostname/port, or new
	   url is *not* keepalive, then we need to close connection and start
	   a new connection */
	if (registry->keepalive[c->url] &&
	    strcmp(registry->hostname[c->url], registry->hostname[prev]) == 0
	    && registry->port[c->url] == registry->port[prev]) {
	    write_request(registry, c);
	    registry->started[c->url]++;
	    c->start_time = c->connect_time;	/* zero connect time with keep-alive */
	} else {
	    ab_close(c->fd);
	    FD_CLR(c->fd, &registry->readbits);
	    FD_CLR(c->fd, &registry->writebits);
	    start_connect(registry, c);
	}
    }
}

/* --------------------------------------------------------- */

    /* setup or reset request */
int
reset_request(struct global * registry, struct connection * c) {
    int i = c->url;

    char * ctype = malloc(40 * sizeof(char));
    strcpy(ctype, "application/x-www-form-urlencoded");

#ifdef AB_DEBUG
    printf("AB_DEBUG: reset_request() - stage 0.1\n");
#endif
    if (registry->ctypes[i]) {
#ifdef AB_DEBUG
	printf("AB_DEBUG: reset_request() - stage 0.1.1\n");
#endif
	free(ctype);

#ifdef AB_DEBUG
	printf("AB_DEBUG: reset_request() - stage 0.1.2\n");
#endif
	ctype = registry->ctypes[i];
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: reset_request() - stage 1\n");
#endif

    c->request = calloc(1, registry->buffersize[c->run]);
    c->request_headers = calloc(1, registry->buffersize[c->run]);

    if (registry->posting[i] <= 0) {
#ifdef AB_DEBUG
	printf("AB_DEBUG: reset_request() - stage 1.1 (GET)\n");
#endif
	sprintf(c->request_headers, "%s %s HTTP/1.0\r\n"
		"User-Agent: ApacheBench-Perl/%s\r\n"
		"Host: %s\r\n"
		"Accept: */*\r\n",
		(registry->posting[i] == 0) ? "GET" : "HEAD",
		registry->path[i],
		VERSION,
		registry->hostname[i]);
    } else {
#ifdef AB_DEBUG
	printf("AB_DEBUG: reset_request() - stage 1.1 (POST)\n");
#endif
	sprintf(c->request_headers, "POST %s HTTP/1.0\r\n"
		"User-Agent: ApacheBench-Perl/%s\r\n"
		"Host: %s\r\n"
		"Accept: */*\r\n"
		"Content-length: %d\r\n"
		"Content-type: %s\r\n",
		registry->path[i],
		VERSION,
		registry->hostname[i],
		registry->postlen[i],
		ctype);
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: reset_request() - stage 2\n");
#endif

    if (registry->keepalive[i])
	strcat(c->request_headers, "Connection: Keep-Alive\r\n");
    if (registry->cookie[c->run]) {
	strcat(c->request_headers, "Cookie: ");
	strcat(c->request_headers, registry->cookie[c->run]);
	strcat(c->request_headers, "\r\n");
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: reset_request() - stage 2.1: c->run %d; c->thread %d\n", c->run, c->thread);
#endif

    if (registry->use_auto_cookies[c->run] && registry->auto_cookies[c->run] && registry->auto_cookies[c->run][c->thread]) {
        strcat(c->request_headers, registry->auto_cookies[c->run][c->thread]);

#ifdef AB_DEBUG
        printf("AB_DEBUG: reset_request() - stage 2.2: request_headers %s; auto_cookies: %s\n", c->request_headers, registry->auto_cookies[c->run][c->thread]);
#endif
    }

    if (registry->req_headers[i]) {
	strcat(c->request_headers, registry->req_headers[i]);
	strcat(c->request_headers, "\r\n");
    }

    strcat(c->request_headers, "\r\n");

    strcpy(c->request, c->request_headers);
    c->reqlen = strlen(c->request);

#ifdef AB_DEBUG
    printf("AB_DEBUG: reset_request() - stage 3\n");
#endif

#ifdef CHARSET_EBCDIC
    ebcdic2ascii(c->request, c->request, c->reqlen);
#endif				/* CHARSET_EBCDIC */

    return 0;
}

/* --------------------------------------------------------- */

/* run the tests */

static void
test(struct global * registry) {
    struct timeval timeout, now;
    fd_set sel_read, sel_except, sel_write;
    int i;

    registry->con = calloc(registry->concurrency, sizeof(struct connection));
    memset(registry->con, 0, registry->concurrency * sizeof(struct connection));

#ifdef AB_DEBUG
    printf("AB_DEBUG: start of test()\n");
#endif

    for (i = 0; i < registry->concurrency; i++) {
	registry->con[i].url = registry->ready_to_run_queue[i].url;
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
    gettimeofday(&registry->starttime, 0);

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

#ifdef AB_DEBUG
	printf("AB_DEBUG: test() - stage 5.1, registry->done = %d\n", registry->done);
#endif

	/* setup bit arrays */
	memcpy(&sel_except, &registry->readbits, sizeof(registry->readbits));
	memcpy(&sel_read, &registry->readbits, sizeof(registry->readbits));
	memcpy(&sel_write, &registry->writebits, sizeof(registry->writebits));

#ifdef AB_DEBUG
	printf("AB_DEBUG: test() - stage 5.2, registry->done = %d\n", registry->done);
#endif

	/* Timeout of 30 seconds, or minimum time limit specified by config. */
	timeout.tv_sec = registry->min_tlimit.tv_sec;
	timeout.tv_usec = registry->min_tlimit.tv_usec;
	n = ap_select(FD_SETSIZE, &sel_read, &sel_write, &sel_except, &timeout);
#ifdef AB_DEBUG
	printf("AB_DEBUG: test() - stage 5.3, registry->done = %d\n", registry->done);
#endif
	if (!n)
	    myerr(registry->warn_and_error, "Server timed out");
	if (n < 1)
	    myerr(registry->warn_and_error, "Select error.");
#ifdef AB_DEBUG
	printf("AB_DEBUG: test() - stage 5.4, registry->done = %d\n", registry->done);
#endif
	/* check for time limit expiry */
	gettimeofday(&now, 0);
	if (registry->tlimit &&
	    timedif(now, registry->starttime) > (registry->tlimit * 1000)) {
	    char *warn = malloc(256 * sizeof(char));
	    sprintf(warn, "Global time limit reached (%.2f sec), premature exit", registry->tlimit);
	    myerr(registry->warn_and_error, warn);
	    free(warn);
	    registry->need_to_be_done = registry->done;	/* break out of loop */
	}

	for (i = 0; i < registry->concurrency; i++) {
	    int s = registry->con[i].fd;
#ifdef AB_DEBUG
	    printf("AB_DEBUG: test() - stage 5.5, registry->done = %d, i = %d\n", registry->done, i);
#endif
	    if (registry->started[registry->con[i].url]
		> registry->finished[registry->con[i].url]) {
		struct connection * c = &registry->con[i];
		struct timeval url_now;

		/* check for per-url time limit expiry */
		gettimeofday(&url_now, 0);

#ifdef AB_DEBUG
		printf("AB_DEBUG: test() - stage 5.5.4, Time taken for current request = %d ms; Per-url time limit = %.4f sec; for run %d, url %d\n", timedif(url_now, c->start_time), registry->url_tlimit[c->url], c->run, c->url - registry->position[c->run]);
		printf("AB_DEBUG: test() - stage 5.5.5, registry->done = %d, i = %d\n", registry->done, i);
#endif
		if (registry->url_tlimit[c->url] &&
		    timedif(url_now, c->start_time) > (registry->url_tlimit[c->url] * 1000)) {
		    char *warn = malloc(256 * sizeof(char));
#ifdef AB_DEBUG
		    printf("AB_DEBUG: test() - stage 5.5.5.3, registry->done = %d, i = %d\n", registry->done, i);
#endif
		    sprintf(warn, "Per-url time limit reached (%.3f sec) for run %d, url %d, iteration %d; connection closed prematurely", registry->url_tlimit[c->url], c->run, c->url - registry->position[c->run], c->thread);
		    myerr(registry->warn_and_error, warn);
		    free(warn);

		    registry->failed[c->url]++;
		    close_connection(registry, c);
		    continue;
		}
	    }

	    if (registry->con[i].state == STATE_DONE)
		continue;
#ifdef AB_DEBUG
	    printf("AB_DEBUG: test() - stage 5.6, registry->done = %d, i = %d\n", registry->done, i);
#endif
	    if (FD_ISSET(s, &sel_except)) {
		registry->failed[registry->con[i].url]++;
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
    if (strlen(registry->warn_and_error) == 28)
	myerr(registry->warn_and_error, "None.\n");
    else myerr(registry->warn_and_error, "Done.\n");
}



/* ------------------------------------------------------- */

/* split URL into parts */

static int
parse_url(struct global * registry, char *p, int i) {
    char *url = malloc((strlen(p)+1) * sizeof(char));
    char *port, *tok, *tok2;

    /* first, get a copy of url */
    strcpy(url, p);

    /* remove http:// prefix if it exists */
    if (strlen(url) > 7 && strncmp(url, "http://", 7) == 0)
	url += 7;

#ifdef AB_DEBUG
    printf("AB_DEBUG: parse_url() - stage 1\n");
#endif

    /* first, extract the hostname and port */
    tok = strtok(url, "/");

#ifdef AB_DEBUG
    printf("AB_DEBUG: parse_url() - stage 2\n");
#endif

    /* the remaining part of url is just the uri */
    tok2 = strtok(NULL, "");

#ifdef AB_DEBUG
    printf("AB_DEBUG: parse_url() - stage 3\n");
#endif

    registry->hostname[i] = malloc((strlen(tok)+1) * sizeof(char));
    strcpy(registry->hostname[i], strtok(tok, ":"));
    if ((port = strtok(NULL, "")) != NULL)
	registry->port[i] = atoi(port);

#ifdef AB_DEBUG
    printf("AB_DEBUG: parse_url() - stage 4\n");
#endif

    /* if there is no uri, url was of the form http://host.name - assume / */
    if (tok2 == NULL) {
	registry->path[i] = "/";
	return 0;
    }

#ifdef AB_DEBUG
    printf("AB_DEBUG: parse_url() - stage 5\n");
#endif

    /* need to allocate memory for uri */
    registry->path[i] = malloc((strlen(tok2)+2) * sizeof(char));

    /* only add leading / if not proxy request */
    if (strncmp(tok2, "http://", 7) != 0) {
	strcpy(registry->path[i], "/");
	strcat(registry->path[i], tok2);
    } else
	strcpy(registry->path[i], tok2);

    return 0;
}


/* ------------------------------------------------------- */

void
initialize(struct global * registry) {
    int i,j;

    registry->cookie = malloc(registry->number_of_runs * sizeof(char *));
    registry->buffersize = malloc(registry->number_of_runs * sizeof(int));
    registry->which_thread = malloc(registry->number_of_urls * sizeof(int *));
    registry->arranged = malloc(registry->number_of_urls * sizeof(int));

    registry->auto_cookies = calloc(registry->number_of_runs, sizeof(char *));

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
		registry->ready_to_run_queue[registry->tail].url = registry->position[i];
		registry->ready_to_run_queue[registry->tail++].thread = 0;
	    }
	} else for (j = 0; j < registry->repeats[i]; j++)
	    if ((registry->priority == EQUAL_OPPORTUNITY) || (registry->tail < registry->concurrency)) {
		registry->arranged[registry->position[i]] += 1;
		registry->ready_to_run_queue[registry->tail].run = i;
		registry->ready_to_run_queue[registry->tail].thread = j;
		registry->ready_to_run_queue[registry->tail++].url = registry->position[i];
	    }
    }
    registry->hostname = malloc(registry->number_of_urls * sizeof(char *));
    registry->path = malloc(registry->number_of_urls * sizeof(char *));
    registry->port = malloc(registry->number_of_urls * sizeof(int));
    registry->ctypes = malloc(registry->number_of_urls * sizeof(char *));
    registry->req_headers = malloc(registry->number_of_urls * sizeof(char *));
    registry->keepalive = malloc(registry->number_of_urls * sizeof(bool));
    registry->url_tlimit = malloc(registry->number_of_urls * sizeof(double));
    registry->started = malloc(registry->number_of_urls * sizeof(int));
    registry->finished = malloc(registry->number_of_urls * sizeof(int));
    registry->failed = malloc(registry->number_of_urls * sizeof(int));
    registry->good = malloc(registry->number_of_urls * sizeof(int));
    registry->postdata = malloc(registry->number_of_urls * sizeof(char *));
    registry->postlen = malloc(registry->number_of_urls * sizeof(int));
    registry->posting = malloc(registry->number_of_urls * sizeof(int));
    registry->totalposted = malloc(registry->number_of_urls * sizeof(int));
    for (i = 0; i < registry->number_of_urls; i++) {
	registry->totalposted[i] = 0;
	registry->port[i] = 80;	/* default port number */
	registry->started[i] = 0;
	registry->finished[i] = 0;
	registry->failed[i] = 0;
	registry->good[i] = 0;
    }
}


MODULE = HTTPD::Bench::ApacheBench	PACKAGE = HTTPD::Bench::ApacheBench
PROTOTYPES: ENABLE


HV *
ab(input_hash)
    SV * input_hash;

    PREINIT:
    char *pt,**url_keys;
    int i,j,k,arrlen,arrlen2;
    int def_buffersize; /* default buffersize for all runs */
    int def_repeat; /* default number of repeats if unspecified in runs */
    int def_memory; /* default memory setting if unspecified in runs */
    bool def_keepalive = 0; /* default keepalive setting */
    struct global *registry = calloc(1, sizeof(struct global));
    int total_started = 0, total_good = 0, total_failed = 0;

    CODE:
    SV * runs;
    SV * urls;
    SV * post_data;
    SV * head_requests;
    SV * cookies;
    SV * ctypes;
    SV * req_headers;
    SV * keepalive;
    SV * url_tlimits;
    AV * run_group, *tmpav, *tmpav2;
    SV * tmpsv, *tmpsv2;
    HV * tmphv;
    STRLEN len;

    if (AB_DEBUG_XS) printf("AB_DEBUG: start of ab()\n");

    registry->concurrency = 1;
    registry->requests = 0;
    registry->tlimit = 0;
    registry->min_tlimit.tv_sec = 30;
    registry->min_tlimit.tv_usec = 0;
    registry->tail = 0;
    registry->done = 0;
    registry->need_to_be_done = 0;
    strcpy(registry->warn_and_error, "\nWarning messages from ab():");
    registry->total_bytes_received = 0;
    registry->number_of_urls = 0;

    /*Get necessary initial information and initialize*/
    tmphv = (HV *)SvRV(input_hash);

    tmpsv = *(hv_fetch(tmphv, "concurrency", 11, 0));
    registry->concurrency = SvIV(tmpsv);

    tmpsv = *(hv_fetch(tmphv, "timelimit", 9, 0));
    if (SvOK(tmpsv)) {
	registry->tlimit = SvNV(tmpsv);
	registry->min_tlimit =
	    double2timeval(ap_min(timeval2double(registry->min_tlimit),
				  registry->tlimit));
    }

    tmpsv = *(hv_fetch(tmphv, "buffersize", 10, 0));
    def_buffersize = SvIV(tmpsv);

    tmpsv = *(hv_fetch(tmphv, "repeat", 6, 0));
    def_repeat = SvIV(tmpsv);

    tmpsv = *(hv_fetch(tmphv, "memory", 6, 0));
    def_memory = SvIV(tmpsv);

    if (AB_DEBUG_XS) printf("AB_DEBUG: ab() init - stage 1\n");

    tmpsv = *(hv_fetch(tmphv, "keepalive", 9, 0));
    if (SvTRUE(tmpsv))
        def_keepalive = 1;

    if (AB_DEBUG_XS) printf("AB_DEBUG: ab() init - stage 2\n");

    tmpsv = *(hv_fetch(tmphv, "priority", 8, 0));
    pt = SvPV(tmpsv, len);
    if (strcmp(pt, "run_priority") == 0)
        registry->priority = RUN_PRIORITY;
    else {
	registry->priority = EQUAL_OPPORTUNITY;
	if (strcmp(pt, "equal_opportunity") != 0)
	    myerr(registry->warn_and_error, "Unknown priority value (the only possible priorities are run_priority and equal_opportunity), using default: equal_opportunity");
    }

    if (AB_DEBUG_XS) printf("AB_DEBUG: ab() init - stage 3\n");

    runs = *(hv_fetch(tmphv, "runs", 4, 0));
    run_group = (AV *)SvRV(runs);
    registry->number_of_runs = av_len(run_group) + 1;

    registry->order = malloc(registry->number_of_runs * sizeof(int));
    registry->repeats = malloc(registry->number_of_runs * sizeof(int));
    registry->position = malloc((registry->number_of_runs+1) * sizeof(int));
    registry->memory = malloc(registry->number_of_runs * sizeof(int));
    registry->use_auto_cookies = malloc(registry->number_of_runs * sizeof(bool));

    if (AB_DEBUG_XS) printf("AB_DEBUG: done with ab() initialization\n");

    for (i = 0,j = 0; i < registry->number_of_runs; i++) {
	if (AB_DEBUG_XS) printf("AB_DEBUG: starting run %d setup\n", i);

	tmpsv = *(av_fetch(run_group, i, 0));

	if (SvROK(tmpsv))
	    tmphv = (HV *)SvRV(tmpsv);

	registry->memory[i] = def_memory;
	if (hv_exists(tmphv, "memory", 6)) {
	    tmpsv = *(hv_fetch(tmphv, "memory", 6, 0));
	    registry->memory[i] = SvIV(tmpsv);
	}

	registry->repeats[i] = def_repeat;
	if (hv_exists(tmphv, "repeat", 6)) {
	    /* Number of requests to make */
	    tmpsv = *(hv_fetch(tmphv, "repeat", 6, 0));
	    registry->repeats[i] = SvIV(tmpsv);
	}

	registry->use_auto_cookies[i] = 1;
	if (hv_exists(tmphv, "use_auto_cookies", 16)) {
	    tmpsv = *(hv_fetch(tmphv, "use_auto_cookies", 16, 0));
	    registry->use_auto_cookies[i] = SvTRUE(tmpsv) ? 1 : 0;
	}

	registry->requests = ap_max(registry->requests, registry->repeats[i]);

	urls = *(hv_fetch(tmphv, "urls", 4, 0));
	tmpav = (AV *) SvRV(urls);
	registry->position[i] = registry->number_of_urls;
	registry->number_of_urls += av_len(tmpav) + 1;

	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d: position[%d] == %d\n", i, i, registry->position[i]);

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
	myerr(registry->warn_and_error, "No urls.");
	return;
    }
    registry->position[registry->number_of_runs] = registry->number_of_urls;
    registry->concurrency = ap_min(registry->concurrency, j);

    if (AB_DEBUG_XS) printf("AB_DEBUG: set all run info, ready to call initialize()\n");

    initialize(registry);

    url_keys = malloc(registry->number_of_urls * sizeof(char *));

    for (k = 0; k < registry->number_of_runs; k++) {
	if (AB_DEBUG_XS) printf("AB_DEBUG: starting run %d setup2 - postdata + cookie\n", k);

	registry->buffersize[k] = def_buffersize;
	tmpsv = *(av_fetch(run_group, k, 0));
	if (SvROK(tmpsv)) {
	    tmphv = (HV *)SvRV(tmpsv);
	    if (hv_exists(tmphv, "buffersize", 10)) {
		tmpsv = *(hv_fetch(tmphv, "buffersize", 10, 0));
		registry->buffersize[k] = SvIV(tmpsv);
	    }
	}

	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 1\n", k);

	/* error checking: make sure all of the run specific hashkeys exist */
	if (hv_exists(tmphv, "urls", 4))
	    urls = *(hv_fetch(tmphv, "urls", 4, 0));
	if (hv_exists(tmphv, "postdata", 8))
	    post_data = *(hv_fetch(tmphv, "postdata", 8, 0));
	if (hv_exists(tmphv, "head_requests", 13))
	    head_requests = *(hv_fetch(tmphv, "head_requests", 13, 0));
	if (hv_exists(tmphv, "cookies", 7))
	    cookies = *(hv_fetch(tmphv, "cookies", 7, 0));
	if (hv_exists(tmphv, "content_types", 13))
	    ctypes = *(hv_fetch(tmphv, "content_types", 13, 0));
	if (hv_exists(tmphv, "request_headers", 15))
	    req_headers = *(hv_fetch(tmphv, "request_headers", 15, 0));
	if (hv_exists(tmphv, "keepalive", 9))
	    keepalive = *(hv_fetch(tmphv, "keepalive", 9, 0));
	if (hv_exists(tmphv, "timelimits", 10))
	    url_tlimits = *(hv_fetch(tmphv, "timelimits", 10, 0));

	/* configure urls */
	for (i = registry->position[k]; i < registry->position[k+1]; i++) {
	    tmpav =(AV *) SvRV(urls);
	    tmpsv = *(av_fetch(tmpav, i - registry->position[k], 0));
	    if (SvPOK(tmpsv)) {
		pt = SvPV(tmpsv, len);
		url_keys[i] = pt;

		if (parse_url(registry, pt, i)) {
		    char *warn = malloc(256 * sizeof(char));
		    sprintf(warn, "Invalid url: %s, the information for this url may be wrong", pt);
		    myerr(registry->warn_and_error, warn);
		    free(warn);
		}
	    } else {
		myerr(registry->warn_and_error, "Undefined url in urls list");
	    }
	}


	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 2\n", k);

	/* find smaller of post_data array length and urls array length */
	tmpav = (AV *) SvRV(post_data);
	arrlen = av_len(tmpav);
	i = ap_min(registry->position[k+1],
		   registry->position[k] + arrlen + 1);

	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 2.1\n", k);

	/* find larger of head_requests and post_data (to get the most data) */
	tmpav2 = (AV *) SvRV(head_requests);
	arrlen2 = av_len(tmpav2);
	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 2.1.1\n", k);
	i = ap_max(i, registry->position[k] + arrlen2 + 1);

	/* configure post_data or head_requests */
	for (j = registry->position[k]; j < i; j++) {
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 2.2, j=%d\n", k, j);
	    if (j - registry->position[k] <= arrlen)
		tmpsv = *(av_fetch(tmpav, j - registry->position[k], 0));
	    if (j - registry->position[k] <= arrlen2)
		tmpsv2 = *(av_fetch(tmpav2, j - registry->position[k], 0));
	    if (j - registry->position[k] <= arrlen && SvPOK(tmpsv)) {
		if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 2.3, j=%d\n", k, j);
		pt = SvPV(tmpsv, len);
		if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 2.4, j=%d\n", k, j);
		registry->postdata[j] = pt;
		registry->postlen[j] = len;
		/* this url is a POST request */
		registry->posting[j] = 1;
	    } else if (j - registry->position[k] <= arrlen2 && SvTRUE(tmpsv2))
		/* this url is a HEAD request */
		registry->posting[j] = -1;
	    else
		registry->posting[j] = 0;
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 2.5, j=%d\n", k, j);
	}

	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 2.6\n", k);

	/*If the number of postdata strings is less than
	  that of urls, then assign empty strings to force GET requests*/
	for (j = i; j < registry->position[k+1]; j++)
	    registry->posting[j] = 0;


	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 3\n", k);

	/* configure cookies */
	registry->cookie[k] = NULL;
	tmpav = (AV *) SvRV(cookies);
	if (av_len(tmpav) >= 0) {
	    tmpsv = *(av_fetch(tmpav, 0, 0));
	    if (SvPOK(tmpsv)) {
		pt = SvPV(tmpsv, len);
		if (len != 0) {
		    registry->cookie[k] = malloc((len+1) * sizeof(char));
		    strcpy(registry->cookie[k], pt);
		    if (AB_DEBUG_XS) printf("AB_DEBUG: cookie[%d] == '%s'\n", k, registry->cookie[k]);
		}
	    }
	}


	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 4\n", k);

	/* find smaller of req_headers array length and urls array length */
	tmpav = (AV *) SvRV(req_headers);
	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 4.1\n", k);
	i = ap_min(registry->position[k+1],
		   registry->position[k] + av_len(tmpav) + 1);

	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 4.2\n", k);
	/* configure arbitrary request headers */
	for (j = registry->position[k]; j < i; j++) {
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 4.3, j=%d\n", k, j);
	    tmpsv = *(av_fetch(tmpav, j - registry->position[k], 0));
	    if (SvPOK(tmpsv)) {
		if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 4.4, j=%d\n", k, j);
		pt = SvPV(tmpsv, len);
		if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 4.5, j=%d\n", k, j);
		registry->req_headers[j] = pt;
	    } else
		registry->req_headers[j] = 0;
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 4.6, j=%d\n", k, j);
	}

	/*If the number of req_headers strings is less than
	  that of urls, then assign NULL (undef) */
	for (j = i; j < registry->position[k+1]; j++)
	    registry->req_headers[j] = 0;


	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 5\n", k);

	/* find smaller of ctypes array length and urls array length */
	tmpav = (AV *) SvRV(ctypes);
	i = ap_min(registry->position[k+1],
		   registry->position[k] + av_len(tmpav) + 1);

	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 5.1\n", k);

	/* configure ctypes */
	for (j = registry->position[k]; j < i; j++) {
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 5.2, j=%d\n", k, j);
	    tmpsv = *(av_fetch(tmpav, j - registry->position[k], 0));
	    if (SvPOK(tmpsv)) {
		if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 5.3, j=%d\n", k, j);
		pt = SvPV(tmpsv, len);
		if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 5.4, j=%d\n", k, j);
		registry->ctypes[j] = pt;
	    } else
		registry->ctypes[j] = 0;
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 5.5, j=%d\n", k, j);
	}

	/*If the number of ctypes strings is less than
	  that of urls, then assign NULL (undef) */
	for (j = i; j < registry->position[k+1]; j++)
	    registry->ctypes[j] = 0;


	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 6\n", k);

	/* find smaller of keepalive array length and urls array length */
	tmpav = (AV *) SvRV(keepalive);
	i = ap_min(registry->position[k+1],
		   registry->position[k] + av_len(tmpav) + 1);

	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 6.1\n", k);

	/* configure keepalive */
	for (j = registry->position[k]; j < i; j++) {
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 6.2, j=%d\n", k, j);
	    tmpsv = *(av_fetch(tmpav, j - registry->position[k], 0));
	    if (SvOK(tmpsv))
		if (SvTRUE(tmpsv))
		    registry->keepalive[j] = 1;
		else
		    registry->keepalive[j] = 0;
	    else
		registry->keepalive[j] = def_keepalive;
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 6.3, j=%d\n", k, j);
	}

	/*If the number of keepalive strings is less than
	  that of urls, then assign object's default keepalive value */
	for (j = i; j < registry->position[k+1]; j++)
	    registry->keepalive[j] = def_keepalive;


	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 7\n", k);

	/* find smaller of url_tlimits array length and urls array length */
	tmpav = (AV *) SvRV(url_tlimits);
	i = ap_min(registry->position[k+1],
		   registry->position[k] + av_len(tmpav) + 1);

	if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 7.1\n", k);

	/* configure url_tlimits */
	for (j = registry->position[k]; j < i; j++) {
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 7.2, j=%d\n", k, j);
	    tmpsv = *(av_fetch(tmpav, j - registry->position[k], 0));
	    if (SvOK(tmpsv)) {
		if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 7.3, j=%d\n", k, j);
		registry->url_tlimit[j] = SvNV(tmpsv);
		registry->min_tlimit =
		    double2timeval(ap_min(timeval2double(registry->min_tlimit),
					  registry->url_tlimit[j]));
		if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 7.3.1, j=%d, url_tlimit=%.3f sec, min tlimit so far = %.3f sec\n", k, j, registry->url_tlimit[j], timeval2double(registry->min_tlimit));
	    } else
		registry->url_tlimit[j] = 0;
	    if (AB_DEBUG_XS) printf("AB_DEBUG: run %d setup2 - stage 7.4, j=%d\n", k, j);
	}

	/*If the number of url_tlimits is less than
	  that of urls, then assign 0 for no time limit */
	for (j = i; j < registry->position[k+1]; j++)
	    registry->url_tlimit[j] = 0;

    }

    if (AB_DEBUG_XS) printf("AB_DEBUG: ready to test()\n");

    test(registry);

    if (AB_DEBUG_XS) printf("AB_DEBUG: done with test()\n");

    RETVAL = newHV();/* ready to get information stored in global variables */

    for (k = 0; k < registry->number_of_runs; k++) {
	if (registry->memory[k] >= 1) {
	    HV * run_hash = newHV();
	    AV *started = newAV();/* number of started requests for each url */
	    AV *good = newAV();   /* number of good responses for each url */
	    AV *failed = newAV(); /* number of bad responses for each url */
	    tmpav = newAV();	     /* array to keep the thread information */

	    if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression info for run %d\n", k);

	    for (i = 0; i < registry->repeats[k]; i++) {
		AV *th_t = newAV();  /* times for processing and connecting */
		AV *th_r = newAV();  /* times for http request */
		AV *th_c = newAV();  /* connecting times */
		AV *page_contents = newAV(); /* pages read from servers */
		AV *request_headers = newAV(); /* HTTP requests sent to servers */
		AV *request_body = newAV(); /* HTTP requests sent to servers */
		AV *headers = newAV();
		AV *bytes_posted = newAV();
		AV *doc_length = newAV();
		AV *bytes_read = newAV();

		/* variables for calculating min/max/avg times*/
		int totalcon = 0, totalreq = 0, total = 0;
		int mincon = 999999, minreq = 999999, mintot = 999999;
		int maxcon = 0, maxreq = 0, maxtot = 0;

		/* byte counters */
		int total_bytes_posted = 0, total_bytes_read = 0;

		for (j = registry->position[k]; j < registry->position[k+1]; j++) {
		    struct data s = registry->stats[j][i];
		    mincon = ap_min(mincon, s.ctime);
		    minreq = ap_min(minreq, s.rtime);
		    mintot = ap_min(mintot, s.time);
		    maxcon = ap_max(maxcon, s.ctime);
		    maxreq = ap_max(maxreq, s.rtime);
		    maxtot = ap_max(maxtot, s.time);
		    totalcon += s.ctime;
		    totalreq += s.rtime;
		    total += s.time;
		    if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1 - i,j=%d,%d: mintot=%d maxtot=%d total=%d\n", i, j, mintot, maxtot, total);

		    total_bytes_posted += registry->totalposted[j];
		    total_bytes_read += registry->stats[j][i].read;

		    av_push(th_c, newSVnv(registry->stats[j][i].ctime));
		    av_push(th_r, newSVnv(registry->stats[j][i].rtime));
		    av_push(th_t, newSVnv(registry->stats[j][i].time));
		    if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1.1 - i,j=%d,%d\n", i, j);
		    if (i == 0) {
			av_push(started, newSViv(registry->started[j]));
			av_push(good, newSViv(registry->good[j]));
			av_push(failed, newSViv(registry->failed[j]));
			total_started += registry->started[j];
			total_good += registry->good[j];
			total_failed += registry->failed[j];
		    }

		    if (registry->memory[k] >= 2) {
			if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1.1.0 - i,j=%d,%d  header='%s'\n", i, j, registry->stats[j][i].response_headers);
			if (registry->stats[j][i].response_headers &&
			    strlen(registry->stats[j][i].response_headers) > 0)
			    av_push(headers, newSVpv(registry->stats[j][i].response_headers, 0));
			else
			    av_push(headers, &PL_sv_undef);
			if (registry->stats[j][i].request_headers &&
			    strlen(registry->stats[j][i].request_headers) > 0)
			    av_push(request_headers, newSVpv(registry->stats[j][i].request_headers, 0));
			else
			    av_push(request_headers, &PL_sv_undef);
			if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1.1.1 - i,j=%d,%d\n", i, j);
			av_push(doc_length, newSVnv(registry->stats[j][i].bread));
			if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1.1.2 - i,j=%d,%d\n", i, j);
			av_push(bytes_read, newSVnv(registry->stats[j][i].read));
			if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1.1.3 - i,j=%d,%d\n", i, j);
			/*if (registry->posting[j] > 0)*/
			av_push(bytes_posted, newSVnv(registry->totalposted[j]));
		    }
		    if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1.2 - i,j=%d,%d\n", i, j);
		    if (registry->memory[k] >= 3) {
			if (registry->stats[j][i].response &&
			    strlen(registry->stats[j][i].response) > 0)
			    av_push(page_contents, newSVpv(registry->stats[j][i].response, 0));
			else
			    av_push(page_contents, &PL_sv_undef);
			if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1.2.5 - i,j=%d,%d\n", i, j);
			if (registry->stats[j][i].request &&
			    strlen(registry->stats[j][i].request) > 0)
			    av_push(request_body, newSVpv(registry->stats[j][i].request, 0));
			else
			    av_push(request_body, &PL_sv_undef);
		    }
		    if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 1.3 - i,j=%d,%d\n", i, j);
		}

		if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 3 - i=%d\n", i);
		tmphv = newHV();
		hv_store(tmphv, "max_connect_time", 16, newSVnv(maxcon), 0);
		hv_store(tmphv, "max_request_time", 16, newSVnv(maxreq), 0);
		hv_store(tmphv, "max_response_time", 17, newSVnv(maxtot), 0);
		hv_store(tmphv, "min_connect_time", 16, newSVnv(mincon), 0);
		hv_store(tmphv, "min_request_time", 16, newSVnv(minreq), 0);
		hv_store(tmphv, "min_response_time", 17, newSVnv(mintot), 0);
		if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 4 - i=%d\n", i);
		hv_store(tmphv, "total_connect_time", 18, newSVnv(totalcon), 0);
		hv_store(tmphv, "total_request_time", 18, newSVnv(totalreq), 0);
		hv_store(tmphv, "total_response_time", 19, newSVnv(total), 0);
		hv_store(tmphv, "average_connect_time", 20, newSVnv((float)totalcon/(j-registry->position[k])), 0);
		hv_store(tmphv, "average_request_time", 20, newSVnv((float)totalreq/(j-registry->position[k])), 0);
		hv_store(tmphv, "average_response_time", 21, newSVnv((double)total/(j-registry->position[k])), 0);
		hv_store(tmphv, "total_bytes_read", 16, newSVnv(total_bytes_read), 0);
		hv_store(tmphv, "total_bytes_posted", 18, newSVnv(total_bytes_posted), 0);
		if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 5 - i=%d\n", i);

		/* started/good/failed are url-specific, store only 1st time */
		if (i == 0) {
		    hv_store(tmphv, "started", 7, newRV_inc((SV *)started), 0);
		    hv_store(tmphv, "good", 4, newRV_inc((SV *)good), 0);
		    hv_store(tmphv, "failed", 6, newRV_inc((SV *)failed), 0);
		}
		hv_store(tmphv, "connect_time", 12, newRV_inc((SV *)th_c), 0);
		hv_store(tmphv, "request_time", 12, newRV_inc((SV *)th_r), 0);
		hv_store(tmphv, "response_time", 13, newRV_inc((SV *)th_t), 0);
		if (registry->memory[k] >= 2) {
		    hv_store(tmphv, "headers", 7, newRV_inc((SV *)headers), 0);
		    hv_store(tmphv, "doc_length", 10, newRV_inc((SV *)doc_length), 0);
		    hv_store(tmphv, "bytes_read", 10, newRV_inc((SV *)bytes_read), 0);
		    hv_store(tmphv, "bytes_posted", 12, newRV_inc((SV *)bytes_posted), 0);
		    hv_store(tmphv, "request_headers", 15, newRV_inc((SV *)request_headers), 0);
		}
		if (registry->memory[k] >= 3) {
		    hv_store(tmphv, "page_content", 12, newRV_inc((SV *)page_contents), 0);
		    hv_store(tmphv, "request_body", 12, newRV_inc((SV *)request_body), 0);
		}
		if (AB_DEBUG_XS) printf("AB_DEBUG: getting regression - stage 6 - i=%d\n", i);
		av_push(tmpav, newRV_inc((SV*)tmphv));
	    }
	    {
		char key[10];
		sprintf(key, "run%d", k);
		hv_store(RETVAL, key, strlen(key), newRV_inc((SV *)tmpav), 0);
	    }
	}
    }

    hv_store(RETVAL, "warnings", 8, newSVpv(registry->warn_and_error, 0), 0);
    hv_store(RETVAL, "total_time", 10,
	     newSVnv(timedif(registry->endtime, registry->starttime)), 0);
    hv_store(RETVAL, "bytes_received", 14,
	     newSVnv(registry->total_bytes_received), 0);
    hv_store(RETVAL, "started", 7, newSViv(total_started), 0);
    hv_store(RETVAL, "good", 4, newSViv(total_good), 0);
    hv_store(RETVAL, "failed", 6, newSViv(total_failed), 0);

    OUTPUT:
    RETVAL
