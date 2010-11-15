#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "regression_data.h"


static void
initialize_regression_data(struct data * s) {
    s->run = 0;
    s->thread = 0;
    s->read = 0;
    s->bread = 0;
    s->ctime = 0;
    s->rtime = 0;
    s->time = 0;

    s->request = 0;
    s->request_headers = 0;

    s->response_headers = 0;
    s->response = 0;
}
/* --------------------------------------------------------- */

/* save regression data and benchmark timings */

static void
store_regression_data(struct global * registry, struct connection * c) {
    struct data s;
    initialize_regression_data(&s);

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
