#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "http_util.h"

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


/* --------------------------------------------------------- */

/* extract cookies from response_data (Set-Cookie: headers) and save to auto_cookies */

static void
extract_cookies_from_response(struct global * registry, struct connection * c) {
    char * set_cookie_hdr, * eoh;

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

    set_cookie_hdr = strstr(c->response_headers, "\r\nSet-Cookie: ");
    while (set_cookie_hdr) {
        remove_existing_cookie_from_auto_cookies(registry, c, set_cookie_hdr);

        eoh = strstr(set_cookie_hdr+2, "\r\n");
        if (! strnstr(set_cookie_hdr, "=; Expires=", eoh - set_cookie_hdr)) // hack: do not set expired headers
            // drop the "Set-" from beginning to just append "Cookie: ....\r\n"
            strncat(registry->auto_cookies[c->run][c->thread], set_cookie_hdr + 6, eoh - set_cookie_hdr - 4);

        set_cookie_hdr = strstr(set_cookie_hdr+1, "\r\nSet-Cookie: ");
    }
}

/* remove existing cookies from registry->auto_cookies[..][..] which will be set again by extract_cookies_from_response() */

static void
remove_existing_cookie_from_auto_cookies(struct global * registry, struct connection * c, char * set_cookie_hdr) {
    char * existing_cookie, * end_of_existing_cookie;

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

    existing_cookie = strstr(registry->auto_cookies[c->run][c->thread], cookie_name);
    if (existing_cookie) {
        char * new_auto_cookies = calloc(strlen(registry->auto_cookies[c->run][c->thread]), sizeof(char));
        strncpy(new_auto_cookies, registry->auto_cookies[c->run][c->thread], existing_cookie - registry->auto_cookies[c->run][c->thread]);
        //char * end_of_existing_cookie;
        end_of_existing_cookie = strstr(existing_cookie, "\r\n");
        strcat(new_auto_cookies, end_of_existing_cookie+2);

        // overwrite auto_cookies with new version with existing_cookie removed
        strcpy(registry->auto_cookies[c->run][c->thread], new_auto_cookies);
        free(new_auto_cookies);
    }

    free(cookie_name);
}
