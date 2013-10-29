
#include "nanomsg/nn.h"


struct nn_cmsghdr *wfirsthdr(struct nn_msghdr *hdr);
struct nn_cmsghdr *wnxthdr(struct nn_msghdr *hdr, struct nn_cmsghdr *cmsg);
unsigned char *wdata(struct nn_cmsghdr *cmsg);
size_t wspace(size_t len);
size_t wlen(size_t len);
typedef struct nn_iovec nn_iovec;
typedef struct nn_msghdr nn_msghdr;
typedef struct nn_cmsghdr nn_cmsghdr;
