#include "inlinemacro.h"
#include <stddef.h>
#include <errno.h>



struct nn_cmsghdr *wfirsthdr(struct nn_msghdr *hdr){ return NN_CMSG_FIRSTHDR(hdr);}
struct nn_cmsghdr *wnxthdr(struct nn_msghdr *hdr, struct nn_cmsghdr *cmsg){ return NN_CMSG_NXTHDR(hdr, cmsg);}
unsigned char *wdata(struct nn_cmsghdr *cmsg){return NN_CMSG_DATA(cmsg);}
size_t wlen(size_t len){return NN_CMSG_LEN(len);}
size_t wspace(size_t len){return NN_CMSG_SPACE(len);}
