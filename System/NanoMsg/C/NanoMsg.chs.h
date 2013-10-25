#include "nanomsg/nn.h"
#include "nanomsg/pair.h"
#include "nanomsg/reqrep.h"
#include "nanomsg/pubsub.h"
#include "nanomsg/survey.h"
#include "nanomsg/pipeline.h"
#include "nanomsg/bus.h"
#include "nanomsg/tcp.h"
#include "nanomsg/inproc.h"
#include "nanomsg/ipc.h"
#include "inlinemacro.h"
enum __c2hs_enum__0 {
    __c2hs_enr__1 = ENOTSUP,
    __c2hs_enr__2 = ENOMEM,
    __c2hs_enr__3 = EPROTONOSUPPORT,
    __c2hs_enr__4 = ENAMETOOLONG,
    __c2hs_enr__5 = ENODEV,
    __c2hs_enr__6 = ENOBUFS,
    __c2hs_enr__7 = ENETDOWN,
    __c2hs_enr__8 = EADDRINUSE,
    __c2hs_enr__9 = EADDRNOTAVAIL,
    __c2hs_enr__10 = ECONNREFUSED,
    __c2hs_enr__11 = EINPROGRESS,
    __c2hs_enr__12 = ENOTSOCK,
    __c2hs_enr__13 = EAFNOSUPPORT,
    __c2hs_enr__14 = EPROTO,
    __c2hs_enr__15 = EAGAIN,
    __c2hs_enr__16 = EBADF,
    __c2hs_enr__17 = EINVAL,
    __c2hs_enr__18 = EINTR,
    __c2hs_enr__19 = EMFILE,
    __c2hs_enr__20 = EFAULT,
    __c2hs_enr__21 = EACCESS,
    __c2hs_enr__22 = ENETRESET,
    __c2hs_enr__23 = ENETUNREACH,
    __c2hs_enr__24 = EHOSTUNREACH,
    __c2hs_enr__25 = ENOTCONN,
    __c2hs_enr__26 = EMSGSIZE,
    __c2hs_enr__27 = ETIMEDOUT,
    __c2hs_enr__28 = ECONNABORTED,
    __c2hs_enr__29 = ECONNRESET,
    __c2hs_enr__30 = ENOPROTOOPT,
    __c2hs_enr__31 = EISCONN,
    __c2hs_enr__32 = ETERM,
    __c2hs_enr__33 = EFSM
};
enum __c2hs_enum__34 {
    __c2hs_enr__35 = AF_SP, __c2hs_enr__36 = AF_SP_RAW
};
enum __c2hs_enum__37 {
    __c2hs_enr__38 = NN_IPC,
    __c2hs_enr__39 = NN_INPROC,
    __c2hs_enr__40 = NN_TCP
};
enum __c2hs_enum__41 {
    __c2hs_enr__42 = NN_PROTO_PUBSUB,
    __c2hs_enr__43 = NN_PROTO_BUS,
    __c2hs_enr__44 = NN_PROTO_PAIR,
    __c2hs_enr__45 = NN_PROTO_PIPELINE,
    __c2hs_enr__46 = NN_PROTO_REQREP,
    __c2hs_enr__47 = NN_PROTO_SURVEY
};
enum __c2hs_enum__48 {
    __c2hs_enr__49 = NN_PUB,
    __c2hs_enr__50 = NN_SUB,
    __c2hs_enr__51 = NN_BUS,
    __c2hs_enr__52 = NN_PAIR,
    __c2hs_enr__53 = NN_PUSH,
    __c2hs_enr__54 = NN_PULL,
    __c2hs_enr__55 = NN_REQ,
    __c2hs_enr__56 = NN_REP,
    __c2hs_enr__57 = NN_SURVEYOR,
    __c2hs_enr__58 = NN_RESPONDENT
};
enum __c2hs_enum__59 {
    __c2hs_enr__60 = NN_SOCKADDR_MAX
};
enum __c2hs_enum__61 {
    __c2hs_enr__62 = NN_SOL_SOCKET
};
enum __c2hs_enum__63 {
    __c2hs_enr__64 = NN_LINGER,
    __c2hs_enr__65 = NN_SNDBUF,
    __c2hs_enr__66 = NN_RCVBUF,
    __c2hs_enr__67 = NN_SNDTIMEO,
    __c2hs_enr__68 = NN_RCVTIMEO,
    __c2hs_enr__69 = NN_RECONNECT_IVL,
    __c2hs_enr__70 = NN_RECONNECT_IVL_MAX,
    __c2hs_enr__71 = NN_SNDPRIO,
    __c2hs_enr__72 = NN_SNDFD,
    __c2hs_enr__73 = NN_RCVFD,
    __c2hs_enr__74 = NN_IPV4ONLY
};
enum __c2hs_enum__75 {
    __c2hs_enr__76 = NN_DOMAIN, __c2hs_enr__77 = NN_PROTOCOL
};
enum __c2hs_enum__78 {
    __c2hs_enr__79 = NN_REQ_RESEND_IVL
};
enum __c2hs_enum__80 {
    __c2hs_enr__81 = NN_SURVEYOR_DEADLINE
};
enum __c2hs_enum__82 {
    __c2hs_enr__83 = NN_SUB_SUBSCRIBE,
    __c2hs_enr__84 = NN_SUB_UNSUBSCRIBE
};
enum __c2hs_enum__85 {
    __c2hs_enr__86 = NN_TCP_NODELAY
};
enum __c2hs_enum__87 {
    __c2hs_enr__88 = NN_DONTWAIT
};
