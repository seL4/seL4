/*
 * Copyright 2016, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SYSCALLS_H
#define __LIBSEL4_SYSCALLS_H
#include <autoconf.h>

/**
 * @defgroup SystemCalls System Calls
 * @{
 *
 * @defgroup GeneralSystemCalls General System Calls
 * @{
 */

/**
 * @xmlonly <manual name="Send" label="sel4_send"/> @endxmlonly
 * @brief Send to a capability
 *
 * @xmlonly
 * See <autoref sec="sys_send"/>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 */
LIBSEL4_INLINE_FUNC void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Recv" label="sel4_recv"/> @endxmlonly
 * @brief Block until a message is received on an endpoint
 *
 * @xmlonly
 * See <autoref sec="sys_recv"/>
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word* sender);

/**
 * @xmlonly <manual name="Call" label="sel4_call"/> @endxmlonly
 * @brief  Call a capability
 *
 * @xmlonly
 * See <autoref sec="sys_call"/>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Call(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Reply" label="sel4_reply"/> @endxmlonly
 * @brief Perform a send to a one-off reply capability stored when
 *        the thread was last called
 *
 * @xmlonly
 * See <autoref sec="sys_reply"/>
 * @endxmlonly
 *
 * @param[in] msgInfo The messageinfo structure for the IPC.
 */
LIBSEL4_INLINE_FUNC void
seL4_Reply(seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Polling Send" label="sel4_nbsend"/> @endxmlonly
 * @brief Perform a polling send to a capability
 *
 * @xmlonly
 * See <autoref sec="sys_nbsend"/>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 */
LIBSEL4_INLINE_FUNC void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Reply Recv" label="sel4_replyrecv"/> @endxmlonly
 * @brief Perform a reply followed by a receive in one system call
 *
 * @xmlonly
 * See <autoref sec="sys_replyrecv"/>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_ReplyRecv(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender);

/**
 * @xmlonly <manual name="NBRecv" label="sel4_nbrecv"/> @endxmlonly
 * @brief Receive a message from an endpoint but do not block
 *        in the case that no messages are pending
 *
 * @xmlonly
 * See <autoref sec="sys_nbrecv"/>
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *                    The sender information is the badge of the
 *                    endpoint capability that was invoked by the
 *                    sender, or the notification word of the
 *                    notification object that was signalled.
 *                    This parameter is ignored if `NULL`.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref sec="messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_NBRecv(seL4_CPtr src, seL4_Word* sender);

/**
 * @xmlonly <manual name="Yield" label="sel4_yield"/> @endxmlonly
 * @brief Donate the remaining timeslice to a thread of the same priority
 *
 * @xmlonly
 * See <autoref sec="sys_yield"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_Yield(void);

/**
 * @xmlonly <manual name="Signal" label="sel4_signal"/> @endxmlonly
 * @brief Signal a notification
 *
 * This is not a proper system call known by the kernel. Rather, it is a
 * convenience wrapper which calls seL4_Send().
 * It is useful for signalling a notification.
 *
 * @xmlonly
 * See the description of <nameref name="seL4_Send"/> in <autoref sec="sys_send"/>.
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 */
LIBSEL4_INLINE_FUNC void
seL4_Signal(seL4_CPtr dest);

/**
 * @xmlonly <manual name="Wait" label="sel4_wait"/> @endxmlonly
 * @brief Perform a receive on a notification object
 *
 * This is not a proper system call known by the kernel. Rather, it is a
 * convenience wrapper which calls seL4_Recv().
 *
 * @xmlonly
 * See the description of <nameref name="seL4_Recv"/> in <autoref sec="sys_recv"/>.
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 */
LIBSEL4_INLINE_FUNC void
seL4_Wait(seL4_CPtr src, seL4_Word *sender);

/**
 * @xmlonly <manual name="Poll" label="sel4_poll"/> @endxmlonly
 * @brief Perform a non-blocking recv on a notification object
 *
 * This is not a proper system call known by the kernel. Rather, it is a
 * convenience wrapper which calls seL4_NBRecv().
 * It is useful for doing a non-blocking wait on a notification.
 *
 * @xmlonly
 * See the description of <nameref name="seL4_NBRecv"/> in <autoref sec="sys_nbrecv"/>.
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Poll(seL4_CPtr src, seL4_Word *sender);

/** @} */

/** @} */

#endif /* __LIBSEL4_SYSCALLS_H */
