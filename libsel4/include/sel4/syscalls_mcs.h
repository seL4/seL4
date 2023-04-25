/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once
#include <sel4/config.h>

/**
 * @defgroup MCSSystemCalls MCS System Calls
 * @{
 */

/**
 * @xmlonly <manual name="Send" label="sel4_mcs_send"/> @endxmlonly
 * @brief Send to a capability
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_send"/></docref>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 */
LIBSEL4_INLINE_FUNC void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);

/**
 * @xmlonly <manual name="Recv" label="sel4_mcs_recv"/> @endxmlonly
 * @brief Block until a message is received on an endpoint
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_recv"/></docref>
 * @endxmlonly
 *
 * @param[in] src The capability to be invoked.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 * @param[in] reply The capability to the reply object to use on a call (only used on MCS).
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * <docref>as described in <autoref label="sec:messageinfo"/></docref>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word *sender, seL4_CPtr reply);

/**
 * @xmlonly <manual name="Call" label="sel4_mcs_call"/> @endxmlonly
 * @brief  Call a capability
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_call"/></docref>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * <docref>as described in <autoref label="sec:messageinfo"/></docref>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Call(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);


/**
 * @xmlonly <manual name="Non-Blocking Send" label="sel4_mcs_nbsend"/> @endxmlonly
 * @brief Perform a non-blocking send to a capability
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_nbsend"/></docref>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 */
LIBSEL4_INLINE_FUNC void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo);


/**
 * @xmlonly <manual name="Reply Recv" label="sel4_mcs_replyrecv"/> @endxmlonly
 * @brief Perform a reply followed by a receive in one system call
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_replyrecv"/></docref>
 * @endxmlonly
 *
 * @param[in] src The capability to perform the receive on.
 * @param[in] msgInfo The messageinfo structure for the IPC.
 * @param[out] sender The address to write sender information to.
 *               The sender information is the badge of the
 *               endpoint capability that was invoked by the
 *               sender, or the notification word of the
 *               notification object that was signalled.
 *               This parameter is ignored if `NULL`.
 *
 * @param[in] reply The capability to the reply object, which is first invoked and then used
 *                  for the recv phase to store a new reply capability.
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * <docref>as described in <autoref label="sec:messageinfo"/></docref>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_ReplyRecv(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender, seL4_CPtr reply);

/**
 * @xmlonly <manual name="NBRecv" label="sel4_mcs_nbrecv"/> @endxmlonly
 * @brief Receive a message from an endpoint but do not block
 *        in the case that no messages are pending
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_nbrecv"/></docref>
 * @endxmlonly
 *
 * @param[in] src The capability to receive on.
 * @param[out] sender The address to write sender information to.
 *                    The sender information is the badge of the
 *                    endpoint capability that was invoked by the
 *                    sender, or the notification word of the
 *                    notification object that was signalled.
 *                    This parameter is ignored if `NULL`.
 * @param[in] reply The capability to the reply object to use on a call.
 *
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * <docref>as described in <autoref label="sec:messageinfo"/></docref>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_NBRecv(seL4_CPtr src, seL4_Word *sender, seL4_CPtr reply);

/**
 * @xmlonly <manual name="NBSend Recv" label="sel4_nbsendrecv"/> @endxmlonly
 * @brief Non-blocking send on one capability, and a blocking recieve on another in a single
 *        system call.
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_nbsendrecv"/></docref>
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
 * @param[in] src The capability to receive on.
 * @param[in] reply The capability to the reply object, which is first invoked and then used
 *                  for the recv phase to store a new reply capability.
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref label="sec:messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_NBSendRecv(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_CPtr src, seL4_Word *sender, seL4_CPtr reply);

/**
 * @xmlonly <manual name="NBSend Wait" label="sel4_nbsendwait"/> @endxmlonly
 * @brief Non-blocking invoke of a capability and wait on another in one system call
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_nbsendwait"/></docref>
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
 * @param[in] src The capability to receive on.
 * @return A `seL4_MessageInfo_t` structure
 * @xmlonly
 * as described in <autoref label="sec:messageinfo"/>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_NBSendWait(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_CPtr src, seL4_Word *sender);

/**
 * @xmlonly <manual name="Yield" label="sel4_mcs_yield"/> @endxmlonly
 * @brief Yield the remaining timeslice. Periodic threads will not be scheduled again until their
 *        next sporadic replenishment.
 *
 * @xmlonly
 * <docref>See <autoref label="sec:sys_yield"/></docref>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC void
seL4_Yield(void);

/**
 * @xmlonly <manual name="Wait" label="sel4_mcs_wait"/> @endxmlonly
 * @brief Perform a wait on an endpoint or notification object
 *
 * Block on a notification or endpoint waiting for a message. No reply object is
 * required for a Wait. Wait should not be paired with Call, as it does not provide
 * a reply object. If Wait is paired with a Call the waiter will block after recieving
 * the message.
 *
 * @xmlonly
 * <docref>See the description of <nameref name="seL4_Wait"/> in <autoref label="sec:sys_wait"/>.</docref>
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
seL4_Wait(seL4_CPtr src, seL4_Word *sender);

/**
 * @xmlonly <manual name="NBWait" label="sel4_nbwait"/> @endxmlonly
 * @brief Perform a polling wait on an endpoint or notification object
 *
 * Poll a notification or endpoint waiting for a message. No reply object is
 * required for a Wait. Wait should not be paired with Call.
 *
 * @xmlonly
 * <docref>See the description of <nameref name="seL4_NBWait"/> in <autoref label="sec:sys_nbwait"/>.</docref>
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
seL4_NBWait(seL4_CPtr src, seL4_Word *sender);

/**
 * @xmlonly <manual name="Poll" label="sel4_mcs_poll"/> @endxmlonly
 * @brief Perform a non-blocking recv on a notification object
 *
 * This is not a proper system call known by the kernel. Rather, it is a
 * convenience wrapper which calls seL4_NBWait().
 * It is useful for doing a non-blocking wait on a notification.
 *
 * @xmlonly
 * <docref>See the description of <nameref name="seL4_NBWait"/> in <autoref label="sec:sys_nbwait"/>.</docref>
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
 * <docref>as described in <autoref label="sec:messageinfo"/></docref>
 * @endxmlonly
 */
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t
seL4_Poll(seL4_CPtr src, seL4_Word *sender);

/**
 * @xmlonly <manual name="Signal" label="sel4_mcs_signal"/> @endxmlonly
 * @brief Signal a notification
 *
 * This is not a proper system call known by the kernel. Rather, it is a
 * convenience wrapper which calls seL4_Send().
 * It is useful for signalling a notification.
 *
 * @xmlonly
 * <docref>See the description of <nameref name="seL4_Send"/> in <autoref label="sec:sys_send"/>.</docref>
 * @endxmlonly
 *
 * @param[in] dest The capability to be invoked.
 */
LIBSEL4_INLINE_FUNC void
seL4_Signal(seL4_CPtr dest);

/** @} */

