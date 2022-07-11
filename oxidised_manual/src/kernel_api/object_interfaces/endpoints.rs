//! The kernel object that Threads use to send/recieve IPC messages.
#![allow(unused_variables, dead_code)]
use crate::kernel_api::syscalls::{SeL4Recv, SeL4Send, SeL4SendRecv };
use crate::kernel_api::types::{Badge, CapPtr, message};

#[cfg(doc)]
use super::*;

/// Attaches to a [ThreadControlBlock] to become a Send/Recieve "port" for IPCs
///
/// IPC is synchronous: A thread sending/recieving on an endpoint
/// will bloock untill delivery.
///  * This implies delivery only happens when both sender & reciever
///    rendezvous at the endpoint.
///
/// An end-point can be restricted to be send-only/recieve only.
/// An end point can have the grant right: allows sending of capabilities.
pub struct EndPoint;

impl EndPoint {
    /// adds a capability to be included in the next message send
    /// Requires Grand permissions on this endpoint
    /// * Without Grant permissions, only raw msg sent (no cap x-fer)
    pub fn load_cap(cap: CapPtr) {
        unimplemented!()
    }
}

impl SeL4Send for EndPoint {
    fn send(&self, msg: message::Info) {
        todo!()
    }
    fn nb_send(&self, msg: message::Info) {
        todo!()
    }
}
impl SeL4Recv for EndPoint {
    fn recv(&self, badge: Option<&mut Badge>) -> message::Info {
        todo!()
    }
    fn nb_recv(&self, badge: Option<&mut Badge>) -> message::Info {
        todo!()
    }
    fn reply_recv(&self, msg: message::Info, badge: Option<&mut Badge>) -> message::Info {
        todo!()
    }
}

impl SeL4SendRecv for EndPoint {
    fn call(&self, msg: message::Info) -> message::Info {
        todo!()
    }
}

