//! A simple, non-blocking signalling mechanism that logically represents a set of binary semaphores.
//!
//! Can be [`Badge`]ed on creation to provide the reciever with an identity.

pub mod interrupts;
use crate::kernel_api::{
    syscalls::SeL4SendRecv,
    types::Badge,
    syscalls::SeL4Recv,
    syscalls::SeL4Send,
    types::message,
};


/// A simple signaling mechanism.
///
///
/// Built on an word-size array of flags (aka, binary semaphores)
///
/// Operations include:
/// * Signal a subset of flags in one operation.
/// * Poll to check for flags, blocking until any are signalled.
/// * Block, causing the thread it's bound to to wait for one of its notifications to be signalled.
///
/// can be signal-only or wait-only.
pub struct Notification;

impl Notification {
    /// Convenience wrapper around `nb_recv`
    fn poll(&self) -> message::Info {
        unimplemented!()
    }
    /// Convenience wrapper around `send`
    fn signal(&self) {
        unimplemented!()
    }
    /// Convenience wrapper around `recv`
    fn wait(&self) {
        unimplemented!()
    }
}
impl SeL4Send for Notification {
    fn send(&self, msg: message::Info) {
        todo!()
    }
    fn nb_send(&self, msg: message::Info) {
        todo!()
    }
}
impl SeL4Recv for Notification {
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

impl SeL4SendRecv for Notification {
    fn call(&self, msg: message::Info) -> message::Info {
        todo!()
    }
}
