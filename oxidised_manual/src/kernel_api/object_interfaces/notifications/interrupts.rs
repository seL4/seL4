//! A notification dedicated recieving an interupt.
//!
//! A thread can configure the kernel to signal one interrupt object when an inteript triggers. This allows a thread to wait for interupt to occur by calling [SeL4Recv::recv] on that notification.
//! Configurationo of an interupt is done using an [IRQHandler] capability.
//!
//! The system starts without any IRQHandler capabilities present. The ability to create one is provided through the [IRQControl] capability. This capability can be used to produce a single handler for each interrupt available in the system. Typically, the initial thread of a system will determine which IRQs are required by other components in the system, produce the `IRQHandler` cap for each interupt, then delegate this cap as appropriate.

use crate::kernel_api::object_interfaces::capability_space::{CapSpace, Slot};
#[cfg(doc)]
use crate::kernel_api::syscalls::SeL4Recv;
use crate::kernel_api::syscalls::SeL4Result;
use crate::kernel_api::object_interfaces::notifications::Notification;


/// Required to create [IRQHandler] capabilities using its `get` function
pub struct IRQControl;

/// A special [Notification] capability to be used in response to an interupt.
///
/// Typically produced by the systems initial thread; one for each possible interupt source. Will then be distributed throughout the system in accordance to need. For example, system initialization would provide the clock interupt handler to a timer-driver.
pub struct IRQHandler;
impl IRQHandler {
    pub fn get(aathority: &mut IRQControl, irq_num: usize, cspace: &mut CapSpace, slot: Slot) -> SeL4Result<()> {
        unimplemented!()
    } 

    pub fn ack(&self)  -> SeL4Result<()> {
        // a driver typically polls or waits on self.ntfn after an ack
        unimplemented!()
    }
    pub fn set_notification(&mut self, ntfn: Notification)  -> SeL4Result<()> {
        unimplemented!()
    }
    pub fn clear(&mut self)  -> SeL4Result<()> {
        unimplemented!()
    }
}
