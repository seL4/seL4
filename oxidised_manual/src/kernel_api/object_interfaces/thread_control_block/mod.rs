//! Representing execution context, and an applications processor usage.
//!
//! Is assinged a [CapSpace] and [VSpace]
//!
//! May also be assigned an [IPCBuffer], without which, a thread is limited to
//! the architecture defined message registers.
//!
//! # Thread Creation
//!
//! A newly created thread is initially inactive. Before activating, it requires
//! to be assigned to it a [CapSpace] and [VSpace] (using the `set_space`(TODO) or `configure` methods)
//! as well as setting the initial stack pointer and instruction pointer (set using
//! the `write_registers` method (TODO))
//!
//! # Thread Activation
//!
//! With the spaces and registers set-up, the TCB can be activated by either setting the
//! `resume_target` parameter in `write_registers` to true, or by calling the `resume` method.
//!
//! # Multi-core machines
//!
//! By default, the thread runs on the same CPU which originally created the TCB. It can
//! be migrated to other CPUs using the `sen_affinity` method
//!
//! # Thread Deactivation
//!
//! A thread can be suspended using the `suspend` method. A suspended thread can then later be `resumed`
//!
//! A suspended state can be retrieved with the `read_registers` and `copy_registers` method
//!
//! A TCB can be reconfigured and reused, or left suspended indefinately if not needed.
//! When the last capability to a TCB is deleted, the thread will automatically suspend.
//!
//! # Scheduling
//!
//! TODO
//!
//! # Exceptions
//!
//! TODO
//!
//! # Exceptions
//!
//! TODO
//!
//! # Message Layout of the Read-/Write-Registers methods
//!
//! TODO
//!
//! # Faults
//!
//! TODO

#[cfg(doc)]
use {
    crate::types::message::IPCBuffer,
};


use crate::kernel_api::object_interfaces::vspace::aarch64::VSpace;
use crate::kernel_api::syscalls::SeL4Result;
use crate::kernel_api::object_interfaces::{
    capability_space::{Guard, CapSpace },
};
use crate::kernel_api::object_interfaces::notifications::Notification;
use crate::types::CapPtr;


/// Representing an execution context. Can be scheduled, blocked, unblocked, etc.
pub struct ThreadControlBlock;

#[repr(C)]
pub struct UserContext {
    pub pc: usize,
    pub sp: usize,
    pub spsr: usize,
    pub x0: usize,
    pub x1: usize,
    pub x2: usize,
    pub x3: usize,
    pub x4: usize,
    pub x5: usize,
    pub x6: usize,
    pub x7: usize,
    pub x8: usize,
    pub x16: usize,
    pub x17: usize,
    pub x18: usize,
    pub x29: usize,
    pub x30: usize,
    pub x9: usize,
    pub x10: usize,
    pub x11: usize,
    pub x12: usize,
    pub x13: usize,
    pub x14: usize,
    pub x15: usize,
    pub x19: usize,
    pub x20: usize,
    pub x21: usize,
    pub x22: usize,
    pub x23: usize,
    pub x24: usize,
    pub x25: usize,
    pub x26: usize,
    pub x27: usize,
    pub x28: usize,
    pub tpidr_el0: usize,
    pub tpidrro_el0: usize,
}

/// TODO
/// Lots more needs to be done here.
/// ``` text
/// pub fn config_single_stepping(&mut self, bp_num: u16, num_inst: Option<NonZeroUsize>)
/// pub fn copy_registers
/// pub fn write_registers
/// pub fn get_breakpoint
/// pub fn set_breakpoint
/// pub fn unset_breakpoint
/// pub fn set_affinity
/// pub fn set_ipc_buffer
/// pub fn set_max_ctrl_priority
/// pub fn set_priority
/// pub fn set_sched_params
/// pub fn set_space
/// pub fn suspend
/// pub fn resume
/// pub fn unbind_notification
/// pub fn set_space
/// ```
impl ThreadControlBlock {
    pub fn bind_notification(&mut self, ntfn: Notification) -> SeL4Result<()> {
        unimplemented!()
    }

    /// The first step in making a thread ready for activation
    ///
    /// If `fault_ep` does not have Write and either Grant, or GrantReply permissions
    /// a double fault will occur (generally the thread is simply suspended)
    pub fn configure(
        &mut self,
        fault_ep: Notification,
        // taking ownership would be a policy statement?
        cap_space: &CapSpace,
        guard: Option<Guard>,
        vspace: VSpace,
        buffer_and_frame: Option<(usize, CapPtr)>,
    ) -> SeL4Result<()> {
        unimplemented!()
    }
    pub fn read_registers(&self) -> SeL4Result<UserContext> {
        unimplemented!()
    }
}
