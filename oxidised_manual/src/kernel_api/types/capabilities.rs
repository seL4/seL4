#[cfg(doc)]
use crate::kernel_api::object_interfaces;
use crate::kernel_api::object_interfaces::{
    capability_space::CapNode,
    endpoints::EndPoint,
    notifications::interrupts::IRQControl,
    notifications::interrupts::IRQHandler,
    notifications::Notification,
    reply::Reply,
    thread_control_block::ThreadControlBlock,
    untyped_memory::UntypedMemory,
    vspace::ASIDControl,
    vspace::ASIDPool,
};

/// Marker trait to indicate a type represents a capability
pub trait Capability {}

/// Not all capabilities can be created from [UntypedMemory::retype] alone. For example
/// ASIDPool and IRQHandler also require ASIDControl and IRQControl respectively.
pub trait FromUntyped: Capability {}

/// A special capability automatically given to reciever of a `Call`
impl Capability for Reply {}

impl Capability for EndPoint {}
impl FromUntyped for EndPoint {}

impl Capability for Notification {}
impl FromUntyped for Notification {}

impl Capability for UntypedMemory {}
impl FromUntyped for UntypedMemory {}

impl Capability for IRQHandler {}

impl Capability for IRQControl {}

impl Capability for ThreadControlBlock {}
impl FromUntyped for ThreadControlBlock {}

impl Capability for CapNode {}
impl FromUntyped for CapNode {}

impl Capability for ASIDPool {}

impl Capability for ASIDControl {}
/// sel4 manual ch8
///
/// Used to construct a virtual address space for 1 or more threads
///
/// virtual memory capabilities are directly related to hardware, ond so are architecture-dependant.
///
/// Kernel also includes ASID Pool and ASID Control objects for tracking Addr. Space. status
///
/// See the [object_interfaces::vspace::aarch64] documentation for more info
pub mod aarch64_vspace {
    use super::*;
    use crate::kernel_api::object_interfaces::vspace::aarch64::{
        PageDir,
        PageTable,
        PageUpperDir,
        VSpace,
    };

    impl Capability for VSpace {}
    impl FromUntyped for VSpace {}

    impl Capability for PageUpperDir {}
    impl FromUntyped for PageUpperDir {}

    impl Capability for PageDir {}
    impl FromUntyped for PageDir {}

    impl Capability for PageTable {}
    impl FromUntyped for PageTable {}
}
