//! [ASIDPool] and [ASIDControl] for tracking status of an address space.

pub mod aarch64;
use crate::kernel_api::object_interfaces::capability_space::CapSpace;
use crate::types::SeL4Error;
use crate::kernel_api::object_interfaces::untyped_memory::UntypedMemory;
#[cfg(doc)]
use aarch64::VSpace;

/// Documentation not present for aarch64
pub struct IOSpace;

/// The capability from which [ASIDPool] capabilities can be created
pub struct ASIDControl;

/// Confers the right to create a subset the available applications
///
/// For a VSpace to be usable by an app, it must be assigned an ASID using the [ASIDPool::assign] fn
pub struct ASIDPool;
impl ASIDPool {
    /// Gives a [VSpace] an id, by way of placing it in an `ASIDPool`
    ///
    /// This method must be run before a vspace can be used.
    pub fn assign(&mut self, vspace: aarch64::VSpace) -> Result<(), SeL4Error> {
        unimplemented!()
    }
}


impl ASIDControl {
    pub fn make_pool(
        &mut self,
        untyped: UntypedMemory,
        cspace: &mut CapSpace,
    ) -> Result<Self, SeL4Error> {
        unimplemented!()
    }
}
