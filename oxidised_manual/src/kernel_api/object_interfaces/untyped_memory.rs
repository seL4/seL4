//! A dynamically sized object from which all other kernel objects are created.

use crate::types::capabilities::FromUntyped;
use core::num::NonZeroUsize;
use crate::kernel_api::syscalls::SeL4Result;
use crate::kernel_api::object_interfaces::capability_space::{CapSpace, Slot };

/// sel4 manual section 2.4
///
/// Foundation of memory allocation.
/// Has a single method that allows creation of new kernel objects
/// On success, calling thread gains capability to newly created object
/// Can be divided into groups of smaller `UntypedMemory` objects.
///  * This allows for delegation of part/all of system memory.
pub struct UntypedMemory;

impl UntypedMemory {


    pub fn retype(
        &mut self,
        desired_type: impl FromUntyped,
        size_bits: u8,
        cap_space: CapSpace,
        cnode: Slot,
        cnode_offset: usize,
        count: NonZeroUsize,
    ) -> SeL4Result<()> {
        unimplemented!()
    }
}
