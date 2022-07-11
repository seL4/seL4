//! aarch64 stuff
//!

/// A capability used to create [ASIDPool] capabilities.
///
/// There is a fixed maximum number of applications the system can support. This capability is used to create [ASIDPool] capabilities, which themselves authorise the creation of a subset of these applications.

pub struct ASIDControl;

/// Specifies the cache behaviour of a page being mapped
pub enum VMAttributes {
    /// A mappings data can be cached
    PageCachable,
    /// A mapping can be parity checked
    ParityEnabled,
    /// A mapping is non-executable
    ExecuteNever,
}

/// The physical address of an underlying frame
///
/// TODO: change err into an `Option<...>`
pub struct PageGetAddress {
    pub err: usize,
    pub paddr: usize,
}

/// Extracted contents of the IPC message when a thread triggers a page fault.
///
/// Replying to the fault IPC will restart the thread.
/// TODO put this into thread stuff?
pub struct VMFault {
    pc_restart: usize,
    addr: usize,
    instruction_fault: bool,
    fault_status_register: usize,
}
use crate::kernel_api::object_interfaces::{
    capability_space::{CapSpace, CapRights, Slot},
    untyped_memory::UntypedMemory,
    vspace::IOSpace,
};

/// A 4k object, that can contain up to 1024 `VSpace`s
///
/// For a vspace to be usable by an application, it must be assigned to an `ASIDPool`
pub struct ASIDPool;
impl ASIDPool {
    /// Uses an `ASIDControl` and an `Untyped` representing a 4k memory object to make a new `ASIDPool`
    pub fn new(authority: &mut ASIDControl, untyped_4k: UntypedMemory, cspace: CapSpace, slot: Slot) {
        unimplemented!()
    }
    /// assings this `ASIDPool` to the `VSpace` passed in
    pub fn assign(&mut self, vspace: VSpace) {
        unimplemented!()
    }
}

/// Documentation not present for aarch64
pub struct IOPageTable;
impl IOPageTable {
    pub fn map(&mut self, iospace: IOSpace, addr: usize) {
        unimplemented!()
    }
    pub fn unmap(self) {
        unimplemented!()
    }
}
pub trait CacheControl {
    /// cleans the data cache range given within this page out to RAM
    fn clean_data(&mut self, start_offset: usize, end_offset_excl: usize);
    /// cleans and invalidates the cache range given within this page and flushes data out to RAM
    fn clean_invalidate_data(&mut self, start_offset: usize, end_offset_excl: usize);
    /// Invalidates the cache range within the page.
    /// The start and end should be alligned to a cache line boundary where possible
    /// If they are not aligned, an additional clean is done to the outer cache lines in full also
    fn invalidate_data(&mut self, start_offset: usize, end_offset_excl: usize);
    fn unify_instructions(&mut self, start: usize, end: usize);
}

/// Level 0 in the 4-level page-table structure.
///
/// Provides a global VSpace, i.e. from 0x0 to 2^47 - 1
pub struct VSpace;
/// Level 1 mapping structure. Is effectively a sparse-array of up to 512
/// entries, each being either a 1GiB frame (HugePage), or a PageDir.
///
/// An example structure to represent this is as follows
/// ```
/// enum Level2 {
///     PageDir,
///     HugePage,
/// }
/// struct PUDSpec {
///     inner: [Option<Level2> ; 512]
/// }
/// ```
///
/// ```
/// // Overview of mapping in address ranges 0xA0_0000_0000..=0xA0_001F_FFFF
/// // and 0xA0_0040_0000..=0xA0_005F_FFFF
/// // our test values
/// let a_ptr: *mut u8 = 0xA0_0000_0000
/// let b_ptr: *mut u8 = 0xA0_001F_FFFF
/// let x_ptr: *mut u8 = 0xA0_0040_0000
/// let y_ptr: *mut u8 = 0xA0_005F_FFFF
/// // To start, no addresses can be mapped
/// // Mapping in a PUD gives 0x80_000_0000
/// // 0xA0_0000_0000..=0xA0_3FFF_FFFF
///
/// // this would cause a VMFault, as it's not yet mapped
/// // let x_val = unsafe {*x};
/// // First, make a PUD. The untyped it's coming from must be 12bits in size
/// let mut pud = PageUpperDir::from_ut(
///     &mut untyped,
///     &cap_space,
///     &mut target_cnode,
///     &target_slot,
/// );
/// // Now, map it in so that we have 1GiB address space that can
/// // take either PageDir mappings, or
/// pud.map(vspace, 0xA0_0000_0000, VMAttributes::default())
///
/// let read_rights = RightsBuilder::new().can_read()..build();
/// let write_rights = RightsBuilder::new().can_write().build();
/// let rw_rights = RightsBuilder::new().can_read().can_write().build();
/// // map it in to read
/// ```
pub struct PageUpperDir;
/// Level 2 in the 4-level page-table structure.
///
/// Address bits 21..=29
pub struct PageDir;
/// Level 3 in the 4-level page-table structure.
///
/// Address bits 12..=20
pub struct PageTable;

impl CacheControl for VSpace {
    fn clean_data(&mut self, start_offset: usize, end_offset_excl: usize) {
        unimplemented!()
    }
    fn clean_invalidate_data(&mut self, start: usize, end: usize) {
        unimplemented!()
    }
    fn invalidate_data(&mut self, start: usize, end: usize) {
        unimplemented!()
    }
    fn unify_instructions(&mut self, start: usize, end: usize) {
        unimplemented!()
    }
}

/// Corresponds to a frame of physical memory that that backs virtual memory pages in a virtual address space
///
/// The virtual address for a mapping must be alligned to page size, and must be mapped to a suitable vspace via all the intermediate paging structures required.
/// To map a page readable and/or writable, the page capability must have read ond/or write permissions.
/// If the permissions of the CapRights given to the mapping exceed tthe rights of the page being mapped, then the mapping permissions are silently down-graded.
pub enum Page {
    /// 1GiB
    Huge,
    /// 2MiB
    Large,
    /// 1KiB
    Small,
}

impl CacheControl for Page {
    fn clean_data(&mut self, start_offset: usize, end_offset_excl: usize) {
        unimplemented!()
    }
    fn clean_invalidate_data(&mut self, start_offset: usize, end_offset_excl: usize) {
        unimplemented!()
    }
    fn invalidate_data(&mut self, start_offset: usize, end_offset_excl: usize) {
        unimplemented!()
    }
    fn unify_instructions(&mut self, start_offset: usize, end_offset_excl: usize) {
        unimplemented!()
    }
}

impl Page {
    /// Get the physical address of the underlying frame
    pub fn get_address(&self) -> PageGetAddress {
        unimplemented!()
    }
    pub fn map_io(&mut self, iospace: IOSpace, rights: CapRights, addr: usize) {
        unimplemented!()
    }

    /// Takes a VSpace capability, and installs a reference to this page in the lowest-level
    /// unmapped paging structure corresponding to vaddr. Fails with `FailedLookup` error if
    /// the required paging structures are not present
    ///
    /// To share a Page, copy its capability, and use this method on said copy to put it into the second address space.
    pub fn map(&mut self, vspace: VSpace, vaddr: usize, rights: CapRights, attr: VMAttributes) {
        unimplemented!()
    }

    /// Removes an existing mapping
    pub fn unmap(&self) {
        unimplemented!()
    }

    /// Changes the permissions of an existing mapping
    pub fn re_map(&mut self, vspace: VSpace, rights: CapRights, attr: VMAttributes) {
        unimplemented!()
    }
}


/// For map/unmap of a paging object in/out of its host object
pub trait Mapping {
    type Host;
    fn map(&mut self, host: Self::Host, vaddr: usize, attr: VMAttributes);
    fn unmap(&self);
}
impl Mapping for PageUpperDir {
    type Host = VSpace;
    fn map(&mut self, pgd: Self::Host, vaddr: usize, attr: VMAttributes) {
        unimplemented!()
    }
    fn unmap(&self) {
        unimplemented!()
    }
}
impl Mapping for PageDir {
    type Host = PageUpperDir;
    fn map(&mut self, pud: Self::Host, vaddr: usize, attr: VMAttributes) {
        unimplemented!()
    }
    fn unmap(&self) {
        unimplemented!()
    }
}
impl Mapping for PageTable {
    type Host = PageDir;

    fn map(&mut self, vspace: Self::Host, vaddr: usize, attr: VMAttributes) {
        unimplemented!()
    }
    fn unmap(&self) {
        unimplemented!()
    }
}
