//! A threads capability management system.
//!
//!
//! A diagram showing an example layout of a cap-space with 32 bits. Legal, though impractical.
//!
//!  * It has circular references
//!  * Small number of slots
//!
//! Implemented with same principals as a _guard page table_
//! ```text
//!                                       Guard
//!                                      ┌──────────────┐
//!                             ┌──────► │ 0x0(4bits)   │
//!       ┌───────────────┐     │        └───────┬──────┘
//!       │ CapNode       │     │                │
//!       └─────────┬─────┘     │        ┌───────┴──────┐
//!                 │           │        │              │
//!                 │           │    0x00├────────────┬─┤
//!                 │           │        │  CapNode   │┼┼───► No bits remaining
//!        Guard    ▼           │        ├────────────┴─┤
//!       ┌──────────────┐      │        │              │
//! ┌───► │ 0x000(12bits)│      │        ├──────────────┤
//! │     └──────┬───────┘      │        │ Object       │
//! │            │              │        ├──────────────┤
//! │     ┌──────┴───────┐      │        │              │
//! │     │              │      │        ├──────────────┤
//! │ 0x00├────────────┬─┤      │        │ Object       │
//! │     │  CapNode   │┼┼──────┘        ├──────────────┤
//! │     ├────────────┴─┤               │ Object       │
//! │     │              │               ├──────────────┤
//! │     ├──────────────┤               │              │
//! │     │              │               ├──────────────┤
//! │     ├──────────────┤               │              │
//! │     │              │               ├──────────────┤
//! │     ├──────────────┤               │              │
//! │     │              │               ├──────────────┤
//! │     ├──────────────┤               │              │
//! │     │    Object    │           0xFF└──────────────┘
//! │     ├──────────────┤
//! │     │    Object    │                Guard
//! │     ├──────────────┤               ┌──────────────┐
//! │     │              │   ┌──────────►│ o0(3bits)    │
//! │     ├─────────────┬┤   │           └──────┬───────┘
//! │     │ CapNode     │┼───┘                  │
//! │     ├─────────────┴┤               ┌──────┴───────┐
//! │     │              │               │              │
//! │ 0xFF└──────────────┘           0x00├──────────────┤
//! │                                    │ Object       │
//! │                                    ├──────────────┤
//! │                                    │ Object       │
//! │                                    ├──────────────┤
//! │                                    │              │
//! │                                    ├──────────────┤
//! │                                    │              │
//! │                                    ├──────────────┤
//! │                                    │ Object       │
//! │                                    ├──────────────┤
//! │                                    │              │
//! │                                    ├─────────────┬┤
//! │                                    │ CNode       │┼────► 5 bits remaining
//! │                                    ├─────────────┴┤
//! │                                    │              │
//! │                                    ├──────────────┤
//! │                                    │              │
//! │                                    ├──────────────┤
//! └────────────────────────────────────┤ CNode        │
//!                                   0xF└──────────────┘
//! ```
//!
#![allow(unused_variables, dead_code)]

use crate::kernel_api::syscalls::SeL4Result;
#[cfg(doc)]
use {
    crate::*,
    super::thread_control_block::*,
    super::UntypedMemory,
};
use crate::kernel_api::types::{Badge, CapPtr};

/// See Ch3 of seL4 manual
/// these store capabilities, providing permissions to invoke abject methods
/// Each `CapNode` has a fixed number of slots (2^n)
pub struct CapNode;

/// Bits used to address a specific [CapNode]
pub struct Guard {
    pub value: usize,
    pub bits: u8,
}

/// Used to index into a [CapSpace], to find a specific entry of a particular [CapNode]
/// ```text
/// ┌─────────────────────┐
/// │L1 CapNode CapPtr    │
/// └─────────────────────┘
///                 ▼
///       ┌──────────────────┐
/// Guard │ 0x0(4bits)       │
///       └──────────────────┘
///
///   0x00┌──────────────────┐
///       │                  │
///   0x0F├──────────────────┤
///       │ L2 CapNode CapPtr├────────────┐
///       ├──────────────────┤            ▼
///       │                  │        ┌──────────────────┐
///       │                  │  Guard │ 0x0(4bits)       │
///       │                  │        └──────────────────┘
///       │                  │
///       │                  │    0x00┌──────────────────┐
///   0x60├──────────────────┤        │ L3 CapNode CapPtr├────────────┐
///       │ CapA             │        ├──────────────────┤            ▼
///       ├──────────────────┤        │                  │        ┌──────────────────┐
///       │                  │        │                  │  Guard │ (0bits)          │
///       │                  │        │                  │        └──────────────────┘
///       │                  │        │                  │
///       │                  │        │                  │        ┌──────────────────┐
///       │                  │        │                  │    0x00│                  │
///   0xFF└──────────────────┘        │                  │        │                  │
///                               0x60├──────────────────┤        │                  │
/// CapA: addressed with 0x0_60xxxxx  │ CapB             │        │                  │
/// CabB: 0x0_0F_0_60_xx              ├──────────────────┤    0x60├──────────────────┤
/// CapC: 0x0_0F_0_00_60              │                  │        │                  │
/// C-G:                              │                  │        │ Cap C, D, E, F, G│
///  base: 0x0_0f_0_00_60             │                  │    0x64├──────────────────┤
///  window: 5                        │                  │        │                  │
///                                   │                  │        │                  │
/// L2 cap itself:                0xFF└──────────────────┘        │                  │
///  * set depth limit: 12bits                                0xFF└──────────────────┘
///  * 0x0_0F_MASKED
///  * with depth limit of 12, only left-most 12 bits are assesed, preventing a dereference
/// ```
pub struct Slot {
    pub idx: CapPtr,
    pub depth: u8,
}

// pub type CapLookupResult<T> = Result<T, LookupFailure>;

// pub struct GuardMismatchData {
//     /// Number of bits in the capability pointer left to decode
//     pub bits_left: u8,
//     /// The actual guard of the CNode
//     pub guard_found: u8,
//     /// The CNode guard-size
//     pub bits_found: u8,
// }
// pub struct DepthMismatchData {
//     /// Number of bits in the capability pointer left to decode
//     pub bits_left: u8,
//     /// Bits of current CNode being traversed resolved
//     pub bits_found: u8,
// }

// pub enum LookupFailure {
//     /// The root capability is invaled, e.g. not a CNode cap
//     InvalidRoot,
//     /// A capability needed for an invocation is not present
//     /// or doesn't have sufficient rights.
//     /// Provides bits remaining(what does this mean???)
//     MissingCapability(u8),

//     ///When resolving a cap, a CNode was traveresed that:
//     /// * resolved more bits than was left to decode in the cap, or
//     /// * a non-CNode capability was encountered with bits remaining
//     DepthMisMatch(DepthMismatchData),

//     /// When resolving a cap, a Cnode was traveresd
//     /// * With a guard-size larger than the # of remaning bits, OR
//     /// * The CNode's guard did not match the next bits of cap being resolved
//     GuardMismatch(GuardMismatchData),
// }

/// Used in configuring capability permissions
///
/// ```text
/// +-------------+-------------+-------------+-------------+-------------+
/// | Type        | Read        | Write       | Grant       | GrantReply  |
/// +-------------+-------------+-------------+-------------+-------------+
/// | Endpoint    | Receiving   | Sending     | Sending     |Sending reply|
/// |             |             |             | capabilities|capabilities |
/// +-------------+-------------+-------------+-------------+-------------+
/// | Notification| Waiting     | Signalling  | N/A         | N/A         |
/// |             |             |             |             |             |
/// +-------------+-------------+-------------+-------------+-------------+
/// | Page        | Mapping page| Mapping page| N/A         | N/A         |
/// |             | readable    |writable     |             |             |
/// +-------------+-------------+-------------+-------------+-------------+
/// | Reply       | N/A         | N/A         | Sending any | N/A         |
/// |             |             |             | capabilities|             |
/// |             |             |             | in reply    |             |
/// |             |             |             | message     |             |
/// +-------------+-------------+-------------+-------------+-------------+
/// ```
pub struct CapRights {
    pub can_read: bool,
    pub can_write: bool,
    pub can_grant: bool,
    pub can_grantReply: bool,
}

/// A root [CapNode], allowing a [ThreadControlBlock] to manage its capabilities
pub struct CapSpace {
    pub root_cnode: CapNode,
}

/// Rust equivilent of `seL4_CNode_${Func}` functions
impl CapSpace {
    /// Copy a capability, setting its rights in the process
    ///
    /// if `badge` is `None`, then this is equivilant to `seL4_CNode_Copy`
    ///
    /// if dest_root is Some, then the copy ends up in the dest cap_space.
    ///
    /// if dest_root is None, then `dest_slot` must not equal `src_slot`, otherwise
    /// the clash will result in an error.
    ///
    pub fn mint(
        &mut self,
        src_slot: Slot,
        dest_root: Option<&mut CapSpace>,
        dest_slot: Slot,
        rights: CapRights,
        badge: Option<Badge>,
    ) -> SeL4Result<()> {
        panic!();
    }

    /// Moves a capability from an occupied slot to an empty slot
    ///
    /// If `mutation` is a value of `Some(_)`, then it is the equivilant of `seL4_CNode_Mutate`
    ///
    /// same dest_root and slot rules apply as `mint`
    pub fn move_(
        &mut self,
        src_slot: Slot,
        dest_root: Option<&mut CapNode>,
        dest_slot: Slot,
        mutation: Option<Badge>,
    ) -> SeL4Result<()> {
        panic!();
    }

    /// Two moves in a single, atomic operation
    ///
    /// The pivot slot must be distinct from the source and destination
    /// The destination slot must be empty, unless it's the same as the source,
    /// in which case, its content will be swapped with the pivot slot
    ///
    /// analagous to the following, only done atomically
    /// ```
    /// // src != dest
    /// pivot_cspace.move_(pivot, dest_cspace, dest, None);
    /// src_cspace.move_(src, pivot_cspace, pivot, None);
    /// // src == dst
    /// src_cspace.move_(src, tmp_cspace, tmp, None);
    /// // or pivot_cspace.move_(pivot, src_cspace, src, None), as src == dest
    /// pivot_cspace.move_(pivot, dest_cspace, dest);
    /// temp_cspace.move_(temp, pivot_cspace, pivot, None);
    /// ```
    pub fn rotate(
        dest_slot: Slot,
        pivot_root: Option<&mut CapNode>,
        pivot_slot: Slot,
        source_root: Option<&mut CapNode>,
        source_slot: Slot,
    ) -> SeL4Result<()> {
        panic!();
    }

    /// Removes the capability
    pub fn delete(&mut self, slot: Slot) -> SeL4Result<()> {
        panic!();
    }

    /// Equivilent to [CapSpace::delete] on each capability derived from `slot`
    ///
    /// Refer to [UntypedMemory] documentation for further details on
    /// capability derivation.
    pub fn revoke(&mut self, slot: Slot) -> SeL4Result<()> {unimplemented!()}

    /// Save the kernel generated reply capability from the
    /// most recent time the thread was called, placing it
    /// into this CapSpace so it can be used later
    pub fn save_caller(root_capnode: &mut CapNode, slot: Slot) -> SeL4Result<()> {
        panic!();
    }

    /// Allows the reuse of badges by an authority.
    ///
    /// Badged Endpoints only
    ///   -> anything else, will have no effect
    ///
    /// The badged endpoint being looked up
    /// has its list of outstanding send operations
    /// with a matching badge
    pub fn cancel_badged_sends(
        &mut self,
        index: usize,
        // TODO, restrict this to and endpoint only.
    ) -> SeL4Result<()> {
        panic!();
    }
}
