use crate::types::CapPtr;
#[cfg(doc)]
use crate::types::message;
#[cfg(doc)]
use core::convert::TryFrom;
/// An invalid number used in a system call
pub type InvalidNum = usize;
/// The capability used with a system call in an invalid manner
pub type InvalidCapPtr = CapPtr;
/// Actual memory available
pub type MemoryAvailable = usize;

pub struct AllowedRange {
    pub min_allowed: usize,
    pub max_allowed: usize,
}

pub struct GuardMismatchData {
    /// Number of bits in the capability pointer left to decode
    pub bits_left: u8,
    /// The actual guard of the CNode
    pub guard_found: usize,
    /// The CNode guard-size
    pub bits_found: u8,
}

pub struct DepthMismatchData {
    /// Number of bits in the capability pointer left to decode
    pub bits_left: u8,
    /// Bits of current CNode being traversed resolved
    pub bits_found: u8,
}

/// Deserialised MessageInfo data following a failed CNode lookup.
pub enum LookupFailureDesc {
    /// The root capability is invaled, e.g. not a CNode cap
    InvalidRoot,
    /// A capability needed for an invocation is not present
    /// or doesn't have sufficient rights.
    /// Provides bits remaining(what does this mean???)
    MissingCapability(u8),

    ///When resolving a cap, a CNode was traveresed that:
    /// * resolved more bits than was left to decode in the cap, OR
    /// * a non-CNode capability was encountered with bits remaining
    DepthMisMatch(DepthMismatchData),

    /// When resolving a cap, a Cnode was traveresd
    /// * With a guard-size larger than the # of remaning bits, OR
    /// * The CNode's guard did not match the next bits of cap being resolved
    GuardMismatch(GuardMismatchData),
}


pub type ForSourceCap = bool;
/// This deserialises the error data contained in a [message::Info]
pub enum SeL4Error {
    /// A non-capability argument is invalid
    InvalidArgument(InvalidNum),
    /// A capability is invoked by invalid means
    InvalidCapability(InvalidCapPtr),
    /// A requested operation is not permitted
    IllegalOperation,
    /// An argument is out of the allowed range
    RangeError(AllowedRange),
    /// A supplied argument does not meet the allignment requirements
    AlignmentError,
    /// A capability could not be looked up
    FailedLookup(ForSourceCap, LookupFailureDesc),
    /// Too few message words or capabilities were sent in the message
    TruncatedMessage,
    /// A destination slot specified in the syscall arguments is occupied
    DeleteFirst,
    /// The object currently has other object derived from it
    ///
    /// The requested invocation can be performed when derived objects are deleted, or revoke is invoked on this capability
    RevokeFirst,
    /// Insufficient unallocated space to complete a retype request
    NotEnoughMemory(MemoryAvailable),
}
