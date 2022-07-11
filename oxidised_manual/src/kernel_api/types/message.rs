
/// A bit-packed word that provides meta-information about an IPC
///
/// TODO: write an example of recieving, and using some of the data in here.
///
/// Not every use of the sel4 system-calls requires direct manipulation of this struct. libsel4 provides many convinience wrappers that handle configuration for common use-cases.


pub struct Info {
    pub label: usize,
    pub caps_unwrapped: usize,
    pub extra_caps: usize,
    pub length: usize,
}


/// A representation of the data transferred in a systemcall.
pub struct IPCBuffer {
    pub tag: Info,
    /// Content
    pub msg: [usize; 120],
    /// Base address of structure.
    /// Used by supporting libraries
    pub user_data: usize,
    /// Buffer for sending caps
    pub caps: [usize ; 120],
    /// Buffer for receiving badges
    pub badges: [usize ; 120],
    /// A CNode to find the recieve slot
    pub recv_idx: super::CapPtr,
    /// number of bits recv_indx is to use
    pub recv_depth: usize,
}
