//! [Reply] is a special, "free" capability that is saved to a single, dedicated slot in a thread after it  recieves a message. [reply] is equivilant to [CapSpace::save_caller] followed by a  [SeL4Send::send] 

#[cfg(doc)]
use {
    super::capability_space::*,
    super::syscalls::*,
};
pub struct Reply;
